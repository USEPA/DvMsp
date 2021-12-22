library(tidyverse)
library(parallel)

# write results?
write_out <- TRUE

## set up grid for simulation settings
N <- 30 ^ 2
n <- c(50, 100, 200)
psill_ratio <- c(0, 0.5, 0.9)
range <- sqrt(2) / 3
gridded <- c(TRUE, FALSE)
resptype <- c("normal", "lognormal")
total_var_norm <- 2
# find variance that gives lognormal var of 2
total_var_lognorm <- uniroot(function(x) (exp(x) - 1) * exp(x) - total_var_norm, interval = c(0, 10))$root

parm_df <- expand_grid(N, n, psill_ratio,
                       range, gridded, resptype) %>%
  mutate(total_var = case_when(
    resptype == "normal" ~ total_var_norm,
    resptype == "lognormal" ~ total_var_lognorm
  )) %>%
  mutate(nugget_ratio = 1 - psill_ratio,
         psill = total_var * psill_ratio,
         nugget = total_var * nugget_ratio)

library(DvMsp)

## loop through each row of parm_df (could be done with purrr::pmap() instead)
for (i in 1:nrow(parm_df)) {
  n_trials <- 2000
  seed <- 1:n_trials
  n_cluster <- detectCores() # find cores (48 on mine)
  cluster <- makeCluster(n_cluster) # make cluster
  clusterEvalQ(cluster, library(DvMsp)) # export DvMsp to cluster
  clusterEvalQ(cluster, library(spsurvey)) # export spsurvey to cluster v >= 5.1.0
  clusterEvalQ(cluster, library(sptotal)) # export sptotal to cluster
  clusterEvalQ(cluster, library(dplyr)) # export dplyr to cluster

  parm_df_sim <- parm_df %>% slice(i) ## grab row for ith parameter combo
  sim_output <- parLapply(
    cluster, # the cluster
    seed, # some seeds
    safely(sim_trial), # see if any errors
    ## pull each parameter from the single row data frame
    N = parm_df_sim %>% pull(N), # pop size
    n = parm_df_sim %>% pull(n), # sample size
    psill = parm_df_sim %>% pull(psill), # partial sill
    nugget = parm_df_sim %>% pull(nugget), # nugget
    range = parm_df_sim %>% pull(range), # range
    gridded = parm_df_sim %>% pull(gridded), # grid (TRUE) or random (FALSE) both in [0, 1] x [0,1]
    resptype = parm_df_sim %>% pull(resptype) # normal response
  )

  stopCluster(cluster) # stop cluster

  # check errors (deprecated)
  # map(sim_output, "error") %>%
  #   bind_rows() %>%
  #   nrow()

  # do summaries, could be done with a for loop instead ;)
  safe_output <- map(sim_output, "result") %>%
    bind_rows()

  # write output
  all_output <- safe_output %>%
    mutate(
      resid = true_mean - estimate,
      cover = 1 * (lb <= true_mean & true_mean <= ub)
    ) %>%
    group_by(seed) %>%
    mutate(closer = case_when(
      resid == min(resid) ~ 1,
      resid != min(resid) ~ 0
    )) %>%
    mutate(parm_df %>% slice(i)) ## add parameters to data frame

  # write summaries
  summ_output <- all_output %>%
    group_by(approach) %>%
    summarize(
      mpbias = mean(resid),
      rmspe = sqrt(mean(resid^2)),
      medae = median(abs(resid)),
      mse = mean(sd),
      coverage = mean(cover),
      prcloser = mean(closer),
      med_ci_len = median(ub - lb)
    ) %>%
    mutate(parm_df %>% slice(i)) ## add parameters to data frame


  if (write_out == TRUE) {
    library(readr)
    write_csv(all_output, glue::glue("inst/output/simulation_raw/allsims_raw", i, ".csv"))
    write_csv(summ_output, glue::glue("inst/output/simulation_summary/allsims", i, ".csv"))
  }

  print(i)
}


