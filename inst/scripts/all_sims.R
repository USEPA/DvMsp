library(tidyverse)
library(parallel)

# write results?
write_out <- TRUE

## set up grid for simulation settings
N <- 30 ^ 2
n <- c(10, 50, 100, 200)
total_var <- 2
psill_ratio <- c(0, 0.5, 0.9)
erange <- sqrt(4)
gridded <- c(TRUE, FALSE)
resptype <- c("normal", "lognormal")

parm_df <- expand_grid(N, n, total_var, psill_ratio,
                       erange, gridded, resptype) %>%
  mutate(nugget_ratio = 1 - psill_ratio,
         psill = total_var * psill_ratio,
         nugget = total_var * nugget_ratio)

# source functions for now
source("R/sim_pop.R")
source("R/sim_trial.R")

## loop through each row of parm_df
for (i in 1:nrow(parm_df)) {
  n_trials <- 5
  seed <- sample.int(1e7, size = n_trials)
  n_cluster <- detectCores() # find cores (48 on mine)
  cluster <- makeCluster(n_cluster) # make cluster
  clusterExport(cluster, varlist = c("sim_pop", "covmx_exp"))
  clusterEvalQ(cluster, library(spsurvey)) # export spsurvey to cluster
  clusterEvalQ(cluster, library(sptotal)) # export sptotal to cluster
  clusterEvalQ(cluster, library(dplyr)) # export sptotal to cluster

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
    erange = parm_df_sim %>% pull(erange), # effective range
    gridded = parm_df_sim %>% pull(gridded), # grid (TRUE) or random (FALSE) both in [0, 1] x [0,1]
    resptype = parm_df_sim %>% pull(resptype) # normal response
  )

  stopCluster(cluster) # stop cluster

  # check errors
  # map(sim_output, "error") %>%
  #   bind_rows() %>%
  #   nrow()

  # do summaries
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
      coverage = mean(cover),
      prcloser = mean(closer),
      med_ci_len = median(ub - lb)
    ) %>%
    mutate(parm_df %>% slice(i)) ## add parameters to data frame


  if (write_out == TRUE) {
    library(readr)
    write_csv(all_output, glue::glue("inst/output/sims914raw/allsims_raw", i, ".csv"))
    write_csv(summ_output, glue::glue("inst/output/sims914/allsims", i, ".csv"))
  }

  print(i)
}


