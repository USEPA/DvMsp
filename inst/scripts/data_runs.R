library(tidyverse)
library(parallel)
library(DvMsp)

# write results?
write_out <- TRUE

n <- c(50, 100, 200)
vars <- c("TOTALHG_RESULT", "MMI_ZOOP_NLA6")
parm_df <- expand_grid(n = n, var = vars)

n_trials <- 2000
# set overall seed
set.seed(2)
parm_df$seed <- lapply(seq_len(NROW(parm_df)), function(x) sample(1e9, size = n_trials))

data("nla_df")

## loop through each row of parm_df (could be done with purrr::pmap() instead)
for (i in 1:nrow(parm_df)) {
  n_cluster <- detectCores() # find cores (48 on mine)
  cluster <- makeCluster(n_cluster) # make cluster
  clusterEvalQ(cluster, library(DvMsp)) # export DvMsp to cluster
  clusterEvalQ(cluster, library(spsurvey)) # export spsurvey to cluster v >= 5.1.0
  clusterEvalQ(cluster, library(sptotal)) # export sptotal to cluster
  clusterEvalQ(cluster, library(dplyr)) # export dplyr to cluster

  parm_df_sim <- parm_df %>% slice(i) ## grab row for ith parameter combo
  sim_output <- parLapply(
    cluster, # the cluster
    unlist(parm_df_sim %>% pull(seed)), # some seeds
    safely(data_trial), # see if any errors
    data = nla_df,
    ## pull each parameter from the single row data frame
    var = parm_df_sim %>% pull(var), # variable
    n = parm_df_sim %>% pull(n) # sample size
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
    write_csv(all_output, glue::glue("inst/output/data_raw/datasims_raw", i, ".csv"))
    write_csv(summ_output, glue::glue("inst/output/data_summary/datasims", i, ".csv"))
  }

  print(i)
}

