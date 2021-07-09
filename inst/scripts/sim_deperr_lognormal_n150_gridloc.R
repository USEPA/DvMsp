# write results?
write_out <- TRUE

# source functions for now
source("R/sim_pop.R")
source("R/sim_trial.R")


# parallel
n_trials <- 2000
seed <- seq_len(n_trials)
library(parallel) # load parallel
library(purrr) # load purrr
n_cluster <- detectCores() # find cores (48 on mine)
cluster <- makeCluster(n_cluster) # make cluster
clusterExport(cluster, varlist = c("sim_pop", "covmx_exp"))
clusterEvalQ(cluster, library(spsurvey)) # export spsurvey to cluster
clusterEvalQ(cluster, library(sptotal)) # export sptotal to cluster
clusterEvalQ(cluster, library(dplyr)) # export sptotal to cluster
sim_output <- parLapply(
  cluster, # the cluster
  seed, # some seeds
  safely(sim_trial), # see if any errors
  N = 30^2, # pop size
  n = 150, # sample size
  psill = 0.9, # partial sill
  nugget = 0.1, # nugget
  erange = sqrt(4), # effective range
  gridded = TRUE, # grid (TRUE) or random (FALSE) both in [0, 1] x [0,1]
  resptype = "lognormal" # normal response
)
stopCluster(cluster) # stop cluster

# model summaries
library(dplyr)

# check errors
map(sim_output, "error") %>%
  bind_rows() %>%
  nrow()

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
  ))

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
  )

if (write_out) {
  library(readr)
  # write_csv(all_output, "inst/output/sim_one/all_output.csv")
  write_csv(summ_output, "inst/output/sim_deperr_lognormal_n150_gridloc.csv")
}

