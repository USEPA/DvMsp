# write results?
write_out <- FALSE

# source functions for now
source("R/sim_pop.R")
source("R/sim_one.R")

# parallel
n_trials <- 16 * 65 #(1040)
seed <- seq_len(n_trials)
library(parallel) # load parallel
library(purrr) # load purrr
n_cluster <- detectCores() # find cores (48 on mine)
cluster <- makeCluster(n_cluster) # make cluster
clusterExport(cluster, varlist = c("sim_pop", "covmx_exp"))
clusterEvalQ(cluster, library(spsurvey)) # export spsurvey to cluster
clusterEvalQ(cluster, library(sptotal)) # export sptotal to cluster
clusterEvalQ(cluster, library(dplyr)) # export sptotal to cluster
sim1_output <- parLapply(cluster, seed, safely(sim_one), N = 30^2, n = 150, psill = 0.9, nugget = 0.1, erange = sqrt(4)) # could initially use safely(sim1)
stopCluster(cluster) # stop cluster

# model summaries
library(dplyr)

# check errors
map(sim1_output, "error") %>%
  bind_rows() %>%
  nrow()

# do summaries
safe_output <- map(sim1_output, "result") %>%
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
  write_csv(all_output, "inst/output/sim_one/all_output.csv")
  write_csv(summ_output, "inst/output/sim_one/summ_output.csv")
}

