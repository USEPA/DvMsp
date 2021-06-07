# load packages (not needed if everything contained in the function)
library(spsurvey)
library(sptotal)

# create simulation function 1
sim1 <- function(seed, ...) { # dots are placeholders for other arguments

  ## sim 1 model specified correctly
  ##     - grts gets grts sample
  ##     - model gets srs
  ##         - one range, one sample size, for now
  ##         - predict/estimate total
  ##
  # simulate data

  # fit models

  # make predictions (sptotal)

  # return relevant output
}

# parallel
n_trials <- 2000
seed <- seq_len(n_trials)
library(parallel) # load parallel
n_cluster <- detectCores() # find cores (48 on mine)
cluster <- makeCluster(n_cluster) # make cluster
clusterEvalQ(cluster, library(spsurvey)) # export spsurvey to cluster
clusterEvalQ(cluster, library(sptotal)) # export sptotal to cluster
sim1_output <- parLapply(cluster, seed, sim1, ...) # could initially use safely(sim1)
stopCluster(cluster) # stop cluster

# model summaries
library(tidyverse)

# do summaries

# write summaries
