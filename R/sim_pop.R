## simulate population
# need spsurvey and sptotal and dplyr
sim_pop <- function(N = 100, n = 50, gridded = TRUE, cortype, psill, erange, nugget, ...) {

  # simulating the locations
  if (gridded) {
    if (ceiling(sqrt(N)) != floor(sqrt(N))) {
      warning("N is not a perfect square - rounding N up to nearest perfect square")
    }
    ## simulate grid
    sqrN <- ceiling(sqrt(N))
    N <- sqrN^2
    x <- seq(from = 0, to = 1, length.out = sqrN)
    y <- seq(from = 0, to = 1, length.out = sqrN)
    data <- expand.grid(x = x, y = y)
  } else {
    ## simulate square
    x <- rnorm(N)
    y <- rnorm(N)
    data <- data.frame(x = x, y = y)
  }

  # simulate the population
  ## make distance matrix
  distmx <- as.matrix(dist(data, ...))
  ## make covariance matrix
  covmx <- switch(cortype,
                  Exponential = covmx_exp(distmx, psill, erange, nugget)
                  )
  ## simulate response
  chol_covmx <- chol(covmx)
  data$response <- as.vector(t(chol_covmx) %*% rnorm(N))

  ## return data
  data
}

covmx_exp <- function(distmx, psill, erange, nugget) {
  psill * exp(-3 * distmx / erange) + nugget * (distmx == 0)
}

# sim_pop(N = 100, n = 50, gridded = TRUE, cortype = "exponential", psill = 1, erange = 1, nugget = 0.2)
