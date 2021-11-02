#' Simulate spatially correlated data
#'
#' simulates a spatially correlated response on either a gridded or
#' random unit square. The resulting data can then be used in
#' \code{\link{sim_trial}()}.
#'
#' @param N is the total number of data points. For \code{gridded = TRUE},
#' this must be a perfect square.
#' @param gridded \code{TRUE} for gridded sites and \code{FALSE} for
#' points with random locations.
#' @param cortype is the correlation function type used to generate the population.
#' Available options include
#' \code{"Exponential"} for the exponential correlation, \code{"Gaussian"} for
#' the Gaussian correlation, and \code{"Spherical"} for the spherical correlation.
#' The default is \code{"Exponential"}.
#' @param psill is the partial sill.
#' @param range is the range.
#' @param nugget is the nugget.
#' @param resptype the response type of the errors. Available options:
#' \code{"normal"} for Gaussian errors and \code{"lognormal"} for log Gaussian
#' errors. The default is Gaussian errors.
#' @param mu is the mean of response vector and is \code{0} for all population
#' units by default.
#' @param ... further arguments passed to or from other methods.
#' @return a data frame with \itemize{
#'   \item \code{x}, a column with the spatial x-coordinates.
#'   \item \code{y}, a column with the spatial y-coordinates.
#'   \item \code{response}, a column with the response variable.
#' }
#' @examples
#' sim_pop(N = 100, gridded = TRUE, cortype = "Exponential", psill = 1,
#' range = 1, nugget = 0.2, resptype = "normal", mu = 0)
#' @import stats
#' @export

## simulate population
# need spsurvey and sptotal and dplyr
sim_pop <- function(N = 100, gridded = TRUE,
                    cortype = "Exponential", psill, range,
                    nugget, resptype = "normal", mu = 0, ...) {

  # simulating the locations
  if (gridded) {
    if (ceiling(sqrt(N)) != floor(sqrt(N))) {
      warning("N is not a perfect square - rounding N up to nearest perfect square")
    }
    ## simulate grid
    sqrN <- ceiling(sqrt(N))
    N <- sqrN ^ 2
    x <- seq(from = 0, to = 1, length.out = sqrN)
    y <- seq(from = 0, to = 1, length.out = sqrN)
    data <- expand.grid(x = x, y = y)
  } else {
    ## simulate square
    x <- runif(N)
    y <- runif(N)
    data <- data.frame(x = x, y = y)
  }

  # simulate the population
  ## make distance matrix
  distmx <- as.matrix(stats::dist(data))

  ## make covariance matrix
  covmx <- switch(cortype,
                  Exponential = covmx_exp(distmx, psill, range, nugget),
                  Gaussian = covmx_gauss(distmx, psill, range, nugget),
                  Spherical = covmx_gauss(distmx, psill, range, nugget))

  ## simulate response
  chol_covmx <- chol(covmx)
  response <- mu + as.vector(t(chol_covmx) %*% rnorm(N))

  if (resptype == "normal") {
    data$response <- response
  }

  if (resptype == "lognormal") {
    data$response <- exp(response)
  }

  ## return data
  return(data)
}

#' Spatial Correlation Models
#'
#' @param distmx is the distance matrix for sampled sites.
#' @param psill is the partial sill.
#' @param range is the effective range.
#' @param nugget is the nugget effect.
#' @return Covariance Matrix
#' @noRd

# Exponential Covariance
covmx_exp <- function(distmx, psill, range, nugget) {
  psill * exp(- distmx / range) + nugget * (distmx == 0)
}

# Gaussian Covariance
covmx_gauss <- function(distmx, psill, range, nugget) {
  psill * exp(- distmx^2 / range^2) +
    nugget * (distmx == 0)
}

# Spherical Covariance
covmx_sph <- function(distmx, psill, range, nugget) {
  distratio <- distmx / range
  psill * (1 - (3 / 2) * distratio + (1 / 2) * distratio^3) + nugget * (distmx == 0)
}



