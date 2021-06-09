#' Simulate spatially correlated data
#'
#' simulates a spatially correlated response on either a gridded or
#' random unit square. The resulting data can then be used in
#' \code{\link{sim_one}()}.
#'
#' @param N is the total number of data points. For \code{gridded = TRUE},
#' this must be a perfect square.
#' @param gridded \code{TRUE} for gridded sites and \code{FALSE} for
#' points with random locations.
#' @param cortype is the correlation function, which is
#' \code{"Exponential"} by default.
#' @param psill is the partial sill.
#' @param erange is the effective range.
#' @param nugget is the nugget.
#' @param ... further arguments passed to or from other methods.
#' @return a data frame with \itemize{
#'   \item \code{x}, a column with the spatial x-coordinates.
#'   \item \code{y}, a column with the spatial y-coordinates.
#'   \item \code{response}, a column with the response variable.
#' }
#' @examples
#' sim_pop(N = 100, gridded = TRUE, cortype = "Exponential", psill = 1, erange = 1, nugget = 0.2)
#' @import stats
#' @export

## simulate population
# need spsurvey and sptotal and dplyr
sim_pop <- function(N = 100, gridded = TRUE,
                    cortype = "Exponential", psill, erange,
                    nugget, ...) {

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
  distmx <- matrix(0, nrow = N, ncol = N)
  distmx[lower.tri(distmx)] <- stats::dist(as.matrix(data))
  distmx <- distmx + t(distmx)


  ## make covariance matrix
  covmx <- switch(cortype,
                  Exponential = covmx_exp(distmx, psill, erange, nugget)
                  )
  ## simulate response
  chol_covmx <- chol(covmx)
  data$response <- as.vector(t(chol_covmx) %*% rnorm(N))

  ## return data
  return(data)
}

#' Spatial Correlation Models
#'
#' Note that, currently, only one of these models is implemented
#' in this package: \code{covmx_exp()}.
#'
#' @param distmx is the distance matrix for sampled sites.
#' @param psill is the partial sill.
#' @param erange is the effective range.
#' @param nugget is the nugget effect.
#' @return Covariance Matrix

#' @describeIn covmx_exp Exponential Covariance
covmx_exp <- function(distmx, psill, erange, nugget) {
  psill * exp(-3 * distmx / erange) + nugget * (distmx == 0)
}

#' @describeIn covmx_exp Gaussian Covariance
covmx_gauss <- function(distmx, psill, erange, nugget) {
  psill * exp(-sqrt(3) * distmx ^ 2 / erange) +
    nugget * (distmx == 0)
}


