#' Simulate spatially correlated data
#'
#' simulates a spatially correlated response on either a gridded or
#' random unit square. The resulting data can then be used in
#' \code{\link{sim_trial}()}.
#'
#' @param N is the total number of data points (must be a perfect square).
#' @param cortype is the correlation function, which is
#' \code{"Exponential"} by default.
#' @param psill is the partial sill.
#' @param erange is the effective range.
#' @param nugget is the nugget.
#' @param resptype the response type. The default is Gaussian errors.
#' @return a data frame with \itemize{
#'   \item \code{x}, a column with the spatial x-coordinates.
#'   \item \code{y}, a column with the spatial y-coordinates.
#'   \item \code{response}, a column with the response variable.
#' }
#' @examples
#' sim_pop_both(N = 100, cortype = "Exponential", psill = 1, erange = 1, nugget = 0.2)
#' @import stats
#' @export

## simulate population
# need spsurvey and sptotal and dplyr
sim_pop_both <- function(N = 100,
                    cortype = "Exponential", psill = 0.5, erange = 1,
                    nugget = 0.5, resptype = "normal") {

  # simulating the locations

    if (ceiling(sqrt(N)) != floor(sqrt(N))) {
      warning("N is not a perfect square - rounding N up to nearest perfect square")
    }
    ## simulate grid
    sqrN <- ceiling(sqrt(N))
    N <- sqrN ^ 2
    x_grid <- seq(from = 0, to = 1, length.out = sqrN)
    y_grid <- seq(from = 0, to = 1, length.out = sqrN)
    data_grid <- expand.grid(x = x_grid, y = y_grid, method = "grid")

    ## simulate square
    x_rand <- runif(N)
    y_rand <- runif(N)
    data_rand <- data.frame(x = x_rand, y = y_rand, method = "rand")

    data <- dplyr::bind_rows(data_grid, data_rand)
  # simulate the population
  ## make distance matrix
  distmx <- as.matrix(stats::dist(data[ c(1, 2)]))


  ## make covariance matrix
  covmx <- switch(cortype,
                  Exponential = covmx_exp(distmx, psill, erange, nugget)
  )

  ## simulate response

  chol_covmx <- chol(covmx)
  response <- as.vector(t(chol_covmx) %*% rnorm(2 * N))

  if (resptype == "normal") {
    data$response <- response
    }

  if (resptype == "lognormal") {
    data$response <- exp(response)
  }

  ## return data
  data_grid <- data[data$method == "grid", ] # using dplyr::filter gives note about NSE
  data_rand <- data[data$method == "rand", ]
  data <- list(data_grid, data_rand)
  return(data)
}

