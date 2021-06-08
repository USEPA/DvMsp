#' Simulate spatially correlated data
#'
#' Take a sample from and subsequently analyze spatially correlated data
#' using \enumerate{
#'          \item GRTS with a spatially balanced sample
#'          \item FPBK with a simple random sample
#' }
#'
#' @param seed is a random seed, by default a random integer.
#' @param N is the total number of data points.
#'  For \code{gridded = TRUE}, this must be a perfect square.
#' @param n is the number of data points sampled.
#' @param gridded \code{TRUE} for gridded sites and \code{FALSE} for
#' points with random locations.
#' @param cortype is the true correlation function, which is
#' \code{"Exponential"} by default.
#' @param psill is the partial sill.
#' @param erange is the effective range.
#' @param nugget is the nugget.
#' @param cortype_est is the (possibly misspecified) correlation
#' function used to estimate/predict the mean, total, or other quantity.
#' @param ... further arguments passed to or from other methods.
#' @return a data frame with \itemize{
#'   \item \code{approach}, the name of th approach (\code{"Degign"}
#'   or \code{"Model"}).
#'   \item \code{seed}, a non-random integer seed.
#'   \item \code{true_mean}, the realized mean from the entire population.
#'   \item \code{true_var}, the realized variance from the entire population.
#'   \item \code{estimate}, the estimated/predicted mean.
#'   \item \code{sd}, the standard error of the estimated/predicted mean.
#'   \item \code{lb}, a lower 95% confidence bound
#'   \item \code{ub}, an upper 95% confidence bound.
#' }
#' @examples
#' sim_one(seed = sample.int(1e7, size = 1), N = 100, n = 50, gridded = TRUE, cortype = "Exponential", psill = 1, erange = 1, nugget = 0.2, cortype_est = "Exponential")
#' @export

sim_one <- function(seed = sample.int(1e7, size = 1),
                    N = 100, n = 50, gridded = TRUE,
                    cortype = "Exponential", psill, erange,
                    nugget, cortype_est = "Exponential", ...) {
  set.seed(seed)
  data <- sim_pop(N, n, gridded, cortype, psill, erange, nugget, ...)
  irs_samp <- dplyr::sample_n(data, n)
  irs_unsamp <- dplyr::anti_join(data, irs_samp)
  irs_unsamp$response <- NA
  full_df <- bind_rows(irs_samp, irs_unsamp)
  full_df$wts <- 1 / nrow(full_df)
  mod <- slmfit(formula = response ~ 1, data = full_df, xcoordcol = "x",
                ycoordcol = "y", CorModel = cortype_est)

  pred_mod <- predict(mod, wtscol = "wts")
  model_mean <- pred_mod$FPBK_Prediction
  model_se <- sqrt(pred_mod$PredVar)
  model_lb <- model_mean + -1 * 1.96 * model_se
  model_ub <- model_mean + 1 * 1.96 * model_se

  # browser()
  # take a sample
  ## convert data to sf object (for spsurvey)
  data <- st_as_sf(data, coords = c("x", "y"), crs = 5070)
  ## select grts sample
  grts_samp <- grts(data, n_base = n, ...)
  ## convert to usable
  grts_bind <- sprbind(grts_samp, ...)
  ## get coordinates
  grts_coords <- st_coordinates(grts_bind)
  ## make data frame
  grts_df <- data.frame(
    response = grts_bind$response,
    x = grts_coords[, "X"],
    y = grts_coords[, "Y"],
    siteID = grts_bind$siteID,
    wgt = grts_bind$wgt
  )
  ## select irs sample
  irs_samp <- irs(data, n_base = n, ...)
  ## convert to usable
  irs_bind <- sprbind(irs_samp, ...)
  ## get coordinates
  irs_coords <- st_coordinates(irs_bind)
  ## make data frame
  irs_df <- data.frame(
    response = irs_bind$response,
    x = grts_coords[, "X"],
    y = grts_coords[, "Y"]
  )

  # analyze the sample
  ## design based
  design_analysis <- cont_analysis(
    grts_df,
    siteID = "siteID",
    vars = "response",
    weight = "wgt",
    xcoord = "x",
    ycoord = "y"
  )
  ## just return the mean info
  design_mean <- subset(design_analysis$Pct, Statistic == "Mean")

  realized_mean <- mean(data$response)
  realized_var <- var(data$response)

  # store output
  output <- data.frame(
    approach = c("Design", "Model"),
    seed = c(seed, seed),
    true_mean = c(realized_mean, realized_mean),
    true_var = c(realized_var, realized_var),
    estimate = c(design_mean$Estimate, as.vector(model_mean)),
    sd = c(design_mean$StdError, as.vector(model_se)),
    lb = c(design_mean$LCB95Pct, as.vector(model_lb)),
    ub = c(design_mean$UCB95Pct, as.vector(model_ub))
  )

  output
}



## example
## data <- sim_one(psill = 1, erange = 1, nugget = 1)
