#' Conduct a simulation trial
#'
#' Take a sample from and subsequently analyze spatially correlated data
#' using \enumerate{
#'          \item GRTS sample, analyzed with GRTS and FPBK
#'          \item SRS sample, analyzed with traditional SRS and FPBK
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
#' sim_trial(seed = sample.int(1e7, size = 1), N = 100, n = 50,
#' gridded = TRUE, cortype = "Exponential", psill = 1, erange = 1,
#' nugget = 0.2, cortype_est = "Exponential")
#' @import stats
#' @export

## library(devtools)
## install_github("https://github.com/highamm/sptotal")
## install_github('USEPA/spsurvey', ref = "develop_5.0")
## library(sptotal)
## library(spsurvey)
## library(dplyr)
sim_trial <- function(seed = sample.int(1e7, size = 1),
                    N = 100, n = 50, gridded = TRUE,
                    cortype = "Exponential", psill, erange,
                    nugget, cortype_est = "Exponential", resptype = "normal", ...) {
  # browser()
  ###############################
  ## Data Prep
  ###############################

  # set a reproducible seed
  set.seed(seed)

  # simulate some data
  data <- sim_pop(N, gridded, cortype, psill, erange, nugget, resptype, ...)

  # make an sf object (for spsurvey)
  data_sf <- sf::st_as_sf(data, coords = c("x", "y"), crs = 5070)

  ###############################
  ## IRS Sample
  ###############################

  # irs sample
  irs_samp <- irs(data_sf, n_base = n, ...)

  # put it together
  irs_bind <- sprbind(irs_samp, ...)

  # get coordinates
  irs_coords <- sf::st_coordinates(irs_bind)

  # make data frame
  irs_df <- data.frame(
    response = irs_bind$response,
    x = irs_coords[, "X"],
    y = irs_coords[, "Y"],
    siteID = irs_bind$siteID,
    wgt = irs_bind$wgt
  )

  # make data frame removing id and wgt
  irs_coords_resp <- irs_df %>%
    select(-siteID, -wgt)

  # find unsampled sites
  irs_unsamp <- anti_join(data, irs_coords_resp)

  # make response NA for unsampled
  irs_unsamp$response <- NA

  # only keep necessary columns
  irs_unsamp <- irs_unsamp %>%
    select(response, x, y)

  # put them together
  irs_full <- dplyr::bind_rows(irs_coords_resp, irs_unsamp)

  # add weights
  irs_full$wts <- 1 / nrow(irs_full)

  ###############################
  ## GRTS Sample
  ###############################

  # grts sample
  grts_samp <- grts(data_sf, n_base = n, ...)

  # put it together
  grts_bind <- sprbind(grts_samp, ...)

  # get coordinates
  grts_coords <- sf::st_coordinates(grts_bind)

  # make data frame
  grts_df <- data.frame(
    response = grts_bind$response,
    x = grts_coords[, "X"],
    y = grts_coords[, "Y"],
    siteID = grts_bind$siteID,
    wgt = grts_bind$wgt
  )

  # make data frame removing id and wgt
  grts_coords_resp <- grts_df %>%
    select(-siteID, -wgt)

  # find unsampled sites
  grts_unsamp <- anti_join(data, grts_coords_resp)

  # make response NA for unsampled
  grts_unsamp$response <- NA

  # only keep necessary columns
  grts_unsamp <- grts_unsamp %>%
    select(response, x, y)

  # put them together
  grts_full <- dplyr::bind_rows(grts_coords_resp, grts_unsamp)

  # add weights
  grts_full$wts <- 1 / nrow(grts_full)


  ##############################
  ## IRS Sample FPBK Analysis
  ##############################

  irs_model <- slmfit(formula = response ~ 1,
                         data = irs_full, xcoordcol = "x",
                         ycoordcol = "y", CorModel = cortype_est)
  ## sptotal::
  irs_pred <- predict(irs_model, wtscol = "wts")
  irs_model_mean <- irs_pred$FPBK_Prediction
  irs_model_se <- sqrt(irs_pred$PredVar)
  irs_model_lb <- irs_model_mean + -1 * 1.96 * irs_model_se
  irs_model_ub <- irs_model_mean + 1 * 1.96 * irs_model_se

  ###############################
  ## IRS Sample IRS Analysis
  ###############################

  irs_analysis <- cont_analysis(irs_df, vars = "response", siteID = "siteID", weight = "wgt", vartype = "SRS", fpc = N, statistics = "mean")$Mean
  irs_analysis_mean <- irs_analysis$Estimate
  irs_analysis_se <- irs_analysis$StdError
  irs_analysis_lb <- irs_analysis$LCB95Pct
  irs_analysis_ub <- irs_analysis$UCB95Pct

  ##############################
  ## GRTS Sample FPBK Analysis
  ##############################

  grts_model <- slmfit(formula = response ~ 1,
                      data = grts_full, xcoordcol = "x",
                      ycoordcol = "y", CorModel = cortype_est)
  ## sptotal::
  grts_pred <- predict(grts_model, wtscol = "wts")
  grts_model_mean <- grts_pred$FPBK_Prediction
  grts_model_se <- sqrt(grts_pred$PredVar)
  grts_model_lb <- grts_model_mean + -1 * 1.96 * grts_model_se
  grts_model_ub <- grts_model_mean + 1 * 1.96 * grts_model_se

  ###############################
  ## GRTS Sample GRTS Analysis
  ###############################

  grts_analysis <- cont_analysis(grts_df, vars = "response", siteID = "siteID",
                                 weight = "wgt", xcoord = "x", ycoord = "y", statistics = "mean")$Mean
  grts_analysis_mean <- grts_analysis$Estimate
  grts_analysis_se <- grts_analysis$StdError
  grts_analysis_lb <- grts_analysis$LCB95Pct
  grts_analysis_ub <- grts_analysis$UCB95Pct

  ###############################
  ## Return Output
  ###############################

  # store realized values
  realized_mean <- mean(data$response)
  realized_var <- var(data$response)

  # store output
  output <- data.frame(
    approach = c("Design IRS", "Design GRTS", "Model IRS", "Model GRTS"),
    seed = c(seed, seed, seed, seed),
    true_mean = c(realized_mean, realized_mean, realized_mean,
                  realized_mean),
    true_var = c(realized_var, realized_var, realized_var,
                 realized_var),
    estimate = c(irs_analysis_mean,
                 grts_analysis_mean, as.vector(irs_model_mean),
                 as.vector(grts_model_mean)),
    sd = c(irs_analysis_se, grts_analysis_se, as.vector(irs_model_se),
           as.vector(grts_model_se)),
    lb = c(irs_analysis_lb, grts_analysis_lb, as.vector(irs_model_lb),
           as.vector(grts_model_lb)),
    ub = c(irs_analysis_ub, grts_analysis_ub, as.vector(irs_model_ub),
           as.vector(grts_model_ub))
  )

  # return output
  output
}



## example
## data <- sim_trial(psill = 1, erange = 1, nugget = 1)
