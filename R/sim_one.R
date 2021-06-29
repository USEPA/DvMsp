#' Simulate spatially correlated data
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
#' sim_one(seed = sample.int(1e7, size = 1), N = 100, n = 50,
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


## irs(same_arguments_to_grts) to select irs
## cont_analysis(same_arguments, vartype = "SRS")
##

sim_one <- function(seed = sample.int(1e7, size = 1),
                    N = 100, n = 50, gridded = TRUE,
                    cortype = "Exponential", psill, erange,
                    nugget, cortype_est = "Exponential", ...) {
  set.seed(seed)
  data <- sim_pop(N, gridded, cortype, psill, erange, nugget, ...)

  ###############################
  ## obtain IRS Sample ##########
  ###############################

  irs_samp <- dplyr::sample_n(data, n)
  irs_unsamp <- dplyr::anti_join(data, irs_samp)
  irs_unsamp$response <- NA
  full_df <- dplyr::bind_rows(irs_samp, irs_unsamp)
  full_df$wts <- 1 / nrow(full_df)

  ##############################
  ## FPBK Analysis on IRS Sample
  ##############################

  mod <- sptotal::slmfit(formula = response ~ 1,
                         data = full_df, xcoordcol = "x",
                         ycoordcol = "y", CorModel = cortype_est)
  ## sptotal::
  pred_mod <- predict(mod, wtscol = "wts")
  model_mean <- pred_mod$FPBK_Prediction
  model_se <- sqrt(pred_mod$PredVar)
  model_lb <- model_mean + -1 * 1.96 * model_se
  model_ub <- model_mean + 1 * 1.96 * model_se

  ###############################
  ## IRS Analysis on IRS Sample
  ###############################

  irs_mean_irs_samp <- mean(irs_samp$response)
  irs_se_irs_samp <- sqrt((var(irs_samp$response) / n) * (N - n) / N)
  irs_lb_irs_samp <- irs_mean_irs_samp - 1.96 * irs_se_irs_samp
  irs_ub_irs_samp <- irs_mean_irs_samp + 1.96 * irs_se_irs_samp

  # take a sample
  ## convert data to sf object (for spsurvey)
  data_sf <- sf::st_as_sf(data, coords = c("x", "y"), crs = 5070)
  ## select grts sample
  ## spsurvey::
  grts_samp <- grts(data_sf, n_base = n, ...)
  ## convert to usable
  ## spsurvey::
  grts_bind <- sprbind(grts_samp, ...)
  ## get coordinates
  grts_coords <- sf::st_coordinates(grts_bind)
  ## make data frame
  grts_df <- data.frame(
    response = grts_bind$response,
    x = grts_coords[, "X"],
    y = grts_coords[, "Y"],
    siteID = grts_bind$siteID,
    wgt = grts_bind$wgt
  )
  ## select irs sample
  ##  spsurvey::
  ## irs_samp <- irs(data_sf, n_base = n, ...)
  ## Error in FUN(X[[i]], ...) : object 'n_legacy' not found

  ## convert to usable
  ## spsurvey::
  ## irs_bind <- sprbind(irs_samp, ...)

  ## get coordinates
  ## irs_coords <- sf::st_coordinates(irs_bind)
  ## make data frame
  # irs_df <- data.frame(
  #   response = irs_bind$response,
  #   x = grts_coords[, "X"],
  #   y = grts_coords[, "Y"]
  # )

  ## GRTS-analysis on GRTS sample
  ## spsurvey::
  design_analysis <- cont_analysis(
    grts_df,
    siteID = "siteID",
    vars = "response",
    weight = "wgt",
    xcoord = "x",
    ycoord = "y"
  )
  ## just return the mean info
  ## design_mean <- subset(design_analysis$Pct, Statistic == "Mean")
  design_mean <- design_analysis$Mean

  grts_coords_resp <- grts_df %>% select(-siteID, -wgt)
  grts_unsamp <- anti_join(data, grts_coords_resp)
  grts_unsamp$response <- NA
  grts_unsamp <- grts_unsamp %>% select(response, x, y)
  grts_full <- dplyr::bind_rows(grts_coords_resp, grts_unsamp)

  ## FPBK analysis on GRTS sample
  grts_full$wts <- 1 / nrow(grts_full)
  mod_grts <- slmfit(response ~ 1, data = grts_full, xcoordcol = "x",
         ycoordcol = "y", CorModel = cortype_est)
  pred_mod_grts <- predict(mod_grts, wtscol = "wts")
  model_mean_grts <- pred_mod_grts$FPBK_Prediction
  model_se_grts <- sqrt(pred_mod_grts$PredVar)
  model_lb_grts <- model_mean_grts + -1 * 1.96 * model_se_grts
  model_ub_grts <- model_mean_grts + 1 * 1.96 * model_se_grts

  ## IRS analysis on GRTS sample
  ## spsurvey::
  irs_analysis_grts_samp <- cont_analysis(
    grts_df,
    siteID = "siteID",
    vars = "response",
    weight = "wgt",
    xcoord = "x",
    ycoord = "y",
    vartype = "SRS"
  )


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
    estimate = c(irs_mean_irs_samp,
                 design_mean$Estimate, as.vector(model_mean),
                 as.vector(model_mean_grts)),
    sd = c(irs_se_irs_samp, design_mean$StdError, as.vector(model_se),
           as.vector(model_se_grts)),
    lb = c(irs_lb_irs_samp, design_mean$LCB95Pct, as.vector(model_lb),
           as.vector(model_lb_grts)),
    ub = c(irs_ub_irs_samp, design_mean$UCB95Pct, as.vector(model_ub),
           as.vector(model_ub_grts))
  )

  output
}



## example
## data <- sim_one(psill = 1, erange = 1, nugget = 1)
