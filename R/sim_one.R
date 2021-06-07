## get one simulation
sim_one <- function(data, xcoord = "x", ycoord = "y", response = "response", cortype_est = "exponential", ...) {
  irs_samp <- sample_n(data, n)
  irs_unsamp <- anti_join(data, irs_samp)
  irs_unsamp$response <- NA
  full_df <- bind_rows(irs_samp, irs_unsamp)
  browser()
  mod <- slmfit(formula = response ~ 1, data = full_df, xcoordcol = "x",
                ycoordcol = "y")
  full_df$wts <- 1 / nrow(full_df)
  pred_mod <- predict(mod, wtscol = "wts")
  model_mean <- pred_mod$FPBK_Prediction
  model_se <- sqrt(pred_mod$PredVar)
  model_lb <- model_mean + -1 * 1.96 * se
  model_ub <- model_mean + 1 * 1.96 * se

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


  # store output
  output <- data.frame(
    approach = c("Design", "Model"),
    estimate = c(design_mean$Estimate, as.vector(model_mean)),
    sd = c(design_mean$StdError, as.vector(se_mean)),
    lb = c(design_mean$LCB95Pct, as.vector(model_lb)),
    ub = c(design_mean$UCB95Pct, as.vector(model_ub))
  )

  output
}
