# preliminaries

## load packages
library(tidyverse)
library(here)
library(sptotal)
library(spsurvey) # depends on sf
library(DvMsp) # need package installed to load data

## do you want to write out? default of FALSE
write_out <- TRUE

## manipulate data

data(nla_df)

nla_df %>%
  group_by(SITE_ID) %>%
  summarise(nonmiss = sum(!is.na(TOTALHG_RESULT))) %>%
  ungroup() %>%
  summarise(maxnonmiss = max(nonmiss))

## HG never measured twice at the same site.
nla_df %>%
  group_by(SITE_ID) %>%
  summarise(nonmiss = sum(!is.na(TOTALHG_RESULT))) %>%
  summarise(sumnomiss = sum(nonmiss == 0))

## 35 sites never have HG measured. These will be dropped as well as the duplicate sites.
## in duplicate sites, fill in the value for RDis_IX
nla_nomiss <- nla_df %>%
  group_by(SITE_ID) %>%
  fill(RDis_IX, .direction = "downup") %>%
  ungroup() %>%
  filter(!is.na(TOTALHG_RESULT)) %>%
  rename(total_hg = "TOTALHG_RESULT")

nla_nomiss_both <- nla_nomiss
## check
nla_nomiss_both %>%
  group_by(SITE_ID) %>% count() %>%
  ungroup() %>%
  summarise(not1 = sum(n != 1))

## transform coordinates
coord_list <- LLtoTM(cm = mean(nla_nomiss_both$INDEX_LON_DD),
                     lat = nla_nomiss_both$INDEX_LAT_DD,
                     lon = nla_nomiss_both$INDEX_LON_DD)
nla_nomiss_both$xcoords <- coord_list$xy[ ,1]
nla_nomiss_both$ycoords <- coord_list$xy[ ,2]

## find population mean
realized_mean <- mean(nla_nomiss_both$total_hg)

## set seed and other preliminaries
set.seed(080421)
n <- 100

## IRS Sample, Model Analysis
lakesobs <- nla_nomiss_both %>% sample_n(n)
lakesunobs <- anti_join(nla_nomiss_both, lakesobs)
lakesunobs$total_hg <- NA

lakes_test <- bind_rows(lakesobs, lakesunobs)
lakes_test$wts <- 1 / nrow(lakes_test) ## predicting for mean

slmfitout_exp_lakes <- slmfit(formula = total_hg ~ 1,
                              data = lakes_test,
                              xcoordcol = 'xcoords',
                              ycoordcol = 'ycoords',
                              CorModel = "Exponential")

pred_exp_lakes <- predict(slmfitout_exp_lakes, wtscol = "wts",
                          conf_level = 0.95)

mean_irs_mod <- pred_exp_lakes$FPBK_Prediction
se_irs_mod <- sqrt(pred_exp_lakes$PredVar)
lb_irs_mod <- pred_exp_lakes$conf_bounds[1]
ub_irs_mod <- pred_exp_lakes$conf_bounds[2]

## IRS Sample, Design Analysis
irs_info <- lakesobs %>%
  summarise(meandoc = mean(total_hg), vardoc = var(total_hg))
N <- nrow(nla_nomiss_both)
n <- nrow(lakesobs)
irs_se_irs_samp <- sqrt((irs_info$vardoc / n) * (N - n) / N)
irs_lb_irs_samp <- irs_info$meandoc - 1.96 * irs_se_irs_samp
irs_ub_irs_samp <- irs_info$meandoc + 1.96 * irs_se_irs_samp
irs_info$meandoc; sqrt(irs_info$vardoc)
irs_lb_irs_samp; irs_ub_irs_samp

## GRTS Sample, Design Analysis
data_sf <- sf::st_as_sf(nla_nomiss_both, coords = c("xcoords", "ycoords"),
                        crs = 5070)

grts_samp <- grts(data_sf, n_base = n)
grts_bind <- sp_rbind(grts_samp)

## get coordinates
grts_coords <- sf::st_coordinates(grts_bind)
## make data frame
grts_df <- data.frame(
  response = grts_bind$total_hg,
  x = grts_coords[, "X"],
  y = grts_coords[, "Y"],
  siteID = grts_bind$siteID,
  wgt = grts_bind$wgt
)
## head(grts_df)
summary(grts_df$response)

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
design_mean_grts <- design_analysis$Mean

## GRTS Sample, Model Analysis
grts_coords_resp <- grts_df %>% select(-siteID, -wgt) %>%
  rename(total_hg = "response", xcoords = "x", ycoords = "y")
grts_unsamp <- anti_join(nla_nomiss_both, grts_coords_resp)
grts_unsamp$total_hg <- NA
grts_unsamp <- grts_unsamp %>% select(total_hg, xcoords, ycoords)
grts_full <- dplyr::bind_rows(grts_coords_resp, grts_unsamp)

grts_full$wts <- 1 / nrow(grts_full)
mod_grts <- slmfit(total_hg ~ 1, data = grts_full,
                   xcoordcol = "xcoords",
                   ycoordcol = "ycoords")
pred_mod_grts <- predict(mod_grts, wtscol = "wts")
model_mean_grts <- pred_mod_grts$FPBK_Prediction
model_se_grts <- sqrt(pred_mod_grts$PredVar)
model_lb_grts <- model_mean_grts + -1 * 1.96 * model_se_grts
model_ub_grts <- model_mean_grts + 1 * 1.96 * model_se_grts

## combine results
res_df <- tibble(approach = c("IRS-Design", "GRTS-Design", "IRS-Model", "GRTS-Model"),
                 realized_mean = c(realized_mean, realized_mean, realized_mean,
                                   realized_mean),
                 estimate = c(irs_info$meandoc,
                              design_mean_grts$Estimate, as.vector(mean_irs_mod),
                              as.vector(model_mean_grts)),
                 se = c(irs_se_irs_samp, design_mean_grts$StdError, as.vector(se_irs_mod),
                        as.vector(model_se_grts)),
                 lb = c(irs_lb_irs_samp, design_mean_grts$LCB95Pct, as.vector(lb_irs_mod),
                        as.vector(model_lb_grts)),
                 ub = c(irs_ub_irs_samp, design_mean_grts$UCB95Pct, as.vector(ub_irs_mod),
                        as.vector(model_ub_grts)))
res_df <- res_df %>% slice(1, 3, 2, 4)

if (write_out) {
  readr::write_csv(res_df, here("inst", "output", "application", "application.csv"))
}
