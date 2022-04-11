# preliminaries

## load packages
library(tidyverse)
library(here)

## do you want to write out? default of FALSE
write_out <- TRUE

## set base size
base_size <- 10

## read in files and create data
files <- list.files(path = here("inst", "output", "simulation_summary"), pattern = "*.csv", full.names = TRUE)

combo_data <- purrr::map_df(files,
                            ~read_csv(.x) %>% mutate(filename = .x))

combo_data <- combo_data %>%
  dplyr::filter(gridded == FALSE) %>%
  mutate(sim = interaction(n, psill, resptype)) %>% ## , gridded
  mutate(approach = fct_relevel(approach, c("Design IRS", "Model IRS",
                                            "Design GRTS", "Model GRTS"))) %>%
  mutate(resptype = fct_relevel(resptype, c("normal", "lognormal"))) %>%
  group_by(sim) %>%
  mutate(designirsrmspe = if_else(approach == "Design IRS", true = rmspe, false = NA_real_)) %>%
  mutate(designirsmse = if_else(approach == "Design IRS", true = mse, false = NA_real_)) %>%
  fill(designirsrmspe, .direction = "downup") %>%
  fill(designirsmse, .direction = "downup") %>%
  mutate(rel_efficiency = rmspe / designirsrmspe) %>%
  mutate(mse_efficiency = mse / designirsmse) %>%
  ungroup() %>%
  mutate(psill_ratio = 1 - nugget_ratio) %>%
  mutate(n_factor = factor(n))

# create adjusted factor
combo_data <- combo_data %>%
  mutate(n_factor_adj = case_when(
    n == 50 & approach == "Design IRS" ~ 35,
    n == 50 & approach == "Model IRS" ~ 45,
    n == 50 & approach == "Design GRTS" ~ 55,
    n == 50 & approach == "Model GRTS" ~ 65,
    n == 100 & approach == "Design IRS" ~ 85,
    n == 100 & approach == "Model IRS" ~ 95,
    n == 100 & approach == "Design GRTS" ~ 105,
    n == 100 & approach == "Model GRTS" ~ 115,
    n == 200 & approach == "Design IRS" ~ 135,
    n == 200 & approach == "Model IRS" ~ 145,
    n == 200 & approach == "Design GRTS" ~ 155,
    n == 200 & approach == "Model GRTS" ~ 165,
    TRUE ~ NA_real_
  ))

# figure 2
colour_scale <- c("goldenrod1", "goldenrod4", "mediumpurple1", "mediumpurple4")
resptype_labs <- c(normal = "Response: Normal", lognormal = "Response: Skewed")
psill_ratio_labs <- c("0" = "Prop DE: 0", "0.5" = "Prop DE: 0.5", "0.9" = "Prop DE: 0.9")
rmspe_eff <- combo_data %>%
  ggplot(aes(x = n_factor_adj, y = rel_efficiency, colour = approach)) +
  facet_grid(
    psill_ratio ~ resptype,
    labeller = labeller(resptype = resptype_labs, psill_ratio = psill_ratio_labs)
  ) +
  scale_colour_manual(values = colour_scale) +
  geom_point(size = 2) +
  geom_vline(xintercept = c(75, 125), lty = "solid") +
  scale_x_continuous(breaks = c(50, 100, 150), labels = c("50", "100", "200")) +
  theme_bw(base_size = base_size) +
  labs(x = "Sample Size", colour = "Approach", y = "Relative rMS(P)E") +
  lims(y = c(0.4, 1.1)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )

# figure 2
# colour_scale <- c("goldenrod1", "goldenrod4", "mediumpurple1", "mediumpurple4")
# resptype_labs <- c(normal = "Response: Normal", lognormal = "Response: Lognormal")
# psill_ratio_labs <- c("0" = "Prop DE: 0", "0.5" = "Prop DE: 0.5", "0.9" = "Prop DE: 0.9")
# rmspe_eff <- combo_data %>%
#   ggplot(aes(x = n_factor, y = rel_efficiency, colour = approach)) +
#   facet_grid(
#     psill_ratio ~ resptype,
#     labeller = labeller(resptype = resptype_labs, psill_ratio = psill_ratio_labs)
#   ) +
#   geom_jitter(width = 0.24, size = 2) +
#   scale_colour_manual(values = colour_scale) +
#   theme_bw(base_size = base_size) +
#   labs(x = "Sample Size", colour = "Approach", y = "Relative rMS(P)E") +
#   lims(y = c(0.4, 1.1)) +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#   )

### rmspe_eff
if (write_out) {
  ggsave(
    plot = rmspe_eff,
    file = here("inst", "manuscript", "figures", "rmspe_eff.jpeg"),
    dpi = 300,
    width = 5.07,
    height = 4.39
  )
}


# figure 3
# colour_scale <- c("goldenrod1", "goldenrod4", "mediumpurple1", "mediumpurple4")
# resptype_labs <- c(normal = "Response: Normal", lognormal = "Response: Lognormal")
# psill_ratio_labs <- c("0" = "Prop DE: 0", "0.5" = "Prop DE: 0.5", "0.9" = "Prop DE: 0.9")
# mse_eff <- combo_data %>%
#   ggplot(aes(x = n_factor_adj, y = mse_efficiency, colour = approach)) +
#   facet_grid(
#     psill_ratio ~ resptype,
#     labeller = labeller(resptype = resptype_labs, psill_ratio = psill_ratio_labs)
#   ) +
#   geom_point(size = 2) +
#   geom_vline(xintercept = c(75, 125), lty = "dashed") +
#   scale_x_continuous(breaks = c(50, 100, 150), labels = c("50", "100", "200")) +
#   scale_colour_manual(values = colour_scale) +
#   theme_bw(base_size = base_size) +
#   labs(x = "Sample Size", colour = "Approach", y = "Relative MStdE") +
#   lims(y = c(0.4, 1.1)) +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#   )

# figure 3
# colour_scale <- c("goldenrod1", "goldenrod4", "mediumpurple1", "mediumpurple4")
# resptype_labs <- c(normal = "Response: Normal", lognormal = "Response: Lognormal")
# psill_ratio_labs <- c("0" = "Prop DE: 0", "0.5" = "Prop DE: 0.5", "0.9" = "Prop DE: 0.9")
# mse_eff <- combo_data %>%
#   ggplot(aes(x = n_factor, y = mse_efficiency, colour = approach)) +
#   facet_grid(
#     psill_ratio ~ resptype,
#     labeller = labeller(resptype = resptype_labs, psill_ratio = psill_ratio_labs)
#   ) +
#   geom_jitter(width = 0.24, size = 2) +
#   scale_colour_manual(values = colour_scale) +
#   theme_bw(base_size = base_size) +
#   labs(x = "Sample Size", colour = "Approach", y = "Relative MStdE") +
#   lims(y = c(0.4, 1.1)) +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#   )

### mse_eff
# if (write_out) {
#   ggsave(
#     plot = mse_eff,
#     file = here("inst", "manuscript", "figures", "mse_eff.jpeg"),
#     dpi = 300,
#     width = 5.07,
#     height = 4.39
#   )
# }

# figure 4
coverage <- combo_data %>%
  ggplot(aes(x = n_factor_adj, y = coverage, colour = approach)) +
  geom_hline(yintercept = 0.95, lty = "dashed") +
  facet_grid(
    psill_ratio ~ resptype,
    labeller = labeller(resptype = resptype_labs, psill_ratio = psill_ratio_labs)
  ) +
  scale_colour_manual(values = colour_scale) +
  geom_point(size = 2) +
  geom_vline(xintercept = c(75, 125), lty = "solid") +
  scale_x_continuous(breaks = c(50, 100, 150), labels = c("50", "100", "200")) +
  ylim(0.85, 1) +
  theme_bw(base_size = base_size) +
  labs(x = "Sample Size", colour = "Approach", y = "Interval Coverage") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )


# # figure 4
# coverage <- combo_data %>%
#   ggplot(aes(x = n_factor, y = coverage, colour = approach)) +
#   geom_hline(yintercept = 0.95) +
#   geom_jitter(width = 0.24, size = 2) +
#   facet_grid(
#     psill_ratio ~ resptype,
#     labeller = labeller(resptype = resptype_labs, psill_ratio = psill_ratio_labs)
#   ) +
#   scale_colour_manual(values = colour_scale) +
#   ylim(0.80, 1) +
#   theme_bw(base_size = base_size) +
#   labs(x = "Sample Size", colour = "Approach", y = "Interval Coverage") +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#   )

### coverage
if (write_out) {
  ggsave(
    plot = coverage,
    file = here("inst", "manuscript", "figures", "coverage.jpeg"),
    dpi = 300,
    width = 5.07,
    height = 4.39
  )
}
