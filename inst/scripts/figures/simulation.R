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
  mutate(designirsmil = if_else(approach == "Design IRS", true = med_ci_len, false = NA_real_)) %>%
  fill(designirsrmspe, .direction = "downup") %>%
  fill(designirsmil, .direction = "downup") %>%
  mutate(rel_efficiency = rmspe / designirsrmspe) %>%
  mutate(mil_efficiency = med_ci_len / designirsmil) %>%
  ungroup() %>%
  mutate(psill_ratio = 1 - nugget_ratio) %>%
  mutate(n_factor = factor(n))

# figure 2
colour_scale <- c("goldenrod1", "goldenrod4", "mediumpurple1", "mediumpurple4")
resptype_labs <- c(normal = "Response: Normal", lognormal = "Response: Lognormal")
psill_ratio_labs <- c("0" = "Prop DE: 0", "0.5" = "Prop DE: 0.5", "0.9" = "Prop DE: 0.9")
rmspe_eff <- combo_data %>%
  ggplot(aes(x = n_factor, y = rel_efficiency, colour = approach)) +
  facet_grid(
    psill_ratio ~ resptype,
    labeller = labeller(resptype = resptype_labs, psill_ratio = psill_ratio_labs)
  ) +
  geom_jitter(width = 0.24, size = 2) +
  scale_colour_manual(values = colour_scale) +
  theme_bw(base_size = base_size) +
  labs(x = "Sample Size", colour = "Approach", y = "Relative Efficiency") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )

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
coverage <- combo_data %>%
  ggplot(aes(x = n_factor, y = coverage, colour = approach)) +
  geom_hline(yintercept = 0.95) +
  geom_jitter(width = 0.24, size = 2) +
  facet_grid(
    psill_ratio ~ resptype,
    labeller = labeller(resptype = resptype_labs, psill_ratio = psill_ratio_labs)
  ) +
  scale_colour_manual(values = colour_scale) +
  ylim(0.80, 1) +
  theme_bw(base_size = base_size) +
  labs(x = "Sample Size", colour = "Approach", y = "Interval Coverage") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )

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
