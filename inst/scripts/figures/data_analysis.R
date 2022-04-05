# preliminaries

## load packages
library(tidyverse)
library(here)

## do you want to write out? default of FALSE
write_out <- TRUE

## set base size
base_size <- 10

## read in files and create data
files <- list.files(path = here("inst", "output", "data_summary"), pattern = "*.csv", full.names = TRUE)

combo_data <- purrr::map_df(files,
                            ~read_csv(.x) %>% mutate(filename = .x))

combo_data <- combo_data %>%
  mutate(sim = interaction(n, var)) %>% ## , gridded
  mutate(approach = fct_relevel(approach, c("Design IRS", "Model IRS",
                                            "Design GRTS", "Model GRTS"))) %>%
  mutate(resptype = fct_relevel(var, c("MMI_BENT_NLA12", "TOTALHG_RESULT"))) %>%
  group_by(sim) %>%
  mutate(designirsrmspe = if_else(approach == "Design IRS", true = rmspe, false = NA_real_)) %>%
  mutate(designirsmse = if_else(approach == "Design IRS", true = mse, false = NA_real_)) %>%
  fill(designirsrmspe, .direction = "downup") %>%
  fill(designirsmse, .direction = "downup") %>%
  mutate(rel_efficiency = rmspe / designirsrmspe) %>%
  mutate(mse_efficiency = mse / designirsmse) %>%
  ungroup() %>%
  mutate(n_factor = factor(n))

# figure 2
colour_scale <- c("goldenrod1", "goldenrod4", "mediumpurple1", "mediumpurple4")
resptype_labs <- c(MMI_BENT_NLA12 = "Response: MMI_BENT_NLA12", TOTALHG_RESULT = "Response: TOTALHG_RESULT")
rmspe_eff <- combo_data %>%
  ggplot(aes(x = n_factor, y = rel_efficiency, colour = approach)) +
  facet_grid(
    rows = vars(var)
    # labeller = labeller(var = resptype_labs)
  ) +
  geom_jitter(width = 0.24, size = 2) +
  scale_colour_manual(values = colour_scale) +
  theme_bw(base_size = base_size) +
  labs(x = "Sample Size", colour = "Approach", y = "Relative rMS(P)E") +
  lims(y = c(0.4, 1.1)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )

### rmspe_eff
if (write_out) {
  ggsave(
    plot = rmspe_eff,
    file = here("inst", "manuscript", "figures", "data_rmspe_eff.jpeg"),
    dpi = 300,
    width = 5.07,
    height = 4.39
  )
}


# figure 3
colour_scale <- c("goldenrod1", "goldenrod4", "mediumpurple1", "mediumpurple4")
resptype_labs <- c(MMI_BENT_NLA12 = "Response: MMI_BENT_NLA12", TOTALHG_RESULT = "Response: TOTALHG_RESULT")
mse_eff <- combo_data %>%
  ggplot(aes(x = n_factor, y = mse_efficiency, colour = approach)) +
  facet_grid(
    rows = vars(var)
    # labeller = labeller(var = resptype_labs)
  ) +
  geom_jitter(width = 0.24, size = 2) +
  scale_colour_manual(values = colour_scale) +
  theme_bw(base_size = base_size) +
  labs(x = "Sample Size", colour = "Approach", y = "Relative MStdE") +
  lims(y = c(0.4, 1.1)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )

### mse_eff
if (write_out) {
  ggsave(
    plot = mse_eff,
    file = here("inst", "manuscript", "figures", "data_mse_eff.jpeg"),
    dpi = 300,
    width = 5.07,
    height = 4.39
  )
}


# figure 4
colour_scale <- c("goldenrod1", "goldenrod4", "mediumpurple1", "mediumpurple4")
resptype_labs <- c(MMI_BENT_NLA12 = "Response: MMI_BENT_NLA12", TOTALHG_RESULT = "Response: TOTALHG_RESULT")
coverage <- combo_data %>%
  ggplot(aes(x = n_factor, y = coverage, colour = approach)) +
  geom_hline(yintercept = 0.95) +
  facet_grid(
    rows = vars(var)
    # labeller = labeller(var = resptype_labs)
  ) +
  geom_jitter(width = 0.24, size = 2) +
  scale_colour_manual(values = colour_scale) +
  theme_bw(base_size = base_size) +
  labs(x = "Sample Size", colour = "Approach", y = "Interval Coverage") +
  lims(y = c(0.4, 1.1)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )

### coverage
if (write_out) {
  ggsave(
    plot = coverage,
    file = here("inst", "manuscript", "figures", "data_coverage.jpeg"),
    dpi = 300,
    width = 5.07,
    height = 4.39
  )
}
