# preliminaries

## load packages
library(tidyverse)
library(here)
library(cowplot)
library(ggpubr)

## do you want to write out? default of FALSE
write_out <- TRUE

## set base size
base_size <- 9

## set a reproducible seed
set.seed(10102021)

## load sim_pop function (if running from within package project)
source(here("R", "sim_pop.R"))

## load sim_pop function (if running after installation)
# library(DvMsp)

### simulate populations
N <- 9
pop_df <- sim_pop(
  N = N,
  gridded = TRUE,
  cortype = "Exponential",
  psill = 0.9,
  range = 1,
  nugget = 0.1
)

### select samples
n <- 4
samp_df <- pop_df %>% sample_n(n)
samp_df2 <- pop_df %>% sample_n(n)
samp_df3 <- pop_df %>% sample_n(n)


### create plots
d1 <- pop_df %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(colour = response), size = 4) +
  scale_colour_viridis_c() +
  geom_point(
    data = samp_df,
    size = 5.5,
    shape = 1,
    show.legend = FALSE,
    stroke = 1.5
  ) +
  labs(title = "Sample 1", colour = "Response") +
  theme_bw(base_size = base_size) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  ) +
  xlim(-0.1, 1.1) +
  ylim(-0.1, 1.1)

d2 <- pop_df %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(colour = response), size = 4) +
  scale_colour_viridis_c() +
  geom_point(
    data = samp_df2,
    size = 5.5,
    shape = 1,
    show.legend = FALSE,
    stroke = 1.5
  ) +
  labs(title = "Sample 2", colour = "Response") +
  theme_bw(base_size = base_size) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  ) +
  xlim(-0.1, 1.1) +
  ylim(-0.1, 1.1)

d3 <- pop_df %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(colour = response), size = 4) +
  scale_colour_viridis_c() +
  geom_point(
    data = samp_df3,
    size = 5.5,
    shape = 1,
    show.legend = FALSE,
    stroke = 1.5
  ) +
  labs(title = "Sample 3", colour = "Response") +
  theme_bw(base_size = base_size) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  ) +
  xlim(-0.1, 1.1) +
  ylim(-0.1, 1.1)

pop_df_mod <- sim_pop(N = N, gridded = TRUE, cortype = "Exponential", psill = 0.9, range = 1, nugget = 0.1)
pop_df_mod2 <- sim_pop(N = N, gridded = TRUE, cortype = "Exponential", psill = 0.9, range = 1, nugget = 0.1)
pop_df_mod3 <- sim_pop(N = N, gridded = TRUE, cortype = "Exponential", psill = 0.9, range = 1, nugget = 0.1)

indices <- sample(1:N, size = n, replace = FALSE)
samp_df_mod <- pop_df_mod %>% slice(indices)
samp_df_mod2 <- pop_df_mod2 %>% slice(indices)
samp_df_mod3 <- pop_df_mod3 %>% slice(indices)

m1 <- pop_df_mod %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(colour = response), size = 4) +
  scale_colour_viridis_c() +
  geom_point(
    data = samp_df_mod,
    size = 5.5,
    shape = 1,
    show.legend = FALSE,
    stroke = 1.5
  ) +
  labs(title = "Population 1", colour = "Response") +
  theme_bw(base_size = base_size) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  ) +
  xlim(-0.1, 1.1) +
  ylim(-0.1, 1.1)

m2 <- pop_df_mod2 %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(colour = response), size = 4) +
  scale_colour_viridis_c() +
  geom_point(
    data = samp_df_mod2,
    size = 5.5,
    shape = 1,
    show.legend = FALSE,
    stroke = 1.5
  ) +
  labs(title = "Population 2", colour = "Response") +
  theme_bw(base_size = base_size) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  ) +
  xlim(-0.1, 1.1) +
  ylim(-0.1, 1.1)

m3 <- pop_df_mod3 %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(colour = response), size = 4) +
  scale_colour_viridis_c() +
  geom_point(
    data = samp_df_mod2,
    size = 5.5,
    shape = 1,
    show.legend = FALSE,
    stroke = 1.5
  ) +
  labs(title = "Population 3", colour = "Response") +
  theme_bw(base_size = base_size) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  ) +
  xlim(-0.1, 1.1) +
  ylim(-0.1, 1.1)

# make the final figure
dvm_comp <- ggarrange(d1, d2, d3, m1, m2, m3, nrow = 2, ncol = 3, common.legend = TRUE, legend = "right")

### dvm_comp
# write out the image
if (write_out) {
  ggsave(
    plot = dvm_comp,
    file = here("inst", "manuscript", "figures", "dvm_comp.jpeg"),
    dpi = 300,
    width = 5.07,
    height = 3.4
  )
}
