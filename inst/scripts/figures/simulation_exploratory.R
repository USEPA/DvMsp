# preliminaries

## load packages
library(ggplot2)
library(here)
library(DvMsp)

## do you want to write out? default of FALSE
write_out <- TRUE

## histogram for the simultation
set.seed(2)
symm_pop_var <- 2
symm_pop <- sim_pop(N = 900, gridded = FALSE, cortype = "Exponential", psill = symm_pop_var * 0.5,
                    range = sqrt(2) / 3, nugget = 0.5 * symm_pop_var, resptype = "normal", mu = 0)
symm_plot <- ggplot(symm_pop, aes(x = response)) +
    geom_histogram(bins = 20) +
    labs(x = "Normal Response", y = "Count") +
    theme_bw(base_size = 18) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
    )


# write out the image
if (write_out) {
  ggsave(
    plot = symm_plot,
    file = here("inst", "manuscript", "figures", "symm_pop_hist.jpeg"),
    dpi = 300,
    width = 5.07,
    height = 4.39
  )
}



skew_pop_var <- uniroot(function(x) (exp(x) - 1) * exp(x) - 2, interval = c(0, 10))$root
skew_pop <- sim_pop(N = 900, gridded = FALSE, cortype = "Exponential", psill = skew_pop_var * 0.5,
                    range = sqrt(2) / 3, nugget = 0.5 * skew_pop_var, resptype = "lognormal", mu = 0)
skew_plot <- ggplot(skew_pop, aes(x = response)) +
  geom_histogram(bins = 20) +
  labs(x = "Skewed Response", y = "Count") +
  theme_bw(base_size = 18) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
  )


# write out the image
if (write_out) {
  ggsave(
    plot = skew_plot,
    file = here("inst", "manuscript", "figures", "skew_pop_hist.jpeg"),
    dpi = 300,
    width = 5.07,
    height = 4.39
  )
}


# # population simulations for single observation
#
# ## normal
# df_norm <- data.frame(x = seq(-5, 5, length.out = 1000))
# df_norm$y <- dnorm(df_norm$x, mean = 0, sd = sqrt(2))
# ggplot(df_norm, aes(x = x, ymin = 0, ymax = y)) + geom_ribbon()
#
# ## lognormal
# df_lnorm <- data.frame(x = seq(0, 10, length.out = 1000))
# df_lnorm$y <- dlnorm(df_lnorm$x, meanlog = 0, sdlog = sqrt(2))
# ggplot(df_lnorm, aes(x = x, ymin = 0, ymax = y)) + geom_ribbon()
