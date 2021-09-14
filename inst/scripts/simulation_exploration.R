library(tidyverse)
library(here)

files <- list.files(path = here("inst", "output", "sims914"), pattern = "*.csv", full.names = TRUE)

combo_data <- purrr::map_df(files,
                            ~read_csv(.x) %>% mutate(filename = .x))

# %>%
#   separate(filename, into = c("junk", "error_dep", "dist", "n", "location"),
#            sep = "_") %>%
#   select(-junk)

combo_data <- combo_data %>% mutate(sim = interaction(n, psill, resptype, gridded))

##
ggplot(data = combo_data, aes(x = sim, y = rmspe, colour = approach)) +
  geom_point() +
  coord_flip()

combo_data_grts <- combo_data %>% filter(approach %in% c("Design GRTS", "Model GRTS"))

## the significance (p-values) of these are fairly meaningless, but still
## might help as a starting point
summary(lm(rmspe ~ approach, data = combo_data_grts))

## evidence that independent error gives worse estimators (makes sense)
## evidence that normal distribution gives better estimators (makes sense)
## evidence that n of 50 gives worse estimators (makes sense)

summary(lm(rmspe ~ approach + resptype + psill + n + gridded,
           data = combo_data_grts))


## visually explore the two approaches

ggplot(data = combo_data_grts, aes(x = sim, y = rmspe, colour = approach)) +
  geom_point() +
  coord_flip()

dep_only <- combo_data_grts %>% filter(psill == max(psill))
dep_long <- dep_only %>%
  pivot_wider(names_from = approach, values_from = rmspe, sim) %>%
  mutate(perc = 100 * (`Design GRTS` - `Model GRTS`) / `Model GRTS`)
ggplot(data = dep_long, aes(x = fct_reorder(sim, perc), y = perc)) +
  geom_point() +
  coord_flip() +
  labs(x = "Percent Change in rmspe (Design - Model) / Model",
       caption = "Percent > 0 means Model has better rmspe")

combo_perc <- combo_data_grts %>%
  pivot_wider(names_from = approach, values_from = rmspe, sim) %>%
  mutate(perc = 100 * (`Design GRTS` - `Model GRTS`) / `Model GRTS`)
ggplot(data = combo_perc, aes(x = fct_reorder(sim, perc), y = perc)) +
  geom_point() +
  coord_flip()

## investigate discrepancy in grid vs random
combo_data_grts %>% filter(psill == max(psill), resptype == "lognormal", n == 200)

combo_perc <- combo_data_grts %>%
  pivot_wider(names_from = approach, values_from = medae, sim) %>%
  mutate(perc = 100 * (`Design GRTS` - `Model GRTS`) / `Model GRTS`)
ggplot(data = combo_perc, aes(x = sim, y = perc)) +
  geom_point() +
  coord_flip()


ggplot(data = combo_data_grts, aes(x = sim, y = coverage, colour = approach)) +
  geom_point() +
  coord_flip()



## all

# grid_df <- read_csv(here("inst", "all_output", "sim_deperr_lognormalbig_n150_gridloc.csv"))
# grid_df$location <- "grid"
# rand_df <- read_csv(here("inst", "all_output", "sim_deperr_lognormalbig_n150_randloc.csv"))
# rand_df$location <- "rand"
#
# both_df <- bind_rows(grid_df, rand_df)
# both_df %>% group_by(location) %>%
#   summarise(max(true_mean))
# ggplot(data = both_df, aes(x = true_mean)) +
#   geom_histogram(bins = 15) +
#   facet_wrap(~ location)
#
#
