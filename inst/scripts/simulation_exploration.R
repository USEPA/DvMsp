read_csv(here("inst", "output", "sim_deperr_lognormal_n50_gridloc.csv"))

files <- list.files(path = here("inst", "output"), pattern = "*.csv",
                    full.names = TRUE)

combo_data <- purrr::map_df(files,
                            ~read_csv(.x) %>% mutate(filename = .x)) %>%
  separate(filename, into = c("junk", "error_dep", "dist", "n", "location"),
           sep = "_") %>%
  select(-junk)
combo_data <- combo_data %>% mutate(sim = interaction(error_dep, dist, n, location))

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

summary(lm(rmspe ~ approach + error_dep + dist + n + location,
           data = combo_data_grts))

summary(lm(rmspe ~ approach + error_dep*approach + dist* approach + n * approach + location * approach, data = combo_data_grts))


## checking other things
summary(lm(med_ci_len ~ approach, data = combo_data_grts))
summary(lm(med_ci_len ~ approach + error_dep + dist + n + location,
           data = combo_data_grts))



## visually explore the two approaches

ggplot(data = combo_data_grts, aes(x = sim, y = rmspe, colour = approach)) +
  geom_point() +
  coord_flip()

combo_perc <- combo_data_grts %>%
  pivot_wider(names_from = approach, values_from = rmspe, sim) %>%
  mutate(perc = 100 * (`Design GRTS` - `Model GRTS`) / `Model GRTS`)
ggplot(data = combo_perc, aes(x = sim, y = perc)) +
  geom_point() +
  coord_flip()
## investigate discrepancy in grid vs random
combo_data_grts %>% filter(error_dep == "deperr", dist == "lognormal", n == "n150")


ggplot(data = combo_data_grts, aes(x = sim, y = coverage, colour = approach)) +
  geom_point() +
  coord_flip()


