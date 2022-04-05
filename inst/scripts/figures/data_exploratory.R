# preliminaries

## load packages
library(tidyverse)
library(cowplot)
library(here)
library(sptotal)
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

## figure 4a
mercury_map <- nla_nomiss_both %>%
  ggplot(aes(x = xcoords, y = ycoords)) +
  geom_point(aes(colour = total_hg)) +
  scale_colour_viridis_c() +
  labs(x = "", y = "", colour = "Hg") +
  coord_quickmap() +
  theme_bw(base_size = 10) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
  )

### mercury_map
# write out the image
if (write_out) {
  ggsave(
    plot = mercury_map,
    file = here("inst", "manuscript", "figures", "mercury_map.jpeg"),
    dpi = 300,
    width = 5.07,
    height = 4.39
  )
}

## figure 4 b
mercury_hist <- nla_nomiss_both %>%
  ggplot(aes(x = total_hg)) +
  geom_histogram(bins = 20) +
  labs(x = "Hg", y = "Count") +
  theme_bw(base_size = 18) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
  )

### mercury_hist
# write out the image
if (write_out) {
  ggsave(
    plot = mercury_hist,
    file = here("inst", "manuscript", "figures", "mercury_hist.jpeg"),
    dpi = 300,
    width = 5.07,
    height = 4.39
  )
}

## figure 4c
sv_val <- sv(nla_nomiss_both, "xcoords", "ycoords", "total_hg") # same as residuals because model is mean-only
sv_plot <- sv_val %>%
  ggplot(aes(x = dist, y = gamma * (1/1000))) +
  geom_point(size = 3) +
  labs(x = "Distance (Kilometers)", y = "Semi-Variance (Thousands)", title = "") +
  ylim(c(0, 15)) +
  # scale_size_continuous(name = "Pairs") +
  theme_bw(base_size = 18) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
  )


### sv_plot
# write out the image
if (write_out) {
  ggsave(
    plot = sv_plot,
    file = here("inst", "manuscript", "figures", "sv_plot.jpeg"),
    dpi = 300,
    width = 5.07,
    height = 4.39
  )
}

data(nla_df)

nla_df %>%
  group_by(SITE_ID) %>%
  summarise(nonmiss = sum(!is.na(MMI_BENT_NLA12))) %>%
  ungroup() %>%
  summarise(maxnonmiss = max(nonmiss))

## bmmi never measured twice at the same site.
nla_df %>%
  group_by(SITE_ID) %>%
  summarise(nonmiss = sum(!is.na(MMI_BENT_NLA12))) %>%
  summarise(sumnomiss = sum(nonmiss == 0))

## some sites never have HG measured. These will be dropped as well as the duplicate sites.
## in duplicate sites, fill in the value for RDis_IX
nla_nomiss <- nla_df %>%
  group_by(SITE_ID) %>%
  fill(RDis_IX, .direction = "downup") %>%
  ungroup() %>%
  filter(!is.na(MMI_BENT_NLA12))

nla_nomiss_both <- nla_nomiss
## check
nla_nomiss_both %>%
  group_by(SITE_ID) %>% count() %>%
  ungroup() %>%
  summarise(not1 = sum(n != 1))

# average
nla_nomiss_both <- nla_nomiss_both %>%
  group_by(SITE_ID) %>%
  summarize(MMI_BENT_NLA12 = mean(MMI_BENT_NLA12), INDEX_LON_DD = mean(INDEX_LON_DD), INDEX_LAT_DD = INDEX_LAT_DD) %>%
  ungroup()

## transform coordinates
coord_list <- LLtoTM(cm = mean(nla_nomiss_both$INDEX_LON_DD),
                     lat = nla_nomiss_both$INDEX_LAT_DD,
                     lon = nla_nomiss_both$INDEX_LON_DD)
nla_nomiss_both$xcoords <- coord_list$xy[ ,1]
nla_nomiss_both$ycoords <- coord_list$xy[ ,2]

## figure 4a
bmmi_map <- nla_nomiss_both %>%
  ggplot(aes(x = xcoords, y = ycoords)) +
  geom_point(aes(colour = MMI_BENT_NLA12)) +
  scale_colour_viridis_c() +
  labs(x = "", y = "", colour = "BMMI") +
  coord_quickmap() +
  theme_bw(base_size = 10) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
  )

### bmmi_map
# write out the image
if (write_out) {
  ggsave(
    plot = bmmi_map,
    file = here("inst", "manuscript", "figures", "bmmi_map.jpeg"),
    dpi = 300,
    width = 5.07,
    height = 4.39
  )
}

## figure 4 b
bmmi_hist <- nla_nomiss_both %>%
  ggplot(aes(x = MMI_BENT_NLA12)) +
  geom_histogram(bins = 20) +
  labs(x = "Hg", y = "Count") +
  theme_bw(base_size = 18) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
  )

### bmmi_hist
# write out the image
if (write_out) {
  ggsave(
    plot = bmmi_hist,
    file = here("inst", "manuscript", "figures", "bmmi_hist.jpeg"),
    dpi = 300,
    width = 5.07,
    height = 4.39
  )
}

## figure 4c
sv_val <- sv(nla_nomiss_both, "xcoords", "ycoords", "total_hg") # same as residuals because model is mean-only
sv_plot <- sv_val %>%
  ggplot(aes(x = dist, y = gamma * (1/10))) +
  geom_point(size = 3) +
  labs(x = "Distance (Kilometers)", y = "Semi-Variance (Tens)", title = "") +
  ylim(c(0, 30)) +
  # scale_size_continuous(name = "Pairs") +
  theme_bw(base_size = 18) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
  )


### sv_plot
# write out the image
if (write_out) {
  ggsave(
    plot = sv_plot,
    file = here("inst", "manuscript", "figures", "bmmi_sv_plot.jpeg"),
    dpi = 300,
    width = 5.07,
    height = 4.39
  )
}
