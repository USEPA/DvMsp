# preliminaries

## load packages
library(tidyverse)
library(here)
library(cowplot)

## do you want to write out? default of FALSE
write_out <- TRUE

## set base size
base_size <- 9

# set some graphical parameters
ht <-  4
wid <- 4 / 3 * ht

## grid preliminaries ---------------------------------------------------------
set.seed(5)
n <- 20
grts_data <- data.frame(lon = runif(n), lat = runif(n))
min_lon <- 0
max_lon <- 1
min_lat <- 0
max_lat <- 1
grts_box <- data.frame(lon = c(0, 1, 1, 0, 0), lat = c(0, 0, 1, 1, 0))
low <- 0
q1 <- 0.25
mid <- 0.5
q3 <- 0.75
high <- 1
q18 <- 1 / 8
q38 <- 3 / 8
q58 <- 5 / 8
q78 <- 7 / 8

# figure a     ----------------------------------------------------------------
annotate_size <- 30
sample(0:3)
grts_level1 <- ggplot() +
  geom_path(grts_box, mapping = aes(x = lon, y = lat), col = "black") +
  geom_point(data = grts_data, mapping = aes(x = lon, y = lat), size = 5, col = "grey35") +
  geom_segment(aes(x = low, xend = high, y = low, yend = low), linetype = "dashed", col = "black") +
  geom_segment(aes(x = low, xend = high, y = mid, yend = mid), linetype = "dashed", col = "black") +
  geom_segment(aes(x = low, xend = high, y = high, yend = high), linetype = "dashed", col = "black") +
  geom_segment(aes(x = low, xend = low, y = low, yend = high), linetype = "dashed", col = "black") +
  geom_segment(aes(x = mid, xend = mid, y = low, yend = high), linetype = "dashed", col = "black") +
  geom_segment(aes(x = high, xend = high, y = low, yend = high), linetype = "dashed", col = "black") +
  annotate(geom = "text", x = q1, y = q1, label = "2", size = annotate_size, col = "black") +
  annotate(geom = "text", x = q1, y = q3, label = "1", size = annotate_size, col = "black") +
  annotate(geom = "text", x = q3, y = q3, label = "3", size = annotate_size, col = "black") +
  annotate(geom = "text", x = q3, y = q1, label = "0", size = annotate_size, col = "black") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank())

# write out the image
if (write_out) {
  ggsave(
    plot = grts_level1,
    file = here("inst", "manuscript", "figures", "grts_level1.jpeg"),
    dpi = 300,
    width = 5.07,
    height = 4.39
  )
}

# figure b     ----------------------------------------------------------------
annotate_size <- 15
lapply(1:4, function(x) sample(0:3))
grts_level2 <- ggplot() +
  geom_path(grts_box, mapping = aes(x = lon, y = lat), col = "black") +
  geom_point(data = grts_data, mapping = aes(x = lon, y = lat), size = 5, col = "grey35") +
  geom_segment(aes(x = low, xend = high, y = low, yend = low), linetype = "dashed", col = "black") +
  geom_segment(aes(x = low, xend = high, y = mid, yend = mid), linetype = "dashed", col = "black") +
  geom_segment(aes(x = low, xend = high, y = high, yend = high), linetype = "dashed", col = "black") +
  geom_segment(aes(x = low, xend = low, y = low, yend = high), linetype = "dashed", col = "black") +
  geom_segment(aes(x = mid, xend = mid, y = low, yend = high), linetype = "dashed", col = "black") +
  geom_segment(aes(x = high, xend = high, y = low, yend = high), linetype = "dashed", col = "black") +
  geom_segment(aes(x = q1, xend = q1, y = low, yend = high), linetype = "dashed", col = "black") +
  geom_segment(aes(x = q3, xend = q3, y = low, yend = high), linetype = "dashed", col = "black") +
  geom_segment(aes(x = low, xend = high, y = q1, yend = q1), linetype = "dashed", col = "black") +
  geom_segment(aes(x = low, xend = high, y = q3, yend = q3), linetype = "dashed", col = "black") +
  # low left
  annotate(geom = "text", x = q18, y = q18, label = "21", size = annotate_size, col = "black") +
  annotate(geom = "text", x = q18, y = q38, label = "20", size = annotate_size, col = "black") +
  annotate(geom = "text", x = q38, y = q18, label = "22", size = annotate_size, col = "black") +
  annotate(geom = "text", x = q38, y = q38, label = "23", size = annotate_size, col = "black") +
  # top left
  annotate(geom = "text", x = q18, y = q58, label = "13", size = annotate_size, col = "black") +
  annotate(geom = "text", x = q18, y = q78, label = "12", size = annotate_size, col = "black") +
  annotate(geom = "text", x = q38, y = q58, label = "11", size = annotate_size, col = "black") +
  annotate(geom = "text", x = q38, y = q78, label = "10", size = annotate_size, col = "black") +
  # top right
  annotate(geom = "text", x = q58, y = q18, label = "03", size = annotate_size, col = "black") +
  annotate(geom = "text", x = q58, y = q38, label = "00", size = annotate_size, col = "black") +
  annotate(geom = "text", x = q78, y = q18, label = "01", size = annotate_size, col = "black") +
  annotate(geom = "text", x = q78, y = q38, label = "02", size = annotate_size, col = "black") +
  # low right
  annotate(geom = "text", x = q58, y = q58, label = "31", size = annotate_size, col = "black") +
  annotate(geom = "text", x = q58, y = q78, label = "30", size = annotate_size, col = "black") +
  annotate(geom = "text", x = q78, y = q58, label = "33", size = annotate_size, col = "black") +
  annotate(geom = "text", x = q78, y = q78, label = "32", size = annotate_size, col = "black") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank())

# write out the image
if (write_out) {
  ggsave(
    plot = grts_level2,
    file = here("inst", "manuscript", "figures", "grts_level2.jpeg"),
    dpi = 300,
    width = 5.07,
    height = 4.39
  )
}
