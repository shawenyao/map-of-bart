library(dplyr)
library(purrr)
library(ggplot2)

setwd("C:/Users/Wenyao/Desktop/R/BART/")
source("./functions/functions_smooth_path.R")

# the Cartiesian coordinates of the original path
path <- data.frame(
  x = c(1, 3, 4, 1,-1,-0.5, 2),
  y = c(3, 4, 2, 1.5, 4, 6, 7)
)

bind_rows(
  path %>% mutate(type = "original"),
  path %>% smooth_path() %>% mutate(type = "smooth x 1"),
  path %>% smooth_path() %>% smooth_path() %>% mutate(type = "smooth x 2"),
  path %>% smooth_path() %>% smooth_path() %>% smooth_path() %>%  mutate(type = "smooth x 3")
) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_path(size = 3, linejoin = "round", lineend = "round") +
  geom_point(size = 6, aes(color = type)) +
  facet_grid(.~type) +
  theme_minimal() +
  theme(legend.position = "none")
