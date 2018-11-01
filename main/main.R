library(tidyverse)
library(magrittr)
library(rio)
library(jsonlite)
library(beepr)
library(ggmap)


#==== general setup ====
setwd("C:/Users/Wenyao/Desktop/R/BART/")
source("./functions/functions_bart.R")
source("./functions/functions_smooth_path.R")


#==== load data ====
# get BART stations info from api
stations <- get_bart_stations()

# get BART routes info from api
routes <- get_bart_routes(c(19, 7, 1, 11, 5, 3))

# get bay area map info
map_data <- get_map(
  location = c(lon = mean(stations$longitude), lat = mean(stations$latitude)), 
  maptype = "roadmap",
  zoom = 10,
  scale = 2
)


#==== clean data ====
# bring the station coordinates into the routes info
routes_with_station_coordinates <- routes %>% 
  left_join(
    stations %>% select(abbr, county, city, longitude, latitude),
    by = c("stations" = "abbr")
  )

# spread the station coordinates if multiple routes share the same station
routes_spreaded <- spread_routes(routes_with_station_coordinates, spread_width = 3.5e-3)

# smooth the route based on the curvature
routes_smoothed <- routes_spreaded %>% 
  select(number, hexcolor, x = longitude, y = latitude) %>% 
  group_by(number, hexcolor) %>% 
  do(
    smooth_path(., lamda = 0.15) %>% 
      smooth_path() %>% 
      smooth_path()
  )


#==== plot ====
plot <- plot_bart(map_data = map_data, routes = routes_smoothed, stations = stations)
print(plot)


#==== output =====
svg("output/BART.svg", width = 10, height = 10)
print(plot)
dev.off()

png("output/BART.png", width = 880, height = 880)
print(plot)
dev.off()

# play sound when finished
beep(sound = 2)
