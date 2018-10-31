library(tidyverse)
library(magrittr)
library(rio)
library(jsonlite)
library(beepr)
library(ggmap)

setwd("C:/Users/Wenyao/Desktop/R/BART/")

# get BART stations info from api
stations <- "http://api.bart.gov/api/stn.aspx?cmd=stns&key=MW9S-E7SL-26DU-VV8V&json=y" %>% 
  read_lines() %>% 
  fromJSON() %>% 
  use_series(root) %>% 
  use_series(stations) %>% 
  use_series(station) %>% 
  mutate(
    longitude = as.numeric(gtfs_longitude),
    latitude = as.numeric(gtfs_latitude)
  )

# get BART routes info from api
all_routes_numbers <- c(7, 1, 11, 5, 19, 3)

spread_width <- 3e-3
routes <- all_routes_numbers %>% 
  paste0(
    "http://api.bart.gov/api/route.aspx?cmd=routeinfo&route=",
    .,
    "&key=MW9S-E7SL-26DU-VV8V&json=y"
  ) %>% 
  map(read_lines) %>% 
  map(fromJSON) %>%
  map(function(route){
    tibble(
      name = route$root$routes$route$name,
      abbr = route$root$routes$route$abbr,
      route_id = route$root$routes$route$routeID,
      number = route$root$routes$route$number,
      origin = route$root$routes$route$origin,
      destination = route$root$routes$route$destination,
      direction = route$root$routes$route$direction,
      hexcolor = route$root$routes$route$hexcolor,
      num_stations = route$root$routes$route$num_stns,
      stations = route$root$routes$route$config$station
    )
  }) %>% 
  bind_rows() %>% 
  left_join(
    stations %>% select(abbr, county, city, longitude, latitude),
    by = c("stations" = "abbr")
  ) %>% 
  group_by(stations) %>% 
  mutate(
    # spread the stations coordinates if multiple routes share the same station
    longitude = case_when(
      n() == 1 ~ longitude,
      TRUE ~ longitude + seq(-spread_width, spread_width, length.out = n()) * n()
    ),
    latitude = case_when(
      n() == 1 ~ latitude,
      (county %in% c("sanfrancisco") | stations %in% c("DALY") | city %in% c("Oakland")) & !stations %in% c("LAKE", "FTVL", "COLS") ~ 
        latitude - seq(-spread_width, spread_width, length.out = n()) * n(),
      TRUE ~ latitude + seq(-spread_width, spread_width, length.out = n()) * n()
    )
  )
  
# get bay area map info
map_data <- get_map(
  location = c(lon = mean(stations$longitude), lat = mean(stations$latitude)), 
  maptype = "roadmap",
  zoom = 10,
  scale = 2
)

# plot
plot <- ggmap(map_data) +
  geom_path(data = routes, aes(x = longitude, y = latitude, group = number), color = routes$hexcolor, size = 3, linejoin = "round", lineend = "round") +
  geom_point(data = stations, aes(x = longitude, y = latitude), color = "black", size = 9, alpha = 0.5) +
  geom_point(data = stations, aes(x = longitude, y = latitude), color = "white", size = 7, alpha = 0.8) +
  # geom_text(data = stations, aes(label = abbr, x = longitude, y = latitude)) +
  labs(
    x = "",
    y = ""
  ) +
  theme(
    plot.margin = margin(0, 0, -16, -16, "pt"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank()
  )

print(plot)


#==== output =====
svg("output/BART.svg", width = 8, height = 8)
print(plot)
dev.off()

png("output/BART.png", width = 880, height = 880)
print(plot)
dev.off()

# play sound when finished
beep(sound = 2)
