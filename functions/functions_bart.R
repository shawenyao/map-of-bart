#'
#'
get_bart_stations <- function(){
  
  "http://api.bart.gov/api/stn.aspx?cmd=stns&key=MW9S-E7SL-26DU-VV8V&json=y" %>% 
    read_lines() %>% 
    fromJSON() %>% 
    use_series(root) %>% 
    use_series(stations) %>% 
    use_series(station) %>% 
    mutate(
      longitude = as.numeric(gtfs_longitude),
      latitude = as.numeric(gtfs_latitude)
    )
}


#'
#'
get_bart_routes <- function(route_numbers){
  
  paste0(
    "http://api.bart.gov/api/route.aspx?cmd=routeinfo&route=",
    route_numbers,
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
    bind_rows()
}


#' spread the station coordinates if multiple routes share the same station
#'
spread_routes <- function(routes_with_station_coordinates, spread_width){
  
  routes_with_station_coordinates %>% 
    group_by(stations) %>% 
    mutate(
      # spread the stations coordinates if multiple routes share the same station
      longitude = case_when(
        # route 19 is an exception (only 2 stops)
        n() == 1 | number == 19 ~ longitude,
        TRUE ~ longitude + seq(-spread_width, spread_width, length.out = n()) * n()
      ),
      latitude = case_when(
        # route 19 is an exception (only 2 stops)
        n() == 1 | number == 19 ~ latitude,
        # determine the direction of the shift based on stations
        (county %in% c("sanfrancisco") | stations %in% c("DALY") | city %in% c("Oakland")) & !stations %in% c("LAKE", "FTVL", "COLS") ~ 
          latitude - seq(-spread_width, spread_width, length.out = n()) * n(),
        TRUE ~ latitude + seq(-spread_width, spread_width, length.out = n()) * n()
      )
    ) %>% 
    ungroup()
}


#'
#'
plot_bart <- function(map_data, routes, stations){
  
  ggmap(map_data) +
    # the stroke of the routes
    geom_path(data = routes, aes(x = x, y = y, group = number), color = "black", size = 4, alpha = 0.5, linejoin = "round", lineend = "round") +
    # the routes
    geom_path(data = routes, aes(x = x, y = y, group = number), color = routes$hexcolor, size = 3, alpha = 0.99, linejoin = "round", lineend = "round") +
    # the stroke of the stations
    geom_point(data = stations, aes(x = longitude, y = latitude), color = "black", size = 9, alpha = 0.5) +
    # th stations
    geom_point(data = stations, aes(x = longitude, y = latitude), color = "white", size = 7, alpha = 0.8) +
    labs(x = "", y = "") +
    theme(
      plot.margin = margin(0, 0, -16, -16, "pt"),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.border = element_blank()
    )
}
