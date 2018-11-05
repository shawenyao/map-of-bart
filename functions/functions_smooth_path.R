#' reverse the row order of a data.frame
#' 
#' @param df a data.frame
#' 
#' @return a data.frame with reversed row order
#' 
reverse_df <- function(df){
  
  df %>% slice(seq(nrow(.), 1, by = -1))
}

#' normalize a vector to become a unit vector
#' 
#' @param numeric_vector a numeric vector to be normalized
#' 
#' @return the unit vector
#' 
normalize_vector <- function(numeric_vector){
  
  if(all(numeric_vector == c(0, 0))){
    numeric_vector
  }else{
    numeric_vector / sqrt(sum(numeric_vector ^ 2))
  }
}


#' smooth a given path defined by three points p1, p2 and p3
#'
#' @param subpath a data.frame of the Cartiesian coordinates of p1, p2 and p3
#' @param lamda a constant that controls the extent to which the curvature is applied
#' 
#' @return a data.frame of the coordinates of p1 and the interpolated point
#' 
smooth_subpath <- function(subpath, lamda = 0.2){
  
  x <- subpath$x
  y <- subpath$y
  
  # solving the system of equations
  # y = a * x + b
  # y = c * x + d
  # to find the projection of p3 on the line that crosses p1 and p2
  a <- (y[2] - y[1]) / (x[2] - x[1])
  b <- y[1] - (y[2] - y[1]) / (x[2] - x[1]) * x[1]
  c <- -(x[2] - x[1]) / (y[2] - y[1])
  d <- y[3] + (x[2] - x[1]) / (y[2] - y[1]) * x[3]
  
  projection_x3 <- (d - b) / (a - c)
  projection_y3 <- (a * d - b * c) / (a - c)
  
  unit_vector <- c(projection_x3 - x[3], projection_y3 - y[3]) %>% normalize_vector()
  
  # find the angle between vector p1 - p2 and p3 - p2 to determine the curvature
  vector1 <- c(x[1] - x[2], y[1] - y[2])
  vector2 <- c(x[3] - x[2], y[3] - y[2])
  
  cos_theta <- sum(vector1 * vector2) / (norm(vector1, type = "2") * norm(vector2, type = "2"))
  
  # find the interpolated point
  new_point <- 
    # the midpoint of the segment
    c(1 / 2 * (x[1] + x[2]), 1 / 2 * (y[1] + y[2])) +
    # the curvature adjustment rate
    lamda *
    # the curvature
    (cos_theta + 1) *
    # the direction at which to apply the shift
    unit_vector *
    # the length of the segment to smooth
    norm(vector1, type = "2")
  
  # return p1 along with the new point
  tibble(
    x = c(x[1], new_point[1]),
    y = c(y[1], new_point[2])
  )
}


#' interpolate over every consecutive 3-point subpath
#' 
#' @param path a data.frame of the Cartiesian coordinates of the path
#' 
#' @return a data.frame of the smoothed path
#' 
smooth_path <- function(path, lamda = 0.2){
  
  seq_len(nrow(path) - 2) %>% 
    # loop for every 3-point subpath
    map(function(i){
      smooth_subpath(subpath = path %>% slice(i:(i+2)), lamda = lamda)
    }) %>% 
    bind_rows() %>% 
    # append the last two points as is, plus the midpoint between them
    bind_rows(
      tibble(
        x = c(
          # the second to last point
          tail(path$x, 2)[1],
          # the midpoint between the last 2 points
          mean(tail(path$x, 2)),
          # the last point
          tail(path$x, 1)
        ),
        y = c(
          # the second to last point
          tail(path$y, 2)[1],
          # create the midpoint between the last 2 points
          mean(tail(path$y, 2)),
          # the last point
          tail(path$y, 1)
        )
      )
    )
}


#' interpolate over every consecutive 3-point subpath
#' both in the original and the reversed order
#' and average them
#' 
#' @param path a data.frame of the Cartiesian coordinates of the path
#' 
#' @return a data.frame of the smoothed path
#' 
smooth_path_double <- function(path, lamda = 0.2){
  
  path1 <- smooth_path(path, lamda = lamda)
  path2 <- smooth_path(path %>% reverse_df(), lamda = lamda)
  
  # the average position between interpolation from the original path and reversed path
  # except for the first and last 2 points
  weight <- c(rep(1, times = 2), rep(0.5, times = nrow(path1) - 4), rep(0, times = 2))
  tibble(
    x = path1$x * weight + rev(path2$x) * (1 - weight),
    y = path1$y * weight + rev(path2$y) * (1 - weight)
  )
}
