max_diff <- function(x, y) {
  # Calculate the difference between consecutive x and y values
  run <- diff(x)
  rise <- diff(y)
  
  # Calculate the slope for each interval
  slope <- rise / run
  
  max_differential <- max(abs(slope))
  
  max_differential
}