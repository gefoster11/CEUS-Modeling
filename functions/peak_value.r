# finds the peak value whether it be positive or negative

peak_value <- function(x) {
  
  max_positive <- max(x, na.rm = TRUE)
  min_negative <- min(x, na.rm = TRUE)
  
  # Determine which has larger magnitude
  if (abs(max_positive) > abs(min_negative)) {
    peak_value <- max_positive
  } else {
    peak_value <- min_negative
  }
  
  return(peak_value)
}