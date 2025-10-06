
AUC <- function(x, y) {
  
  # Calculate the differences between adjacent x-values
  dx <- diff(x)
  
  # Average the y-values for each trapezoid
  y_avg <- (y[-length(y)] + y[-1]) / 2
  
  # Multiply the averaged y-values by the x-differences and sum them
  area_base_r <- sum(dx * y_avg)
  
  # Print the result
  area_base_r  
  
}

