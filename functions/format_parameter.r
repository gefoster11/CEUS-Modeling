# formats parameter to "fit" nicely with model data
# parameter_name = "R-squared"
# value = round(fitted$r_squared,2)
# round_to = 2 - number of decimals places.

format_parameter <- function(parameter_name, value){
  data.frame(
    parameter = parameter_name,
    value = value,
    std.error = NA,
    t.value = NA,
    p.value = NA,
    stringsAsFactors = FALSE
  )
}