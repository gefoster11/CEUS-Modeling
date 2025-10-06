#### Background Subtract ####

back_subtract <- function(x) {
  x %>% mutate(dVI = VI - first(VI))
}