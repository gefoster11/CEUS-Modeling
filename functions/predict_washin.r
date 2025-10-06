# Predict Washin
# Takes provided Beta and Alpha values and generates washin curve

predict_washin <- function(t, A, B) {
  y <- A * (1 - exp(-B * t))
  y <- y %>% rename(dVI = Time)
  return(y)
}