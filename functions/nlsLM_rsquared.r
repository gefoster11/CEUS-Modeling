#### determine rsquared from nlsLM model from the augment data frame ####
# y_actual
# y_predict

nlsLM_rsquared <- function(y_actual, y_predicted) {
  rss <- sum((y_actual - y_predicted)^2)
  tss <- sum((y_actual - mean(y_actual))^2)
  rsq <- 1 - rss / tss
  rsq
}