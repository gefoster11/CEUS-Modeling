

glance_mod <- function(glance, augment, predict) {
  glance <- glance %>% select(term, estimate)
  rsquared <- nlsLM_rsquared(augment$dVI, augment$`.fitted`)
  
  AUC <- AUC(predict$Time, predict$dVI_predict)
  
  max_diff <- max_diff(predict$Time, predict$dVI_predict)
  
  temp <- glance %>% rbind(., c("R2", rsquared)) %>% mutate(across(estimate, as.numeric)) %>%
    pivot_wider(names_from = term, values_from = estimate) %>% 
    mutate("AB" = A*B, AUC = AUC, max_diff = max_diff) %>% relocate(AB, .after = B)
  
}


