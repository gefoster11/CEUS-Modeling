#### Fit wash-in curve from Narnar ####

washin_fit <- function(x, dVI, Time, A = max(x[[dVI]], na.rm = TRUE)*0.90, B = 0.1, 
                       lower = c(A = A*0.90, B = 0.1), upper = c(A = A, B = 5.0)) {
  
  # formula using column names as strings
  formula <- as.formula(paste0(dVI, " ~ A * (1 - exp(-B * ", Time, "))"))
  
  initial_guesses <- list(A = A, B = B)
  
  nlsLM(
    formula,
    data = x,
    start = initial_guesses,
    lower = lower,
    upper = upper,
    control = nls.lm.control(maxiter = 1024)
  )
}