# remotes::install_github('SEEG-Oxford/ageStand')
library(ageStand)

p2i_0_to_5  <- function(x) {(2.38*x + 3.92*x^2 - 9.30*x^3 + 5.57*x^4 - 0.53*x^5)}
p2i_5_to_15 <- function(x) {(2.34*x + 2.29*x^2 -13.30*x^3 + 13.18*x^4 - 4.36*x^5)}
p2i_15plus  <- function(x) {(2.87*x - 10.14*x^2 + 14.90*x^3 - 10.14*x^4 + 2.82*x^5)}

cprev_st <- convertPrevalence(prevalence = seq(0, 1, by = 0.001),
                              age_min_in = rep(0, 1001),
                              age_min_out = rep(2, 1001),
                              age_max_in = rep(5, 1001),
                              age_max_out = rep(10, 1001))


prev_u5_to_incd_all <- function (prev_u5, age_struct) {
  if (length(age_struct) != 3 | sum(age_struct) != 1) {
    stop("Age Structure is wrong.")
  } else if (prev_u5 < 0 | prev_u5 > 1) {
    stop("Prevalence out of range.")
  }
  
  prev_conv <- quantile(cprev_st, prev_u5)
  
  incd_all <- (c(p2i_0_to_5(prev_conv),
                 p2i_5_to_15(prev_conv),
                 p2i_15plus(prev_conv)) * age_struct) %>% sum
    
  return(incd_all)
}

# plot(seq(0, 1, by = 0.01), 
#      sapply(seq(0, 1, by = 0.01),
#             function (x) prev_u5_to_incd_all(x, c(0.142, 0.266, 0.592))))
