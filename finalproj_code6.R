# Continued from finalproj_code5

# Building confidence intervals for interpreting our Hurst coefficients
# Below we simulate gBm processes for each contract and collect interpreted 
# Hurst coefficients from 10,000 simulations
# We may examine the histograms of these distributions for each contract to see 
# whether our resulting Hurst coefficients as implied by our analysis are 
# significant for observed data

set.seed(101495)
x <- 10000
Hurst_ngbm_sims <- rep(NA, x)
k_change_var_ngbm_sims <- rep(NA, length(k_steps))
j=0
while (j <x) {
  data$NGEO_gBm_sims <- rep(NA, length(data$NGEO_Close))
  data$NGEO_gBm_sims[1] <- data$NGEO_Close[1]
  for (i in 2:length(data$NGEO_gBm_sims)) { ## calculating gBm for each 
    ## subsequent period
    data$NGEO_gBm_sims[i] = data$NGEO_gBm_sims[i-1]*exp(((ngeodrift)-.5*
                                                           (ngeo_ann_vol)^2)*dt+
                                                          ((ngeo_ann_vol)*rnorm(1)*
                                                             sqrt(dt)))
  }
  for (k in 1:length(k_steps)) {
    w <- length(data$NGEO_gBm_sims)-k
    k_changes_ngbm_sims <- rep(NA, w)
    for (i in 1:w) {
      k_changes_ngbm_sims[i] <- log(data$NGEO_gBm_sims[i+k]/data$NGEO_gBm_sims[i])
    }
    k_change_var_ngbm_sims[k] <- var(k_changes_ngbm_sims)
  }
  k_var_ratios_ngbm_sims <- rep(NA, length(k_change_var_ngbm_sims))
  for (i in (1:length(k_var_ratios_ngbm_sims))) {
    k_var_ratios_ngbm_sims[i] <- k_change_var_ngbm_sims[i]/k_change_var_ngbm_sims[1]
  }
  hurst_ngbm_df_sims <- data.frame(k_steps, k_var_ratios_ngbm_sims)
  reg_ngbm_sims <- lm(log(k_var_ratios_ngbm_sims)~log(k_steps), hurst_ngbm_df_sims)
  ngbm_sims <- as.matrix(coef(reg_ngbm_sims))
  j <- j + 1
  Hurst_ngbm_sims[j] <- ngbm_sims[2,1]/2
}
Hurst_ngbm_sims
hist(Hurst_ngbm_sims)
# We can calculate the 95% confidence interval for interpreting our Hurst coefficients by using the mean and standard deviation of the 10,000 simulations
Hurst_lb <- mean(Hurst_ngbm_sims)-(1.96*sd(Hurst_ngbm_sims))
Hurst_ub <- mean(Hurst_ngbm_sims)+(1.96*sd(Hurst_ngbm_sims))

# By doing the same for geo contracts, we can show that the variance ratio and, 
# by extension, the Hurst coefficient are independent of choice of drift rate or 
# volatility
# This can be seen in the identical lower and upper bounds given by our results
# Previous analyses have shown that the confidence bands increase with 
# increasing k steps in time and decrease with N number of observations
# We calculate a 95% confidence interval based on only the number of 
# observations N and the length of time step k>=sqrt(N) relevant to this analysis

set.seed(101495)
x <- 10000
Hurst_ggbm_sims <- rep(NA, x)
k_change_var_ggbm_sims <- rep(NA, length(k_steps))
j=0
while (j <x) {
  data$GEO_gBm_sims <- rep(NA, length(data$GEO_Close))
  data$GEO_gBm_sims[1] <- data$GEO_Close[1]
  for (i in 2:length(data$GEO_gBm_sims)) { ## calculating gBm for each subsequent 
    ## period
    data$GEO_gBm_sims[i] = data$GEO_gBm_sims[i-1]*exp(((geodrift)-.5*
                                                           (geo_ann_vol)^2)*dt+
                                                          ((geo_ann_vol)*rnorm(1)*
                                                             sqrt(dt)))
  }
  for (k in 1:length(k_steps)) {
    w <- length(data$GEO_gBm_sims)-k
    k_changes_ggbm_sims <- rep(NA, w)
    for (i in 1:w) {
      k_changes_ggbm_sims[i] <- log(data$GEO_gBm_sims[i+k]/data$GEO_gBm_sims[i])
    }
    k_change_var_ggbm_sims[k] <- var(k_changes_ggbm_sims)
  }
  k_var_ratios_ggbm_sims <- rep(NA, length(k_change_var_ggbm_sims))
  for (i in (1:length(k_var_ratios_ggbm_sims))) {
    k_var_ratios_ggbm_sims[i] <- k_change_var_ggbm_sims[i]/k_change_var_ggbm_sims[1]
  }
  hurst_ggbm_df_sims <- data.frame(k_steps, k_var_ratios_ggbm_sims)
  reg_ggbm_sims <- lm(log(k_var_ratios_ggbm_sims)~log(k_steps), hurst_ggbm_df_sims)
  ggbm_sims <- as.matrix(coef(reg_ggbm_sims))
  j <- j + 1
  Hurst_ggbm_sims[j] <- ggbm_sims[2,1]/2
}
Hurst_ggbm_sims
hist(Hurst_ggbm_sims)
# We can calculate the 95% confidence interval for interpreting our Hurst 
# coefficients by using the mean and standard deviation of the 10,000 simulations
Hurst_lb <- mean(Hurst_ggbm_sims)-(1.96*sd(Hurst_ggbm_sims))
Hurst_ub <- mean(Hurst_ggbm_sims)+(1.96*sd(Hurst_ggbm_sims))

df2 <- data.frame(
  Observed_Hurst_NGEO = Hurst_n,
  Observed_Hurst_GEO = Hurst_g,
  Lower_Bound = Hurst_lb,
  Upper_Bound = Hurst_ub
)
df2

# The End
