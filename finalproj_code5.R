# Continued from finalproj_code4

# Now we can visualize whether variance is increasing over time
# We will use k_steps = 20 to observe correlations over all available (our 
# number of matched day observations is 394, the square root being 19.8494)
# consecutive periods in the data from 1 to 20 days in length as we want to 
# test whether the variance over a 20 day period is 10 times that of the
# variance of a 2 day period
k_steps <- c(1:20)
k_change_var_n <- rep(NA, length(k_steps))

for (k in 1:length(k_steps)) {
  w <- length(data$NGEO_Close)-k
  k_changes_n <- rep(NA, w)
  for (i in 1:w) {
    k_changes_n[i] <- log(data$NGEO_Close[i+k]/data$NGEO_Close[i])
  }
  k_change_var_n[k] <- var(k_changes_n)
}
k_change_var_n
## k_change_var shows the variance for each period of time from k=1 to k=20
length(k_change_var_n)
k_var_ratios_n <- rep(NA, length(k_change_var_n))
for (i in (1:length(k_var_ratios_n))) {
  k_var_ratios_n[i] <- k_change_var_n[i]/k_change_var_n[1]
}
k_var_ratios_n

# This shows the variance properties over time... a true geometric Brownian 
# motion with a Hurst of 0.5 would be linear in variance over time

# Let's show how variance over time increases with a randomly simulated
# geometric Brownian motion using the realized drift and volatility calculated
# in finalproj_code3
k_change_var_ngbm <- rep(NA, length(k_steps))

for (k in 1:length(k_steps)) {
  w <- length(data$NGEO_gBm)-k
  k_changes_ngbm <- rep(NA, w)
  for (i in 1:w) {
    k_changes_ngbm[i] <- log(data$NGEO_gBm[i+k]/data$NGEO_gBm[i])
  }
  k_change_var_ngbm[k] <- var(k_changes_ngbm)
}
k_change_var_ngbm
## k_change_var shows the variance for each period of time from k=1 to k=20
## to find the hurst coefficient, we can divide each calculated variance for 
## k=1,...,20 by the variance for k=1
length(k_change_var_ngbm)
k_var_ratios_ngbm <- rep(NA, length(k_change_var_ngbm))
for (i in (1:length(k_var_ratios_ngbm))) {
  k_var_ratios_ngbm[i] <- k_change_var_ngbm[i]/k_change_var_ngbm[1]
}
k_var_ratios_ngbm
png("NGEO_variance_ratios.png", width=800, height=600, pointsize=20, 
    bg="white", 
    type="windows")
plot(k_steps, k_var_ratios_n, type="l", lwd=1.5, col="red", ylab = "", 
     xlab = "")
title(main="Variance Ratios, NGEO and NGEO Simulated gBm",  
      ylab = 'Variance Ratio', xlab = "Number of Time Steps (k)")
lines(k_steps, k_var_ratios_ngbm, lwd=1.5, col="blue")

dev.off()

# We will do the same for GEO futures
k_change_var_g <- rep(NA, length(k_steps))

for (k in 1:length(k_steps)) {
  w <- length(data$GEO_Close)-k
  k_changes_g <- rep(NA, w)
  for (i in 1:w) {
    k_changes_g[i] <- log(data$GEO_Close[i+k]/data$GEO_Close[i])
  }
  k_change_var_g[k] <- var(k_changes_g)
}
k_change_var_g
## k_change_var shows the variance for each period of time from k=1 to k=20
length(k_change_var_g)
k_var_ratios_g <- rep(NA, length(k_change_var_g))
for (i in (1:length(k_var_ratios_g))) {
  k_var_ratios_g[i] <- k_change_var_g[i]/k_change_var_g[1]
}
k_var_ratios_g

# This shows the variance properties over time... a true geometric Brownian 
# motion with a Hurst of 0.5 would be linear in variance over time

# Let's show how variance over time increases with a randomly simulated
# geometric Brownian motion using the realized drift and volatility calculated
# in finalproj_code3
k_change_var_ggbm <- rep(NA, length(k_steps))

for (k in 1:length(k_steps)) {
  w <- length(data$GEO_gBm)-k
  k_changes_ggbm <- rep(NA, w)
  for (i in 1:w) {
    k_changes_ggbm[i] <- log(data$GEO_gBm[i+k]/data$GEO_gBm[i])
  }
  k_change_var_ggbm[k] <- var(k_changes_ggbm)
}
k_change_var_ggbm
## k_change_var shows the variance for each period of time from k=1 to k=20
## to find the hurst coefficient, we can divide each calculated variance for 
## k=1,...,20 by the variance for k=1
length(k_change_var_ggbm)
k_var_ratios_ggbm <- rep(NA, length(k_change_var_ggbm))
for (i in (1:length(k_var_ratios_ggbm))) {
  k_var_ratios_ggbm[i] <- k_change_var_ggbm[i]/k_change_var_ggbm[1]
}
k_var_ratios_ggbm
png("GEO_variance_ratios.png", width=800, height=600, pointsize=20, 
    bg="white", 
    type="windows")
plot(k_steps, k_var_ratios_g, type="l", lwd=1.5, col="red", ylab = "", 
     xlab = "")
title(main="Variance Ratios, GEO and GEO Simulated gBm",  
      ylab = 'Variance Ratio', xlab = "Number of Time Steps (k)")
lines(k_steps, k_var_ratios_ggbm, lwd=1.5, col="blue")

dev.off()

# Two more .png images are now in the project folder: NGEO_variance_ratios and 
# GEO_variance_ratios

# Below we calculate the Hurst coefficients for the simulated geometric Brownian 
# motions and for the observed data:
hurst_ngbm_df <- data.frame(k_steps, k_var_ratios_ngbm)
reg_ngbm <- lm(log(k_var_ratios_ngbm)~log(k_steps), hurst_ngbm_df)
ngbm <- as.matrix(coef(reg_ngbm))
Hurst_ngbm <- ngbm[2,1]/2
Hurst_ngbm
hurst_ggbm_df <- data.frame(k_steps, k_var_ratios_ggbm)
reg_ggbm <- lm(log(k_var_ratios_ggbm)~log(k_steps), hurst_ggbm_df)
ggbm <- as.matrix(coef(reg_ggbm))
Hurst_ggbm <- ggbm[2,1]/2
Hurst_ggbm
# We can see that the Hurst coefficient for the simulated gBm is not 
# significantly different from 0.5, which confirms our model produces the 
# desired coefficient
hurst_n_df <- data.frame(k_steps, k_var_ratios_n)
reg_n <- lm(log(k_var_ratios_n)~log(k_steps), hurst_n_df)
n <- as.matrix(coef(reg_n))
Hurst_n <- n[2,1]/2
Hurst_n
hurst_g_df <- data.frame(k_steps, k_var_ratios_g)
reg_g <- lm(log(k_var_ratios_g)~log(k_steps), hurst_g_df)
g <- as.matrix(coef(reg_g))
Hurst_g <- g[2,1]/2
Hurst_g

# Move to finalproj_code6
