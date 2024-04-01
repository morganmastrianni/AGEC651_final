# Continued from finalproj_code3

# We want to model a geometric Brownian motion using the realized drift rates 
# and volatility of our futures prices to obtain our Hurst coefficients...

dt = 1/252 
# Above represents our time step; this annualizes the data since we have daily
# futures prices

# Let's model a geometric Brownian motion using the price data (the first entry
# and the drift and volatility)
data$NGEO_gBm <- rep(NA, length(data$NGEO_Close))
data$NGEO_gBm[1] <- data$NGEO_Close[1]
for (i in 2:length(data$NGEO_gBm)) { ## calculating gBm for each subsequent 
  ## period
  data$NGEO_gBm[i] = data$NGEO_gBm[i-1]*exp(((ngeodrift)-.5*(ngeo_ann_vol)^2)*
                                              dt+((ngeo_ann_vol)*rnorm(1)*
                                                    sqrt(dt)))
}

## note: col2rgb("royalblue") will return properties of color "royalblue"
myroyalblue <- rgb(65,105,225, max=395, alpha=75, names = "royalbluecustom")

png("NGEO_simulated_paths_gBm.png", width=800, height=600, pointsize=20, 
    bg="white", 
    type="windows")
plot(data$Date, data$NGEO_gBm, type = "l", lwd = 1.5, col = myroyalblue, 
     ylim = c(0,45), xaxt = "n", ylab = "", xlab = "")
title(main="NGEO Futures (simulated), Aug '21 - Feb '23",  
      ylab = 'NGEO Futures Price @ Close')

# We can repeat this x times; try changing the value of x to see how the output
# graph changes

set.seed(101495)
x <- 50
j=0
while (j <=x) {
  data$NGEO_gBm <- rep(NA, length(data$NGEO_Close))
  data$NGEO_gBm[1] <- data$NGEO_Close[1]
  for (i in 2:length(data$NGEO_gBm)) { ## calculating gBm for each subsequent 
    ## period
    data$NGEO_gBm[i] = data$NGEO_gBm[i-1]*exp(((ngeodrift)-.5*
                                                 (ngeo_ann_vol)^2)*dt+
                                                ((ngeo_ann_vol)*rnorm(1)*
                                                   sqrt(dt)))
  }
  lines(data$Date, data$NGEO_gBm, lwd = 1.5, col = myroyalblue)
  j <- j + 1
}
## add line for observed data
lines(data$Date, data$NGEO_Close, lwd = 1.5, col = "red")
dev.off()

# Below we do the same for GEO futures
data$GEO_gBm <- rep(NA, length(data$GEO_Close))
data$GEO_gBm[1] <- data$GEO_Close[1]
for (i in 2:length(data$GEO_gBm)) { ## calculating gBm for each subsequent 
  ## period
  data$GEO_gBm[i] = data$GEO_gBm[i-1]*exp(((geodrift)-.5*(geo_ann_vol)^2)*dt+
                                            ((geo_ann_vol)*rnorm(1)*sqrt(dt)))
}

png("GEO_simulated_paths_gBm.png", width=800, height=600, pointsize=20, 
    bg="white", 
    type="windows")
plot(data$Date, data$GEO_gBm, type = "l", lwd = 1.5, col = myroyalblue, 
     ylim = c(0,22), xaxt = "n", ylab = "", xlab = "")
title(main="GEO Futures (simulated), Aug '21 - Feb '23",  
      ylab = 'GEO Futures Price @ Close')

# We can repeat this x times; try changing the value of x to see how the output
# graph changes
set.seed(101495)
x <- 50
j=0
while (j <=x) {
  data$GEO_gBm <- rep(NA, length(data$GEO_Close))
  data$GEO_gBm[1] <- data$GEO_Close[1]
  for (i in 2:length(data$GEO_gBm)) { ## calculating gBm for each subsequent 
    ## period
    data$GEO_gBm[i] = data$GEO_gBm[i-1]*exp(((geodrift)-.5*(geo_ann_vol)^2)*dt+
                                              ((geo_ann_vol)*rnorm(1)*sqrt(dt)))
  }
  lines(data$Date, data$GEO_gBm, lwd = 1.5, col = myroyalblue)
  j <- j + 1
}
## add line for observed data
lines(data$Date, data$GEO_Close, lwd = 1.5, col = "red")
dev.off()

# Two more .png images are now in the project folder; NGEO_simulated_paths_gBm
# and GEO_simulated_paths_gBm

# Move to finalproj_code5
