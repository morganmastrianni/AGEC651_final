# Continued from finalproj_code2

# Below we gather some descriptive statistics
rownames(data) <- c(1:nrow(data))

# Get matched day data by removing NAs
data <- na.omit(data)

length(data)
nrow(data)

# Average prices over all matched day observations
ngeo_mean <- mean(data$NGEO_Close)
geo_mean <- mean(data$GEO_Close)

# Variances and standard deviations (equal to VAR.S in Excel)
ngeo_var <- var(data$NGEO_Close)
geo_var <- var(data$GEO_Close)
ngeo_stdev <- sqrt(ngeo_var)
geo_stdev <- sqrt(geo_var)
ngeo_var
geo_var
ngeo_stdev
geo_stdev

## note: we can look at the geometric mean growth over a 252 day trading period 
## using the mean of the log of price data over each 252 day sub-period 
j = 1
vec <- c(252:nrow(data))
ngeoms <- rep(NA, length(vec))
while (j <= length(vec)) {
  ngeoms[j] = exp(mean(log(data$NGEO_Close[j:vec[j]])))
  j <- j + 1
}
ngeogrowth <- mean(ngeoms)
ngeogrowth

j = 1
vec <- c(252:nrow(data))
geoms <- rep(NA, length(vec))
while (j <= length(vec)) {
  geoms[j] = exp(mean(log(data$GEO_Close[j:vec[j]])))
  j <- j + 1
}
geogrowth <- mean(geoms)
geogrowth

# Now we need to calculate annualized volatility
# First, calculate daily percentage change
data$NGEO_Daily_Perc <- rep(NA, length(data$NGEO_Close))
for (i in 2:length(data$NGEO_Daily_Perc)) { ## doesn't calculate change for 
  ## first row
  data$NGEO_Daily_Perc[i] = (data$NGEO_Close[i]-
                               data$NGEO_Close[i-1])/data$NGEO_Close[i-1]
}
data$NGEO_Daily_Perc
ngeodrift <- mean(data$NGEO_Daily_Perc[2:length(data$NGEO_Daily_Perc)])*252

data$GEO_Daily_Perc <- rep(NA, length(data$GEO_Close))
for (i in 2:length(data$GEO_Daily_Perc)) { ## doesn't calculate change for 
  ## first row
  data$GEO_Daily_Perc[i] = (data$GEO_Close[i]-
                              data$GEO_Close[i-1])/data$GEO_Close[i-1]
}
data$GEO_Daily_Perc
geodrift <- mean(data$GEO_Daily_Perc[2:length(data$GEO_Daily_Perc)]*252)

# We can calculate annualized volatility from daily log returns
data$NGEO_Log_Ret <- rep(NA, length(data$NGEO_Close))
for (i in 2:length(data$NGEO_Log_Ret)) { ## doesn't calculate change for first 
  ## row
  data$NGEO_Log_Ret[i] = log(data$NGEO_Close[i]/data$NGEO_Close[i-1])
}

data$GEO_Log_Ret <- rep(NA, length(data$GEO_Close))
for (i in 2:length(data$GEO_Log_Ret)) { ## doesn't calculate change for first 
  ## row
  data$GEO_Log_Ret[i] = log(data$GEO_Close[i]/data$GEO_Close[i-1])
}

# Below calculates the standard deviations and annualized volatility for each 
# contract
ngeo_dailyperc_var <- var(data$NGEO_Log_Ret[2:length(data$NGEO_Log_Ret)])
geo_dailyperc_var <- var(data$GEO_Log_Ret[2:length(data$GEO_Log_Ret)]) ## equal 
## to var.s in excel
ngeo_dailyperc_stdev <- sqrt(ngeo_dailyperc_var)
geo_dailyperc_stdev <- sqrt(geo_dailyperc_var)
ngeo_ann_vol <- sqrt(252)*ngeo_dailyperc_stdev
geo_ann_vol <- sqrt(252)*geo_dailyperc_stdev

# Table for descriptive stats, including maximums and minimums
geo_max <- max(data$GEO_Close)
ngeo_max <- max(data$NGEO_Close)
geo_min <- min(data$GEO_Close)
ngeo_min <- min(data$NGEO_Close)

df <- data.frame(
  Contract = c("GEO", "NGEO"),
  Mean = c(geo_mean, ngeo_mean),
  Maximum = c(geo_max, ngeo_max),
  Minimum = c(geo_min, ngeo_min),
  Annual_Volatility = c(geo_ann_vol, ngeo_ann_vol),
  Annual_Drift = c(geodrift, ngeodrift)
)
df
# This is the table for our descriptive statistics

# Now that we have volatilities and drift rates for each contract, we can model
# a geometric Brownian motion for each series given the price on the first
# day of matched data

# Move to finalproj_code4

data$avg_Daily_Perc <- rep(NA, length(data$GEO_Close))
for (i in 2:length(data$avg_Daily_Perc)) { ## doesn't calculate change for 
  ## first row
  data$avg_Daily_Perc[i] = (((data$GEO_Close[i]+data$NGEO_Close[i])/2)-
                              ((data$GEO_Close[i-1]+data$NGEO_Close[i-1])/2))/((data$GEO_Close[i-1]+data$NGEO_Close[i-1])/2)
}
data$avg_Daily_Perc
avgdrift <- mean(data$avg_Daily_Perc[2:length(data$avg_Daily_Perc)]*252)

# We can calculate annualized volatility from daily log returns
data$avg_Log_Ret <- rep(NA, length(data$NGEO_Close))
for (i in 2:length(data$avg_Log_Ret)) { ## doesn't calculate change for first 
  ## row
  data$avg_Log_Ret[i] = log(((data$GEO_Close[i]+data$NGEO_Close[i])/2)/((data$GEO_Close[i-1]+data$NGEO_Close[i-1])/2))
}

avg_dailyperc_var <- var(data$avg_Log_Ret[2:length(data$avg_Log_Ret)]) ## equal 
## to var.s in excel
avg_dailyperc_stdev <- sqrt(avg_dailyperc_var)

avg_ann_vol <- sqrt(252)*avg_dailyperc_stdev
avgdrift
avg_ann_vol

