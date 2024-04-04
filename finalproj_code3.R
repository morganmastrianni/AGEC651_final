# Continued from finalproj_code2

# Below we gather some descriptive statistics
# rownames(data) <- c(1:nrow(data))

# # Get matched day data by removing NAs
# data <- na.omit(data)

# length(data)
# nrow(data)

# Average prices over all matched day observations
# wheat_mean = mean(wheat)
# maize_mean = mean(maize)
# soy_mean = mean(soy)
# rice_mean = mean(rice)
# barley_mean = mean(barley)

# # Variances and standard deviations (equal to VAR.S in Excel)
# wheat_var = var(wheat)
# maize_var = var(maize)
# soy_var = var(soy)
# rice_var = var(rice)
# barley_var = var(barley)
# ngeo_stdev <- sqrt(ngeo_var)
# wheat_stdev = sqrt(wheat_var)
# geo_stdev <- sqrt(geo_var)




## note: we can look at the geometric mean growth over a 252 day trading period 
## using the mean of the log of price data over each 252 day sub-period 
ticks = ["w", "m", "s", "r", "b"]
geogrowths = zeros(Float64, length(ticks))

vec = 252:dims[1] - 1
geo = zeros(Float64, length(vec))
for commodity in 1:length(ticks)
  j = 1
  while j <= length(vec)
    # geo[j] = (all_commodities[vec[j], commodity]/all_commodities[j, commodity]) - 1
    geo[j] = 1 + (all_commodities[vec[j], commodity] - all_commodities[j, commodity])/all_commodities[j, commodity]
    j += 1
  end
  geogrowths[commodity] = prod(geo)^(1/length(geo)) - 1
end

# # Now we need to calculate annualized volatility
# # First, calculate daily percentage change
# data$NGEO_Daily_Perc <- rep(NA, length(data$NGEO_Close))
# # for (i in 2:length(data$NGEO_Daily_Perc)) { ## doesn't calculate change for 
# #   ## first row
# #   data$NGEO_Daily_Perc[i] = (data$NGEO_Close[i]-
# #                                data$NGEO_Close[i-1])/data$NGEO_Close[i-1]
# # }

# subtract 1 from each of d_w, etc from matrix practice to get perc changes
perc_changes = hcat(d_w, d_m, d_s, d_r, d_b)
# MAY CHANGE to log(wheat[i+1]/wheat[i])
for i in 1:size(perc_changes)[2]
  perc_changes[:, i] = log.(perc_changes[:, i])
end

drift = zeros(Float64, length(ticks))
for commodity in 1:length(ticks)
  drift[commodity] = mean((perc_changes[:, commodity]))*252
end

volatility = zeros(Float64, length(ticks))
for commodity in 1:length(ticks)
  volatility[commodity] = sqrt(252*(sum(perc_changes[:, commodity] .^2)/length(perc_changes[:, commodity])))
end

# vec = 252:dims[1] - 1
# geo = zeros(Float64, length(vec))
# for commodity in 1:length(ticks)
#   j = 1
#   while j <= length(vec)
#     geo[j] = 1 + (all_commodities[vec[j], commodity] - all_commodities[j, commodity])/all_commodities[j, commodity]
#     j += 1
#   end
#   geogrowths[commodity] = prod(geo)^(1/length(geo)) - 1
# end

# # We can calculate annualized volatility from daily log returns
# data$NGEO_Log_Ret <- rep(NA, length(data$NGEO_Close))
# for (i in 2:length(data$NGEO_Log_Ret)) { ## doesn't calculate change for first 
#   ## row
#   data$NGEO_Log_Ret[i] = log(data$NGEO_Close[i]/data$NGEO_Close[i-1])
# }

# data$GEO_Log_Ret <- rep(NA, length(data$GEO_Close))
# for (i in 2:length(data$GEO_Log_Ret)) { ## doesn't calculate change for first 
#   ## row
#   data$GEO_Log_Ret[i] = log(data$GEO_Close[i]/data$GEO_Close[i-1])
# }

# # Below calculates the standard deviations and annualized volatility for each 
# # contract
# ngeo_dailyperc_var <- var(data$NGEO_Log_Ret[2:length(data$NGEO_Log_Ret)])
# geo_dailyperc_var <- var(data$GEO_Log_Ret[2:length(data$GEO_Log_Ret)]) ## equal 
# ## to var.s in excel
# ngeo_dailyperc_stdev <- sqrt(ngeo_dailyperc_var)
# geo_dailyperc_stdev <- sqrt(geo_dailyperc_var)
# ngeo_ann_vol <- sqrt(252)*ngeo_dailyperc_stdev
# geo_ann_vol <- sqrt(252)*geo_dailyperc_stdev

# # Table for descriptive stats, including maximums and minimums
# geo_max <- max(data$GEO_Close)
# ngeo_max <- max(data$NGEO_Close)
# geo_min <- min(data$GEO_Close)
# ngeo_min <- min(data$NGEO_Close)

# df <- data.frame(
#   Contract = c("GEO", "NGEO"),
#   Mean = c(geo_mean, ngeo_mean),
#   Maximum = c(geo_max, ngeo_max),
#   Minimum = c(geo_min, ngeo_min),
#   Annual_Volatility = c(geo_ann_vol, ngeo_ann_vol),
#   Annual_Drift = c(geodrift, ngeodrift)
# )
# df
# # This is the table for our descriptive statistics

# # Now that we have volatilities and drift rates for each contract, we can model
# # a geometric Brownian motion for each series given the price on the first
# # day of matched data

# # Move to finalproj_code4

# data$avg_Daily_Perc <- rep(NA, length(data$GEO_Close))
# for (i in 2:length(data$avg_Daily_Perc)) { ## doesn't calculate change for 
#   ## first row
#   data$avg_Daily_Perc[i] = (((data$GEO_Close[i]+data$NGEO_Close[i])/2)-
#                               ((data$GEO_Close[i-1]+data$NGEO_Close[i-1])/2))/((data$GEO_Close[i-1]+data$NGEO_Close[i-1])/2)
# }
# data$avg_Daily_Perc
# avgdrift <- mean(data$avg_Daily_Perc[2:length(data$avg_Daily_Perc)]*252)

# # We can calculate annualized volatility from daily log returns
# data$avg_Log_Ret <- rep(NA, length(data$NGEO_Close))
# for (i in 2:length(data$avg_Log_Ret)) { ## doesn't calculate change for first 
#   ## row
#   data$avg_Log_Ret[i] = log(((data$GEO_Close[i]+data$NGEO_Close[i])/2)/((data$GEO_Close[i-1]+data$NGEO_Close[i-1])/2))
# }

# avg_dailyperc_var <- var(data$avg_Log_Ret[2:length(data$avg_Log_Ret)]) ## equal 
# ## to var.s in excel
# avg_dailyperc_stdev <- sqrt(avg_dailyperc_var)

# avg_ann_vol <- sqrt(252)*avg_dailyperc_stdev
# avgdrift
# avg_ann_vol


avg_ann_vol <- sqrt(252)*avg_dailyperc_stdev
avgdrift
avg_ann_vol

