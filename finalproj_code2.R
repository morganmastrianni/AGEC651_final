# Continued from finalproj_code1

# Initial plotting to visualize data
plot(data[,c("GEO_Close", "NGEO_Close")], type = "p")
## note: it looks like there is some correlation between the futures prices 
## based on the other's price; may imply that they are both based on the same 
## underlying "commodity", aka the price of carbon

Date <- c(data$Date)

# Shows GEO price path
png("GEO_prices.png", width=800, height=600, pointsize=20, bg="white", 
    type="windows")
plot(Date, data$GEO_Close, type = "l", lwd = 1.5, col = "royalblue", ylab = "", 
     xlab = "", xaxt = "n")
title(main="GEO Futures, Aug '21 - Feb '23",  
      ylab = 'GEO Futures Price @ Close')
dev.off()

# Shows NGEO price path
png("NGEO_prices.png", width=800, height=600, pointsize=20, bg="white", 
    type="windows")
plot(Date, data$NGEO_Close, type = "l", lwd = 1.5, 
     col = "forestgreen", 
     ylim = c(0,16), xaxt = "n", ylab = "", xlab = "")
title(main="NGEO Futures, Aug '21 - Feb '23",  
      ylab = 'NGEO Futures Price @ Close')
dev.off()

# Shows both price paths together
png("GEO&NGEO_prices.png", width=800, height=600, pointsize=20, bg="white", 
    type="windows")
plot(Date, data$NGEO_Close, type = "l", lwd = 1.5, col = "forestgreen", 
     ylim = c(0,16), xaxt = "n", ylab = "", xlab = "")
title(main="GEO and NGEO Futures, Aug '21 - Feb '23",  
      ylab = 'Futures Price @ Close')
lines(Date, data$GEO_Close, lwd = 1.5, col = "royalblue")
lines(Date, data$Close_Difference, lwd = 1.5, col = "salmon")
legend('topleft', legend=c("NGEO Futures", "GEO Futures", 
                           "Difference in Price"), lwd = 2.5, 
       col = c("forestgreen", "royalblue", "salmon"),pch=15)
dev.off()

# Show plot change in diff to proxy corr over each period?
png("Change_in_diffs.png", width=800, height=600, pointsize=20, bg="white", 
    type="windows")
data$Close_Diff_Change <- rep(NA, length(data$Close_Difference))
for (i in 2:length(data$Close_Diff_Change)) { # doesn't calculate change for 
  # first row
  data$Close_Diff_Change[i] = data$Close_Difference[i]-
    data$Close_Difference[i-1]
}
plot(Date[109:393], data$Close_Diff_Change[109:393], type = "l", lwd = 1.5, 
     col = "red4", xaxt = "n", ylab = "", xlab = "")
abline(h = 0)
title(main="Change in Difference Between Futures, Aug '21 - Feb '23")
dev.off()

# Four .png images are now in the project folder; GEO_prices, NGEO_prices,
# GEO&NGEO_prices, and Change_in_diffs
# Next is descriptive statistics and variance and drift rates and properties

# Move to finalproj_code3
