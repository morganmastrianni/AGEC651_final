#=============================================
#AEM 6850 FINAL PROJECT
#=============================================

# Clean up workspace and load or install necessary packages if necessary
rm(list=ls())
want <- c("data.table", "foreign", "gdata","plyr", "readxl", "data.table")
need <- want[!(want %in% installed.packages()[,"Package"])]
if (length(need)) install.packages(need)
lapply(want, function(i) require(i, character.only=TRUE))
rm(want, need)

# Working directories
dir <- list()
dir$root <- dirname(paste(getwd(),"/Final Project",sep=""))
dir$output   <- paste(dir$root,"/output",sep="")

#=============================================

### 1. Cleaning data

data <- read_xlsx("N-GEO-GEO.GIZ23_Barchart_Interactive_Chart_Daily_Nearby_03_01_2023.xlsx",
                  col_types = c("date", "text", rep("numeric", 5)), skip = 1,
                  n_max = 395)

# Removing superfluous columns
data = data[-c(2,4,6,7)]

# Renaming columns
colnames(data) <- c("Date", "NGEO_Close", "GEO_Close")

# Appending columns with variables of potential interest
data$Close_Difference <- data$NGEO_Close - data$GEO_Close

# Cleaning data
# Let's establish a change column for each contract..
ntest<- rep(NA, length(data$NGEO_Close))
length(ntest)
for (i in 1:length(ntest)) {
  if (i > 1) {
    ntest[i] = data$NGEO_Close[i] - data$NGEO_Close[i-1]
  }  else  {
    ntest[i] = 0
  }
}

data$NGEO_Change <- ntest

# Below we do the same for GEO contracts
gtest<- rep(NA, length(data$GEO_Close))
length(gtest)
for (i in 1:length(gtest)) {
  if (i > 1) {
    gtest[i] = data$GEO_Close[i] - data$GEO_Close[i-1]
  }  else  {
    gtest[i] = 0
  }
}

data$GEO_Change <- gtest

# Move to finalproj_code2

