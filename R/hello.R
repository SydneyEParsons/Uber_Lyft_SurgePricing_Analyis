
# Checking Unique Values --------------------------------------------------
length(unique(cab_rides$time_stamp))
length(unique(weather$time_stamp))


# Merging A Range of Weather & Rides --------------------------------------
library(data.table)

setDT(cab_rides)
setDT(weather)

setkey(cab_rides, time_stamp)
setkey(weather, time_stamp)

merged_data <- weather[cab_rides, roll = "nearest"]

dim(merged_data)
head(merged_data)

