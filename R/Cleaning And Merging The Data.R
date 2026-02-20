# Load the data files
library(data.table)
library(dplyr)
library(lubridate)

cab_rides <- read.csv("cab_rides.csv")
weather <- read.csv("weather.csv")

# Checking Unique Values --------------------------------------------------
length(unique(cab_rides$time_stamp))
length(unique(weather$time_stamp))

# Check for duplicates before cleaning
cat("Cab rides before:", nrow(cab_rides), "\n")
cat("Duplicates found:", sum(duplicated(cab_rides)), "\n")

#Removing taxi
cab_rides <- cab_rides %>%
  filter(name != "Taxi")


# Merging A Range of Weather & Rides --------------------------------------
cab_rides <- cab_rides %>%
  mutate(time_stamp = as.POSIXct(time_stamp / 1000, origin = "1970-01-01", tz = "UTC"), hour_ts = floor_date(time_stamp, unit = "hour"))


setDT(cab_rides)
setDT(weather)

setkey(cab_rides, time_stamp)
setkey(weather, time_stamp)

merged_data <- weather[cab_rides, roll = "nearest", allow.cartesian = TRUE]

# dim(merged_data)
head(merged_data)
print(merged_data)

write.csv(merged_data, "merged_data_cleaned.csv", row.names = FALSE)



