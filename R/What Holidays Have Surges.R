#What Holidays Have Surges

# Load libraries
library(dplyr)
library(lubridate)

# Load merged data
merged_data <- read.csv("merged_data_cleaned.csv")

# Extracting the date from the time stamp
merged_data <- merged_data %>%
  mutate(date = as.Date(time_stamp))
head(merged_data$date)

#Hanukkah

#Cyber Monday
