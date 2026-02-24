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

#Holidays
merged_data <- merged_data %>%
  mutate(holiday = case_when(
    date == as.Date("2018-11-26") ~ "Cyber Monday",
    date == as.Date("2018-12-06") ~ "St. Nicholas Day",
    date == as.Date("2018-12-07") ~ "Pearl Harbor Day",
    date >= as.Date("2018-12-03") & date <= as.Date("2018-12-10") ~ "Hanukkah",
    date == as.Date("2018-12-02") ~ "Advent",
    TRUE ~ "None"
  ))
