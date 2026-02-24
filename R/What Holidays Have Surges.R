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

#Holiday - Price and Surge
merged_data <- merged_data %>%
  mutate(holiday = case_when(
    date == as.Date("2018-11-26") ~ "Cyber Monday",
    date >= as.Date("2018-12-02") & date <= as.Date("2018-12-10") ~ "Hanukkah",
    TRUE ~ "None"
  ))

holiday_summary_detailed <- merged_data %>%
  group_by(holiday, cab_type, name) %>%
  summarise(
    n = n(),
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    mean_surge = mean(surge_multiplier, na.rm = TRUE),
    median_surge = median(surge_multiplier, na.rm = TRUE)
  )

print(holiday_summary_detailed %>% select(holiday, cab_type, name, mean_surge, median_surge), n = 48)

#---------------------------------------------------------------------

#Weekends and Weekdays - Price and Surge
merged_data <- merged_data %>%
  mutate(day_of_week = wday(date, label = TRUE),
         is_weekend = ifelse(wday(date) %in% c(1, 7), "Weekend", "Weekday"))

#Weekends - cab_type and name
weekend_summary <- merged_data %>%
  group_by(is_weekend, cab_type, name) %>%
  summarise(
    n = n(),
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    mean_surge = mean(surge_multiplier, na.rm = TRUE),
    median_surge = median(surge_multiplier, na.rm = TRUE)
  )

print(weekend_summary, n = 48)

#Weekdays - cab_type and name
day_of_week <- merged_data %>%
  group_by(day_of_week, cab_type, name) %>%
  summarise(
    n = n(),
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    mean_surge = mean(surge_multiplier, na.rm = TRUE),
    median_surge = median(surge_multiplier, na.rm = TRUE)
  )

print(day_of_week %>% select(day_of_week, cab_type, name, mean_surge, median_surge), n = 84)


#--------------------------------------------------------------------

#Time of Day
merged_data <- merged_data %>%
  mutate(hour = hour(as.POSIXct(time_stamp)))

#Price and Surge for Time of Day
time_summary <- merged_data %>%
  group_by(hour, cab_type, name) %>%
  summarise(
    n = n(),
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    mean_surge = mean(surge_multiplier, na.rm = TRUE),
    median_surge = median(surge_multiplier, na.rm = TRUE)
  )
print(time_summary, n = 288)

#----------------------------------------------------------------------------

#Distribution of the Surge Multiplier
table(merged_data$surge_multiplier)
#Looking at surge so far it appears that none of the three factors analysed have any impact on surge pricing. However, we are limited to two holidays in this data set that we can analyse.
