#What Holidays Have Surges
merged_data <- merged_data %>%
  mutate(date = as.Date(time_stamp))
