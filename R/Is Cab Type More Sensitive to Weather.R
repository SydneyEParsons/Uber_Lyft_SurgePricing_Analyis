#Is Cab Type More Sensitive to Weather
merged_df <- read.csv("merged_data_cleaned.csv")

# create fomat like "Uber-UberX" / "Lyft-Lyft XL" since only use name to count is not clear enough
merged_df <- merged_df %>%
  mutate(cab_label = paste(cab_type, name, sep = "-"))

# check all combinations
unique(merged_df$cab_label)

# order quantity + average price for each combination
merged_df %>%
  group_by(cab_label) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  ) %>%
  arrange(avg_price) %>%
  print(n = 50)

library(dplyr)
library(ggplot2)
library(tidyr)

# keep needed facotrs
analysis_df <- merged_df %>%
  select(cab_label, price, temp, clouds, pressure, humidity, wind, rain)

# correlation analysis: correlation coefficient between each cab_label and weather variables
cor_results <- analysis_df %>%
  group_by(cab_label) %>%
  summarise(
    cor_temp     = cor(price, temp,     use = "complete.obs"),
    cor_clouds   = cor(price, clouds,   use = "complete.obs"),
    cor_pressure = cor(price, pressure, use = "complete.obs"),
    cor_humidity = cor(price, humidity, use = "complete.obs"),
    cor_wind     = cor(price, wind,     use = "complete.obs"),
    cor_rain     = cor(price, rain,     use = "complete.obs"),
    .groups = "drop"
  )

print(cor_results)

# visualization: heat maps show the weather sensitivity of each vehicle model
cor_long <- cor_results %>%
  pivot_longer(-cab_label, names_to = "weather_var", values_to = "correlation") %>%
  mutate(weather_var = gsub("cor_", "", weather_var))

ggplot(cor_long, aes(x = weather_var, y = cab_label, fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlation, 2)), size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(
    title = "Cab Type Sensitivity to Weather Variables",
    x = "Weather Variable", y = "Cab Type",
    fill = "Correlation\nwith Price"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

#———————————————————————————————————————————————————————————————————————————————
# analyze the impact of weather on number of orders

demand_weather <- merged_df %>%
  group_by(cab_label, hour_ts) %>%
  summarise(
    ride_count   = n(),
    avg_temp     = mean(temp,     na.rm = TRUE),
    avg_clouds   = mean(clouds,   na.rm = TRUE),
    avg_humidity = mean(humidity, na.rm = TRUE),
    avg_wind     = mean(wind,     na.rm = TRUE),
    avg_pressure = mean(pressure, na.rm = TRUE),
    .groups = "drop"
  )

# weather vs. demand
demand_cor <- demand_weather %>%
  group_by(cab_label) %>%
  summarise(
    cor_temp     = cor(ride_count, avg_temp,     use = "complete.obs"),
    cor_clouds   = cor(ride_count, avg_clouds,   use = "complete.obs"),
    cor_humidity = cor(ride_count, avg_humidity, use = "complete.obs"),
    cor_wind     = cor(ride_count, avg_wind,     use = "complete.obs"),
    cor_pressure = cor(ride_count, avg_pressure, use = "complete.obs"),
    .groups = "drop"
  )

print(demand_cor)

# visualization
demand_cor_long <- demand_cor %>%
  pivot_longer(-cab_label, names_to = "weather_var", values_to = "correlation") %>%
  mutate(weather_var = gsub("cor_", "", weather_var))

ggplot(demand_cor_long, aes(x = weather_var, y = cab_label, fill = correlation)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(correlation, 2)), size = 3.5) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(
    title = "Cab Type Demand Sensitivity to Weather",
    subtitle = "Correlation between hourly ride count and weather variables",
    x = "Weather Variable", y = "Cab Type",
    fill = "Correlation\nwith Demand"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

#———————————————————————————————————————————————————————————————————————————————
# check is there any differences at the platform level between Uber and Lyft
platform_cor <- demand_weather %>%
  mutate(platform = ifelse(grepl("Uber", cab_label), "Uber", "Lyft")) %>%
  group_by(platform) %>%
  summarise(
    cor_pressure = cor(ride_count, avg_pressure, use = "complete.obs"),
    cor_wind     = cor(ride_count, avg_wind,     use = "complete.obs"),
    cor_clouds   = cor(ride_count, avg_clouds,   use = "complete.obs"),
    .groups = "drop"
  )

print(platform_cor)

platform_cor_long <- platform_cor %>%
  pivot_longer(-platform, names_to = "weather_var", values_to = "correlation") %>%
  mutate(weather_var = gsub("cor_", "", weather_var))

ggplot(platform_cor_long, aes(x = weather_var, y = correlation, fill = platform)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_text(aes(label = round(correlation, 3)),
            position = position_dodge(width = 0.6),
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Lyft" = "#FF69B4", "Uber" = "#000000")) +
  labs(
    title = "Uber vs Lyft: Demand Sensitivity to Weather",
    subtitle = "Correlation between hourly ride count and weather variables",
    x = "Weather Variable", y = "Correlation with Demand",
    fill = "Platform"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

