library(tidyverse)
library(lubridate)
library(mgcv)      # GAM

cab <- read_csv("cab_rides.csv")
weather <- read_csv("weather.csv")

# 1) timestamp：cab - ms，weather - s
# Convert timestamps to datetime + hour bins
cab2 <- cab %>%
  mutate(
    dt = as_datetime(floor(time_stamp / 1000), tz = "UTC"),
    hour_ts = floor_date(dt, unit = "hour")
  )

weather2 <- weather %>%
  mutate(
    dt = as_datetime(time_stamp, tz = "UTC"),
    hour_ts = floor_date(dt, unit = "hour")
  )

# 2) Aggregate weather within each hour
w_hour <- weather2 %>%
  group_by(hour_ts) %>%
  summarise(
    across(c(temp, clouds, pressure, rain, humidity, wind),
           ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# 3) merge and clean
df <- cab2 %>%
  left_join(w_hour, by = "hour_ts") %>%
  filter(!is.na(price), price > 0, name != "Taxi") %>%
  mutate(
    hour = hour(hour_ts),
    route = paste(source, destination, sep = " -> ")
  )

# plot 1：temp vs price
ggplot(df %>% sample_n(min(nrow(df), 50000)), aes(temp, price)) +
  geom_point(alpha = 0.05) +
  geom_smooth() +
  scale_y_log10()

# plot 2：price distribution
ggplot(df, aes(price)) +  geom_histogram(bins = 60)

# plot 3：uber vs lyft (platform difference)
ggplot(df, aes(cab_type, price)) +  geom_boxplot() + scale_y_log10()

# temp effect
m1 <- gam(
  price ~ s(temp) + distance + surge_multiplier + name +
    clouds + pressure + rain + humidity + wind +
    factor(hour) + route,
  family = Gamma(link = "log"),
  data = df
)

# uber vs lyft difference
m2 <- glm(
  price ~ cab_type * temp + cab_type * distance +
    surge_multiplier + name + clouds + pressure + rain + humidity + wind +
    factor(hour) + route,
  family = Gamma(link = "log"),
  data = df
)
