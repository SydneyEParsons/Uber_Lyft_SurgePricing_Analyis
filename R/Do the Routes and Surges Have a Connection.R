#Do the Routes and Surges Have a Connection
merged_df <- read.csv("merged_data_cleaned.csv")
library(dplyr)
library(ggplot2)

# explore data
unique(merged_df$surge_multiplier)
unique(merged_df$source)
unique(merged_df$destination)

# check the surge multiplier distribution for each platform
merged_df %>%
  group_by(cab_type) %>%
  summarise(
    unique_surge_values = n_distinct(surge_multiplier),
    min_surge  = min(surge_multiplier, na.rm = TRUE),
    max_surge  = max(surge_multiplier, na.rm = TRUE),
    mean_surge = mean(surge_multiplier, na.rm = TRUE),
    .groups = "drop"
  )
# check specifically surge multiplier for each platform
merged_df %>%
  group_by(cab_type, surge_multiplier) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(cab_type, surge_multiplier)

# only consider lyft since only lyft has different surge multipliers

# create route
merged_df <- merged_df %>%
  mutate(route = paste(source, "->", destination))

length(unique(merged_df$route))

# Lyft
lyft_df <- merged_df %>%
  filter(cab_type == "Lyft") %>%
  mutate(route = paste(source, "->", destination))

# quick check
cat("Lyft routes count:", length(unique(lyft_df$route)), "\n")
cat("Surge multiplier:\n")
table(lyft_df$surge_multiplier)

# each route surge
route_surge <- lyft_df %>%
  group_by(route) %>%
  summarise(
    total_rides    = n(),
    avg_surge      = mean(surge_multiplier, na.rm = TRUE),
    surge_rate     = mean(surge_multiplier > 1, na.rm = TRUE), # surge 发生率
    max_surge      = max(surge_multiplier, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_surge))

print(route_surge, n = 72)

# Top 10 surge visualization
route_surge %>%
  slice_max(avg_surge, n = 10) %>%
  ggplot(aes(x = reorder(route, avg_surge), y = avg_surge, fill = surge_rate)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(avg_surge, 2)), hjust = -0.2, size = 3.5) +
  scale_fill_gradient(low = "#790", high = "#210", labels = scales::percent) +
  coord_flip() +
  labs(
    title = "Top 10 Routes by Average Surge Multiplier (Lyft Only)",
    subtitle = "Color intensity shows surge occurrence rate",
    x = "Route", y = "Average Surge Multiplier",
    fill = "Surge Rate"
  ) +
  theme_minimal()

# aggregating by source allows for a clearer view of the surging patterns at the location
source_surge <- lyft_df %>%
  group_by(source) %>%
  summarise(
    surge_rate = mean(surge_multiplier > 1, na.rm = TRUE),
    avg_surge  = mean(surge_multiplier, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(surge_rate))

# visualize surge rate
ggplot(source_surge, aes(x = reorder(source, surge_rate), y = surge_rate, fill = surge_rate)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(surge_rate, accuracy = 0.1)),
            hjust = -0.1, size = 3.5) +
  scale_fill_gradient(low = "#790", high = "#210", labels = scales::percent) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  labs(
    title = "Surge Occurrence Rate by Pickup Location (Lyft)",
    subtitle = "% of rides with surge_multiplier > 1",
    x = "Pickup Location", y = "Surge Rate",
    fill = "Surge Rate"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

dest_surge <- lyft_df %>%
  group_by(destination) %>%
  summarise(
    surge_rate = mean(surge_multiplier > 1, na.rm = TRUE),
    avg_surge  = mean(surge_multiplier, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(surge_rate))

ggplot(dest_surge, aes(x = reorder(destination, surge_rate), y = surge_rate, fill = surge_rate)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(surge_rate, accuracy = 0.1)),
            hjust = -0.1, size = 3.5) +
  scale_fill_gradient(low = "#790", high = "#210") +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  labs(
    title = "Surge Occurrence Rate by Drop-off Location (Lyft)",
    subtitle = "% of rides with surge_multiplier > 1",
    x = "Drop-off Location", y = "Surge Rate"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

lyft_df <- merged_df %>%
  filter(cab_type == "Lyft") %>%
  mutate(
    route = paste(source, "->", destination),
    surge_binary = ifelse(surge_multiplier > 1, 1, 0)  # 1=surge, 0=no
  )

# GLM Logistic
glm_model <- glm(surge_binary ~ source + destination,
                 data = lyft_df,
                 family = binomial(link = "logit"))

summary(glm_model)

# check source/destination’s odds ratio
exp(coef(glm_model)) %>% round(3)


# glmm random effect

install.packages("lme4")
library(lme4)

# source and destination as random effect
# This means that we don't care about the coefficient for each specific location, but rather control for the impact of specific location differences on the surge.
glmm_model <- glmer(surge_binary ~ (1 | source) + (1 | destination),
                    data = lyft_df,
                    family = binomial(link = "logit"))

summary(glmm_model)
