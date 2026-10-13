# ============================================================
# Smart Traffic Congestion Prediction
# Phase 5 - Exploratory Data Analysis
# ============================================================

library(tidyverse)
library(lubridate)
library(janitor)

traffic_eda <- read_csv(
  "data/traffic_preprocessed.csv"
)

dim(traffic_eda)

glimpse(traffic_eda)

ggplot(
  hourly_traffic,
  aes(
    x = hour,
    y = average_traffic
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Average Traffic Volume by Hour",
    x = "Hour of Day",
    y = "Average Traffic Volume"
  ) +
  theme_minimal()

ggsave(
  "plots/01_average_traffic_by_hour.png",
  width = 10,
  height = 6,
  dpi = 300
)

weekend_traffic <- traffic_eda %>%
  mutate(
    day_type = if_else(
      is_weekend,
      "Weekend",
      "Weekday"
    )
  ) %>%
  group_by(day_type) %>%
  summarise(
    average_traffic = mean(traffic_volume)
  )

hourly_traffic <- traffic_eda %>%
  group_by(hour) %>%
  summarise(
    average_traffic = mean(traffic_volume)
  )
hourly_traffic
print(hourly_traffic, n = 24)
weekend_traffic
ggplot(
  weekend_traffic,
  aes(
    x = day_type,
    y = average_traffic
  )
) +
  geom_col() +
  labs(
    title = "Average Traffic Volume: Weekday vs Weekend",
    x = "Day Type",
    y = "Average Traffic Volume"
  ) +
  theme_minimal()
ggsave(
  "plots/02_weekday_vs_weekend.png",
  width = 8,
  height = 6,
  dpi = 300
)
daily_traffic <- traffic_eda %>%
  group_by(day_of_week) %>%
  summarise(
    average_traffic = mean(traffic_volume)
  )
print(daily_traffic, n = 7)
ggplot(
  daily_traffic,
  aes(
    x = day_of_week,
    y = average_traffic
  )
) +
  geom_col() +
  labs(
    title = "Average Traffic Volume by Day of Week",
    x = "Day of Week",
    y = "Average Traffic Volume"
  ) +
  theme_minimal()
daily_traffic <- daily_traffic %>%
  mutate(
    day_of_week = factor(
      day_of_week,
      levels = c(
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
        "Sunday"
      )
    )
  )
ggplot(
  daily_traffic,
  aes(
    x = day_of_week,
    y = average_traffic
  )
) +
  geom_col() +
  labs(
    title = "Average Traffic Volume by Day of Week",
    x = "Day of Week",
    y = "Average Traffic Volume"
  ) +
  theme_minimal()
ggsave(
  "plots/03_average_traffic_by_day.png",
  width = 9,
  height = 6,
  dpi = 300
)
monthly_traffic <- traffic_eda %>%
  group_by(month) %>%
  summarise(
    average_traffic = mean(traffic_volume)
  )
print(monthly_traffic, n = 12)
monthly_counts <- traffic_eda %>%
  group_by(month) %>%
  summarise(
    observations = n()
  )

print(monthly_counts, n = 12)
monthly_traffic <- monthly_traffic %>%
  mutate(
    month = factor(
      month,
      levels = c(
        "January",
        "February",
        "March",
        "April",
        "May",
        "June",
        "July",
        "August",
        "September",
        "October",
        "November",
        "December"
      )
    )
  )
monthly_traffic
ggplot(
  monthly_traffic,
  aes(
    x = month,
    y = average_traffic,
    group = 1
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Average Traffic Volume by Month",
    x = "Month",
    y = "Average Traffic Volume"
  ) +
  theme_minimal()
ggsave(
  "plots/04_average_traffic_by_month.png",
  width = 10,
  height = 6,
  dpi = 300
)
holiday_traffic <- traffic_eda %>%
  mutate(
    day_type = if_else(
      holiday == "None",
      "Non-Holiday",
      "Holiday"
    )
  ) %>%
  group_by(day_type) %>%
  summarise(
    average_traffic = mean(traffic_volume),
    observations = n()
  )
holiday_traffic
ggplot(
  holiday_traffic,
  aes(
    x = day_type,
    y = average_traffic
  )
) +
  geom_col() +
  labs(
    title = "Average Traffic Volume: Holiday vs Non-Holiday",
    x = "Day Type",
    y = "Average Traffic Volume"
  ) +
  theme_minimal()
ggsave(
  "plots/05_holiday_vs_nonholiday.png",
  width = 8,
  height = 6,
  dpi = 300
)
weather_traffic <- traffic_eda %>%
  group_by(weather_main) %>%
  summarise(
    average_traffic = mean(traffic_volume),
    observations = n()
  )
print(weather_traffic, n = 11)
ggplot(
  weather_traffic,
  aes(
    x = weather_main,
    y = average_traffic
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average Traffic Volume by Weather Condition",
    x = "Weather Condition",
    y = "Average Traffic Volume"
  ) +
  theme_minimal()

ggsave(
  "plots/06_average_traffic_by_weather.png",
  width = 10,
  height = 7,
  dpi = 300
)
ggplot(
  traffic_eda,
  aes(
    x = temp,
    y = traffic_volume
  )
) +
  geom_point(alpha = 0.2) +
  labs(
    title = "Temperature vs Traffic Volume",
    x = "Temperature (Kelvin)",
    y = "Traffic Volume"
  ) +
  theme_minimal()
ggsave(
  "plots/07_temperature_vs_traffic.png",
  width = 10,
  height = 7,
  dpi = 300
)
rainfall_traffic <- traffic_eda %>%
  mutate(
    rain_category = case_when(
      rain_1h == 0 ~ "No Rain",
      rain_1h > 0 & rain_1h <= 1 ~ "Light Rain",
      rain_1h > 1 & rain_1h <= 5 ~ "Moderate Rain",
      rain_1h > 5 ~ "Heavy Rain"
    )
  ) %>%
  group_by(rain_category) %>%
  summarise(
    average_traffic = mean(traffic_volume),
    observations = n()
  )
print(rainfall_traffic, n = 4)
ggplot(
  rainfall_traffic,
  aes(
    x = rain_category,
    y = average_traffic
  )
) +
  geom_col() +
  labs(
    title = "Average Traffic Volume by Rainfall Category",
    x = "Rainfall Category",
    y = "Average Traffic Volume"
  ) +
  theme_minimal()
ggsave(
  "plots/08_rainfall_vs_traffic.png",
  width = 10,
  height = 6,
  dpi = 300
)
snowfall_traffic <- traffic_eda %>%
  mutate(
    snow_category = case_when(
      snow_1h == 0 ~ "No Snow",
      snow_1h > 0 & snow_1h <= 0.1 ~ "Light Snow",
      snow_1h > 0.1 ~ "Snow"
    )
  ) %>%
  group_by(snow_category) %>%
  summarise(
    average_traffic = mean(traffic_volume),
    observations = n()
  )
print(snowfall_traffic, n = 3)
ggplot(
  snowfall_traffic,
  aes(
    x = snow_category,
    y = average_traffic
  )
) +
  geom_col() +
  labs(
    title = "Average Traffic Volume by Snowfall Category",
    x = "Snowfall Category",
    y = "Average Traffic Volume"
  ) +
  theme_minimal()
ggsave(
  "plots/09_average_traffic_by_snowfall.png",
  width = 9,
  height = 6,
  dpi = 300
)
cloud_traffic <- traffic_eda %>%
  mutate(
    cloud_category = case_when(
      clouds_all <= 25 ~ "Low Cloud",
      clouds_all <= 50 ~ "Moderate-Low Cloud",
      clouds_all <= 75 ~ "Moderate-High Cloud",
      clouds_all <= 100 ~ "High Cloud"
    )
  ) %>%
  group_by(cloud_category) %>%
  summarise(
    average_traffic = mean(traffic_volume),
    observations = n()
  )
print(cloud_traffic, n = 4)
ggplot(
  cloud_traffic,
  aes(
    x = cloud_category,
    y = average_traffic
  )
) +
  geom_col() +
  labs(
    title = "Average Traffic Volume by Cloud Coverage",
    x = "Cloud Coverage Category",
    y = "Average Traffic Volume"
  ) +
  theme_minimal()
ggsave(
  "plots/10_cloud_coverage_vs_traffic.png",
  width = 10,
  height = 6,
  dpi = 300
)
correlation_long <- as.data.frame(correlation_matrix) %>%
  rownames_to_column("variable1") %>%
  pivot_longer(
    cols = -variable1,
    names_to = "variable2",
    values_to = "correlation"
  )
ggplot(
  correlation_long,
  aes(
    x = variable1,
    y = variable2,
    fill = correlation
  )
) +
  geom_tile() +
  geom_text(
    aes(label = round(correlation, 2)),
    size = 4
  ) +
  labs(
    title = "Correlation Matrix of Numerical Variables",
    x = "",
    y = "",
    fill = "Correlation"
  ) +
  theme_minimal()
ggsave(
  "plots/11_correlation_matrix.png",
  width = 9,
  height = 7,
  dpi = 300
)