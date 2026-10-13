# ============================================================
# Smart Traffic Congestion Prediction
# Phase 4 - Data Preprocessing
# ============================================================

library(tidyverse)
library(lubridate)
library(janitor)
traffic_preprocessed <- read_csv(
  "data/traffic_wrangled.csv"
)
dim(traffic_preprocessed)
colSums(is.na(traffic_preprocessed))

traffic_preprocessed %>%
  filter(is.na(temp) | is.na(rain_1h)) %>%
  select(
    date_time_local,
    temp,
    rain_1h,
    snow_1h,
    clouds_all,
    weather_main,
    weather_description,
    traffic_volume
  )
traffic_preprocessed <- traffic_preprocessed %>%
  mutate(
    temp = if_else(
      is.na(temp),
      median(temp, na.rm = TRUE),
      temp
    )
  )
sum(is.na(traffic_preprocessed$temp))
rain_median <- median(
  traffic_preprocessed$rain_1h[
    traffic_preprocessed$weather_main == "Rain"
  ],
  na.rm = TRUE
)
rain_median
traffic_preprocessed %>%
  filter(
    weather_description == "very heavy rain",
    !is.na(rain_1h)
  ) %>%
  summarise(
    count = n(),
    median_rain = median(rain_1h),
    mean_rain = mean(rain_1h),
    min_rain = min(rain_1h),
    max_rain = max(rain_1h)
  )
traffic_preprocessed <- traffic_preprocessed %>%
  mutate(
    rain_1h = if_else(
      is.na(rain_1h),
      rain_median,
      rain_1h
    )
  )
sum(is.na(traffic_preprocessed$rain_1h))
colSums(is.na(traffic_preprocessed))
traffic_preprocessed %>%
  summarise(
    holiday_categories = n_distinct(holiday),
    weather_main_categories = n_distinct(weather_main),
    weather_description_categories = n_distinct(weather_description),
    day_categories = n_distinct(day_of_week),
    month_categories = n_distinct(month)
  )
traffic_preprocessed <- traffic_preprocessed %>%
  mutate(
    holiday = as.factor(holiday),
    weather_main = as.factor(weather_main),
    day_of_week = as.factor(day_of_week),
    month = as.factor(month),
    is_weekend = as.factor(is_weekend)
  )
str(
  traffic_preprocessed %>%
    select(
      holiday,
      weather_main,
      day_of_week,
      month,
      is_weekend
    )
)
summary(
  traffic_preprocessed %>%
    select(
      temp,
      rain_1h,
      snow_1h,
      clouds_all,
      hour,
      year
    )
)
sapply(
  traffic_preprocessed %>%
    select(
      temp,
      rain_1h,
      snow_1h,
      clouds_all,
      hour,
      year
    ),
  sd
)
quantile(
  traffic_preprocessed$rain_1h,
  probs = c(0, 0.25, 0.50, 0.75, 0.95, 0.99, 1)
)
numeric_summary <- traffic_preprocessed %>%
  select(
    temp,
    snow_1h,
    clouds_all,
    hour,
    year
  ) %>%
  summarise(
    across(
      everything(),
      list(
        min = ~min(.),
        max = ~max(.),
        median = ~median(.)
      )
    )
  )

numeric_summary
str(traffic_preprocessed)
traffic_preprocessed <- traffic_preprocessed %>%
  mutate(
    date_time_local = force_tz(
      date_time,
      tzone = "America/Chicago"
    )
  )
traffic_preprocessed %>%
  select(
    date_time,
    date_time_local,
    hour
  ) %>%
  head(6)
colSums(is.na(traffic_preprocessed))
sum(duplicated(traffic_preprocessed))
write_csv(
  traffic_preprocessed,
  "data/traffic_preprocessed.csv"
)
file.exists("data/traffic_preprocessed.csv")