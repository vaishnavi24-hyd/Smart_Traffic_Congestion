# ============================================================
# Smart Traffic Congestion Prediction
# Phase 6 - Feature Engineering & Target Creation
# ============================================================
library(tidyverse)
library(lubridate)
library(janitor)

traffic_featured <- read_csv(
  "data/traffic_preprocessed.csv"
)
dim(traffic_featured)
colSums(is.na(traffic_featured))
traffic_featured %>%
  summarise(
    minimum = min(traffic_volume),
    q25 = quantile(traffic_volume, 0.25),
    median = median(traffic_volume),
    q75 = quantile(traffic_volume, 0.75),
    maximum = max(traffic_volume),
    mean = mean(traffic_volume)
  )
quantile(
  traffic_featured$traffic_volume,
  probs = c(
    0,
    0.10,
    0.20,
    0.25,
    0.33,
    0.50,
    0.67,
    0.75,
    0.80,
    0.90,
    1
  )
)
traffic_featured <- traffic_featured %>%
  mutate(
    congestion_level = case_when(
      traffic_volume <= 2157 ~ "Low",
      traffic_volume <= 4585.62 ~ "Medium",
      traffic_volume > 4585.62 ~ "High"
    )
  )
traffic_featured <- traffic_featured %>%
  mutate(
    congestion_level = factor(
      congestion_level,
      levels = c("Low", "Medium", "High"),
      ordered = TRUE
    )
  )
table(traffic_featured$congestion_level)
prop.table(
  table(traffic_featured$congestion_level)
) * 100
traffic_featured %>%
  count(hour)
traffic_featured %>%
  count(hour)
traffic_featured <- traffic_featured %>%
  mutate(
    is_rush_hour = if_else(
      hour %in% c(6, 7, 8, 9, 15, 16, 17, 18),
      "Yes",
      "No"
    )
  )
table(traffic_featured$is_rush_hour)
prop.table(
  table(traffic_featured$is_rush_hour)
) * 100
names(traffic_featured)
ml_data <- traffic_featured %>%
  select(
    holiday,
    temp,
    rain_1h,
    snow_1h,
    clouds_all,
    weather_main,
    hour,
    day_of_week,
    month,
    year,
    is_weekend,
    is_rush_hour,
    congestion_level
  )
dim(ml_data)
names(ml_data)
colSums(is.na(ml_data))
table(ml_data$congestion_level)
str(ml_data)
ml_data <- ml_data %>%
  mutate(
    holiday = as.factor(holiday),
    weather_main = as.factor(weather_main),
    day_of_week = as.factor(day_of_week),
    month = as.factor(month),
    is_weekend = as.factor(is_weekend),
    is_rush_hour = as.factor(is_rush_hour)
  )
str(ml_data)
write_csv(
  ml_data,
  "data/traffic_featured.csv"
)
file.exists("data/traffic_featured.csv")