# ============================================================
# Smart Traffic Congestion Prediction
# Phase 3 - Data Wrangling
# ============================================================

library(tidyverse)
library(lubridate)
library(janitor)

traffic_raw <- read_csv("data/Metro_Interstate_Traffic_Volume.csv")

traffic_clean <- traffic_raw

traffic_clean <- clean_names(traffic_clean)

duplicates <- traffic_clean[
  duplicated(traffic_clean) |
    duplicated(traffic_clean, fromLast = TRUE),
]

duplicates

traffic_clean <- traffic_clean %>%
  distinct()

dim(traffic_clean)

sum(duplicated(traffic_clean))

summary(traffic_clean %>%
          select(temp, rain_1h, snow_1h, clouds_all, traffic_volume))

range(traffic_clean$temp)
range(traffic_clean$rain_1h)
range(traffic_clean$snow_1h)
range(traffic_clean$clouds_all)
range(traffic_clean$traffic_volume)

traffic_clean %>%
  filter(temp <= 1) %>%
  select(date_time, temp, rain_1h, snow_1h, clouds_all,
         weather_main, weather_description, traffic_volume)

traffic_clean %>%
  filter(rain_1h > 100) %>%
  select(date_time, temp, rain_1h, snow_1h, clouds_all,
         weather_main, weather_description, traffic_volume)

traffic_clean %>%
  filter(
    date_time >= as.POSIXct("2016-07-11 15:00:00"),
    date_time <= as.POSIXct("2016-07-11 19:00:00")
  ) %>%
  select(
    date_time,
    temp,
    rain_1h,
    snow_1h,
    clouds_all,
    weather_main,
    weather_description,
    traffic_volume
  )

traffic_clean %>%
  filter(
    date_time >= as.POSIXct("2016-07-11 15:00:00", tz = "UTC"),
    date_time <= as.POSIXct("2016-07-11 19:00:00", tz = "UTC")
  ) %>%
  select(
    date_time,
    temp,
    rain_1h,
    snow_1h,
    clouds_all,
    weather_main,
    weather_description,
    traffic_volume
  )

traffic_clean %>%
  filter(temp <= 1) %>%
  select(
    date_time,
    temp,
    rain_1h,
    snow_1h,
    clouds_all,
    weather_main,
    weather_description,
    traffic_volume
  )

# ============================================================
# Handle identified invalid/anomalous weather values
# ============================================================

# Temperature of 0 Kelvin is treated as an invalid measurement
traffic_clean <- traffic_clean %>%
  mutate(
    temp = na_if(temp, 0)
  )

# Extremely large hourly rainfall value is treated as anomalous
traffic_clean <- traffic_clean %>%
  mutate(
    rain_1h = if_else(rain_1h > 100, NA_real_, rain_1h)
  )
sum(is.na(traffic_clean$temp))
sum(is.na(traffic_clean$rain_1h))
range(traffic_clean$temp, na.rm = TRUE)
range(traffic_clean$rain_1h, na.rm = TRUE)

sort(unique(traffic_clean$weather_description))
sort(unique(tolower(traffic_clean$weather_description)))
sort(unique(traffic_clean$holiday))
sort(unique(traffic_clean$weather_main))

traffic_clean <- traffic_clean %>%
  mutate(
    weather_description = str_to_lower(weather_description)
  )

sort(unique(traffic_clean$weather_description))

length(unique(traffic_clean$weather_description))

length(unique(traffic_clean$weather_main))

# Check timezone of date_time
attr(traffic_clean$date_time, "tzone")

# Check timezone of date_time
attr(traffic_clean$date_time, "tzone")

# Check timezone of date_time
attr(traffic_clean$date_time, "tzone")

head(traffic_clean$date_time)
tail(traffic_clean$date_time)
class(traffic_clean$date_time)

traffic_clean <- traffic_clean %>%
  mutate(
    date_time_local = force_tz(
      date_time,
      tzone = "America/Chicago"
    )
  )

head(
  traffic_clean %>%
    select(date_time, date_time_local)
)

traffic_clean <- traffic_clean %>%
  mutate(
    hour = hour(date_time_local)
  )

traffic_clean %>%
  select(date_time_local, hour) %>%
  head(15)

traffic_clean <- traffic_clean %>%
  mutate(
    day_of_week = wday(
      date_time_local,
      label = TRUE,
      abbr = FALSE
    )
  )
traffic_clean <- traffic_clean %>%
  mutate(
    day_of_week = wday(
      date_time_local,
      label = TRUE,
      abbr = FALSE
    )
  )

traffic_clean %>%
  select(date_time_local, day_of_week) %>%
  head(15)

traffic_clean <- traffic_clean %>%
  mutate(
    month = month(
      date_time_local,
      label = TRUE,
      abbr = FALSE
    )
  )


traffic_clean %>%
  select(date_time_local, month) %>%
  head(15)
traffic_clean <- traffic_clean %>%
  mutate(
    year = year(date_time_local)
  )
traffic_clean %>%
  select(date_time_local, year) %>%
  head(15)

traffic_clean <- traffic_clean %>%
  mutate(
    is_weekend = day_of_week %in% c("Saturday", "Sunday")
  )
traffic_clean %>%
  select(date_time_local, day_of_week, is_weekend) %>%
  head(15)

dim(traffic_clean)
colSums(is.na(traffic_clean))
sum(duplicated(traffic_clean))
summary(
  traffic_clean %>%
    select(
      temp,
      rain_1h,
      snow_1h,
      clouds_all,
      traffic_volume,
      hour,
      year
    )
)
range(traffic_clean$date_time_local)
table(traffic_clean$day_of_week)
table(traffic_clean$is_weekend)

write_csv(
  traffic_clean,
  "data/traffic_wrangled.csv"
)

file.exists("data/traffic_wrangled.csv")