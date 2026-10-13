# ============================================================
# Smart Traffic Congestion Prediction
# Phase 2 - Dataset Collection & Understanding
# ============================================================

library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)

# Import the raw traffic dataset
traffic <- read_csv("data/Metro_Interstate_Traffic_Volume.csv")

# View first records
head(traffic)

dim(traffic)
nrow(traffic)
ncol(traffic)
names(traffic)
str(traffic)

colSums(is.na(traffic))

sum(duplicated(traffic))

# Examine categorical variables

unique(traffic$holiday)
table(traffic$holiday)

unique(traffic$weather_main)
table(traffic$weather_main)

unique(traffic$weather_description)
table(traffic$weather_description)

# Examine traffic volume distribution

summary(traffic$traffic_volume)

sd(traffic$traffic_volume)

quantile(
  traffic$traffic_volume,
  probs = c(0, 0.25, 0.50, 0.75, 1)
)

min(traffic$date_time)
max(traffic$date_time)
# Check the number of unique dates
length(unique(as.Date(traffic$date_time)))
# Check the number of unique years
unique(format(traffic$date_time, "%Y"))