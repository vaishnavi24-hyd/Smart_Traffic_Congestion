# ============================================================
# Smart Traffic Congestion Prediction
# Phase 7 - Machine Learning Model Development
# ============================================================

library(tidyverse)
library(lubridate)
ml_data <- read_csv("data/traffic_featured.csv")

dim(ml_data)

names(ml_data)
table(ml_data$year)
range(ml_data$year)
train_data <- ml_data %>%
  filter(year <= 2017)

test_data <- ml_data %>%
  filter(year == 2018)

dim(train_data)
dim(test_data)
range(train_data$year)
range(test_data$year)
traffic_for_thresholds <- read_csv(
  "data/traffic_preprocessed.csv"
)
training_traffic <- traffic_for_thresholds %>%
  filter(year <= 2017)
train_thresholds <- quantile(
  training_traffic$traffic_volume,
  probs = c(0.33, 0.67)
)

train_thresholds
training_traffic <- training_traffic %>%
  mutate(
    congestion_level_train = case_when(
      traffic_volume <= 2159 ~ "Low",
      traffic_volume <= 4588 ~ "Medium",
      traffic_volume > 4588 ~ "High"
    )
  )
table(training_traffic$congestion_level_train)
prop.table(
  table(training_traffic$congestion_level_train)
) * 100
nrow(traffic_for_thresholds)
nrow(ml_data)
head(traffic_for_thresholds$year)
head(ml_data$year)
tail(traffic_for_thresholds$year)
tail(ml_data$year)
ml_data <- ml_data %>%
  mutate(
    congestion_level = case_when(
      traffic_for_thresholds$traffic_volume <= 2159 ~ "Low",
      traffic_for_thresholds$traffic_volume <= 4588 ~ "Medium",
      traffic_for_thresholds$traffic_volume > 4588 ~ "High"
    ),
    congestion_level = factor(
      congestion_level,
      levels = c("Low", "Medium", "High"),
      ordered = TRUE
    )
  )
table(ml_data$congestion_level)
prop.table(
  table(ml_data$congestion_level)
) * 100

train_data <- ml_data %>%
  filter(year <= 2017)
test_data <- ml_data %>%
  filter(year == 2018)
dim(train_data)
dim(test_data)
table(train_data$congestion_level)
table(test_data$congestion_level)
str(train_data)
train_data <- train_data %>%
  mutate(
    holiday = as.factor(holiday),
    weather_main = as.factor(weather_main),
    day_of_week = as.factor(day_of_week),
    month = as.factor(month),
    is_weekend = as.factor(is_weekend),
    is_rush_hour = as.factor(is_rush_hour)
  )
test_data <- test_data %>%
  mutate(
    holiday = as.factor(holiday),
    weather_main = as.factor(weather_main),
    day_of_week = as.factor(day_of_week),
    month = as.factor(month),
    is_weekend = as.factor(is_weekend),
    is_rush_hour = as.factor(is_rush_hour)
  )
str(train_data)
str(test_data)
test_data <- test_data %>%
  mutate(
    holiday = factor(
      holiday,
      levels = levels(train_data$holiday)
    ),
    weather_main = factor(
      weather_main,
      levels = levels(train_data$weather_main)
    ),
    day_of_week = factor(
      day_of_week,
      levels = levels(train_data$day_of_week)
    ),
    month = factor(
      month,
      levels = levels(train_data$month)
    ),
    is_weekend = factor(
      is_weekend,
      levels = levels(train_data$is_weekend)
    ),
    is_rush_hour = factor(
      is_rush_hour,
      levels = levels(train_data$is_rush_hour)
    )
  )
levels(train_data$holiday)
levels(test_data$holiday)
levels(train_data$weather_main)
levels(test_data$weather_main)
levels(train_data$month)
levels(test_data$month)
identical(
  levels(train_data$holiday),
  levels(test_data$holiday)
)
predictor_names <- c(
  "holiday",
  "temp",
  "rain_1h",
  "snow_1h",
  "clouds_all",
  "weather_main",
  "hour",
  "day_of_week",
  "month",
  "year",
  "is_weekend",
  "is_rush_hour"
)
all(predictor_names %in% names(train_data))
"traffic_volume" %in% names(train_data)
"congestion_level" %in% names(train_data)
sum(is.na(train_data))
sum(is.na(test_data))
library(nnet)
packageVersion("nnet")
logistic_formula <- congestion_level ~
  holiday +
  temp +
  rain_1h +
  snow_1h +
  clouds_all +
  weather_main +
  hour +
  day_of_week +
  month +
  year +
  is_weekend +
  is_rush_hour
logistic_formula
logistic_model <- multinom(
  formula = logistic_formula,
  data = train_data,
  trace = FALSE
)
logistic_model
logistic_predictions <- predict(
  logistic_model,
  newdata = test_data,
  type = "class"
)
head(logistic_predictions)
table(logistic_predictions)
library(rpart)
packageVersion("rpart")

tree_formula <- congestion_level ~
  holiday +
  temp +
  rain_1h +
  snow_1h +
  clouds_all +
  weather_main +
  hour +
  day_of_week +
  month +
  year +
  is_weekend +
  is_rush_hour
tree_formula
tree_model <- rpart(
  formula = tree_formula,
  data = train_data,
  method = "class"
)
print(tree_model)
printcp(tree_model)
tree_predictions <- predict(
  tree_model,
  newdata = test_data,
  type = "class"
)
head(tree_predictions)
table(tree_predictions)
set.seed(123)
rf_model <- randomForest(
  formula = tree_formula,
  data = train_data,
  ntree = 300,
  importance = TRUE
)
library(randomForest)
randomForest
set.seed(123)

rf_model <- randomForest(
  formula = tree_formula,
  data = train_data,
  ntree = 300,
  importance = TRUE
)
rf_model
rf_predictions <- predict(
  rf_model,
  newdata = test_data,
  type = "class"
)
head(rf_predictions)
table(rf_predictions)
dir.create("models", showWarnings = FALSE)
saveRDS(
  logistic_model,
  "models/logistic_model.rds"
)