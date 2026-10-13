# ============================================
# PHASE 8: MODEL EVALUATION
# Smart Traffic Congestion Prediction
# ============================================

library(tidyverse)

# Check actual class distribution in test data
table(test_data$congestion_level)

logistic_cm <- table(
  Actual = test_data$congestion_level,
  Predicted = logistic_predictions
)

logistic_cm
logistic_accuracy <- sum(diag(logistic_cm)) / sum(logistic_cm)

logistic_accuracy

logistic_precision <- diag(logistic_cm) / colSums(logistic_cm)

logistic_recall <- diag(logistic_cm) / rowSums(logistic_cm)

logistic_f1 <- 2 * (
  logistic_precision * logistic_recall
) / (
  logistic_precision + logistic_recall
)
logistic_precision
logistic_recall
logistic_f1

tree_cm <- table(
  Actual = test_data$congestion_level,
  Predicted = tree_predictions
)

tree_cm
tree_accuracy <- sum(diag(tree_cm)) / sum(tree_cm)

tree_accuracy
tree_precision <- diag(tree_cm) / colSums(tree_cm)

tree_recall <- diag(tree_cm) / rowSums(tree_cm)

tree_f1 <- 2 * (
  tree_precision * tree_recall
) / (
  tree_precision + tree_recall
)
tree_precision
tree_recall
tree_f1

rf_cm <- table(
  Actual = test_data$congestion_level,
  Predicted = rf_predictions
)

rf_cm
rf_accuracy <- sum(diag(rf_cm)) / sum(rf_cm)

rf_accuracy
rf_precision <- diag(rf_cm) / colSums(rf_cm)

rf_recall <- diag(rf_cm) / rowSums(rf_cm)

rf_f1 <- 2 * (
  rf_precision * rf_recall
) / (
  rf_precision + rf_recall
)
rf_precision
rf_recall
rf_f1

logistic_macro_f1 <- mean(logistic_f1)
tree_macro_f1 <- mean(tree_f1)
rf_macro_f1 <- mean(rf_f1)

logistic_macro_f1
tree_macro_f1
rf_macro_f1

model_comparison <- data.frame(
  Model = c(
    "Logistic Regression",
    "Decision Tree",
    "Random Forest"
  ),
  Accuracy = c(
    logistic_accuracy,
    tree_accuracy,
    rf_accuracy
  ),
  Macro_F1 = c(
    logistic_macro_f1,
    tree_macro_f1,
    rf_macro_f1
  )
)

model_comparison
class_metrics <- data.frame(
  Model = rep(
    c("Logistic Regression", "Decision Tree", "Random Forest"),
    each = 3
  ),
  Class = rep(
    c("Low", "Medium", "High"),
    times = 3
  ),
  Precision = c(
    logistic_precision,
    tree_precision,
    rf_precision
  ),
  Recall = c(
    logistic_recall,
    tree_recall,
    rf_recall
  ),
  F1 = c(
    logistic_f1,
    tree_f1,
    rf_f1
  )
)

class_metrics
write_csv(
  class_metrics,
  "data/class_metrics.csv"
)