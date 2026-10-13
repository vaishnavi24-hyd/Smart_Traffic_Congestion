# ============================================
# PHASE 9: FEATURE IMPORTANCE & INTERPRETATION
# Smart Traffic Congestion Prediction
# ============================================

library(randomForest)
importance(rf_model)

importance_values <- importance(rf_model)

feature_importance <- data.frame(
  Feature = rownames(importance_values),
  MeanDecreaseAccuracy = importance_values[, "MeanDecreaseAccuracy"],
  MeanDecreaseGini = importance_values[, "MeanDecreaseGini"]
)

feature_importance
feature_importance_ranked <- feature_importance %>%
  arrange(desc(MeanDecreaseAccuracy))

feature_importance_ranked
feature_importance_plot <- ggplot(
  feature_importance_ranked,
  aes(
    x = reorder(Feature, MeanDecreaseAccuracy),
    y = MeanDecreaseAccuracy
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Random Forest Feature Importance",
    x = "Feature",
    y = "Mean Decrease in Accuracy"
  ) +
  theme_minimal()

feature_importance_plot
ggsave(
  "plots/12_random_forest_feature_importance.png",
  plot = feature_importance_plot,
  width = 10,
  height = 7,
  dpi = 300
)
write_csv(
  feature_importance_ranked,
  "data/feature_importance.csv"
)