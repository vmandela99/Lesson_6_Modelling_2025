# Load libraries
library(tidyverse)

# Create example data
set.seed(123) 

# Define models and metrics
models <- c("Decision trees", "ANN", "Random forest", "SVM", "XGBOOST")
metrics <- c("rmse", "mae")

# Define models and metrics
models <- c("Decision trees", "ANN", "Random forest", "SVM", "XGBOOST")
metrics <- c("rmse", "mae")

# Create a table with means and SDs for each Model/Metric combination
param_table <- tribble(
  ~Model,            ~Metric, ~Mean, ~SD,
  "Decision trees",  "rmse",  180,   30,
  "Decision trees",  "mae",   140,   20,
  "ANN",             "rmse",  300,   40,
  "ANN",             "mae",   200,   20,
  "Random forest",   "rmse",  170,   30,
  "Random forest",   "mae",   130,   20,
  "SVM",             "rmse",  180,   25,
  "SVM",             "mae",   120,   15,
  "XGBOOST",         "rmse",  190,   35,
  "XGBOOST",         "mae",   125,   20
)

# Simulate 10 replicates for each combination
example_data <- param_table %>%
  group_by(Model, Metric) %>%
  mutate(
    Value = list(rnorm(10, mean = Mean, sd = SD))
  ) %>%
  unnest(Value) %>%
  group_by(Model, Metric) %>%
  mutate(Replicate = row_number())

# Plot
ggplot(example_data, aes(x = Model, y = Value, fill = Metric)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8)) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 21,
    position = position_dodge(width = 0.8),
    size = 2,
    fill = "black",
    color = "black"
  ) +
  labs(
    y = "Sorghum Yield Prediction Error (Kg/ha)",
    fill = "Metrics"
  ) +
  scale_fill_manual(values = c("rmse" = "#F8766D", "mae" = "#00BFC4")) +
  theme_minimal(base_size = 14)