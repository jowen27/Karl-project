# Load necessary libraries
library(readxl)
library(caret)
library(e1071)
library(lme4)
library(kernlab)  # For ksvm

# Load the dataset
data <- read_excel(here::here("data", "19-03-2024 data.xlsx"))

# Create interaction terms
data$interaction1 <- interaction(data$conc, data$name)
data$interaction2 <- interaction(data$conc, data$treatment)
data$interaction3 <- interaction(data$name, data$treatment)

# Split the data into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(data$gene_expression, p = 0.8, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Define train control for cross-validation
train_control <- trainControl(method = "cv", number = 5)

# Define models including interaction terms
models <- list(
  "Linear Regression" = train(gene_expression ~ . + interaction1 + interaction2 + interaction3, data = train_data, method = "lm", trControl = train_control),
  "Support Vector Regressor" = train(gene_expression ~ . + interaction1 + interaction2 + interaction3, data = train_data, method = "svmRadial", trControl = train_control)
)

# Mixed-effects model with interaction terms
mixed_model <- lmer(gene_expression ~ conc * name * treatment + (1 | cell_line), data = train_data)
train_pred_mixed <- predict(mixed_model, train_data, allow.new.levels = TRUE)
test_pred_mixed <- predict(mixed_model, test_data, allow.new.levels = TRUE)

# Function to calculate AIC and BIC for caret models
calculate_aic_bic <- function(model, data, response) {
  predictions <- predict(model, newdata = data)
  residuals <- data[[response]] - predictions
  rss <- sum(residuals^2)
  n <- nrow(data)

  if (inherits(model$finalModel, "ksvm")) {
    k <- length(model$finalModel@alpha) + 1  # Number of support vectors + intercept
  } else {
    log_lik <- logLik(model$finalModel)
    k <- length(coef(model$finalModel))  # Number of parameters for lm
  }

  aic <- n * log(rss / n) + 2 * k
  bic <- n * log(rss / n) + log(n) * k

  return(list(AIC = aic, BIC = bic))
}

# Evaluate models and calculate AIC/BIC
results <- lapply(models, function(model) {
  train_pred <- predict(model, train_data)
  test_pred <- predict(model, test_data)

  train_mse <- mean((train_data$gene_expression - train_pred)^2)
  test_mse <- mean((test_data$gene_expression - test_pred)^2)

  train_r2 <- cor(train_data$gene_expression, train_pred)^2
  test_r2 <- cor(test_data$gene_expression, test_pred)^2

  aic_bic <- calculate_aic_bic(model, test_data, "gene_expression")

  list(
    "Train MSE" = train_mse,
    "Test MSE" = test_mse,
    "Train R²" = train_r2,
    "Test R²" = test_r2,
    "AIC" = aic_bic$AIC,
    "BIC" = aic_bic$BIC
  )
})

# Add mixed-effects model results
mixed_aic <- AIC(mixed_model)
mixed_bic <- BIC(mixed_model)
mixed_results <- list(
  "Train MSE" = mean((train_data$gene_expression - train_pred_mixed)^2),
  "Test MSE" = mean((test_data$gene_expression - test_pred_mixed)^2),
  "Train R²" = cor(train_data$gene_expression, train_pred_mixed)^2,
  "Test R²" = cor(test_data$gene_expression, test_pred_mixed)^2,
  "AIC" = mixed_aic,
  "BIC" = mixed_bic
)

results$`Mixed Effects Model` <- mixed_results

results_df <- do.call(rbind, lapply(results, function(x) unlist(x)))
rownames(results_df) <- names(results)
print(results_df)

# Function to plot actual vs predicted and residuals
plot_model <- function(y_true, y_pred, model_name) {
  # Actual vs Predicted
  plot(y_true, y_pred, main = paste("Actual vs Predicted (", model_name, ")", sep = ""),
       xlab = "Actual", ylab = "Predicted", pch = 19, col = "blue")
  abline(0, 1, col = "red")

  # Residuals
  residuals <- y_true - y_pred
  hist(residuals, breaks = 30, main = paste("Residuals (", model_name, ")", sep = ""),
       xlab = "Residuals", col = "blue")
}

# Plotting
par(mfrow = c(2, 2)) # Set layout for 4 plots (2 models + 1 mixed-effects)
for (name in names(models)) {
  model <- models[[name]]
  test_pred <- predict(model, test_data)
  plot_model(test_data$gene_expression, test_pred, name)
}

# Plotting for mixed-effects model
plot_model(test_data$gene_expression, test_pred_mixed, "Mixed Effects Model")

mixed <- ggplot(test_data, aes(x = gene_expression, y = test_pred_mixed)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1) +
  labs(
    title = "Actual vs Predicted - Mixed Effects Model",
    x = "Actual",
    y = "Predicted"
  )



results_df <-as.data.frame(results_df)

results_df$model <- rownames(results_df)
rownames(results_df) <- NULL  # Clean up row names to avoid confusion

# Create a GT table
gt_table <- gt(results_df) %>%
  tab_header(
    title = "Model Evaluation Results",
    subtitle = "Comparative performance metrics across models"
  ) %>%
  cols_label(
    model = "Model",
    `Train MSE` = "Train MSE",   # Note the use of backticks for names with spaces
    `Test MSE` = "Test MSE",
    `Train R²` = "Train R²",
    `Test R²` = "Test R²",
    AIC = "AIC",
    BIC = "BIC"
  )


file_path <- here::here("table", "model_results.pdf")
gtsave(gt_table, filename = file_path)
