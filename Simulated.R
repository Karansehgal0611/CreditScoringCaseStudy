# ----------------------------
# AI-Powered Credit Risk Assessment
# Case Study - R Code Skeleton
# ----------------------------

# 1. Load Necessary Libraries
library(dplyr)    # Data manipulation
library(caret)    # Machine learning
library(randomForest) # Random Forest model
library(pROC)     # For AUC-ROC calculation
library(ggplot2)  # For plotting

# 2. Create a Synthetic Dataset (Simulating Mobile Money Data)
set.seed(123) # For reproducibility
n_customers <- 1000

# Simulate customer data
synthetic_data <- data.frame(
  customer_id = 1:n_customers,
  # Alternative Data Features
  avg_transaction_value = runif(n_customers, 10, 500),
  transaction_frequency = rpois(n_customers, 20), # Number of transactions per month
  recency_days = rpois(n_customers, 15), # Days since last transaction
  network_diversity = runif(n_customers, 1, 10), # Number of unique counterparties
  tenure_months = rpois(n_customers, 24), # How long they've used the service
  # Traditional Feature (for comparison)
  has_bank_account = sample(0:1, n_customers, replace = TRUE, prob = c(0.3, 0.7)),
  # Target Variable: Default (1) or Non-Default (0)
  default = sample(0:1, n_customers, replace = TRUE, prob = c(0.85, 0.15))
)

# View the first few rows
head(synthetic_data)

# 3. Data Preprocessing & Feature Engineering
# Check for missing values (none in synthetic data, but you would do this with real data)
sum(is.na(synthetic_data))

# Create a new feature: Transaction Volume
synthetic_data$total_volume <- synthetic_data$avg_transaction_value * synthetic_data$transaction_frequency

# Normalize numerical features (important for some models like Neural Networks)
preprocess_params <- preProcess(synthetic_data[, c("avg_transaction_value", "transaction_frequency", "recency_days", "network_diversity", "tenure_months", "total_volume")], method = c("center", "scale"))
synthetic_data_normalized <- predict(preprocess_params, synthetic_data)

# 4. Split Data into Training and Testing Sets
set.seed(123)
train_index <- createDataPartition(synthetic_data_normalized$default, p = 0.8, list = FALSE)
train_data <- synthetic_data_normalized[train_index, ]
test_data <- synthetic_data_normalized[-train_index, ]

# 5. Model Training
# Define control parameters for training
ctrl <- trainControl(method = "cv", number = 5, summaryFunction = twoClassSummary, classProbs = TRUE, savePredictions = TRUE)

# Note: For binary classification, caret expects factors with levels "X0", "X1"
train_data$default <- as.factor(paste0("X", train_data$default))

# a) Logistic Regression (Conventional Baseline)
set.seed(123)
logit_model <- train(default ~ avg_transaction_value + transaction_frequency + recency_days + network_diversity + tenure_months + total_volume + has_bank_account,
                     data = train_data,
                     method = "glm",
                     family = "binomial",
                     trControl = ctrl)
# b) Random Forest (Advanced ML Model)
set.seed(123)
rf_model <- train(default ~ avg_transaction_value + transaction_frequency + recency_days + network_diversity + tenure_months + total_volume + has_bank_account,
                  data = train_data,
                  method = "rf",
                  trControl = ctrl,
                  ntree = 100)

# 6. Model Evaluation on Test Set
# Prepare test set (ensure factor levels match)
test_data$default <- as.factor(paste0("X", test_data$default))

# Predictions and Probabilities
logit_pred <- predict(logit_model, newdata = test_data)
logit_prob <- predict(logit_model, newdata = test_data, type = "prob")[, "X1"]

rf_pred <- predict(rf_model, newdata = test_data)
rf_prob <- predict(rf_model, newdata = test_data, type = "prob")[, "X1"]

# Calculate Performance Metrics
# Confusion Matrix
confusionMatrix(logit_pred, test_data$default)
confusionMatrix(rf_pred, test_data$default)

# AUC-ROC Curve
logit_roc <- roc(response = as.numeric(test_data$default) - 1, predictor = logit_prob)
rf_roc <- roc(response = as.numeric(test_data$default) - 1, predictor = rf_prob)

plot(logit_roc, col = "blue", main = "ROC Curves")
lines(rf_roc, col = "red")
legend("bottomright", legend = c("Logistic Regression", "Random Forest"), col = c("blue", "red"), lwd = 2)

# AUC Value
cat("Logistic Regression AUC:", auc(logit_roc), "\n")
cat("Random Forest AUC:", auc(rf_roc), "\n")

# 7. Performance Comparison Table
results <- data.frame(
  Model = c("Logistic Regression", "Random Forest"),
  AUC = c(auc(logit_roc), auc(rf_roc))
  # You can add more metrics like Accuracy, F1-Score here
)
print(results)

# 8. Feature Importance (for Random Forest)
varImp(rf_model)
plot(varImp(rf_model), main = "Random Forest - Feature Importance")
