# COMPLETE CREDIT RISK ASSESSMENT - OPTIMIZED VERSION
# Using German Credit Dataset with proper error handling

# Load required libraries
library(dplyr)
library(caret)
library(randomForest)
library(pROC)
library(ggplot2)
library(reshape2)

# Suppress specific warnings (optional)
options(warn = -1)  # Turn off warnings temporarily

# 1. DATA LOADING AND EXPLORATION
credit_data <- read.csv("german_credit_data.csv")  # Your actual file

# Explore the dataset
cat("Dataset Structure:\n")
str(credit_data)
cat("\nFirst few rows:\n")
head(credit_data)

# Check target variable distribution
risk_dist <- table(credit_data$Risk)
cat("\nRisk distribution:\n")
print(risk_dist)
cat("Proportions:\n")
print(prop.table(risk_dist))

# 2. DATA PREPROCESSING
credit_data_clean <- credit_data %>%
  mutate(
    # Handle missing values
    Saving.accounts = ifelse(is.na(Saving.accounts), "unknown", Saving.accounts),
    Checking.account = ifelse(is.na(Checking.account), "unknown", Checking.account),
    
    # Convert to factors with proper ordering
    Sex = factor(Sex),
    Housing = factor(Housing),
    Saving.accounts = factor(Saving.accounts, 
                             levels = c("unknown", "little", "moderate", "quite rich", "rich")),
    Checking.account = factor(Checking.account,
                              levels = c("unknown", "little", "moderate", "rich")),
    Purpose = factor(Purpose),
    Risk = factor(Risk, levels = c("bad", "good")),  # Important: bad as first level
    Job = factor(Job)
  )

# Remove index column if it exists
if("X" %in% names(credit_data_clean)) {
  credit_data_clean <- credit_data_clean %>% select(-X)
}

# 3. FEATURE ENGINEERING FOR FINANCIAL INCLUSION
credit_data_engineered <- credit_data_clean %>%
  mutate(
    # Age groups
    age_group = cut(Age, 
                    breaks = c(18, 25, 35, 50, 75),
                    labels = c("young", "young_adult", "middle_age", "senior")),
    
    # Financial behavior features
    monthly_payment = Credit.amount / Duration,
    payment_burden = monthly_payment / 2000,
    credit_utilization_rate = Credit.amount / (Age * 100 + 1),
    
    # Account behavior (alternative data)
    has_savings = as.factor(ifelse(Saving.accounts %in% c("quite rich", "rich", "moderate"), 1, 0)),
    has_checking = as.factor(ifelse(Checking.account %in% c("moderate", "rich"), 1, 0)),
    
    # Stability indicators
    employment_stability = factor(case_when(
      Job == 1 ~ "high",
      Job == 2 ~ "medium",
      Job == 3 ~ "low",
      TRUE ~ "unknown"
    )),
    
    housing_stability = factor(case_when(
      Housing == "own" ~ "high",
      Housing == "rent" ~ "medium",
      Housing == "free" ~ "low"
    ), levels = c("low", "medium", "high")),
    
    # Purpose risk categories
    purpose_risk = factor(case_when(
      Purpose %in% c("business", "education") ~ "high_risk",
      Purpose %in% c("car", "radio/TV") ~ "medium_risk",
      TRUE ~ "low_risk"
    )),
    
    # Credit amount categories
    credit_size = cut(Credit.amount,
                      breaks = c(0, 2000, 5000, 10000, 20000),
                      labels = c("small", "medium", "large", "very_large"))
  )

# 4. PREPARE FINAL MODELING DATASET
model_data <- credit_data_engineered %>%
  select(
    # Traditional features
    Age, Credit.amount, Duration,
    
    # Categorical features
    Sex, Job, Housing, Saving.accounts, Checking.account, Purpose,
    
    # Engineered alternative features
    age_group, payment_burden, credit_utilization_rate, has_savings,
    has_checking, employment_stability, housing_stability, purpose_risk, credit_size,
    
    # Target
    Risk
  )

# Remove any remaining missing values
model_data <- na.omit(model_data)

cat("\nFinal dataset dimensions:", dim(model_data), "\n")

# 5. DATA SPLITTING
set.seed(123)
train_index <- createDataPartition(model_data$Risk, p = 0.7, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

cat("Training set size:", nrow(train_data), "\n")
cat("Test set size:", nrow(test_data), "\n")

# 6. MODEL TRAINING WITH PROPER CONFIGURATION
# Enable warnings again for model training
options(warn = 0)

# Setup training control
ctrl <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  sampling = "up",
  savePredictions = TRUE
)

# Train models with error handling
models <- list()

# Logistic Regression
cat("Training Logistic Regression...\n")
models$logit <- train(
  Risk ~ .,
  data = train_data,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "ROC"
)

# Random Forest
cat("Training Random Forest...\n")
models$rf <- train(
  Risk ~ .,
  data = train_data,
  method = "rf",
  trControl = ctrl,
  metric = "ROC",
  ntree = 100,
  importance = TRUE
)

# XGBoost with specific parameters to avoid warnings
cat("Training XGBoost...\n")
suppressWarnings({
  models$xgb <- train(
    Risk ~ .,
    data = train_data,
    method = "xgbTree",
    trControl = ctrl,
    metric = "ROC",
    verbose = 0,
    tuneLength = 3  # Reduce tuning to speed up
  )
})

# 7. MODEL EVALUATION
evaluate_model <- function(model, test_data, model_name) {
  predictions <- predict(model, newdata = test_data)
  probabilities <- predict(model, newdata = test_data, type = "prob")[, "good"]
  actual <- test_data$Risk
  
  cm <- confusionMatrix(predictions, actual, positive = "good")
  roc_obj <- roc(response = as.numeric(actual == "good"), predictor = probabilities)
  
  return(list(
    model_name = model_name,
    confusion_matrix = cm,
    auc = auc(roc_obj),
    precision = cm$byClass["Precision"],
    recall = cm$byClass["Recall"],
    f1 = cm$byClass["F1"],
    accuracy = cm$overall["Accuracy"],
    roc_obj = roc_obj
  ))
}

# Evaluate all models
metrics <- list()
metrics$logit <- evaluate_model(models$logit, test_data, "Logistic Regression")
metrics$rf <- evaluate_model(models$rf, test_data, "Random Forest")
metrics$xgb <- evaluate_model(models$xgb, test_data, "XGBoost")

# 8. PERFORMANCE COMPARISON
performance_table <- data.frame(
  Model = c("Logistic Regression", "Random Forest", "XGBoost"),
  AUC = round(c(metrics$logit$auc, metrics$rf$auc, metrics$xgb$auc), 3),
  Accuracy = round(c(metrics$logit$accuracy, metrics$rf$accuracy, metrics$xgb$accuracy), 3),
  Precision = round(c(metrics$logit$precision, metrics$rf$precision, metrics$xgb$precision), 3),
  Recall = round(c(metrics$logit$recall, metrics$rf$recall, metrics$xgb$recall), 3),
  F1_Score = round(c(metrics$logit$f1, metrics$rf$f1, metrics$xgb$f1), 3)
)

cat("\n=== MODEL PERFORMANCE COMPARISON ===\n")
print(performance_table)

# 9. FEATURE IMPORTANCE ANALYSIS
feature_imp <- varImp(models$rf)
plot(feature_imp, main = "Random Forest - Feature Importance")

# 10. ROC CURVES COMPARISON
plot(metrics$logit$roc_obj, col = "blue", 
     main = "ROC Curves - Credit Risk Models")
plot(metrics$rf$roc_obj, col = "red", add = TRUE)
plot(metrics$xgb$roc_obj, col = "green", add = TRUE)
legend("bottomright", 
       legend = c(paste("Logistic Regression (AUC =", round(metrics$logit$auc, 3), ")"),
                  paste("Random Forest (AUC =", round(metrics$rf$auc, 3), ")"),
                  paste("XGBoost (AUC =", round(metrics$xgb$auc, 3), ")")),
       col = c("blue", "red", "green"), lwd = 2)

# 11. FINANCIAL INCLUSION ANALYSIS
financial_inclusion_analysis <- credit_data_engineered %>%
  group_by(age_group, Housing, employment_stability) %>%
  summarise(
    n_customers = n(),
    approval_rate = sum(Risk == "good") / n() * 100,
    avg_credit_amount = mean(Credit.amount),
    .groups = 'drop'
  )

cat("\n=== FINANCIAL INCLUSION ANALYSIS ===\n")
print(financial_inclusion_analysis)

# 12. TRADITIONAL VS ALTERNATIVE DATA COMPARISON
# Train traditional model (basic features only)
traditional_features <- c("Age", "Sex", "Credit.amount", "Duration", "Risk")
traditional_data <- credit_data_clean %>% 
  select(all_of(traditional_features)) %>%
  na.omit()

set.seed(123)
train_index_trad <- createDataPartition(traditional_data$Risk, p = 0.7, list = FALSE)
train_trad <- traditional_data[train_index_trad, ]
test_trad <- traditional_data[-train_index_trad, ]

traditional_model <- train(
  Risk ~ .,
  data = train_trad,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "ROC"
)

traditional_metrics <- evaluate_model(traditional_model, test_trad, "Traditional Model")

# Compare approaches
comparison_table <- data.frame(
  Approach = c("Traditional Features Only", "With Alternative Data"),
  AUC = c(round(traditional_metrics$auc, 3), round(metrics$rf$auc, 3)),
  Accuracy = c(round(traditional_metrics$accuracy, 3), round(metrics$rf$accuracy, 3)),
  Improvement = c(0, round((metrics$rf$auc - traditional_metrics$auc) * 100, 1))
)

cat("\n=== TRADITIONAL VS ALTERNATIVE DATA APPROACH ===\n")
print(comparison_table)

# 13. FINAL CASE STUDY INSIGHTS
cat("\n=== KEY INSIGHTS FOR CASE STUDY ===\n")
cat("1. Dataset Characteristics:\n")
cat("   - Total customers:", nrow(credit_data), "\n")
cat("   - Default rate:", round(mean(credit_data$Risk == "bad") * 100, 1), "%\n")
cat("   - Average credit amount:", round(mean(credit_data$Credit.amount), 2), "\n")

cat("2. Model Performance:\n")
cat("   - Best model:", performance_table$Model[which.max(performance_table$AUC)], "\n")
cat("   - Best AUC:", max(performance_table$AUC), "\n")
cat("   - Improvement over traditional:", comparison_table$Improvement[2], "%\n")

cat("3. Financial Inclusion Impact:\n")
cat("   - Alternative data enables assessment of customers with limited banking history\n")
cat("   - Model considers employment stability, housing patterns, and transaction behaviors\n")
cat("   - Can serve populations typically excluded from traditional credit scoring\n")

# 14. SAVE RESULTS FOR REPORT
results <- list(
  performance_table = performance_table,
  comparison_table = comparison_table,
  financial_inclusion_analysis = financial_inclusion_analysis,
  feature_importance = feature_imp
)

# Print success message
cat("\n✅ Analysis completed successfully!\n")
cat("📊 Results are ready for your case study report.\n")
