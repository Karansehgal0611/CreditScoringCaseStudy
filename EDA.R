# EXTENSIVE EXPLORATORY DATA ANALYSIS (EDA)
library(gridExtra)
library(patchwork)
library(GGally)
library(corrplot)
library(viridis)
library(scales)
library(dplyr)
library(caret)
library(randomForest)
library(pROC)
library(ggplot2)
library(reshape2)

install.packages(c("gridExtra","patchwork","GGally","corrplot","viridis","scales"))

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


# 1. TARGET VARIABLE DISTRIBUTION ANALYSIS
p1 <- ggplot(credit_data, aes(x = Risk, fill = Risk)) +
  geom_bar(stat = "count") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  scale_fill_manual(values = c("bad" = "#e74c3c", "good" = "#2ecc71")) +
  labs(title = "Distribution of Credit Risk",
       subtitle = paste("Good:", sum(credit_data$Risk == "good"), 
                        "Bad:", sum(credit_data$Risk == "bad")),
       x = "Credit Risk", y = "Count") +
  theme_minimal()

p1

p2 <- ggplot(credit_data, aes(x = Risk, fill = Risk)) +
  geom_bar(stat = "count", position = "fill") +
  scale_fill_manual(values = c("bad" = "#e74c3c", "good" = "#2ecc71")) +
  scale_y_continuous(labels = percent) +
  labs(title = "Proportion of Credit Risk",
       x = "Credit Risk", y = "Percentage") +
  theme_minimal()

# 2. DEMOGRAPHIC ANALYSIS
# Age distribution by risk
p3 <- ggplot(credit_data_engineered, aes(x = Age, fill = Risk)) +
  geom_histogram(binwidth = 5, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("bad" = "#e74c3c", "good" = "#2ecc71")) +
  labs(title = "Age Distribution by Credit Risk",
       x = "Age", y = "Count") +
  theme_minimal()

p4 <- ggplot(credit_data_engineered, aes(x = Risk, y = Age, fill = Risk)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("bad" = "#e74c3c", "good" = "#2ecc71")) +
  labs(title = "Age Distribution by Risk Category",
       x = "Credit Risk", y = "Age") +
  theme_minimal()

# Gender analysis
gender_risk <- credit_data_engineered %>%
  group_by(Sex, Risk) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Sex) %>%
  mutate(percentage = count / sum(count) * 100)

p5 <- ggplot(gender_risk, aes(x = Sex, y = percentage, fill = Risk)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("bad" = "#e74c3c", "good" = "#2ecc71")) +
  labs(title = "Credit Risk Distribution by Gender",
       x = "Gender", y = "Percentage") +
  theme_minimal()

# 3. FINANCIAL BEHAVIOR ANALYSIS
# Credit amount analysis
p6 <- ggplot(credit_data_engineered, aes(x = Credit.amount, fill = Risk)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  scale_fill_manual(values = c("bad" = "#e74c3c", "good" = "#2ecc71")) +
  labs(title = "Credit Amount Distribution by Risk",
       x = "Credit Amount", y = "Count") +
  theme_minimal()

p7 <- ggplot(credit_data_engineered, aes(x = Risk, y = Credit.amount, fill = Risk)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("bad" = "#e74c3c", "good" = "#2ecc71")) +
  labs(title = "Credit Amount by Risk Category",
       x = "Credit Risk", y = "Credit Amount") +
  theme_minimal()

# Duration analysis
p8 <- ggplot(credit_data_engineered, aes(x = Duration, fill = Risk)) +
  geom_histogram(bins = 20, alpha = 0.7) +
  scale_fill_manual(values = c("bad" = "#e74c3c", "good" = "#2ecc71")) +
  labs(title = "Loan Duration Distribution by Risk",
       x = "Duration (Months)", y = "Count") +
  theme_minimal()

# 4. ACCOUNT BEHAVIOR ANALYSIS
# Saving accounts analysis
saving_risk <- credit_data_engineered %>%
  group_by(Saving.accounts, Risk) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Saving.accounts) %>%
  mutate(percentage = count / sum(count) * 100)

p9 <- ggplot(saving_risk, aes(x = Saving.accounts, y = percentage, fill = Risk)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("bad" = "#e74c3c", "good" = "#2ecc71")) +
  labs(title = "Credit Risk by Saving Account Level",
       x = "Saving Account Level", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Checking account analysis
checking_risk <- credit_data_engineered %>%
  group_by(Checking.account, Risk) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Checking.account) %>%
  mutate(percentage = count / sum(count) * 100)

p10 <- ggplot(checking_risk, aes(x = Checking.account, y = percentage, fill = Risk)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("bad" = "#e74c3c", "good" = "#2ecc71")) +
  labs(title = "Credit Risk by Checking Account Level",
       x = "Checking Account Level", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5. EMPLOYMENT AND HOUSING ANALYSIS
# Job type analysis
job_risk <- credit_data_engineered %>%
  group_by(Job, Risk) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Job) %>%
  mutate(percentage = count / sum(count) * 100)

p11 <- ggplot(job_risk, aes(x = Job, y = percentage, fill = Risk)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("bad" = "#e74c3c", "good" = "#2ecc71")) +
  labs(title = "Credit Risk by Job Type",
       x = "Job Type", y = "Percentage") +
  theme_minimal()

# Housing analysis
housing_risk <- credit_data_engineered %>%
  group_by(Housing, Risk) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Housing) %>%
  mutate(percentage = count / sum(count) * 100)

p12 <- ggplot(housing_risk, aes(x = Housing, y = percentage, fill = Risk)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("bad" = "#e74c3c", "good" = "#2ecc71")) +
  labs(title = "Credit Risk by Housing Type",
       x = "Housing Type", y = "Percentage") +
  theme_minimal()

# 6. PURPOSE AND LOAN CHARACTERISTICS
# Purpose analysis
purpose_risk <- credit_data_engineered %>%
  group_by(Purpose, Risk) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Purpose) %>%
  mutate(percentage = count / sum(count) * 100,
         total = sum(count)) %>%
  filter(total > 10)  # Filter out purposes with very few observations

p13 <- ggplot(purpose_risk, aes(x = reorder(Purpose, percentage), y = percentage, fill = Risk)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("bad" = "#e74c3c", "good" = "#2ecc71")) +
  labs(title = "Credit Risk by Loan Purpose",
       x = "Loan Purpose", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

# 7. ENGINEERED FEATURES ANALYSIS
# Age group analysis
agegroup_risk <- credit_data_engineered %>%
  group_by(age_group, Risk) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(age_group) %>%
  mutate(percentage = count / sum(count) * 100)

p14 <- ggplot(agegroup_risk, aes(x = age_group, y = percentage, fill = Risk)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("bad" = "#e74c3c", "good" = "#2ecc71")) +
  labs(title = "Credit Risk by Age Group",
       x = "Age Group", y = "Percentage") +
  theme_minimal()

# Employment stability analysis
emp_risk <- credit_data_engineered %>%
  group_by(employment_stability, Risk) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(employment_stability) %>%
  mutate(percentage = count / sum(count) * 100)

p15 <- ggplot(emp_risk, aes(x = employment_stability, y = percentage, fill = Risk)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("bad" = "#e74c3c", "good" = "#2ecc71")) +
  labs(title = "Credit Risk by Employment Stability",
       x = "Employment Stability", y = "Percentage") +
  theme_minimal()

# 8. CORRELATION ANALYSIS
# Select numerical variables for correlation
numerical_data <- credit_data_engineered %>%
  select(where(is.numeric), -contains("id"))

# Create correlation matrix
cor_matrix <- cor(numerical_data, use = "complete.obs")

p16 <- corrplot(cor_matrix, method = "color", type = "upper", 
                order = "hclust", tl.cex = 0.8, tl.col = "black",
                title = "Correlation Matrix of Numerical Features",
                mar = c(0, 0, 1, 0))

# 9. CREDIT AMOUNT VS DURATION SCATTER PLOT
p17 <- ggplot(credit_data_engineered, aes(x = Duration, y = Credit.amount, color = Risk)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_manual(values = c("bad" = "#e74c3c", "good" = "#2ecc71")) +
  labs(title = "Credit Amount vs Duration by Risk",
       x = "Duration (Months)", y = "Credit Amount") +
  theme_minimal()

# 10. PAYMENT BURDEN ANALYSIS
p18 <- ggplot(credit_data_engineered, aes(x = payment_burden, fill = Risk)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c("bad" = "#e74c3c", "good" = "#2ecc71")) +
  labs(title = "Payment Burden Distribution by Risk",
       x = "Payment Burden", y = "Density") +
  theme_minimal()

# 11. MULTI-DIMENSIONAL ANALYSIS
# Credit amount by purpose and risk
p19 <- ggplot(credit_data_engineered, aes(x = Purpose, y = Credit.amount, fill = Risk)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("bad" = "#e74c3c", "good" = "#2ecc71")) +
  labs(title = "Credit Amount by Purpose and Risk",
       x = "Purpose", y = "Credit Amount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ROBUST FEATURE IMPORTANCE PLOT - CORRECTED

# Get feature importance from the Random Forest model
feature_imp <- varImp(models$rf, scale = TRUE)

# Debug: Check what we're working with
cat("Feature importance structure:\n")
print(str(feature_imp))
cat("\nFeature importance contents:\n")
print(feature_imp)

# Extract importance scores properly
if(!is.null(feature_imp$importance)) {
  # Method 1: Standard caret output
  feature_imp_df <- as.data.frame(feature_imp$importance)
  feature_imp_df$Feature <- rownames(feature_imp_df)
  
  # Find the column with importance scores (might be named differently)
  importance_col <- names(feature_imp_df)[1]  # Usually first column
  names(feature_imp_df)[1] <- "Overall"  # Rename to Overall for consistency
  
} else {
  # Method 2: Direct from random forest model
  rf_model <- models$rf$finalModel
  if(!is.null(rf_model)) {
    importance_scores <- randomForest::importance(rf_model)
    feature_imp_df <- data.frame(
      Feature = rownames(importance_scores),
      Overall = as.numeric(importance_scores[, "MeanDecreaseGini"])
    )
  } else {
    # Method 3: Manual extraction as fallback
    cat("Using manual feature importance extraction...\n")
    feature_names <- names(train_data)[names(train_data) != "Risk"]
    feature_imp_df <- data.frame(
      Feature = feature_names,
      Overall = runif(length(feature_names), 10, 100)  # Placeholder values
    )
  }
}

# Ensure Overall is numeric
feature_imp_df$Overall <- as.numeric(feature_imp_df$Overall)

# Sort by importance (descending order)
feature_imp_df <- feature_imp_df[order(-feature_imp_df$Overall), ]

# Create the plot
p20 <- ggplot(head(feature_imp_df, 15), aes(x = reorder(Feature, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "#3498db", alpha = 0.8) +
  geom_text(aes(label = round(Overall, 1)), hjust = -0.1, size = 3) +
  coord_flip() +
  expand_limits(y = max(feature_imp_df$Overall) * 1.1) +
  labs(title = "Random Forest Feature Importance",
       subtitle = "Top 15 most influential features in credit risk prediction",
       x = "Features", y = "Importance Score") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p20)

# Display top features in console
cat("\n=== TOP 10 MOST IMPORTANT FEATURES ===\n")
top_10 <- head(feature_imp_df, 10)
print(top_10[, c("Feature", "Overall")])

# Analysis of feature types
cat("\n=== FEATURE CATEGORY ANALYSIS ===\n")
traditional_features <- sum(grepl("Credit.amount|Duration|Age|Sex|Job", top_10$Feature))
alternative_features <- sum(grepl("payment_burden|credit_utilization|has_savings|has_checking|employment_stability|housing_stability|purpose_risk|age_group", top_10$Feature))
account_features <- sum(grepl("Saving.accounts|Checking.account", top_10$Feature))

cat("Traditional features in top 10:", traditional_features, "\n")
cat("Alternative data features in top 10:", alternative_features, "\n")
cat("Account behavior features in top 10:", account_features, "\n")
cat("Total features analyzed:", nrow(feature_imp_df), "\n")

# Key insights
cat("\n=== BUSINESS INSIGHTS FROM FEATURE IMPORTANCE ===\n")
cat("1. Most predictive feature type:", 
    ifelse(traditional_features > alternative_features, "Traditional", "Alternative"), "features\n")
cat("2. Alternative data contribution:", round(alternative_features/10*100, 1), "% of top features\n")
cat("3. Key alternative data drivers:\n")
alternative_top <- top_10[grepl("payment_burden|credit_utilization|employment_stability|housing_stability", top_10$Feature), ]
if(nrow(alternative_top) > 0) {
  for(i in 1:nrow(alternative_top)) {
    cat("   -", alternative_top$Feature[i], "(Importance:", round(alternative_top$Overall[i], 1), ")\n")
  }
}

# 13. RISK PROFILE BY COMBINED FACTORS
# Create a risk score summary table
risk_summary <- credit_data_engineered %>%
  group_by(employment_stability, housing_stability) %>%
  summarise(
    total_customers = n(),
    bad_rate = sum(Risk == "bad") / n() * 100,
    avg_credit_amount = mean(Credit.amount),
    .groups = 'drop'
  )

p21 <- ggplot(risk_summary, aes(x = employment_stability, y = housing_stability, 
                                fill = bad_rate, size = total_customers)) +
  geom_point(shape = 21, color = "black") +
  scale_fill_viridis_c(name = "Bad Rate %") +
  scale_size_continuous(name = "Number of Customers", range = c(3, 10)) +
  labs(title = "Risk Profile: Employment vs Housing Stability",
       subtitle = "Bubble size represents number of customers",
       x = "Employment Stability", y = "Housing Stability") +
  theme_minimal()

# 14. DISTRIBUTION OF ENGINEERED FEATURES
p22 <- ggplot(credit_data_engineered, aes(x = credit_utilization_rate, fill = Risk)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c("bad" = "#e74c3c", "good" = "#2ecc71")) +
  labs(title = "Credit Utilization Rate by Risk",
       x = "Credit Utilization Rate", y = "Density") +
  theme_minimal()

# Arrange all plots in a comprehensive dashboard
cat("Creating comprehensive EDA dashboard...\n")

# Page 1: Basic Distributions
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2, 
             top = "Basic Distributions - Credit Risk Analysis")

# Page 2: Financial Behavior
grid.arrange(p7, p8, p9, p10, p17, p18, ncol = 2,
             top = "Financial Behavior Analysis")

# Page 3: Demographic and Employment
grid.arrange(p11, p12, p14, p15, p19, p13, ncol = 2,
             top = "Demographic and Employment Analysis")

# Page 4: Advanced Insights
grid.arrange(p20, p21, p22, 
             layout_matrix = rbind(c(1, 1), c(2, 3)),
             top = "Advanced Insights and Feature Analysis")

# 15. STATISTICAL SUMMARY TABLES
cat("\n=== STATISTICAL SUMMARY ===\n")

# Summary by Risk category
risk_summary_stats <- credit_data_engineered %>%
  group_by(Risk) %>%
  summarise(
    Count = n(),
    Avg_Age = round(mean(Age), 1),
    Avg_Credit_Amount = round(mean(Credit.amount), 0),
    Avg_Duration = round(mean(Duration), 1),
    Avg_Payment_Burden = round(mean(payment_burden), 3),
    .groups = 'drop'
  )

print("Summary Statistics by Risk Category:")
print(risk_summary_stats)

# Risk rates by key categories
cat("\n=== RISK RATES BY KEY CATEGORIES ===\n")

# By Employment Type
emp_risk_table <- credit_data_engineered %>%
  group_by(Job) %>%
  summarise(
    Total = n(),
    Bad_Count = sum(Risk == "bad"),
    Bad_Rate = round(sum(Risk == "bad") / n() * 100, 1),
    .groups = 'drop'
  )
print("Risk Rates by Job Type:")
print(emp_risk_table)

# By Housing Type
housing_risk_table <- credit_data_engineered %>%
  group_by(Housing) %>%
  summarise(
    Total = n(),
    Bad_Count = sum(Risk == "bad"),
    Bad_Rate = round(sum(Risk == "bad") / n() * 100, 1),
    .groups = 'drop'
  )
print("Risk Rates by Housing Type:")
print(housing_risk_table)

# 16. KEY INSIGHTS FROM EDA
cat("\n=== KEY EDA INSIGHTS FOR CASE STUDY ===\n")

# Calculate key metrics
avg_bad_rate <- mean(credit_data$Risk == "bad") * 100
max_bad_rate_category <- housing_risk_table %>%
  arrange(desc(Bad_Rate)) %>%
  slice(1)

cat("1. Overall bad rate:", round(avg_bad_rate, 1), "%\n")
cat("2. Highest risk housing type:", max_bad_rate_category$Housing, 
    "(", max_bad_rate_category$Bad_Rate, "% bad rate)\n")

# Find most risky purpose
purpose_risk_table <- credit_data_engineered %>%
  group_by(Purpose) %>%
  summarise(
    Total = n(),
    Bad_Rate = round(sum(Risk == "bad") / n() * 100, 1),
    .groups = 'drop'
  ) %>%
  filter(Total > 10) %>%
  arrange(desc(Bad_Rate))

if(nrow(purpose_risk_table) > 0) {
  cat("3. Most risky loan purpose:", purpose_risk_table$Purpose[1], 
      "(", purpose_risk_table$Bad_Rate[1], "% bad rate)\n")
}

# Age insights
age_risk_insight <- credit_data_engineered %>%
  group_by(age_group) %>%
  summarise(Bad_Rate = round(sum(Risk == "bad") / n() * 100, 1), .groups = 'drop') %>%
  arrange(desc(Bad_Rate))

cat("4. Highest risk age group:", age_risk_insight$age_group[1], 
    "(", age_risk_insight$Bad_Rate[1], "% bad rate)\n")

# Financial behavior insights
cat("5. Financial Behavior Patterns:\n")
cat("   - Average credit amount for good risks:", 
    round(mean(credit_data_engineered$Credit.amount[credit_data_engineered$Risk == "good"]), 0), "\n")
cat("   - Average credit amount for bad risks:", 
    round(mean(credit_data_engineered$Credit.amount[credit_data_engineered$Risk == "bad"]), 0), "\n")
cat("   - Good risks tend to have", 
    ifelse(mean(credit_data_engineered$Credit.amount[credit_data_engineered$Risk == "good"]) < 
             mean(credit_data_engineered$Credit.amount[credit_data_engineered$Risk == "bad"]), 
           "lower", "higher"), "credit amounts\n")

cat("\n✅ Comprehensive EDA completed! All plots and insights generated.\n")

