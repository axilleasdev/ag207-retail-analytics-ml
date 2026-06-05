# ============================================================
# AG207 - Machine Learning with R
# Section B: Linear Regression [24 Marks]
# ============================================================

library(dplyr)
library(ggplot2)

# ============================================================
# Q10: Prepare dataset for regression [10 Marks]
# ============================================================

# Load all three datasets
features <- read.csv("data/Features data set.csv")
sales <- read.csv("data/sales data-set.csv")
stores <- read.csv("data/stores data-set.csv")

# Merge datasets:
# Step 1: Merge sales with features (by Store and Date) - this gives us
#         economic indicators alongside weekly sales figures
# Step 2: Merge with stores (by Store) - this adds store type and size
merged <- sales %>%
  merge(features, by = c("Store", "Date", "IsHoliday")) %>%
  merge(stores, by = "Store")

cat("Merged dataset:", nrow(merged), "rows,", ncol(merged), "columns\n")

# Check NAs
cat("\nNA counts:\n")
print(colSums(is.na(merged)))

# Variable selection reasoning:
# KEEP: Temperature, Fuel_Price, CPI, Unemployment, IsHoliday, Size
#   - These are external factors that influence consumer spending
# DROP: MarkDown1-5 (too many NAs - promotional markdowns not always active)
#   - Date (we extract temporal info from other variables)
#   - Store, Dept (we filter for one store, aggregate departments)
#   - Type (constant for one store)

merged_clean <- merged %>%
  select(Store, Dept, Weekly_Sales, Temperature, Fuel_Price, CPI, Unemployment, IsHoliday, Size)

cat("\nVariables kept for regression:\n")
cat(paste(names(merged_clean), collapse = ", "), "\n")

# ============================================================
# Q11: Multiple Linear Regression for Store 1 [14 Marks]
# ============================================================

# Choose Store 1 (Type A, large store with 151,315 sq ft)
# Aggregate all departments to get total weekly sales per date
store1 <- merged_clean %>%
  filter(Store == 1) %>%
  group_by(Temperature, Fuel_Price, CPI, Unemployment, IsHoliday) %>%
  summarise(Weekly_Sales = sum(Weekly_Sales), .groups = "drop")

cat("\nStore 1 data points:", nrow(store1), "\n")
cat("Weekly Sales range: $", min(store1$Weekly_Sales), "- $", max(store1$Weekly_Sales), "\n")

# Train/Test split (80% train, 20% test)
# We use a random split to evaluate generalization ability
set.seed(42)  # For reproducibility
train_idx <- sample(1:nrow(store1), size = 0.8 * nrow(store1))
train <- store1[train_idx, ]
test <- store1[-train_idx, ]

cat("Training set:", nrow(train), "rows\n")
cat("Test set:", nrow(test), "rows\n")

# Multiple Linear Regression
# Model: Weekly_Sales depends on Temperature, Fuel_Price, CPI, Unemployment, IsHoliday
model <- lm(Weekly_Sales ~ Temperature + Fuel_Price + CPI + Unemployment + IsHoliday, data = train)

cat("\n--- Model Summary ---\n")
print(summary(model))

# Predictions on test set
predictions <- predict(model, newdata = test)

# Evaluation: Mean Squared Error (MSE)
# MSE measures the average squared difference between predicted and actual values
# Lower MSE = better model
mse <- mean((test$Weekly_Sales - predictions)^2)
rmse <- sqrt(mse)

cat("\n--- Model Evaluation ---\n")
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("Mean Weekly Sales:", mean(test$Weekly_Sales), "\n")
cat("RMSE as % of mean:", round(rmse / mean(test$Weekly_Sales) * 100, 2), "%\n")

# Plot: Actual vs Predicted
ggplot(data.frame(Actual = test$Weekly_Sales, Predicted = predictions), 
       aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Store 1: Actual vs Predicted Weekly Sales",
       x = "Actual Sales ($)", y = "Predicted Sales ($)") +
  theme_minimal()
