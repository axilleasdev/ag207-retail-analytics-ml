# ============================================================
# AG207 - Machine Learning with R
# Section C: Naive Bayes Classification [40 Marks]
# ============================================================

library(ggplot2)
library(e1071)
library(caret)

# Load dataset
ads <- read.csv("data/advertising.csv")
cat("Dataset:", nrow(ads), "rows,", ncol(ads), "columns\n")
cat("Columns:", paste(names(ads), collapse = ", "), "\n")

# ============================================================
# Q12: Scatter plots for all numeric variable pairs [15 Marks]
# ============================================================

# Select numeric variables relevant to user behavior
numeric_vars <- ads[, c("Daily.Time.Spent.on.Site", "Age", "Area.Income", "Daily.Internet.Usage")]

# Create scatter plots for all pairs, colored by click outcome
# This helps us visually identify patterns and correlations
pairs_list <- combn(names(numeric_vars), 2, simplify = FALSE)

for (pair in pairs_list) {
  p <- ggplot(ads, aes_string(x = pair[1], y = pair[2], color = "factor(Clicked.on.Ad)")) +
    geom_point(alpha = 0.5) +
    labs(title = paste(pair[1], "vs", pair[2]),
         color = "Clicked on Ad") +
    theme_minimal()
  print(p)
}

# Correlation matrix for numeric variables
cor_matrix <- cor(numeric_vars)
cat("\nCorrelation Matrix:\n")
print(round(cor_matrix, 3))

# Discussion:
# - Daily Time Spent on Site & Daily Internet Usage: positive correlation
#   (users who spend more time online also spend more on our site)
# - Age & Daily Time Spent on Site: negative correlation
#   (older users spend less time on site)
# - Area Income & Daily Internet Usage: positive correlation
#   (higher income areas have more internet usage)
# - The scatter plots show clear separation between clickers/non-clickers,
#   suggesting these features will be good predictors

# ============================================================
# Q13: Prepare data for Naive Bayes [5 Marks]
# ============================================================

# Predictors: Daily.Time.Spent.on.Site, Age, Area.Income, Daily.Internet.Usage, Male
# Reasoning:
#   - These are quantifiable user characteristics
#   - They describe browsing behavior and demographics
#   - We exclude: Ad.Topic.Line (text), City (too many categories),
#     Country (too many categories), Timestamp (would need feature engineering)
#
# Target variable: Clicked.on.Ad (binary: 0 = no click, 1 = click)

ads$Clicked.on.Ad <- factor(ads$Clicked.on.Ad, levels = c(0, 1), labels = c("No", "Yes"))

predictors <- ads[, c("Daily.Time.Spent.on.Site", "Age", "Area.Income", "Daily.Internet.Usage", "Male")]
target <- ads$Clicked.on.Ad

cat("\nPredictors:", paste(names(predictors), collapse = ", "), "\n")
cat("Target: Clicked.on.Ad\n")
cat("Class distribution:\n")
print(table(target))

# ============================================================
# Q14: Naive Bayes + Confusion Matrix [20 Marks]
# ============================================================

# Train/Test split (70% train, 30% test)
set.seed(42)
train_idx <- sample(1:nrow(ads), size = 0.7 * nrow(ads))
train_data <- ads[train_idx, ]
test_data <- ads[-train_idx, ]

# Train Naive Bayes model
# Naive Bayes assumes features are independent given the class (hence "naive")
# It works well for classification even when this assumption is violated
nb_model <- naiveBayes(Clicked.on.Ad ~ Daily.Time.Spent.on.Site + Age + Area.Income + Daily.Internet.Usage + Male,
                       data = train_data)

cat("\n--- Naive Bayes Model ---\n")
print(nb_model)

# Predictions on test set
nb_predictions <- predict(nb_model, newdata = test_data)

# Confusion Matrix
conf_matrix <- confusionMatrix(nb_predictions, test_data$Clicked.on.Ad)
cat("\n--- Confusion Matrix ---\n")
print(conf_matrix)

# Discussion:
cat("\n--- Results Discussion ---\n")
cat("Accuracy:", round(conf_matrix$overall["Accuracy"] * 100, 2), "%\n")
cat("Positive class (caret default = first level):", conf_matrix$positive, "\n")
cat("Sensitivity:", round(conf_matrix$byClass["Sensitivity"] * 100, 2),
    "% -> correctly identifies NON-clickers (positive class = 'No')\n")
cat("Specificity:", round(conf_matrix$byClass["Specificity"] * 100, 2),
    "% -> correctly identifies CLICKERS ('Yes')\n")

# 10-fold cross-validation: confirm the high accuracy is not split-dependent / overfitting
set.seed(42)
k <- 10
folds <- sample(rep(1:k, length.out = nrow(ads)))
cv_acc <- numeric(k)
for (i in 1:k) {
  tr <- ads[folds != i, ]
  va <- ads[folds == i, ]
  m  <- naiveBayes(Clicked.on.Ad ~ Daily.Time.Spent.on.Site + Age +
                   Area.Income + Daily.Internet.Usage + Male, data = tr)
  cv_acc[i] <- mean(predict(m, va) == va$Clicked.on.Ad)
}
cat("\n--- 10-fold Cross-Validation ---\n")
cat("Mean CV accuracy:", round(mean(cv_acc) * 100, 2), "%\n")
cat("Std deviation:", round(sd(cv_acc) * 100, 2), "%\n")

# DECISION: Does the model work? YES. It reaches ~96% accuracy on the held-out test
# set, confirmed by 10-fold CV (~96.6% +/- 1.5%), far above the 50% random baseline
# for balanced classes. Naive Bayes works well here because the class-conditional
# distributions are well separated, despite the independence assumption being mildly
# violated (Time on Site vs Internet Usage, r = 0.52).
