# ============================================================
# AG207 - Machine Learning with R
# Section A: Statistics and Visualization [36 Marks]
# ============================================================

# Load libraries
library(ggplot2)
library(dplyr)
library(corrplot)

# Load dataset
sales <- read.csv("data/sales_data_sample.csv")

# ============================================================
# Q1: Check dataset for null values [4 Marks]
# ============================================================
# Count NA values per column
na_counts <- colSums(is.na(sales))
cat("Null values per column:\n")
print(na_counts[na_counts > 0])
cat("\nTotal null values:", sum(is.na(sales)), "\n")

# ============================================================
# Q2: Decide which variables to keep [4 Marks]
# ============================================================
# Reasoning:
# - ORDERNUMBER, ORDERLINENUMBER: administrative IDs, not useful for analysis
# - PHONE, ADDRESSLINE1, ADDRESSLINE2: personal info, irrelevant to sales patterns
# - CONTACTLASTNAME, CONTACTFIRSTNAME: personal info, irrelevant
# - POSTALCODE, STATE: too granular, COUNTRY is enough for geographic analysis
# - STATUS: most orders are "Shipped", low variability
#
# We keep variables that describe: WHAT was sold, HOW MUCH, WHEN, WHERE, and deal size
sales_clean <- sales %>%
  select(QUANTITYORDERED, PRICEEACH, SALES, ORDERDATE, QTR_ID, MONTH_ID,
         YEAR_ID, PRODUCTLINE, MSRP, PRODUCTCODE, CUSTOMERNAME,
         CITY, COUNTRY, TERRITORY, DEALSIZE)

cat("Variables kept:", ncol(sales_clean), "\n")
cat("Columns:", paste(names(sales_clean), collapse = ", "), "\n")

# ============================================================
# Q3: Top 5 countries by sales - bar plot & pie chart [4 Marks]
# ============================================================
top5_countries <- sales_clean %>%
  group_by(COUNTRY) %>%
  summarise(Total_Sales = sum(SALES)) %>%
  arrange(desc(Total_Sales)) %>%
  head(5)

# Bar plot
ggplot(top5_countries, aes(x = reorder(COUNTRY, -Total_Sales), y = Total_Sales, fill = COUNTRY)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 Countries by Total Sales", x = "Country", y = "Total Sales ($)") +
  theme_minimal() +
  theme(legend.position = "none")

# Pie chart
ggplot(top5_countries, aes(x = "", y = Total_Sales, fill = COUNTRY)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Top 5 Countries by Sales - Pie Chart") +
  theme_void()

# ============================================================
# Q4: Mean, median, mode, min, max of Quantity Ordered and Sales [4 Marks]
# ============================================================
# Mode function (R doesn't have a built-in mode)
get_mode <- function(x) {
  uniq <- unique(x)
  uniq[which.max(tabulate(match(x, uniq)))]
}

cat("\n--- QUANTITYORDERED ---\n")
cat("Mean:", mean(sales_clean$QUANTITYORDERED), "\n")
cat("Median:", median(sales_clean$QUANTITYORDERED), "\n")
cat("Mode:", get_mode(sales_clean$QUANTITYORDERED), "\n")
cat("Min:", min(sales_clean$QUANTITYORDERED), "\n")
cat("Max:", max(sales_clean$QUANTITYORDERED), "\n")

cat("\n--- SALES ---\n")
cat("Mean:", mean(sales_clean$SALES), "\n")
cat("Median:", median(sales_clean$SALES), "\n")
cat("Mode:", get_mode(sales_clean$SALES), "\n")
cat("Min:", min(sales_clean$SALES), "\n")
cat("Max:", max(sales_clean$SALES), "\n")

# ============================================================
# Q5: Peak months for sales + trending plot [4 Marks]
# ============================================================
monthly_sales <- sales_clean %>%
  group_by(MONTH_ID) %>%
  summarise(Total_Sales = sum(SALES))

ggplot(monthly_sales, aes(x = MONTH_ID, y = Total_Sales)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  scale_x_continuous(breaks = 1:12) +
  labs(title = "Sales Trending by Month", x = "Month", y = "Total Sales ($)") +
  theme_minimal()

cat("\nPeak month:", monthly_sales$MONTH_ID[which.max(monthly_sales$Total_Sales)], "\n")

# ============================================================
# Q6: Bar plot sales by year. Is 2005 a good year? [4 Marks]
# ============================================================
yearly_sales <- sales_clean %>%
  group_by(YEAR_ID) %>%
  summarise(Total_Sales = sum(SALES))

ggplot(yearly_sales, aes(x = factor(YEAR_ID), y = Total_Sales, fill = factor(YEAR_ID))) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by Year", x = "Year", y = "Total Sales ($)") +
  theme_minimal() +
  theme(legend.position = "none")

cat("\nSales by year:\n")
print(yearly_sales)

# Check month coverage per year
for (y in sort(unique(sales_clean$YEAR_ID))) {
  cat("Year", y, "- months with data:",
      paste(sort(unique(sales_clean$MONTH_ID[sales_clean$YEAR_ID == y])), collapse = ", "), "\n")
}

# IMPORTANT: 2005 only has Jan-May (5 months) while 2004 has all 12 months.
# Comparing whole years is INVALID. We must compare the SAME months (apples-to-apples).
fair_2004 <- sum(sales_clean$SALES[sales_clean$YEAR_ID == 2004 & sales_clean$MONTH_ID %in% 1:5])
fair_2005 <- sum(sales_clean$SALES[sales_clean$YEAR_ID == 2005 & sales_clean$MONTH_ID %in% 1:5])
cat("\nFair comparison (Jan-May only):\n")
cat("2004 (Jan-May): $", format(round(fair_2004), big.mark = ","), "\n")
cat("2005 (Jan-May): $", format(round(fair_2005), big.mark = ","), "\n")
cat("Change:", round((fair_2005 - fair_2004) / fair_2004 * 100, 1), "%\n")

# DECISION: Is 2005 a good year? YES. On a like-for-like basis (Jan-May), 2005 grew
# ~+36% vs the same period in 2004. The apparent "drop" is purely an artifact of the
# missing Jun-Dec data, not a real decline in performance.

# ============================================================
# Q7: Product with most drop in sales 2004 vs 2005 [4 Marks]
# ============================================================
sales_2004 <- sales_clean %>%
  filter(YEAR_ID == 2004) %>%
  group_by(PRODUCTLINE) %>%
  summarise(Sales_2004 = sum(SALES))

sales_2005 <- sales_clean %>%
  filter(YEAR_ID == 2005) %>%
  group_by(PRODUCTLINE) %>%
  summarise(Sales_2005 = sum(SALES))

sales_comparison <- merge(sales_2004, sales_2005, by = "PRODUCTLINE", all = TRUE)
sales_comparison$Sales_2005[is.na(sales_comparison$Sales_2005)] <- 0
sales_comparison$Drop <- sales_comparison$Sales_2004 - sales_comparison$Sales_2005

cat("\n--- Naive comparison (whole years - misleading) ---\n")
print(sales_comparison %>% arrange(desc(Drop)))
cat("Product with most drop (naive):",
    sales_comparison$PRODUCTLINE[which.max(sales_comparison$Drop)], "\n")

# Fair comparison: same months only (Jan-May), to avoid the Q6 trap
fair_2004 <- sales_clean %>%
  filter(YEAR_ID == 2004, MONTH_ID %in% 1:5) %>%
  group_by(PRODUCTLINE) %>%
  summarise(JanMay_2004 = sum(SALES))

fair_2005 <- sales_clean %>%
  filter(YEAR_ID == 2005, MONTH_ID %in% 1:5) %>%
  group_by(PRODUCTLINE) %>%
  summarise(JanMay_2005 = sum(SALES))

fair_cmp <- merge(fair_2004, fair_2005, by = "PRODUCTLINE", all = TRUE)
fair_cmp[is.na(fair_cmp)] <- 0
fair_cmp$Change <- fair_cmp$JanMay_2005 - fair_cmp$JanMay_2004
fair_cmp$Pct <- round(fair_cmp$Change / fair_cmp$JanMay_2004 * 100, 1)
cat("\n--- Fair comparison (Jan-May) ---\n")
print(fair_cmp %>% arrange(Change))

# CONCLUSION: In the naive whole-year view, Classic Cars show the largest absolute
# "drop" simply because they are the highest-volume line, so missing 7 months hurts
# them most in absolute terms. In the fair like-for-like comparison (Jan-May), NO
# product line actually declined - all grew. The drop is an artifact of incomplete data.

# ============================================================
# Q8: Count product categories & make factor [4 Marks]
# ============================================================
product_factor <- factor(sales_clean$PRODUCTLINE)
cat("\nProduct categories:", nlevels(product_factor), "\n")
cat("Frequency:\n")
print(table(product_factor))

# ============================================================
# Q9: Correlation analysis with heat map [4 Marks]
# ============================================================
# Select numeric variables for correlation
numeric_vars <- sales_clean %>%
  select(QUANTITYORDERED, PRICEEACH, SALES, QTR_ID, MONTH_ID, YEAR_ID, MSRP)

cor_matrix <- cor(numeric_vars, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black",
         title = "Correlation Heatmap", mar = c(0,0,1,0))

cat("\nHighly correlated variable pairs:\n")
cat("- PRICEEACH & MSRP (strong positive, r ~ 0.67 - both are price measures)\n")
cat("- SALES & PRICEEACH (r ~ 0.66) and SALES & MSRP (r ~ 0.64)\n")
cat("- SALES & QUANTITYORDERED (MODERATE positive, r ~ 0.55 - not strong)\n")
cat("- QTR_ID & MONTH_ID (near-perfect, r ~ 0.98 - redundant; keep only one)\n")
cat("\nNote: strongly correlated pairs (PRICEEACH/MSRP, QTR_ID/MONTH_ID) cause\n")
cat("multicollinearity in regression. Checked via VIF in Section B (threshold 5-10).\n")
