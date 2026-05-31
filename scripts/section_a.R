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
