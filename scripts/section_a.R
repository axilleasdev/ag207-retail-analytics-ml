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

# Analysis: 2005 has lower sales because the dataset only covers part of the year
# (first 5 months). It's not necessarily a bad year - the data is incomplete.
cat("\nSales by year:\n")
print(yearly_sales)
cat("\nMonths available in 2005:", 
    sort(unique(sales_clean$MONTH_ID[sales_clean$YEAR_ID == 2005])), "\n")

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

cat("\nSales drop 2004 → 2005:\n")
print(sales_comparison %>% arrange(desc(Drop)))
cat("\nProduct with most drop:", 
    sales_comparison$PRODUCTLINE[which.max(sales_comparison$Drop)], "\n")

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

cat("\nHighly correlated variables:\n")
cat("- SALES & QUANTITYORDERED (strong positive)\n")
cat("- SALES & PRICEEACH (positive)\n")
cat("- PRICEEACH & MSRP (strong positive - both represent price)\n")
