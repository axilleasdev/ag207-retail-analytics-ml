# AG207 — Retail Analytics & Machine Learning

Individual assignment for **AG207 Machine Learning with R** (University of Essex,
BSc Computing — AI, 2025–2026).

The project applies statistics, linear regression, and Naive Bayes classification
to real retail and advertising data, in order to support data-driven marketing
decisions for a manufacturer.

## Overview

The analysis is split into three sections, each answering specific assignment
questions (Q1–Q14):

| Section | Topic | Questions | Key methods |
|---------|-------|-----------|-------------|
| **A** | Statistics & visualization | Q1–Q9 | NA handling, descriptive stats, skewness, correlation / multicollinearity |
| **B** | Linear regression | Q10–Q11 | Multi-dataset merge, MLR, MSE/RMSE, VIF, adjusted R² model comparison |
| **C** | Naive Bayes classification | Q12–Q14 | Scatter analysis, Gaussian NB, confusion matrix, 10-fold cross-validation |

## Key results

- **Q6/Q7 — fair comparison:** 2005 only covers Jan–May, so comparing full years is
  invalid. On a like-for-like basis (Jan–May), 2005 **grew ~+36%** vs 2004 — it was a
  strong year, and no product line actually declined.
- **Q11 — regression:** R² ≈ 0.16, RMSE ≈ 7% of mean weekly sales; CPI is the only
  individually significant predictor. VIF confirms no serious multicollinearity
  (all < 10). The full model beats a CPI-only model on adjusted R².
- **Q14 — Naive Bayes:** 96% test accuracy, confirmed by 10-fold cross-validation
  (~96.6% ± 1.5%) — generalizes well, no overfitting.

## Structure

```
.
├── data/                  # Datasets (CSV)
│   ├── sales_data_sample.csv      # Section A (2 823 transactions)
│   ├── Features data set.csv      # Section B
│   ├── sales data-set.csv         # Section B
│   ├── stores data-set.csv        # Section B
│   └── advertising.csv            # Section C (1 000 users)
├── scripts/
│   ├── section_a.R        # Statistics & visualization (Q1–Q9)
│   ├── section_b.R        # Linear regression (Q10–Q11)
│   └── section_c.R        # Naive Bayes classification (Q12–Q14)
└── report/                # Final report (R Markdown + PDF)
```

## Requirements

- R (≥ 4.0)
- Packages:

```r
install.packages(c("ggplot2", "dplyr", "corrplot", "e1071", "caret"))
```

## How to run

From the **project root** (the scripts expect relative `data/...` paths):

```r
source("scripts/section_a.R")   # Section A
source("scripts/section_b.R")   # Section B
source("scripts/section_c.R")   # Section C
```

Or from the terminal:

```bash
Rscript scripts/section_a.R
```

## Reproducibility

All random operations use `set.seed(42)`, so train/test splits and
cross-validation results are reproducible across runs.
