# varselect: Variable Selection in R

[![R](https://img.shields.io/badge/R-%3E%3D4.0.0-blue.svg)](https://www.r-project.org/) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

**varselect** is a comprehensive R package for automated variable selection using statistical significance testing, multicollinearity detection, and multiple selection methods. It features an intuitive pipeline architecture that allows chaining operations with the `+` operator.

## Features

### Core Capabilities

-   **Multiple Selection Methods**: Backward elimination (default), forward selection, stepwise, and hybrid approaches
-   **Advanced Metrics**:
    -   Relevancy: Spearman, Kendall, Pearson correlations
    -   Predictive: RMSE, MAE, R², Adjusted R²
    -   Classification: AUC, F1, MCC, Precision, Recall
    -   Model Fit: AIC, BIC, AICc, **Mallow's Cp**
-   **Statistical Rigor**: FDR (Benjamini-Hochberg) correction for multiple testing
-   **Multicollinearity Detection**: VIF and Condition Number analysis
-   **Cross-Validation**: Stratified k-fold CV for robust validation
-   **Pipeline Architecture**: Intuitive chaining with `+` operator

### Supported Models

-   Linear Regression (numeric targets)
-   Logistic Regression (binary targets)
-   Multinomial Regression (multiclass targets)

### Beginner User

``` r
# Load your data
data(mtcars)

# Run full pipeline with defaults
pipeline <- varselect(mtcars, "mpg") + 
  fullselection() +
  show_vars(show_scores = TRUE) +
  train()

# View results
summary(pipeline)

# Make predictions
predictions <- predict(pipeline@model, newdata = test_data)
```

### Advanced User

``` r
# Full control over each step
pipeline <- varselect(mtcars, "mpg") +
  validate(exclude = c("cyl", "vs")) +
  significance(alpha = 0.05, correction = "fdr") +
  refine(vif_threshold = 10) +
  selection(
    method = "backward",
    metrics = c("spearman", "rmse", "aic", "mallows_cp", "r2_adj"),
    weights = c(0.3, 0.2, 0.2, 0.15, 0.15),
    cv_folds = 10,
    top = "auto"
  ) +
  prediction(metric = "rmse", cv_folds = 10) +
  visualize(type = "importance", top = 5) +
  show_vars(show_scores = TRUE) +
  train()

# Access results
pipeline@final_vars        
pipeline@scores           
pipeline@performance      
pipeline@model           
```

## Pipeline Functions

| Function | Description | Key Parameters |
|-------------------|------------------------|-----------------------------|
| `varselect()` | Initialize pipeline | `data`, `target` |
| `validate()` | Remove invalid variables | `exclude`, `missing_threshold`, `variance_threshold` |
| `significance()` | Test statistical significance | `alpha`, `correction` ("fdr", "bonferroni") |
| `refine()` | Remove multicollinearity | `vif_threshold`, `condition_threshold` |
| `selection()` | Select best variables | `method`, `metrics`, `weights`, `cv_folds`, `top` |
| `prediction()` | Validate with CV | `metric`, `cv_folds` |
| `visualize()` | Create plots | `type`, `top`, `show` |
| `show_vars()` | Display variables | `show`, `show_scores` |
| `train()` | Train final model | \- |
| `fullselection()` | Run complete pipeline | All of the above |

## Available Metrics

### Relevancy Metrics

-   `"pearson"` - Pearson correlation
-   `"spearman"` - Spearman rank correlation (robust to outliers)
-   `"kendall"` - Kendall's tau (concordance)

### Predictive Metrics (Regression)

-   `"rmse"` - Root Mean Squared Error
-   `"mae"` - Mean Absolute Error
-   `"r2"` - R-Squared
-   `"r2_adj"` - Adjusted R-Squared

### Predictive Metrics (Classification)

-   `"auc"` - Area Under ROC Curve
-   `"f1"` - F1 Score
-   `"precision"` - Precision
-   `"recall"` - Recall/Sensitivity
-   `"mcc"` - Matthews Correlation Coefficient

### Model Fit Metrics

-   `"aic"` - Akaike Information Criterion
-   `"bic"` - Bayesian Information Criterion
-   `"aicc"` - Corrected AIC (small samples)
-   `"mallows_cp"` - Mallow's Cp statistic

## Selection Methods

| Method | Description | Best For |
|-------------------|------------------------------|-----------------------|
| `"backward"` | Start with all, remove worst (default) | Most stable, recommended |
| `"forward"` | Start empty, add best | When many irrelevant variables |
| `"stepwise"` | Bidirectional | Balanced approach |
| `"hybrid"` | Ensemble of methods | Maximum robustness |

```         

## Formulas and Methods

### Mallow's Cp
```

Cp = (SSE_p / σ²) - n + 2p

```         
Where:
- `SSE_p` = Sum of squared errors for p-variable model
- `σ²` = MSE from full model
- `n` = sample size
- `p` = number of parameters

### Spearman's Rank Correlation
```

ρ = 1 - (6∑d²) / (n(n²-1))

```         
Where `d` = difference in ranks

### VIF (Variance Inflation Factor)
```

VIF_j = 1 / (1 - R²_j)

```         
Where `R²_j` = R² from regressing variable j on all other variables

### Condition Number
```

κ = √(λ_max / λ_min)

```         
Where `λ` = eigenvalues of correlation matrix

### FDR Correction (Benjamini-Hochberg)
```

adjusted_p = p.adjust(p_values, method = "BH")

```         

## Default Parameters

```r
Parameter                Default
------------------------------------
selection(method)        "backward"
selection(metrics)       c("spearman", "rmse", "aic", 
                          "mallows_cp", "r2_adj")
selection(weights)       Equal weights
selection(cv_folds)      10
selection(top)           "auto" (elbow method)
significance(alpha)      0.05
significance(correction) "fdr"
refine(vif_threshold)    10
refine(condition_threshold) 30
validate(missing_threshold) 0.3
validate(variance_threshold) 0.01
```

## Performance Tips

1.  **Use FDR correction** for multiple testing (default)
2.  **Start with backward elimination** (most stable)
3.  **Use Spearman correlation** (robust to outliers)
4.  **Set VIF threshold to 10** (standard recommendation)
5.  **Use stratified CV** with 10 folds for reliable estimates
6.  **Let auto-selection find optimal number** of variables
7.  **Weight metrics** based on your domain priorities
