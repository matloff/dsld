# DSLD: Data Science Looks at Discrimination

[![CRAN status](https://www.r-pkg.org/badges/version/dsld)](https://cran.r-project.org/package=dsld)

Statistical and graphical tools for exploring possible discrimination and bias
in data, with an R package and Python interfaces.

## Overview

Discrimination is an important social issue, and many datasets can help us
investigate it. DSLD brings together statistical models, visualizations, and
fair machine learning methods to make these investigations more accessible.

The package is intended for teaching and research in statistics and the social
sciences, organizational analysis, and other applications involving differences
between groups. Its companion book explains the statistical principles behind
the methods and works through practical examples.

- [Companion book](https://htmlpreview.github.io/?https://github.com/matloff/dsldBook/blob/main/_book/index.html)
- [Research paper](https://arxiv.org/abs/2411.04228)
- [Python interface and examples](inst/README.md)
- [Development and validation guide](docs/development.md)
- [Maintenance record](docs/maintenance-2026-10.md) and [release notes](NEWS.md)
- [CRAN readiness audit](docs/cran-readiness-2026-10.md)
- [2.0.0 release candidate and submission guide](docs/release-2.0.0.md)

## Installation

**CRAN status:** DSLD was archived on November 27, 2025 because its required
`fairness` dependency had been archived. This 2.0.0 candidate removes that
unused dependency. It has not yet been submitted to CRAN; `install.packages("dsld")`
is therefore not the installation route for this version.
[CRAN's archive notice](https://cran.r-project.org/package=dsld) preserves the
original releases.

From an R session with this repository as the working directory:

```r
install.packages("remotes")  # Run once if needed.
remotes::install_local(".", dependencies = TRUE, upgrade = "never",
                       build_vignettes = TRUE)
```

If the dependencies are already installed, run `R CMD build .` followed by
`R CMD INSTALL dsld_2.0.0.tar.gz` from the repository root. Building first also
prepares the vignettes. Optional methods use `bnlearn`, `Matching`, `randomForest`,
`cccp`, or `CVXR`; their functions explain which package is needed if it is absent.

After installation:

```r
library(dsld)
vignette("Quick_Start", package = "dsld")
vignette("Function_List", package = "dsld")
```

See the [Python installation guide](inst/README.md) to use the same R
implementations from Python.

## Two kinds of analysis

DSLD distinguishes between estimating group differences and building predictive
models that limit the influence of sensitive attributes.

| | Statistical analysis | Fair machine learning |
| --- | --- | --- |
| Main question | How does an outcome differ between groups after adjustment? | How can predictions limit the influence of sensitive attributes and their proxies? |
| Sensitive attributes | Included to estimate group differences | Excluded or constrained, depending on the method |
| Other variables | Adjust for selected confounders | Control the influence of possible proxies |
| Main outputs | Estimates, standard errors, and group comparisons | Predictions, prediction error, and group correlations |

We use **Y** for the outcome, **S** for a sensitive attribute, **C** for
confounders, and **X** for other predictors. A statistical association after
adjustment depends on the variables and model chosen; it does not by itself
establish a causal explanation.

## Part one: adjustment for confounders

The bundled `svcensus` dataset records age, education, occupation, wage income,
weeks worked, and gender for workers in engineering occupations. We start with
age, wage income, and gender to keep the example focused.

```r
data(svcensus, package = "dsld")
wages <- svcensus[, c("age", "wageinc", "gender")]
head(wages)

wage_model <- dsldLinear(wages, "wageinc", "gender", interactions = FALSE)
summary(wage_model)
```

Without interactions, the model estimates a common age slope and a constant
gender difference in expected wage income. `summary()` reports the coefficients,
the adjusted group comparison, and its standard error. `useSandwich = TRUE`
requests a sandwich covariance estimate.

Allowing interactions fits a separate model for each group. We can then compare
predicted wages at specific ages:

```r
comparison_points <- data.frame(age = c(36, 43))
interaction_model <- dsldLinear(
  wages, "wageinc", "gender",
  interactions = TRUE, sComparisonPts = comparison_points
)
summary(interaction_model)
predict(interaction_model, comparison_points)
```

`dsldLogit()` provides analogous comparisons for a binary outcome; specify its
positive label with `yesYVal`. `dsldML()` uses a selected qeML method for
nonparametric group comparisons. Both accept user-supplied comparison points.

## Part two: fairness in prediction

Suppose we want to predict wage income while reducing the influence of gender
and occupation as a possible proxy. The EDF methods remove sensitive columns
from the predictors and deweight selected remaining features.

Use a reproducible train/test split and evaluate both prediction error and
association with the sensitive attribute:

```r
set.seed(2026)
model_data <- svcensus[, c("age", "occ", "wageinc", "gender")]
model_data <- model_data[sample.int(nrow(model_data), 1200), ]
train_rows <- sample.int(nrow(model_data), 900)
train_data <- model_data[train_rows, ]
test_data <- model_data[-train_rows, ]
test_x <- test_data[, c("age", "occ", "gender")]

baseline <- dsldQeFairKNN(train_data, "wageinc", "gender", k = 25)
fair_model <- dsldQeFairKNN(
  train_data, "wageinc", "gender", deweightPars = list(occ = 0.2), k = 25
)
baseline_predictions <- predict(baseline, test_x)
fair_predictions <- predict(fair_model, test_x)

comparison <- data.frame(
  model = c("KNN", "Deweighted KNN"),
  test_MAE = c(
    mean(abs(baseline_predictions$preds - test_data$wageinc)),
    mean(abs(fair_predictions$preds - test_data$wageinc))
  )
)
comparison
fair_predictions$correlations
```

For KNN and random forests, smaller deweights reduce a feature's influence.
For the ridge variants, larger penalties increase shrinkage. Evaluate the
resulting tradeoff: deweighting does not guarantee that every error or fairness
measure improves.

Search a small parameter grid using cross-validation:

```r
set.seed(2026)
tradeoff <- dsldFairUtils(
  train_data, "wageinc", "gender", "dsldQeFairKNN",
  deweightPars = list(occ = c(1, 0.5, 0.2)),
  k_folds = 3, model_args = list(k = 25)
)
tradeoff
```

### Reading the results

- `trainAcc` and `testAcc` retain their original names but are **errors**:
  mean absolute error (MAE) for regression and misclassification rate for binary
  classification. Lower is better. MAE is not a percentage error.
- `trainCorrs` and prediction `correlations` contain signed correlations between
  predictions and numeric sensitive attributes or factor-level indicators.
  For binary classification, they use the probability of `yesYVal` consistently.
  Compare absolute magnitudes when measuring association; zero correlation is
  not a general guarantee of fairness or independence.
- Cross-validation reuses folds across candidates and stratifies binary outcomes.
  Prediction errors are weighted by held-out sample size. Correlations are
  averaged over folds where they are defined; constant groups or predictions
  yield `NA`, rather than a misleading zero.
- Model fitting requires suitable observed groups and complete, finite data for
  the fair-ML and matching wrappers. Prediction preprocessing follows the
  training column order and factor levels. Unseen factor levels need to be
  resolved before prediction.

## Function guide

| Area | Functions | Purpose |
| --- | --- | --- |
| Group comparisons | `dsldLinear`, `dsldLogit`, `dsldML` | Estimate adjusted differences between sensitive groups |
| Confounders and proxies | `dsldCHunting`, `dsldOHunting`, `dsldTakeALookAround` | Explore variable importance, associations, and predictive feature sets |
| Matching and discovery | `dsldMatchedATE`, `dsldIamb` | Matching-based treatment effects and conditional-independence structure learning |
| Group distributions | `dsldFrequencyByS`, `dsldDensityByS`, `dsldConfounders` | Frequency tables and density comparisons |
| Multivariate graphics | `dsldFreqPCoord`, `dsldScatterPlot3D`, `dsldConditDisparity` | Parallel coordinates, 3D plots, and conditional disparity plots |
| fairml wrappers | `dsldFrrm`, `dsldFgrrm`, `dsldNclm`, `dsldZlm`, `dsldZlrm` | Models that constrain sensitive-attribute influence |
| EDF wrappers | `dsldQeFairKNN`, `dsldQeFairRF`, `dsldQeFairRidgeLin`, `dsldQeFairRidgeLog` | Explicit feature deweighting and penalization |
| Model comparison | `dsldFairUtils` | Cross-validated fairness/utility parameter grids |

The help pages describe individual arguments and return values. For example,
run `?dsldLinear`, `?dsldQeFairKNN`, or `?dsldFairUtils` in R.

## Authors and support

Norm Matloff, Aditya Mittal, Taha Abdullah, Arjun Ashok, Shubhada Martha,
Billy Ouattara, Jonathan Tran, and Brandon Zarate.

Report reproducible problems through [GitHub issues](https://github.com/matloff/dsld/issues),
or contact Aditya Mittal at mittalaa@uci.edu. Include the function call,
`sessionInfo()`, and a small example that demonstrates the problem.
