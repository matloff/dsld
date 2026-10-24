# dsldPy: the Python interface to DSLD

`dsldPy` makes DSLD's statistical models, fairness methods, and visualizations
available from Python through rpy2. The models use the R implementation, so
R and the R package `dsld` must be installed first.

- [Project overview and R examples](https://github.com/matloff/dsld#readme)
- [Companion book](https://matloff.github.io/dsldBook/)
- [Research paper](https://arxiv.org/abs/2411.04228)

## Installation

This repository contains Python interface version 2.0.0, paired with R package
version 2.0.0. Install both from the same checkout.

1. Install R and confirm that `R RHOME` works in your terminal.
2. Install the R package using the [R installation instructions](https://github.com/matloff/dsld#installation).
3. From the repository root, install the Python interface into your environment:

```sh
python -m pip install ./inst
```

Python 3.9 or newer is required. For the example notebooks and their execution
tools, use `python -m pip install './inst[examples]'`.

`pip install dsldPy` installs the version available on PyPI, which may differ
from this checkout. It does not install R or the R package. DSLD is currently
archived on CRAN; installing the local R source is required for this maintenance
version.

## Data and a first model

Functions accept either pandas data frames or R data frames. pandas categorical
columns preserve their levels when converted to R. `preprocess_data()` is a
convenient way to specify categorical and numeric columns explicitly; it
returns an R data frame.

The following example uses data bundled with the R package, so no file paths
need to be edited:

```python
import rpy2.robjects as ro
from rpy2.robjects.packages import importr
from dsldPy import (
    dsld_Rpy2_RDataframeToPandas,
    preprocess_data,
    dsldPyLinear,
    dsldPyLinearSummary,
    dsldPyLinearPredict,
)

bundled = ro.Environment()
importr("utils").data("svcensus", package="dsld", envir=bundled)
wages = dsld_Rpy2_RDataframeToPandas(bundled["svcensus"])
wages = wages[["age", "wageinc", "gender"]]
r_wages = preprocess_data(wages, cat_features=["gender"],
                          num_features=["age", "wageinc"])

model = dsldPyLinear(r_wages, "wageinc", "gender")
dsldPyLinearSummary(model)
predictions = dsldPyLinearPredict(model, wages[["age", "gender"]].head())
print(predictions)
```

For separate group fits, pass `interactions=True`; prediction points then
contain the nonsensitive predictors. `dsldPyLogit()` supports binary outcomes
with `yesYVal` identifying the positive label.

## Fair prediction with held-out data

Use held-out rows for evaluating model quality. This example keeps the positive
class labels and the sensitive attribute available for evaluation:

```python
from dsldPy import dsldPyQeFairRF, dsldPyQeFairML_Predict, dsldPyFairUtils

importr("utils").data("compas1", package="dsld", envir=bundled)
records = dsld_Rpy2_RDataframeToPandas(bundled["compas1"])
records = records[["age", "decile_score", "priors_count", "race", "two_year_recid"]]
records = records.sample(n=800, random_state=2026)
train = records.sample(frac=0.75, random_state=2026)
test = records.drop(train.index)

ro.r["set.seed"](2026)  # Seed R as well as the pandas split.
fair_model = dsldPyQeFairRF(
    train, "two_year_recid", "race",
    deweightPars={"decile_score": 0.3}, nTree=100, yesYVal="Yes",
)
result = dsldPyQeFairML_Predict(fair_model, test.drop(columns="two_year_recid"))
error = (test["two_year_recid"].astype(str).to_numpy() != result["test_classes"]).mean()
print("Test misclassification rate:", error)
print("Group correlations:", result["test_correlations"])

ro.r["set.seed"](2026)
tradeoff = dsldPyFairUtils(
    train, "two_year_recid", "race", "dsldQeFairRF",
    deweightPars={"decile_score": [1.0, 0.3]}, yesYVal="Yes", k_folds=3,
    model_args={"nTree": 50},
)
print(tradeoff)
```

### Return values and metric names

- Linear/logistic fitting functions return an R model object. Their summary,
  coefficient, covariance, data-access, and prediction helpers operate directly
  on that object; prediction tables are converted to pandas.
- Fair-ML fitting functions return a dictionary with `model`,
  `train_predictions`, `train_accuracy`, and `train_correlations`.
- Fair-ML prediction helpers return `test_predictions` and `test_correlations`.
  EDF classifiers also expose `train_classes`/`test_classes` and
  `train_probabilities`/`test_probabilities`. Existing nested prediction lists
  remain available for compatibility.
- The names `train_accuracy` and `testAcc` are retained for compatibility:
  their values are **MAE or misclassification rate**, so lower is better.
  Correlations are signed; absolute magnitude describes association strength.
- Random-forest probabilities have two columns, coded `0` and `1` by the R
  backend; column `1` represents `yesYVal`. KNN/logistic binary probabilities
  describe `yesYVal` directly. Class-label fields use the original labels.
- Matching returns the R result object as well as displaying its summary.
  Confounder hunting includes both variable-importance rankings and `inCommon`.

## Graphics

`dsldPyDensitybyS(..., graphType="plotly")` returns and displays a Python Plotly
figure, including its bandwidth slider. Use `graphType="plot"` for a static
image. Static R plots close their graphics devices and remove temporary images
after displaying them. `dsldPyFreqPCoord(..., show=False)` renders without
showing the image; `plot_filename` saves an explicit output file.

`dsldPyScatterPlot3D()` uses Python Plotly directly and returns its figure.
`dsldPyConfounders()` displays each relevant table or density plot and returns
the results by column name. `dsldPyIamb()` saves its network plot to the
requested filename.

## Examples and function guide

The notebooks load the bundled datasets, set random seeds, and run without
machine-specific file paths:

- [Tabular analysis](https://github.com/matloff/dsld/blob/master/inst/examples/tabular.ipynb): group comparisons, feature sets,
  confounder/proxy exploration, frequency tables, and matching.
- [Machine learning](https://github.com/matloff/dsld/blob/master/inst/examples/machine_learning.ipynb): regression,
  classification, held-out evaluation, and parameter grids.
- [Graphics](https://github.com/matloff/dsld/blob/master/inst/examples/graphical.ipynb): scatterplots, parallel coordinates,
  conditional disparity, network discovery, and group densities.

| Area | Functions |
| --- | --- |
| Group comparisons | `dsldPyLinear`, `dsldPyLogit`, `dsldPyML` |
| Confounders and proxies | `dsldPyCHunting`, `dsldPyOHunting`, `dsldPyTakeALookAround` |
| Matching and discovery | `dsldPyMatchedATE`, `dsldPyIamb` |
| Tables and graphics | `dsldPyFrequencybyS`, `dsldPyDensitybyS`, `dsldPyConfounders`, `dsldPyFreqPCoord`, `dsldPyScatterPlot3D`, `dsldPyConditDisparity` |
| fairml wrappers | `dsldPyFrrm`, `dsldPyFgrrm`, `dsldPyNclm`, `dsldPyZlm`, `dsldPyZlrm` |
| EDF wrappers | `dsldPyQeFairKNN`, `dsldPyQeFairRF`, `dsldPyQeFairRidgeLin`, `dsldPyQeFairRidgeLog` |
| Parameter grids | `dsldPyFairUtils` |

## Troubleshooting

- **R cannot be found:** check `R RHOME` and the R installation used by rpy2.
  If necessary, set `R_HOME` to that reported directory before starting Python.
- **DSLD cannot be loaded:** install the R package into a library visible to
  the R interpreter used by rpy2. `R_LIBS` can select a separate R library.
- **Missing optional solver:** install the named R package (`cccp` or `CVXR`)
  before using the corresponding fairml wrapper.
- **Unexpected categories or missing values:** check pandas dtypes, positive
  class labels, and training factor levels. Model fitting requires complete,
  finite inputs for the fair-ML and matching wrappers.
- **Unstable logistic fit:** inspect the R warning. Small samples, separation,
  or redundant predictors may make a fit unreliable; warnings are preserved.

For issues, contact Aditya Mittal at mittalaa@uci.edu or use
[GitHub issues](https://github.com/matloff/dsld/issues). Include Python and R
versions and a small reproducible example.

## Authors

Norm Matloff, Aditya Mittal, Taha Abdullah, Arjun Ashok, Shubhada Martha,
Billy Ouattara, Jonathan Tran, and Brandon Zarate.
