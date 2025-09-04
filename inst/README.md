# dsldPy — Python Interface to DSLD

Statistical and graphical tools for detecting and measuring discrimination and bias in datasets, 
Python interfaces available via rpy2. **dsldPy** wraps the R package **dsld** with a Python-friendly API 
using the same underlying R implementations.

**Relevant links:** 

- **Quarto Book**: [Paper](https://htmlpreview.github.io/?https://github.com/matloff/dsldBook/blob/main/_book/index.html) - Important statistical principles and applications.
- **Research Paper**: [Paper](https://arxiv.org/abs/2411.04228) - Package implementation details.

## Overview

DSLD addresses two main types of bias analysis:

- **Estimation analysis:** quantify possible discrimination by estimating effects of a sensitive variable S on an outcome Y, while adjusting for confounders C.

- **Prediction analysis (fair ML):** build predictive models that limit the influence of S and its proxies O, trading off fairness and utility.

**dsldPy** provides wrappers for all 24 R functions.

## Prerequisites

- R installed and on PATH (R 4.x recommended)
- R package dsld installed (CRAN or GitHub)
- Python 3.8+

Install dsld in R:

```r
install.packages("dsld")

## or latest development version
# install.packages("remotes")
remotes::install_github("matloff/dsld", force = TRUE)
```

Tip: Ensure rpy2 can find R. From a terminal: `R RHOME` should print your R home. If Python cannot find R, set `R_HOME` in your environment per rpy2’s documentation.

## Installation

Install the Python package from this repository (subdirectory `inst`):

```bash
pip install dsldPy
```

This will install dsldPy and its Python dependencies (pandas, numpy, rpy2, etc.). The user still needs to manually download **R** and the **dsld** package, as noted above.

## Quickstart

Please refer to the instructional jupyter notebooks provided under `examples/` folder. These illustrate examples of all 24 **dsldPy** functions.

Jupyter notebooks are available in this repository:

- `inst/examples/graphical.ipynb`
- `inst/examples/tabular.ipynb`
- `inst/examples/machine_learning.ipynb`

## Available Wrappers

- Analytical: `dsldPyLinear`, `dsldPyLogit`, `dsldPyML`, `dsldPyMatchedATE`, `dsldPyTakeALookAround`, `dsldPyConfounders`, `dsldPyCHunting`, `dsldPyOHunting`

- Fair ML: `dsldPyFrrm`, `dsldPyFgrrm`, `dsldPyNclm`, `dsldPyZlm`, `dsldPyZlrm`, `dsldPyQeFairKNN`, `dsldPyQeFairRF`, `dsldPyQeFairRidgeLin`, `dsldPyQeFairRidgeLog`, , `dsldPyFairUtils`

- Graphical: `dsldPyFreqPCoord`, `dsldPyScatterPlot3D`, `dsldPyConditDisparity`, `dsldPyDensitybyS`, `dsldPyFrequencybyS`, `dsldPyIamb`

Function names mirror the R package. Arguments use standard Python types (pandas.DataFrame, dict, bool, etc.) with the same call forms as the R functions.

## Troubleshooting

- rpy2 cannot find R: confirm `R RHOME` works; if not, add R to PATH or set `R_HOME`. See rpy2 docs for your OS.
- dsld not installed in R: run `install.packages("dsld")` in an R session.

## Authors

- Norm Matloff
- Aditya Mittal
- Taha Abdullah
- Arjun Ashok
- Shubhada Martha
- Billy Ouattara
- Jonathan Tran
- Brandon Zarate

For issues, contact **Aditya Mittal** at mittalaa@uci.edu