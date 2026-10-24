# DSLD release notes

## 2.0.0 — unpublished release candidate

This release preserves the public R interfaces, corrects numerical and
prediction errors, and improves documentation and validation. Both the R
package and Python interface use version 2.0.0.

### Dependencies and packaging

- Remove the unused `fairness` import that caused CRAN to archive DSLD when
  that dependency was archived in November 2025. The active `fairml` dependency
  remains. This change has not been submitted to CRAN.
- Declare the optional `cccp` and `CVXR` solvers and explain how to install
  missing optional packages.
- Add regression checks, documentation/example validation, and scheduled CI.
  Exclude developer artifacts and Python caches from the R source package.
- Update the maintainer address to `mittalaa@uci.edu`, add the methods-paper
  citation, and document the bundled datasets' dimensions and data usage.

### Correctness

- Use the positive-class probability column when scoring binary random forests.
  Cross-validation no longer flattens both probability columns together.
- Preserve `yesYVal` in binary matching and EDF predictions, including a positive
  label of `"0"`; report original class labels in EDF classification results.
- Fix frequency-table column labels and preserve row alignment when sensitive
  attributes contain missing values. Undefined correlations remain `NA`.
- Use fitted model matrices for linear prediction standard errors and sensitive
  group contrasts. Correct contrast ordering for four or more groups and avoid
  confusing similarly named covariates with the sensitive attribute.
- Apply KNN scaling and deweights consistently at training and prediction; honor
  `scaleX = FALSE`, column reordering, and one-predictor data frames.
- Correct linear ridge augmentation so its intercept is unpenalized. Retain the
  original pseudo-observation method for the logistic EDF variant.
- Accept omitted deweights in both ridge variants, and preserve ordered-factor
  and custom-contrast encodings when predicting with fairml models.
- Construct linear, logistic, and linear-ridge formulas from literal column
  names, allowing outcome names with spaces, operators, or backticks without
  interpreting them as expressions or emitting formula-parsing warnings.
- Preserve explicit sensitive-factor contrasts in linear/logistic fits when
  all levels are observed, including their coefficient labels and values.
- Reuse cross-validation folds across candidates, stratify binary outcomes,
  weight errors by test-fold size, and retain undefined group correlations.
- Accept data-frame comparison points in `dsldML`; compare feature sets using a
  shared holdout in `dsldTakeALookAround`.
- Fix density slider indexing, character group handling, plot ranges, scatter
  row limits, and binary-response validation in conditional-disparity plots.
  Drop unused factor levels and omit constant axes in parallel coordinates.
- Detect parallel-coordinate indicators that are constant within a plotted
  group before density scaling; omit them with an explicit warning. This avoids
  a failure found on Linux with current R and dependency binaries.
- Convert integer/character inputs for bnlearn and preserve optimizer warnings
  from fairml instead of suppressing them.

### Documentation and compatibility

- Preserve all 25 exported R function signatures and defaults, model families,
  datasets, and historical result-field names.
- Clarify that `trainAcc` and `testAcc` are prediction errors, and that sensitive
  correlations are associations rather than universal fairness measures.
- Refresh both READMEs, help pages, the function guide, and the Quick Start.
  Quick Start results now come from executable chunks.
- Numerical results affected by the fixes above may change. Invalid inputs now
  produce earlier, clearer errors.

## Python interface 2.0.0 — unpublished maintenance version

- Preserve 42 public callable signatures and defaults; add optional final
  `model_args` support to `dsldPyFairUtils`.
- Accept pandas and R data frames consistently, preserve option types, and call
  R generic methods directly without temporary variables in R's global workspace.
- Preserve existing result fields and add explicit EDF class/probability fields,
  matching results, and common-variable rankings.
- Return usable Plotly figures, honor static-plot display controls, and close
  graphics devices and delete temporary images even when rendering fails.
- Correct notebook imports, dataset preparation, class labels, and train/test
  handling. Rebuilt examples use bundled data and reproducible random seeds.
- Require Python 3.9 or newer and provide optional notebook/example dependencies.
