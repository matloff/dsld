# DSLD maintenance record — October 2026

This records the initial cleanup pass. The subsequent
[CRAN readiness audit](cran-readiness-2026-10.md) adds further fixes, experiments,
real Jupyter-kernel checks, and additional R/platform validation.
See the [release review](release-2.0.0.md) for the current submission archive
and completed platform checks.

## Scope and compatibility

This maintenance pass starts from commit `7e9f875` (DSLD 1.0.0).
It preserves the package structure, datasets, authorship, exported R function
names, existing argument names, and documented result fields. Internal names
and comments are clarified where code is repaired. Changes to incorrect
numerical results and input validation are recorded explicitly below.

The work includes the R implementation, Python interfaces, help pages,
vignettes, README files, regression tests, and repeatable validation commands.
Version numbers describe an unpublished maintenance release; they do not imply
that a release has been submitted or published.

## Confirmed CRAN history

CRAN archived DSLD on November 27, 2025 because it required the archived
`fairness` package. DSLD's last check summary showed OK on all 13 platforms.
The upstream `fairness` check reported a namespace collision between
`e1071::element` and `ggplot2::element`. `fairness` returned as version 1.2.3 on
December 14, 2025; DSLD's archive status is separate.

- [DSLD archive notice](https://cran.r-project.org/package=dsld)
- [DSLD check snapshot](https://cran-archive.r-project.org/web/checks/2025/2025-11-27_check_results_dsld.html)
- [fairness check snapshot](https://cran-archive.r-project.org/web/checks/2025/2025-11-27_check_results_fairness.html)
- [Current fairness listing](https://cran.r-project.org/package=fairness)

The original R source references none of `fairness`'s exported functions.
The maintenance change removes its `Imports` and `NAMESPACE` declarations.
The separate, actively used `fairml` dependency is retained.

## Work log

1. Reviewed the original R and Python modules and package documentation.
2. Captured the original exported R function signatures for compatibility checks.
3. Removed the unused `fairness` dependency; declared optional solver packages.
4. Set up isolated R and Python test installations outside the checkout.
5. Repaired numerical and interface errors, adding reference-based regression checks.
6. Reworked documentation and executed the README, vignette, and notebook examples.
7. Built both distributions and validated the installed packages, including current
   optional solver compatibility. Details and results follow below.

## Implementation record

### Dependency failure and package hygiene

The archival trigger was dependency availability, not a failing DSLD check in
that final historical snapshot. Removing the unused `fairness` dependency
eliminates that dependency path. It does not automatically restore the package
to CRAN. The `fairml` dependency is a different package and remains necessary.

Optional solver declarations now match actual use: `dsldNclm()` needs `cccp`,
and `dsldZlm()`/`dsldZlrm()` need `CVXR`. Missing optional packages produce an
installation instruction. Development files, Python caches, and packaging
artifacts are excluded from the R source distribution. Version numbers are
1.0.1 for R and 0.0.4 for Python, both unpublished.

### Statistical models and matching

| Area | Problem found | Change and evidence |
| --- | --- | --- |
| Linear prediction | Manually reconstructed dummy names and design rows could misalign coefficients, especially with punctuation or different contrasts. | Use the fitted terms, factor levels, contrasts, and coefficient order. Tests compare predictions and standard errors with `lm` and explicit sandwich-covariance calculations. |
| Sensitive-group contrasts | Comparisons for four or more groups could have the wrong ordering; broad name matching could include unrelated predictors. | Construct pairwise differences of fitted design rows. Check labels and values against direct predictions, including a similarly named proxy column. |
| Logistic comparisons | Group-comparison p-values used an inconsistent t reference distribution. | Use the Wald normal reference for logit coefficient contrasts. Check predictions and contrasts against `glm`. |
| Interactions | Shared comparison logic was duplicated and difficult to audit. | Keep separate fits by group and existing output fields, while sharing the calculation of differences and independent-group standard errors. |
| ML comparison points | Comparing an entire data frame to `"rand5"` produced an invalid conditional. | Use `identical()` for the sentinel and accept supplied comparison frames. Random comparison points use at most the available row count. |
| Matching | A recoded binary response was calculated but the original response was passed to `Matching::Match`, ignoring the requested positive outcome. | Pass the recoded response, support explicit numeric binary coding, and validate treatment labels. Tests verify sign reversal when the outcome label is reversed, and exercise covariate, logistic-propensity, and KNN-propensity matching. |

The mathematical reference tests cover ordinary and sandwich covariance,
non-default contrasts, dotted predictor names, multiple sensitive groups, and
both interaction settings. Rank-deficient models now produce a clear error when
full-rank coefficient contrasts or standard errors are required.

### Fair prediction and cross-validation

| Area | Problem found | Change and evidence |
| --- | --- | --- |
| Random-forest classification | The two-column probability matrix was flattened before scoring, mixing positive and negative probabilities. A clearly predictive fixture could report approximately 50% error. | Select the column representing the encoded positive class. The regression check requires low error on a strongly predictive held-out fixture. |
| Positive-class handling | Binary model errors and correlations used inconsistent coding; the logistic EDF variant confused positive label `"0"` with regression. | Separate regression/classification state from the label, use positive-class probabilities consistently, and restore original labels in class predictions. Test KNN, RF, and logistic EDF with reversed labels and the `"0"` case. |
| KNN preprocessing | Scaling and distance weights were not applied consistently between fitting and prediction; the backend's weight handling failed for one predictor. | Store training scaling/encoding metadata and apply distance weights once on both sides. Compare against explicitly transformed qeKNN data, including `scaleX = FALSE`. |
| Linear ridge | Penalty rows also affected the intercept through the implicit intercept column. A scalar diagonal penalty also failed in the one-predictor case. | Give real observations an intercept of one and penalty rows an intercept of zero; construct explicit penalty dimensions. Verify the result against penalized normal equations with an unpenalized intercept. |
| Logistic EDF | Its augmentation is a pseudo-observation construction, rather than conventional logistic ridge. | Preserve that original method, clarify the documentation, and repair label/scaling handling. Do not present it as a newly implemented standard logistic-ridge estimator. |
| Column and factor handling | Single-column selections could drop dimensions; prediction columns could arrive in a different order. | Preserve data-frame dimensions, store predictor order and encoded names, and restore training factor levels. Validate missing predictors, non-finite values, and invalid deweights. |
| Correlation alignment | Dummy encoding could omit rows with missing S values; undefined correlations were treated as zero. | Build factor indicators without dropping rows, use finite aligned pairs, and return `NA` for constant groups or predictions. |
| CV aggregation | Duplicate model-specific loops made scoring and aggregation inconsistent. | Use common candidate folds, binary stratification, sample-weighted error, feature-aligned correlation columns, and finite-fold means. Validate grids and prevent `model_args` from overriding data or grid parameters. |
| Feature-set exploration | Different feature sets used different random holdouts, confounding their comparison. | Use one shared holdout for every fit; retain `holdout = NULL` for in-sample errors. Verify results against a reference fit on the exact sampled rows. |
| fairml wrappers | Blanket warning suppression hid numerical diagnostics; strict backend arguments rejected integer-valued Python defaults. | Preserve warnings, keep data frames and predictor order, and pass continuous tuning parameters as numeric values. Exercise all five wrappers with their optional solvers. |

The legacy field names `trainAcc`, `testAcc`, and Python `train_accuracy` remain
for compatibility. They report errors, so lower is better. Classifications use
a probability threshold strictly greater than 0.5. At an exact tie, the negative
class is selected consistently.

### Tables, graphics, and structure discovery

- Frequency-table labels now use the actual contingency-table columns, rather
  than category order of first appearance. This correction changes the
  interpretation of previously mislabeled tables.
- Density plots accept factor or character groups, omit unusable observations,
  require enough values per group, and calculate limits across all groups.
  Every Plotly bandwidth step selects exactly the traces belonging to that
  bandwidth; the slider's initial setting matches the displayed curves.
- Conditional-disparity plots validate a genuinely binary factor outcome,
  report when no groups meet `minS`, and include every retained group's range.
  Probability-matrix backends select the first outcome factor level explicitly.
- R scatterplots handle character groups, validate sorting and bounds, filter
  ranges correctly, and clamp `maxPoints` without adding missing rows.
- Parallel coordinates drop unused levels, preserve data-frame dimensions,
  and omit constant predictor axes with a warning before backend scaling.
- Confounder plots request interactive input only in an interactive R session.
- IAMB converts integer columns to continuous numeric values and character
  columns to factors because bnlearn does not accept the original integer type.
- Python static plots close devices and remove temporary files in `finally`
  blocks, including on exceptions. Interactive density figures are converted
  into usable Python Plotly figures after removing R-only trace metadata.

Static density and conditional-disparity output was also rendered and visually
inspected for labels, curves, group legends, and axis coverage. Interactive
figures were checked through their serialized structures and slider controls;
this was not a manual browser interaction test.

### Python interfaces

The wrapper modules and public function names remain in their existing
locations. Shared helpers now handle pandas/R data frames, typed scalar/list
arguments, nested prediction results, and graphics consistently.

- Bad data types raise `TypeError`, rather than returning a sentinel and failing
  later inside an unrelated R call.
- Preprocessing converts numeric factor labels to their values, not internal
  factor codes. R functions are looked up directly when they are internal.
- Summary, coefficient, covariance, data-access, and prediction helpers pass
  model objects directly. Tests verify that these calls do not create temporary
  bindings in R's global workspace.
- Fairness options preserve booleans, strings, integers, numeric vectors, and
  R functions instead of coercing every option to a floating-point value.
- Existing dictionaries and nested classifier predictions remain available.
  Explicit class/probability fields, matching result objects, and `inCommon`
  rankings are additive. Regression predictions remain ordinary Python lists.
- `dsldPyFairUtils` adds one optional final argument, `model_args`; existing
  positional calls remain valid. Historical public spellings such as `lamda`
  are retained despite clearer internal naming.
- Optional notebook dependencies are an `examples` extra. Packaging was tested
  as a source distribution and a wheel, and integration tests were repeated
  against the installed wheel without `PYTHONPATH` pointing at source.

### Documentation and examples

Both README files retain the project's purpose, authors, book/paper links, and
major analysis categories, while adding current installation instructions,
complete examples, return-value explanations, and troubleshooting.

The Quick Start retains the law-school, recidivism, and wage-data examples. Its
results now come from executed R chunks. Removed stale pasted outputs included
mislabeled frequency tables, missing positive-label arguments, and a Python
import path that no longer existed. Computationally expensive help examples use
reproducible subsets while demonstrating the same methods.

All three notebook topics remain: tabular analysis, machine learning, and
graphics. Their examples load bundled data, seed randomness, use valid category
labels, and keep fitting and held-out evaluation separate. Outputs are cleared
so saved notebooks do not misrepresent results from an older implementation.
The notebook checker executes all 27 code cells and validates their schema and
Python syntax.

`NEWS.md` describes user-visible changes. `docs/development.md` records the
repository layout, exact validation commands, dependency requirements, and
release preparation. The weekly CI workflow is intended to catch dependency
changes of the kind that caused the original archival.

## Compatibility boundaries

- Automated baseline checks preserve all 25 exported R signatures/defaults and
  42 Python callable signatures/defaults, with only the optional Python CV
  argument addition described above.
- Datasets and their existing documentation/attribution, model families, and
  package ownership are retained. No trained model or dataset is replaced.
- Corrected numerical outputs are intentional behavior changes: RF errors,
  ridge fits, group contrasts, matching signs, CV aggregates, category labels,
  and undefined correlations may differ from 1.0.0.
- Invalid inputs fail earlier. Training/prediction need compatible factor
  levels, suitable groups, and complete finite values for the fair-ML wrappers.
- New fits store preprocessing metadata. Compatibility with every serialized
  model produced by older package/dependency versions is not guaranteed; refit
  models when upgrading for the corrected numerical behavior.
- Preserving the original API does not imply that every backend accepts every
  statistical problem. Singular designs, separation, tiny groups, and solver
  convergence issues still need attention; warnings are now visible.

## Validation environment and evidence

Validation ran locally on Apple Silicon/macOS with R 4.5.1 and Python 3.14.0.
Separate temporary libraries and a virtual environment were used; the user's
global R/Python installations were not upgraded. Exact tested versions are
recorded in [the R package manifest](validation-2026-10/r-package-versions.tsv)
and [the Python package manifest](validation-2026-10/python-package-versions.json).

The current-dependency pass used fairml 0.9.1, qeML 1.1, regtools 1.7.0, and
CVXR 1.9.3. The regression suite also passed with the older installed CVXR
1.0-15. All optional methods exercised by the test suites were available; the
reported successful runs did not skip missing solvers or matching packages.

Earlier passes identified and resolved failures in one-predictor KNN/ridge,
Python numeric argument conversion, R function arguments, Plotly trace metadata,
notebook category labels, bnlearn integer inputs, and single-row probability
shape/correlation handling. The retained logs are from the final successful
runs, rather than those intermediate failures.

| Check | Result | Evidence |
| --- | --- | --- |
| R regression suite | 40 checks passed, zero failures | [R regression log](validation-2026-10/r-regression.log) |
| Python integration suite, installed wheel | 21 tests passed, including all five fairml wrappers | [Python integration log](validation-2026-10/python-integration.log) |
| Public R API | All 25 exported signatures/defaults preserved | [R API log](validation-2026-10/r-api.log) |
| Public Python API | All 42 existing callable signatures/defaults preserved; optional final CV argument added | [Python API log](validation-2026-10/python-api.log) |
| README examples | Five R analysis blocks and two Python blocks executed successfully | [README execution log](validation-2026-10/readme-examples.log) |
| Notebooks | All three notebooks, 27 code cells, executed successfully | [Notebook execution log](validation-2026-10/notebook-examples.log) |
| R build | Source archive built; both vignettes executed | [Build log](validation-2026-10/r-build.log) |
| Full `R CMD check --as-cran` | Zero errors, zero warnings, one incoming/archive-status note; examples including `donttest`, tests, vignettes, PDF and HTML manuals passed | [Complete check log](validation-2026-10/R-CMD-check.log) |
| Python build | Source distribution and wheel built; wheel contents matched source | [Archive SHA-256 manifest](validation-2026-10/archive-sha256.json) |
| Static checks | Ruff undefined/unused-name checks and formatting passed; Python 3.9 grammar parsed | Validation tools in `tools/` |
| Documentation checks | Local link targets, code fences, notebook schemas, syntax, and cleared outputs passed | `tools/check_docs.py` |
| Packaging hygiene | R archive matched current R/help/test sources and excluded developer caches; datasets unchanged | Archive inspection and Git diff review |
| CI configuration | YAML parsed with four R matrix jobs and one Python job | Remote jobs have not been run in this session |

The single CRAN-style check note states “New submission” and “Package was
archived on CRAN,” reproducing the original `fairness` archival comment. It is
not a current code-check failure. An earlier run's warning that a downloaded
fairml binary was built under R 4.5.2 was resolved by rebuilding fairml 0.9.1
from source under R 4.5.1 in the temporary library. The exact final archive
completed without that warning. Reducing the expensive help examples to seeded
subsets reduced the extended-example pass from about 71 seconds to 12 seconds
in this local environment.

The Python 3.9 grammar check is not a Python 3.9 runtime test. Windows, Linux,
R development builds, and remote CI have not been executed locally. The notebook
runner executes code cells without a Jupyter kernel; interactive front-end
behavior is not fully covered by these tests.

Expected backend diagnostics remain visible, including warnings about nearly
separated logistic fits, constant features, and unequal random-forest split
weights. The tests check numerical behavior despite these diagnostics; they do
not promise convergence for arbitrary data or establish statistical validity
for a particular application.

Build products and complete temporary working logs were kept under
`/private/tmp/dsld-cleanup-20261023`; the compact evidence linked above is retained
in the repository. At the end of this initial pass, no commit, push, CRAN/PyPI
submission, or external message had been performed.
