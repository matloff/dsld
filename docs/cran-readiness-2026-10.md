# CRAN readiness audit — October 2026

This records the second audit candidate. The later
[2.0.0 release review](release-2.0.0.md) adds another code review, formula and
contrast repairs, independent numerical checks, and the GitHub handoff. Its
artifact manifest supersedes this document's archive hash and final status.

This second audit extends the [maintenance record](maintenance-2026-10.md).
It evaluates the unpublished R 2.0.0 candidate and Python 2.0.0 interface.
The maintainer requested version 2.0.0 during the audit. Earlier 1.0.1/0.0.4
records are historical evidence, not final-version checks.
Checks began on October 23, 2026, Pacific time; retained logs can show October 24
in UTC. No package submission, repository push, or external message is part of
this audit.

## Status

The final 2.0.0 source archive passes all six CRAN-style check configurations
across macOS, Linux release, and Linux R-devel: zero errors, zero warnings, and
one expected incoming/archive note per run. The numerical suites, examples,
notebooks, and repeated experiments also pass. This is a tested submission
candidate; acceptance remains subject to CRAN review. Windows and hosted CI
were not run. The existing Python copyright attribution still needs maintainer
confirmation, as described in the handoff below.

## Why CRAN archived DSLD

The [CRAN package notice](https://cran.r-project.org/package=dsld) explicitly
records archival on November 27, 2025 because DSLD required the archived
`fairness` package. The historical check snapshots and upstream failure are
linked in the maintenance record. This is a dependency-availability failure;
it is not evidence that CRAN identified a statistical error in DSLD itself.

The unused `fairness` import has been removed. The distinct `fairml` package
remains in use. On this audit date, the current CRAN source index contained all
150 non-base/non-recommended packages in the checked strong dependency closure
and direct optional dependencies, with none marked orphaned. No current CRAN
package declared a strong reverse dependency, directly or recursively, or a
direct suggested dependency on DSLD. This query does not cover archived
packages, Bioconductor, private code, or undeclared use.

## Research and its application

Official requirements take precedence over community experiences. The sources
below were read during the audit, rather than inferred from a passing check.

| Source | Relevant point and audit response |
| --- | --- |
| [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html) | Review portability, dependency availability, runtime, session side effects, and maintainer contact. Tests use at most two computational threads; the maintainer address is now `mittalaa@uci.edu`. |
| [Submission checklist](https://cran.r-project.org/web/packages/submission_checklist.html) | Check on current R-devel and explain substantive notes. Improve the package description and add the methods paper's arXiv DOI. |
| [Writing R Extensions: suggested packages](https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Suggested-packages) | Check normal, depends-only, and suggests-only configurations; guard optional examples. A flag alone is not proof that optional packages were unavailable. |
| [CRAN Cookbook: code](https://contributor.r-project.org/cran-cookbook/code_issues.html), [documentation](https://contributor.r-project.org/cran-cookbook/docs_issues.html), and [DESCRIPTION](https://contributor.r-project.org/cran-cookbook/description_issues.html) | Review suppressible startup output, return-value documentation, data help pages, examples, and metadata. Add formal data documentation and shorten the startup message. |
| [LatinR 2024 conference abstract](https://github.com/LatinR/presentaciones-latinr2024/blob/main/contribuciones/40.pdf) and [Cookbook preface](https://contributor.r-project.org/cran-cookbook/preface.html) | Read the conference contribution and accompanying written guidance on recurring manual-review issues. The linked video was not viewed; this is a review of written session material. |
| [Reddit: first-package review experiences](https://www.reddit.com/r/rstats/comments/mrl8ww) | Authors describe documentation/platform review and requested revisions. These are anecdotes, not current deadlines or acceptance guarantees. |
| [Reddit: a package disappearing from CRAN](https://www.reddit.com/r/rstats/comments/k85a8n) | Dependency changes and maintenance matter even when existing installations work. Confirm DSLD's actual cause using CRAN's own notice and check present dependencies independently. |
| [Reddit: CRAN checks and scientific review](https://www.reddit.com/r/rstats/comments/16as1jq) | A passing package check does not validate an estimator's scientific claims. Use independent numerical references and describe the limits of the experiments. |
| [DSLD methods paper, version 3](https://arxiv.org/html/2411.04228v3) | Execute the two Section 4.4 parameter-grid forms on the complete bundled datasets, with an explicit seed and corrected error definitions. |

## Additional repairs

- **Default ridge calls:** `deweightPars = NULL` produced `sqrt(NULL)` while
  constructing penalty rows. The empty-weight assignment is now skipped. A
  reference test compares the default linear fit with `lm` excluding S, and
  verifies finite binary probabilities from the default logistic call.
- **Ordered factors and contrasts:** prediction rebuilt factors as unordered
  and lost custom contrasts. Fairml could then fail on its own training data
  because the encoded columns differed. Fitted models now retain ordered status
  and explicitly supplied contrasts. Regression tests compare training and
  prediction results for ordered factors and sum contrasts, including character
  input at prediction time.
- **Optional examples:** help examples conditionally use `Matching`, `bnlearn`,
  and `randomForest`. Solver tests already guard `cccp` and `CVXR`.
- **Parallel-coordinate portability:** Linux checks exposed a failure when
  dummy-coded predictors are constant within a sensitive group. The backend
  scales within each group and tests for exactly zero standard deviation;
  pre-scaling detection avoids relying on floating-point roundoff. Such axes
  are excluded from density estimation and plotting with an explicit warning,
  while their encoded data remain available in the backend's retained rows.
  The help page explains why an omitted deterministic group relationship still
  deserves separate inspection. The regression test checks the warning and
  exact remaining axes; a Linux smoke test also covers a one-axis plot.
- **Data documentation:** all four bundled datasets have `docType{data}`, a data
  usage declaration, and verified dimensions. Dataset contents are unchanged.
- **Metadata:** the same maintainer now uses the user-confirmed UCI email. The
  description includes the methods-paper citation. Startup output remains
  suppressible and points to installed vignettes.
- **Distribution alignment:** both distributions, version constants, tests,
  installation examples, and release notes use 2.0.0. The Python metadata also
  names Aditya Mittal and `mittalaa@uci.edu` as maintainer. Explicit packaging
  configuration includes the existing MIT license notice in the wheel and
  sdist; archive checks prevent it from being accidentally omitted again.
- **Repeatable validation:** add dependency-index inspection, held-out/paper
  experiments, and real Jupyter-kernel execution tools. CI caps computational
  threads and includes the kernel checks; configured CI is not a completed run.
- **Notebook presentation:** visual inspection found overlapping axes in the
  parallel-coordinate example. Select three numeric measurements and facet by
  sex, then re-execute the notebook and inspect the resulting plot.

These changes preserve exported R/Python signatures. The logistic EDF method
still uses its original pseudo-observation construction. Even zero penalty
weights leave appended negative-outcome observations, so this estimator need
not equal ordinary logistic regression; the help page now states this explicitly.

## Experiments

`tools/run_experiments.R` uses seeds 2026, 4101, and 7303. Each held-out experiment
samples 600 rows, fits on 450, and evaluates on the other 150. It checks disjoint
splits, finite predictions, binary probability bounds, invariance to column
reordering, and sensitive-group correlations against independent calculations.

The wage experiment uses `svcensus`, six regression methods, gender as S, and
occupation as the deweighted proxy. Wages are expressed in thousands of dollars
only in these held-out fits. The binary experiment uses African-American and
Caucasian COMPAS records, five classifiers, race as S, and decile score as the
proxy. Every method runs with baseline and constrained settings: 66 fits total.
The other two datasets, `lak` and `mortgageSE`, receive independent `glm`
prediction comparisons. All checks passed with optional solvers installed.

The full paper grids add 60 cross-validated fits: five unfairness settings times
five folds on all 5,855 COMPAS rows, and seven occupation weights times five
folds on all 20,090 census rows, with both gender and age as S. All runs completed.
The COMPAS error ranges from 0.2651 to 0.2717, while the largest absolute reported
group correlation drops from about 0.2733 to 0.0158 as the constraint tightens.
The census grid's MAE ranges from about $26,028 to $26,559. Its correlations are
not monotone in the occupation weight. The results therefore do not support a
claim that every feature-deweighting change improves every fairness measure.

These are reproducible execution and numerical-consistency checks. They do not
reproduce the paper's historical tables exactly or establish causal fairness.
The regression error is MAE, not the percentage-error label used in parts of
the historical paper. Corrected implementation outputs can differ from 1.0.0.

## Validation results

Evidence is retained in the [validation record](submission-validation-2.0.0/README.md).
Full working directories are under
`/private/tmp/dsld-2.0.0-audit-20261023`; Linux container mounts and earlier
investigation logs remain under `/private/tmp/dsld-submission-audit-20261023`.

| Check | Result |
| --- | --- |
| R numerical/behavioral suite | [42 passed, zero failures](submission-validation-2.0.0/r-regression.log) |
| Python integration, installed wheel | [21 tests passed](submission-validation-2.0.0/python-integration.log) |
| Public API baselines | [25 R exports](submission-validation-2.0.0/r-api.log) and [42 Python callables](submission-validation-2.0.0/python-api.log) preserved |
| Held-out experiments | 66 fits, zero failed checks |
| Complete paper grids | 12 candidate rows / 60 fold fits completed |
| Reproducibility | [Two independent complete runs produced byte-identical CSVs](submission-validation-2.0.0/experiments/reproducibility.json) for all 66 held-out fits and both full-data grids |
| Bundled-dataset logistic references | Both passed |
| Jupyter notebooks | [Three fresh kernels; all 27 code cells passed](submission-validation-2.0.0/notebook-kernels.log) |
| README execution | [Five R blocks and two Python blocks passed](submission-validation-2.0.0/readme-examples.log) |
| Live package URL check | [Passed](submission-validation-2.0.0/urls.log) |
| Current CRAN dependency index | [150 checked dependencies available, none orphaned; no declared current CRAN reverse dependencies](submission-validation-2.0.0/dependencies.log) |
| macOS R 4.5.1, full `--as-cran` | [Zero errors, zero warnings, one incoming/archive note](submission-validation-2.0.0/macos-full.log); PDF manual, examples including `donttest`, tests, and vignettes passed |
| macOS depends-only / suggests-only | [Depends-only](submission-validation-2.0.0/macos-depends.log) and [suggests-only](submission-validation-2.0.0/macos-suggests.log): zero errors, zero warnings, one incoming/archive note each; `--no-manual` |
| Linux R 4.6.1, full `--as-cran` | [Zero errors, zero warnings, one incoming/archive note](submission-validation-2.0.0/linux-release.log); PDF manual, extended examples, all 42 regression checks, and vignettes passed |
| Linux R 4.6.1, depends-only | [Zero errors, zero warnings, one incoming/archive note](submission-validation-2.0.0/linux-depends.log); 37 applicable regression checks passed with optional solvers/matching unavailable; `--no-manual` |
| Linux R-devel 4.7.0, full `--as-cran` | [Zero errors, zero warnings, one incoming/archive note](submission-validation-2.0.0/linux-devel.log); October 22 snapshot r90748; PDF manual, extended examples, all 42 regression checks, and vignettes passed |
| Release contents and original data | [All three archives match source](submission-validation-2.0.0/artifact-verification.log); [four datasets unchanged from the original commit](submission-validation-2.0.0/original-data-sha256.json) |
| Documentation and static checks | [Local links, notebooks, Python lint/format/3.9 grammar, CI YAML, and Git whitespace checks passed](submission-validation-2.0.0/static-checks.log) |

The note describes a new submission of an archived package and repeats the old
`fairness` archival reason. It is retained and explained rather than suppressed.
On macOS, some optional packages live in R's main library, which the
depends-only environment flag cannot hide: that run executes 41 checks. The
Linux depends-only run genuinely excludes optional packages and executes 37
checks, skipping three solver checks and two matching checks. Full runs execute
all 42. These restricted runs validate graceful absence, not the omitted
methods. Current R's `--as-cran` also enables suggests-only internally; the
explicit suggests-only run is a configuration confirmation, not an additional
independent numerical suite.

Linux checks use isolated official R-hub containers on Ubuntu 26.04.1, running
as x86_64 guests on the Apple Silicon host. Initial source-only dependency
builds were stopped in favor of Posit Package Manager binaries. The release
library was also used with R-devel after verifying that every optional backend
loaded. These dependency binaries were built for R 4.6; DSLD itself is installed
and checked separately by each R version and has no compiled native code.

The binary mirror supplied `lme4` 2.0-6 while CRAN source metadata listed 2.1-0.
Version 2.1-0 was successfully compiled from CRAN source under R-devel with
Clang 23 and installed in a separate library for the final R-devel check. The
other missing entries in the initial binary-library comparison (`BH`, `cpp11`,
`RcppArmadillo`, and `RcppEigen`) were transitive build-time headers, not missing
runtime dependencies of DSLD. The source build installed its required headers.

Jupyter execution validates kernel-side outputs, including Plotly MIME output.
It does not constitute a manual test of browser interactions. Windows and hosted
GitHub Actions have not been run in this session.

Reproducibility was checked in two fresh R processes with the same package
versions, seeds, data, and thread settings. All three result CSVs are identical
byte for byte; the comparison also checks row/column structure and a numerical
tolerance of `1e-10`. This does not promise bit-identical results across different
R, BLAS, solver, or package versions. Both session records and the comparison
report are retained with the experiment data.

## Submission handoff

The maintainer email change belongs in `cran-comments.md`. CRAN's policy asks
for an explanation of a maintainer-address change and, if possible, confirmation
from the previous address. No such confirmation has been sent here; CRAN may
request it during the submission process.

The Python MIT notice is included unchanged in both Python distributions. Its
existing copyright line names “The Python Packaging Authority” (2018), whereas
the package metadata names the DSLD authors. The maintainer was asked to confirm
the appropriate attribution; no replacement holder or year has been assumed.
This attribution question remains distinct from the completed numerical and
packaging checks.

At the end of this audit, the local release directory was
`/Users/adityamittal/Desktop/PhD/dsld-release-2.0.0`. The follow-up review replaces
its contents with the newer checked candidate. This audit's earlier R archive
`dsld_2.0.0.tar.gz` had SHA-256:

```text
520bbf003a6f13866d4fdf4b22e18ba5ddc151dd0bd1349af987a3fa473a7b3c
```

The [complete artifact manifest](submission-validation-2.0.0/archive-sha256.json)
also records the Python hashes and sizes. The [submission comments](../cran-comments.md)
explain the archive note, maintainer-address change, completed platform checks,
and reverse-dependency scope. No CRAN/PyPI submission or repository push has
been made. Changes to package sources after this audit require rebuilding and
checking the resulting archives again.
