## Submission purpose

This is a maintenance release of the archived package `dsld`, version 2.0.0.
CRAN archived version 1.0.0 on 2025-11-27 because it required the archived
package `fairness`. The unused import and namespace declaration have been
removed. The actively used `fairml` dependency remains.

The release also repairs numerical scoring, prediction preprocessing, matching,
and plotting errors; adds regression tests; and refreshes examples and help.
Public R function signatures and bundled datasets are preserved. `NEWS.md`
records the changes.

The final documentation pass simplifies the README resource links, corrects
the companion-book URL, and clarifies installation and guide wording. Runtime
code and datasets are unchanged from the preceding validated candidate. The
rebuilt source includes both prebuilt vignettes and their index; all six local
check configurations below were rerun on this exact documentation rebuild.

## Maintainer address

The maintainer remains Aditya Mittal. The contact address has changed from
`adityamittal2031@gmail.com` to `mittalaa@uci.edu`, which the maintainer has
confirmed is the address to use. No confirmation message from the previous
address has been sent as part of this preparation.

## Check results

- macOS 27.0.1, aarch64, R 4.5.1: full `R CMD check --as-cran`, including
  PDF manual, extended examples, regression tests, and vignettes: 0 errors,
  0 warnings, 1 note.
- Same environment with `_R_CHECK_DEPENDS_ONLY_=true` and separately
  `_R_CHECK_SUGGESTS_ONLY_=true`, both with `--no-manual`: 0 errors,
  0 warnings, 1 note each.
- Ubuntu 26.04.1, x86_64, R 4.6.1: full `R CMD check --as-cran`, including
  PDF manual, extended examples, all 45 regression checks, and vignettes:
  0 errors, 0 warnings, 1 note.
- Same Linux release environment with `_R_CHECK_DEPENDS_ONLY_=true`,
  `_R_CHECK_FORCE_SUGGESTS_=false`, and `--no-manual`: 0 errors, 0 warnings,
  1 note. All 40 applicable regression checks passed with optional solvers
  and matching unavailable.
- Ubuntu 26.04.1, x86_64, R-devel 4.7.0 (2026-10-22 r90748): full
  `R CMD check --as-cran`, including PDF manual, extended examples, all 45
  regression checks, and vignettes: 0 errors, 0 warnings, 1 note.
- GitHub Actions R 4.6.1 on Windows Server 2022 x64 (build 26100), macOS
  26.6.2 arm64, and Ubuntu 24.04.5 x86_64: `R CMD check --no-manual --as-cran`
  passed with `Status: OK` on all three platforms.
- GitHub Actions Ubuntu 24.04.5 x86_64, R-devel 4.7.0 (2026-10-22 r90748),
  with CRAN dependencies built from source: `R CMD check --no-manual --as-cran`
  passed with `Status: OK`.

The local Linux checks ran in official R-hub containers. Local R-devel used R 4.6
dependency binaries, with the current CRAN `lme4` 2.1-0 built separately from
source under R-devel. The hosted R check action disables CRAN incoming checks,
so its zero-note results supplement the local full checks and their incoming
note; they do not replace them. Hosted checks also omit the PDF manual.
All five jobs for the preceding candidate passed in the corrected hosted
workflow, including the separate
Python 3.12 interface and executable-documentation job:
<https://github.com/matloff/dsld/actions/runs/40069425096>.

The incoming note states that this is a new submission and that the package
was archived, repeating the historical `fairness` dependency reason. The
dependency causing that archival is no longer required.

Additional checks passed: 45 R regression checks, 21 Python integration tests,
all 27 example-notebook code cells in separate Jupyter kernels, README examples,
66 held-out model fits, and both full-data five-fold grids from the methods
paper (60 additional fold fits per run). Two complete independent experiment
runs produced byte-identical result CSVs. Package URLs pass live validation.
An additional independent numerical suite verifies linear/logistic predictions
and standard errors in 120 configurations, plus 24 cross-validation candidates
against penalized normal equations and independently constructed folds.

## Reverse dependencies

Current CRAN source metadata was inspected on 2026-10-24 UTC. No current CRAN
packages declare strong reverse dependencies (direct or recursive) or direct
suggested dependencies on `dsld`. No reverse-dependency installations therefore
needed testing. This statement does not cover archived or non-CRAN packages.
