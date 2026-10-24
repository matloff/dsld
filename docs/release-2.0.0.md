# DSLD 2.0.0 release review and submission guide

Maintainer: Aditya Mittal <mittalaa@uci.edu>.

This final review follows the [initial maintenance record](maintenance-2026-10.md)
and [CRAN readiness audit](cran-readiness-2026-10.md). It retains their research,
repairs, and validation evidence while checking the final changes separately.
The maintainer authorized committing and pushing the work to GitHub, and plans
to submit the R source package to CRAN personally.

## Additional code and documentation review

- Construct formulas from literal column names. Linear/logistic fits previously
  failed for outcome names containing spaces or operators; linear ridge could
  emit a deprecated formula-parsing warning. Tests cover spaces, operators, and
  backticks, both group-model interaction settings, and unchanged predictions.
- Preserve explicit sensitive-factor contrasts when all levels are observed.
  An unconditional `droplevels` call had reset them, changing coefficient names
  and values even though fitted predictions were equivalent. Tests compare
  coefficients directly with `lm` and `glm`. Unused levels are still dropped,
  with their standard contrast-reset behavior documented.
- Clarify linear/logistic return structures, prediction standard errors,
  log-odds versus probability comparisons, and the independence assumption for
  separate-group differences. Correct old help-page naming and wording errors.
- Add independent numerical checks across 120 linear/logistic configurations
  and 24 cross-validation candidates, covering group counts, contrast schemes,
  interactions, sandwich covariance, uneven folds, scaling, and penalized
  normal equations. The additional check also runs in CI.

## Final validation

All six local CRAN-style configurations pass on the same final source archive,
with **zero errors, zero warnings, and one incoming/archive note** each. The
note repeats the historical archival reason, now addressed by removing the
unused `fairness` dependency. Raw evidence is in the
[final validation record](release-validation-2.0.0/README.md).

| Check | Result and evidence |
| --- | --- |
| macOS R 4.5.1, full `--as-cran` | [Passed](release-validation-2.0.0/macos-full.log), including PDF/HTML manuals, extended examples, tests, and vignettes |
| macOS dependency configurations | [Depends-only](release-validation-2.0.0/macos-depends.log) and [suggests-only](release-validation-2.0.0/macos-suggests.log) passed; no PDF manual in these runs |
| Linux R 4.6.1, full `--as-cran` | [Passed](release-validation-2.0.0/linux-release.log), including manuals and all 45 regression checks |
| Linux R 4.6.1, depends-only | [Passed](release-validation-2.0.0/linux-depends.log); 40 applicable regression checks, with optional solvers and matching unavailable |
| Linux R-devel 4.7.0, r90748 | [Passed](release-validation-2.0.0/linux-devel.log), including manuals and all 45 regression checks |
| Standalone R regression suite | [45 passed](release-validation-2.0.0/r-regression.log) |
| Independent numerical references | [120 model configurations and 24 CV candidates passed](release-validation-2.0.0/numerical-references.log), with warnings treated as errors |
| Installed Python wheel | [21 integration tests passed](release-validation-2.0.0/python-integration.log) against the final R implementation |
| Executable documentation | [Seven README analysis blocks](release-validation-2.0.0/readme-examples.log) and [27 cells in three fresh Jupyter kernels](release-validation-2.0.0/notebook-kernels.log) passed |
| Repeated experiments | Each run completed 66 held-out fits, 60 full-data CV fold fits, and two dataset-specific reference checks; [all three result CSVs are byte-identical](release-validation-2.0.0/reproducibility.json) |
| Public API and data | [25 R signatures](release-validation-2.0.0/r-api.log) and [42 Python callables](release-validation-2.0.0/python-api.log) remain compatible; [all four datasets are unchanged](release-validation-2.0.0/original-data-sha256.json) |
| Dependencies | [150 current CRAN dependencies available, none orphaned, no declared current CRAN reverse dependencies](release-validation-2.0.0/dependencies.log) |
| Packaging | [R source and Python distributions match source, versions, contact metadata, and license files](release-validation-2.0.0/artifact-verification.log) |

The [environment manifests](release-validation-2.0.0/README.md) distinguish the
macOS and Linux libraries, including R-devel's source-built `lme4` 2.1-0.
The Linux check is the genuine optional-dependency absence test; the macOS
depends-only run still sees some optional packages in R's main library and
executes 44 checks. The explicit suggests-only configuration overlaps with
current R's `--as-cran` settings. Expected diagnostic warnings from deliberately
difficult regression fixtures remain visible in test output; they are not
package-check warnings.

The source was committed and pushed as
[`c078abf`](https://github.com/matloff/dsld/commit/c078abf81a1df39b5b7d50e8d31733be1bcec4c1).
The [first hosted run](https://github.com/matloff/dsld/actions/runs/40069000775)
passed its macOS and Windows R release checks. It exposed a Python API-checker
portability issue: Python 3.12's `ast.dump` includes empty fields that the
Python 3.14-generated baseline omits. The API itself had not changed. The
checker now emits a stable structural representation, preserving the original
baseline and detecting changes to defaults, parameter kinds, and variadic
arguments. It passes locally under Python 3.12, 3.13, and 3.14. The updated
workflow also uses the current Node 24-based checkout and Python setup actions.
These changes affect development tooling only; the checked source archive is
unchanged. Hosted results for the corrected workflow are recorded separately.

GitHub's R check action disables CRAN incoming checks and omits the PDF manual,
so a hosted `Status: OK` is consistent with the local full checks' archive note.
The [Windows log](release-validation-2.0.0/github-windows-check.log) and
[macOS log](release-validation-2.0.0/github-macos-check.log) preserve the complete
package-check portions of those jobs, including platform and R versions.

## Exact submission file

Use `dsld_2.0.0.tar.gz` in
`/Users/adityamittal/Desktop/PhD/dsld-release-2.0.0`. Its SHA-256 is:

```text
00c708de2da7b68fe13d26f3fa78ed7ea90eb245f54973a33ede7ac254619523
```

The [artifact manifest](release-validation-2.0.0/archive-sha256.json) records
the size and all distribution hashes. The release directory also contains
`SHA256SUMS`, Python distributions, and the prepared `cran-comments.md`.
The [submission comments](../cran-comments.md) explain the archive note,
maintainer address, platform checks, and reverse-dependency scope.

Upload the R source archive through CRAN's submission process and use the
prepared comments for the resubmission explanation. Keep the maintainer email
as `mittalaa@uci.edu` and complete CRAN's email confirmation when requested.
Do not substitute GitHub's generated source ZIP. See the current
[CRAN submission checklist](https://cran.r-project.org/web/packages/submission_checklist.html)
for the remaining maintainer steps. Rebuild and recheck if package sources
change after this record.

## Submission scope

CRAN needs the source archive produced by `R CMD build`, named
`dsld_2.0.0.tar.gz`. It does not need the GitHub repository download, the Python
wheel, or an archive of the complete development workspace.

The source archive and `cran-comments.md` explain the removal of the unused
`fairness` dependency and the expected archived-package incoming note. The
maintainer remains the same person with a new UCI email address; CRAN may
request confirmation of that change. No CRAN or PyPI submission is performed
as part of this work.

The existing Python MIT notice is preserved, including its original
“The Python Packaging Authority” (2018) attribution. The maintainer has been
asked to confirm that attribution; no ownership or copyright year has been
invented. Passing package checks does not resolve that factual question or
guarantee CRAN acceptance.
