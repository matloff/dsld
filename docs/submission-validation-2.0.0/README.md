# DSLD 2.0.0 validation evidence

These files support the [readiness report](../cran-readiness-2026-10.md).
The audit started October 23, 2026 in Pacific time; UTC logs show October 24.
The earlier `validation-2026-10` directory documents the first cleanup pass,
not this release candidate.

## Artifact identity

`archive-sha256.json` identifies the R source archive, Python wheel, and Python
source distribution. `artifact-verification.log` records comparison of their
contents with the checkout, including versions, maintainer metadata, and the
unchanged Python license notice. `original-data-sha256.json` verifies that all
four data files are byte-identical to commit `7e9f875`.

## Environments and commands

The final package checks use `R CMD check --as-cran` on the same R source
archive. Files named `*-full.log`, `linux-release.log`, and `linux-devel.log`
include the PDF manual check. The depends/suggests runs use `--no-manual`.

- macOS: R 4.5.1, arm64. Full, `_R_CHECK_DEPENDS_ONLY_=true` with
  `_R_CHECK_FORCE_SUGGESTS_=false`, and `_R_CHECK_SUGGESTS_ONLY_=true` runs.
- Linux release: R 4.6.1, Ubuntu 26.04.1, x86_64. Full and depends-only runs.
- Linux development: R 4.7.0 development snapshot, October 22, 2026, revision
  90748. Full run with `R_LIBS=/check/latest-library:/check/library`.

`R_RANGER_NUM_THREADS`, `OMP_NUM_THREADS`, and `OPENBLAS_NUM_THREADS` are set to
2. Linux and CI also set `MKL_NUM_THREADS=2`. Containers are x86_64 guests on
Apple Silicon. Their immutable image IDs are in `container-images.txt`.
The R-devel image uses R 4.6 dependency binaries, with `lme4` 2.1-0 compiled
separately from source using Clang 23. Its installation log is retained.

The package-version manifests include installed copies in each library; later
copies of the same package can be shadowed by earlier library paths. The
experiment session records show the versions actually loaded. Each CRAN check
installs its own copy of DSLD 2.0.0; it does not use an older installed DSLD.

## Numerical and example evidence

`r-regression.log`, `python-integration.log`, the API logs, README execution log,
and Jupyter-kernel log record the standalone suites. The `*-regression.Rout`
files preserve full test output from each package check, including optional
dependency skips. `static-checks.log` records the final documentation, lint,
format, compatibility-syntax, workflow-YAML, and whitespace checks.

`experiments/run1` and `experiments/run2` contain complete independent runs of
`tools/run_experiments.R --full-paper`: 66 held-out fits, 60 cross-validation
fold fits, and two dataset-specific `glm` comparisons per run. The comparison
report verifies identical result CSVs and retains their hashes. Both session
records are kept. The [development guide](../development.md) gives the commands
to reproduce the checks with a fresh installation.

## Research and failure evidence

`cran-dependencies.tsv` and `dependencies.log` record the live CRAN dependency
and reverse-dependency query; `urls.log` records live URL validation.
`linux-release-before-plot-fix.log` intentionally preserves one failed earlier
candidate, before the within-group constant-axis repair. It is not a result for
the final archive. `linux-plot-fix-smoke.log` records the focused repair check;
the final package-check logs cover the complete repaired package.

No remote CI run, Windows runtime check, publication, submission, or external
message is represented by this evidence.
