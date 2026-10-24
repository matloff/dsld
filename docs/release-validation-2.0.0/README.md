# Final 2.0.0 release evidence

See the [release review](../release-2.0.0.md) for findings and the submission
handoff. These files supersede the earlier candidate's checks in
`submission-validation-2.0.0`; both records are retained so their hashes and
test counts remain auditable.

- `archive-sha256.json` identifies the exact R source archive and Python
  distributions. `artifact-verification.log` verifies their contents against
  source. The Python distributions are unchanged from the preceding pass.
- `macos-*.log`, `linux-release.log`, `linux-depends.log`, and `linux-devel.log`
  are complete `R CMD check` logs. Matching `*-regression.Rout` files preserve
  each check's numerical tests and optional-dependency skips.
- `r-regression.log` and `python-integration.log` record the standalone suites.
  `numerical-references.log` records the new independent mathematical checks,
  run with `options(warn = 2)` so unexpected warnings fail that suite.
- `edge-cases-before.log` intentionally records failures of the previous
  candidate for unusual outcome names. It is not a final-candidate result.
- `experiments/run1` and `experiments/run2` contain independent runs of all
  held-out experiments and full paper grids. `reproducibility.json` compares
  all result rows and records matching hashes.
- The README, notebook, API, static-check, and dependency logs provide the
  remaining validation evidence. Dataset hashes still match commit `7e9f875`.
- `github-*-check.log` captures complete R package-check sections from the
  corrected hosted workflow. `github-python-validation.log` contains its
  complete Python validation step, including both notebook execution methods.
  Hosted checks build their own archives; hashes at the end of the Python log
  identify those temporary builds, not the submission archive in
  `archive-sha256.json`. Generated vignettes and archive metadata can differ
  across build environments; the archive checker verifies included sources.
- `github-workflow-results.json` records all five successful jobs at commit
  `60edf3e`, whose package sources match `c078abf`. The R-devel dependency-build
  log and session manifest record a fresh installation from source. The Python
  installation log and R session manifest record that job's dependency versions.
- `github-python-checker-before-fix.log` records the first run's Python 3.12
  checker failure. The three `python-api-3.*.log` files verify the corrected
  checker against the unchanged baseline on Python 3.12, 3.13, and 3.14.

The macOS runs use R 4.5.1/arm64; Linux uses Ubuntu 26.04.1/x86_64 with R 4.6.1
and R-devel 4.7.0 (2026-10-22 r90748). Thread limits are two, as in the earlier
audit. Full runs include PDF manuals; depends-only/suggests-only runs use
`--no-manual`. The macOS depends-only run cannot hide packages in R's main
library, so the separate Linux restricted run is the meaningful absence test.

Linux library manifests and container image IDs carry forward from the earlier
audit because those environments were reused unchanged. R-devel uses R 4.6
dependency binaries plus source-built `lme4` 2.1-0. Each package check installs
the final DSLD archive separately. Duplicate package versions in a manifest
reflect library search order; experiment session records show loaded versions.

The [development guide](../development.md) contains reproducible commands.
Full working files are in `/private/tmp/dsld-publish-audit-20261023`, with Linux
check directories under the earlier container mounts in
`/private/tmp/dsld-submission-audit-20261023`.

Raw logs and session records retain their original whitespace. The final staged
whitespace review checks source and authored documentation separately, so
formatting diagnostics in captured tool output do not require rewriting the
evidence. The API baseline's trailing spaces were removed without changing its
stored signatures.
