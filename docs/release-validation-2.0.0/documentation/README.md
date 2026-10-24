# Documentation rebuild — October 24, 2026

This is the current 2.0.0 submission archive. The
[release review](../../release-2.0.0.md) describes the complete maintenance work.
This pass makes minor documentation and packaging changes; runtime code, help
sources, tests, datasets, and notebook code are unchanged from `da6f2fe`.

## Changes

- Keep only the book, paper, and Python-interface links in the README overview.
- Clarify installation wording and remove internal audit history from public
  release notes. Keep contributor records under `docs/`.
- Link to the published companion book. The old HTML-preview wrapper returned
  HTTP 200 while the underlying book file returned HTTP 404.
- Use absolute public links in the Python README so they also work on PyPI.
- Ignore local environments, notebook checkpoints, distributions, and generated
  validation output. Preserve R's generated `build/vignette.rds` in the source
  package, and require it and the rendered vignettes in the archive checker.

## Validation

`archive-sha256.json` identifies all three distributions. The R archive's SHA-256
is `969f3f14bb9970d3e43b583b6d8a3804db33590518e20a8c63574895a8be9ca4`.
`artifact-verification.log` compares the distributions with the checkout and
checks versions, maintainer metadata, license files, and built vignettes.

All six configurations in `check-summary.json` pass with zero errors, zero
warnings, and one expected archived-package incoming note. The three full runs
include PDF manuals, extended examples, tests, and vignette rebuilding.
Dependency-restricted runs omit the PDF manual. Each check installs this exact
archive; matching `*-regression.Rout` files record tests and optional skips.

The standalone suite passes all 45 R regression checks, 120 independent model
configurations, 24 CV candidates, and 21 Python integration tests. All seven
README analysis blocks and 27 notebook cells in fresh kernels pass. The
`vignettes.log` check confirms that both installed guides are discoverable.

`unchanged-source.json` records the comparison with `da6f2fe`; all four datasets
also match the original `7e9f875` commit. The unchanged implementation retains
the [two complete experiment runs](../reproducibility.json) with byte-identical
result CSVs. Those experiments were not rerun solely for wording changes.

The R and Python API checks retain the original signature baselines. The
environment manifests in the parent directory remain applicable because the
same isolated libraries and containers were reused. `public-links.log` records
successful requests to the final book, paper, Python-guide, and notebook URLs.

`github-workflow-results.json` records all five successful hosted jobs for
`f11b3aa`: R release on Windows, macOS, and Linux; Linux R-devel; and Python 3.12.
Hosted jobs rebuild distributions from the same source and pass the strengthened
vignette-index check. Their archives can differ in generated metadata from the
submission archive. Hosted R checks omit the PDF manual and incoming checks.

The two `*-before-*` logs intentionally record findings from earlier builds in
this pass: the stale book URL and the missing vignette index. The latter was
caused by an overbroad build exclusion added during this pass, then corrected
before publication. The strengthened archive checker rejects that earlier
archive and accepts the submission archive.

Raw logs retain diagnostic output and whitespace. Complete working files and
rendered README previews are under `/private/tmp/dsld-docs-audit-20261024`.
No CRAN or PyPI submission is performed by these checks.
