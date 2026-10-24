# Development and validation

DSLD contains an R package at the repository root and a Python interface under
`inst/`. Make changes to source files and keep public names, argument order,
model classes, and documented return fields stable unless a release explicitly
announces a change.

## Repository map

| Location | Contents |
| --- | --- |
| `R/` | Statistical models, fairness wrappers, graphics, and internal utilities |
| `man/` | R help pages, maintained directly as Rd files |
| `data/` | Bundled datasets; preserve their contents and attribution |
| `vignettes/` | Quick Start and categorized function guide |
| `inst/src/dsldPy/` | Python wrappers and shared rpy2 conversion/graphics helpers |
| `inst/examples/` | Tabular, machine-learning, and graphical notebooks |
| `tests/regression.R` | Numerical and behavioral checks run by `R CMD check` |
| `python-tests/` | Python integration tests against the installed R package |
| `tools/` | API, documentation, README, and notebook validation scripts |
| `.github/workflows/checks.yaml` | R platform matrix and Python/example CI |
| `NEWS.md`, `docs/maintenance-2026-10.md` | Release changes and detailed maintenance evidence |
| `docs/cran-readiness-2026-10.md` | Additional CRAN research, experiment design, and submission validation |

## Install a development environment

Use R with the packages listed in `DESCRIPTION`, Python 3.9 or newer, and a
Python virtual environment. A separate R library keeps development dependencies
out of an existing installation. Run these commands from the repository root:

```sh
repo_dir="$PWD"
validation_dir="$(mktemp -d "${TMPDIR:-/tmp}/dsld-check.XXXXXX")"
mkdir -p "$validation_dir/library"
export R_LIBS="$validation_dir/library${R_LIBS:+:$R_LIBS}"
Rscript --vanilla -e 'install.packages("remotes", repos="https://cloud.r-project.org"); remotes::install_local(".", dependencies=TRUE, upgrade="never")'
python3 -m venv "$validation_dir/venv"
. "$validation_dir/venv/bin/activate"
python -m pip install './inst[examples]' build ruff
```

The optional `Matching`, `bnlearn`, `randomForest`, `cccp`, and `CVXR` packages
should be present for the complete validation pass. The tests skip selected
optional methods when their dependencies are absent; inspect skips before
calling a run complete. The notebooks exercise optional matching and structure
discovery methods and therefore require those dependencies.

R, the Python interpreter, and rpy2 must target compatible architectures. If a
binary rpy2 installation references a different R installation, rebuild its
`rpy2-rinterface` component against the intended R version. This is an
environment issue, not a reason to change DSLD's model implementations.

## Fast checks while editing

After changes to R source, reinstall the package before running integration
checks. Otherwise Python and `library(dsld)` may continue using an older build.

```sh
R CMD INSTALL --library="$validation_dir/library" .
Rscript --vanilla tests/regression.R
Rscript --vanilla tools/check_api.R
Rscript --vanilla tools/check_numerical_references.R
python tools/check_python_api.py
ruff check --select F inst/src python-tests tools
ruff format --check inst/src python-tests tools
python -m unittest discover -s python-tests -v
python tools/check_docs.py
```

The API baselines come from commit `7e9f875`: 25 exported R signatures and 42
Python callables. The Python checker permits the documented addition of an
optional final `model_args` argument to `dsldPyFairUtils`. Do not regenerate
these baselines merely to make an accidental API change pass.

The R regression suite checks numerical results against independent reference
calculations where practical: `lm`/`glm`, covariance-based contrasts, penalized
normal equations, and directly weighted KNN data. Other checks cover class
coding, row alignment, cross-validation, matching, plot controls, and edge cases.
The Python suite exercises the installed R implementations, conversion, generic
methods, returned values, and graphics-device cleanup.

The additional numerical-reference script checks linear/logistic predictions
and standard errors in 120 settings spanning ten seeds, two contrast schemes,
two interaction settings, and two, three, or five sensitive groups. It also
checks 24 cross-validation candidates against independently constructed folds,
penalized normal equations, held-out MAE, and group correlations.

## Execute the examples

```sh
python tools/run_readme_examples.py
python tools/run_notebooks.py
python tools/run_notebooks_kernel.py "$validation_dir/notebooks"
Rscript --vanilla tools/run_experiments.R "$validation_dir/experiments" --full-paper
```

The README runner executes analysis code and excludes dependency-installation
commands and vignette viewer calls. The notebook runner executes every code
cell in order, with a fresh Python namespace per notebook, while suppressing
browser/display launches. It does not launch a Jupyter kernel or persist
outputs. It also shares one embedded R interpreter; each notebook seeds R and
loads data into its own R environment.

The separate kernel runner needs `nbclient` and `ipykernel` (included in the
Python `examples` extra). It executes each notebook in a fresh Jupyter kernel,
captures display output, and saves executed copies outside the checkout. If
`python3` resolves to another environment, register the intended interpreter
with `python -m ipykernel install --prefix="$validation_dir/jupyter"
--name=dsld-check`, set `JUPYTER_PATH="$validation_dir/jupyter/share/jupyter"`,
and pass `--kernel=dsld-check` to the runner.

The experiment runner evaluates six regression and five classification methods
across three seeds and two settings, with disjoint training/test rows. The
optional `--full-paper` flag also runs both full-dataset grids from Section 4.4
of the methods paper. Outputs are CSV files and an R session record. These
longer experiments run separately from routine package examples and checks.

To verify repeatability in the same dependency environment, run the experiment
script in two fresh R processes and compare the saved CSVs:

```sh
Rscript --vanilla tools/run_experiments.R "$validation_dir/experiments/run1" --full-paper
Rscript --vanilla tools/run_experiments.R "$validation_dir/experiments/run2" --full-paper
python tools/check_experiment_reproducibility.py "$validation_dir/experiments/run1" "$validation_dir/experiments/run2" --report "$validation_dir/experiments/reproducibility.json"
```

The comparator checks every row and field, reports byte identity, and permits
only a `1e-10` absolute/relative tolerance for differing numeric text. Keep both
session records and dependency manifests: equal seeds alone do not guarantee
identical numerical output across different R, solver, or BLAS versions.

Commit notebooks with empty outputs and execution counts. `check_docs.py`
validates their schema, Python syntax, and empty-output state, along with local
Markdown link targets and balanced code fences. External URLs and Markdown
anchor fragments require separate review.

## Build and check the R distribution

Pandoc is needed to build the vignettes. A LaTeX installation is needed for the
full PDF manual check. From the repository root, with the variables above still
set:

```sh
cd "$validation_dir"
R CMD build "$repo_dir"
R CMD check --as-cran dsld_2.0.0.tar.gz
R CMD INSTALL --library="$validation_dir/library" dsld_2.0.0.tar.gz
cd "$repo_dir"
```

Inspect `dsld.Rcheck/00check.log`, example output, regression output, and
vignette results. The full check includes examples, tests, vignette rebuilding,
and the PDF manual. `--no-manual` is useful on machines without LaTeX, but record
that omission. CRAN incoming checks require network access and may report
archive status or new-submission notes that are separate from code failures.

Also run dependency-restricted configurations in separate check directories:

```sh
mkdir -p "$validation_dir/depends-only" "$validation_dir/suggests-only"
cd "$validation_dir/depends-only"
_R_CHECK_DEPENDS_ONLY_=true _R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran "$validation_dir/dsld_2.0.0.tar.gz"
cd "$validation_dir/suggests-only"
_R_CHECK_SUGGESTS_ONLY_=true R CMD check --as-cran "$validation_dir/dsld_2.0.0.tar.gz"
cd "$repo_dir"
Rscript --vanilla tools/check_dependencies.R "$validation_dir/dependencies"
```

Inspect actual skipped tests and library contents: packages installed into R's
main library may remain visible even in depends-only mode. Use a clean
environment with contributed packages in a separate library to verify optional
dependencies are genuinely unavailable. Current R's `--as-cran` also enables
the suggests-only restriction internally; an explicit run documents that mode
but should not be counted as a different statistical test suite.

## Build and test the Python distribution

```sh
python -m build inst --outdir "$validation_dir/dist"
python -m pip install --force-reinstall --no-deps "$validation_dir"/dist/*.whl
python tools/check_release_artifacts.py "$validation_dir"/dsld_*.tar.gz "$validation_dir"/dist/*.whl "$validation_dir"/dist/*.tar.gz --report "$validation_dir/archive-sha256.json"
python -m unittest discover -s python-tests -v
python tools/run_readme_examples.py
python tools/run_notebooks.py
```

Run distribution tests without a source-directory `PYTHONPATH` override so they
exercise the installed wheel. Keep build products outside the repository;
Python-generated `inst/build` and `inst/src/*.egg-info` are ignored and should
be removed after packaging.

The artifact checker compares source files with all three distributions,
verifies coordinated versions and maintainer metadata, and checks that the
existing Python license notice is actually included in the wheel and sdist.
It also writes SHA-256 hashes for the exact archives being reviewed.

## CI and release preparation

The workflow checks R release on Linux, macOS, and Windows; R development on
Linux; and the Python wheel, wrappers, and examples on Linux/Python 3.12. It
runs on pushes, pull requests, manual dispatch, and a weekly schedule to detect
upstream dependency changes. CI omits the PDF manual check. A configured
workflow is not evidence of a successful remote run; review the actual results.

Before a release, rerun checks using current dependencies, review `NEWS.md`,
confirm the maintainer/contact metadata, and retain any unresolved notes in the
submission explanation. R 2.0.0 and Python 2.0.0 in this checkout are unpublished
maintenance versions. Publishing to CRAN/PyPI or pushing the repository is a
separate action.
