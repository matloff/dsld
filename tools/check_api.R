# Compare the installed package with the exported 1.0.0 signatures.
expected <- dget("tools/baselines/r-api-1.0.0.R")
namespace <- asNamespace("dsld")
actual_names <- getNamespaceExports(namespace)
stopifnot(setequal(actual_names, names(expected)))
for (name in names(expected)) {
  if (!identical(formals(get(name, namespace)), expected[[name]])) {
    stop("Public R signature changed: ", name)
  }
}
cat("PASS:", length(expected), "exported R signatures and defaults preserved.\n")
