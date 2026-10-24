# Inspect current CRAN dependency availability and reverse dependencies.
# Rscript tools/check_dependencies.R /path/to/results
arguments <- commandArgs(trailingOnly = TRUE)
output_directory <- if (length(arguments)) arguments[1L] else tempfile("dsld-dependencies-")
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
repository <- "https://cloud.r-project.org"
database <- available.packages(repos = repository, type = "source",
  fields = c("Maintainer", "License"), filters = "duplicates")
stopifnot(nrow(database) > 1000L)
candidate <- read.dcf("DESCRIPTION")
candidate_row <- matrix(NA_character_, 1L, ncol(database),
  dimnames = list("dsld", colnames(database)))
common_fields <- intersect(colnames(candidate), colnames(database))
candidate_row[1L, common_fields] <- candidate[1L, common_fields]
database <- rbind(database[rownames(database) != "dsld", , drop = FALSE], candidate_row)
strong <- tools::package_dependencies("dsld", database,
  which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE)[[1L]]
direct <- tools::package_dependencies("dsld", database,
  which = c("Depends", "Imports", "LinkingTo", "Suggests"))[[1L]]
builtin <- rownames(installed.packages(priority = c("base", "recommended")))
required <- setdiff(union(strong, direct), c("R", builtin))
missing <- setdiff(required, rownames(database))
stopifnot(!length(missing))
manifest <- database[sort(required), c("Package", "Version", "Maintainer", "License"), drop = FALSE]
write.table(manifest, file.path(output_directory, "cran-dependencies.tsv"),
  sep = "\t", quote = FALSE, row.names = FALSE)
orphaned <- manifest[grepl("ORPHANED", manifest[, "Maintainer"], ignore.case = TRUE), "Package"]
stopifnot(!length(orphaned))
reverse_strong <- tools::package_dependencies("dsld", database,
  which = c("Depends", "Imports", "LinkingTo"), reverse = TRUE, recursive = TRUE)[[1L]]
reverse_all <- tools::package_dependencies("dsld", database,
  which = c("Depends", "Imports", "LinkingTo", "Suggests"), reverse = TRUE)[[1L]]
cat("CRAN source metadata retrieved:", format(Sys.time(), tz = "UTC"), "UTC\n")
cat("Repository:", repository, "\n")
cat("Available, non-orphaned packages in checked closure:", length(required), "\n")
cat("Recursive strong reverse dependencies:", paste(reverse_strong, collapse = ", "), "\n")
cat("Direct reverse dependencies including Suggests:", paste(reverse_all, collapse = ", "), "\n")
cat("Scope: current CRAN only; not archived packages, Bioconductor, or private users.\n")
