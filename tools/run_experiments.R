# Held-out experiments and paper-example grids for release validation.
# Rscript tools/run_experiments.R /path/to/results [--full-paper]
suppressPackageStartupMessages(library(dsld))
arguments <- commandArgs(trailingOnly = TRUE)
output_directory <- if (length(arguments)) arguments[1L] else tempfile("dsld-experiments-")
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
full_paper <- "--full-paper" %in% arguments
set.seed(2026)
results <- list()
failures <- character()
run_check <- function(label, code) {
  tryCatch({ force(code); cat("PASS:", label, "\n") }, error = function(error) {
    failures <<- c(failures, paste(label, conditionMessage(error), sep = ": "))
    cat("FAIL:", tail(failures, 1L), "\n")
  })
}
assert_close <- function(actual, expected) {
  stopifnot(isTRUE(all.equal(as.numeric(actual), as.numeric(expected), tolerance = 1e-8)))
}
positive_probability <- function(prediction) {
  if (!is.list(prediction)) return(as.numeric(prediction))
  probabilities <- prediction$probs
  if (is.matrix(probabilities)) {
    probabilities <- if (ncol(probabilities) == 1L) probabilities[, 1L] else probabilities[, "1"]
  }
  as.numeric(probabilities)
}

data(svcensus, package = "dsld")
data(compas1, package = "dsld")
# Wages are measured in thousands of dollars in this experiment only.
wages <- svcensus[c("age", "wkswrkd", "occ", "educ", "gender", "wageinc")]
wages$wageinc <- wages$wageinc / 1000
records <- droplevels(subset(compas1, race %in% c("African-American", "Caucasian")))
records <- records[c("age", "priors_count", "decile_score", "sex", "race", "two_year_recid")]
regression_models <- c("dsldQeFairKNN", "dsldQeFairRF", "dsldQeFairRidgeLin",
                        "dsldFrrm", "dsldNclm", "dsldZlm")
classification_models <- c("dsldQeFairKNN", "dsldQeFairRF", "dsldQeFairRidgeLog",
                            "dsldFgrrm", "dsldZlrm")
for (seed in c(2026, 4101, 7303)) {
  for (classification in c(FALSE, TRUE)) {
    set.seed(seed)
    frame <- if (classification) records else wages
    selected <- sample.int(nrow(frame), 600)
    frame <- frame[selected, , drop = FALSE]
    train_rows <- sample.int(nrow(frame), 450)
    test_rows <- setdiff(seq_len(nrow(frame)), train_rows)
    stopifnot(!length(intersect(train_rows, test_rows)))
    training <- frame[train_rows, , drop = FALSE]
    testing <- frame[test_rows, , drop = FALSE]
    y_name <- if (classification) "two_year_recid" else "wageinc"
    s_name <- if (classification) "race" else "gender"
    proxy <- if (classification) "decile_score" else "occ"
    predictor_names <- setdiff(names(frame), y_name)
    models <- if (classification) classification_models else regression_models
    for (model_name in models) {
      for (setting in c("baseline", "constrained")) {
        label <- paste(seed, if (classification) "classification" else "regression", model_name, setting)
        run_check(label, {
          set.seed(seed)
          call_args <- list(data = training, yName = y_name)
          if (startsWith(model_name, "dsldQe")) {
            call_args$sNames <- s_name
            weight <- if (grepl("Ridge", model_name)) {
              if (setting == "baseline") 0 else 10
            } else if (setting == "baseline") 1 else .2
            call_args$deweightPars <- setNames(list(weight), proxy)
          } else {
            call_args$sName <- s_name
            call_args$unfairness <- if (setting == "baseline") 1 else .2
          }
          if (classification) call_args$yesYVal <- "Yes"
          if (model_name == "dsldQeFairKNN") call_args$k <- 15
          if (model_name == "dsldQeFairRF") call_args$nTree <- 80
          fitted <- do.call(model_name, call_args)
          predicted <- predict(fitted, testing[, predictor_names, drop = FALSE])
          values <- positive_probability(predicted$preds)
          stopifnot(length(values) == nrow(testing), all(is.finite(values)))
          if (classification) {
            stopifnot(all(values >= 0 & values <= 1))
            error <- mean((values > .5) != (testing[[y_name]] == "Yes"))
          } else error <- mean(abs(values - testing[[y_name]]))
          # Reordering columns cannot change predictions.
          reordered <- predict(fitted, testing[, rev(predictor_names), drop = FALSE])
          assert_close(values, positive_probability(reordered$preds))
          # Independently calculate positive-group correlation.
          group <- levels(testing[[s_name]])[1L]
          expected <- cor(values, as.numeric(testing[[s_name]] == group))
          actual <- predicted$correlations$correlation[
            predicted$correlations$feature == paste0(s_name, "==", group)]
          assert_close(actual, expected)
          results[[length(results) + 1L]] <- data.frame(seed = seed,
            dataset = if (classification) "compas1" else "svcensus",
            model = model_name, setting = setting, n_train = nrow(training),
            n_test = nrow(testing), error = error,
            metric = if (classification) "misclassification" else "MAE_thousand_dollars",
            max_abs_correlation = max(abs(predicted$correlations$correlation), na.rm = TRUE))
        })
      }
    }
  }
}
if (length(results)) write.csv(do.call(rbind, results), file.path(output_directory, "heldout-results.csv"), row.names = FALSE)

# Cover all bundled datasets with an independently checked group model.
for (name in c("lak", "mortgageSE")) {
  run_check(paste("bundled dataset", name), {
    bundled <- new.env(); data(list = name, package = "dsld", envir = bundled)
    frame <- bundled[[name]]
    if (name == "lak") {
      frame <- frame[c("education", "yearsexp", "race", "call")]
      y_name <- "call"; s_name <- "race"
    } else {
      frame <- frame[c("p_irat", "loan_val", "black", "deny")]
      y_name <- "deny"; s_name <- "black"
    }
    positive <- levels(frame[[y_name]])[2L]
    fit <- dsldLogit(frame, y_name, s_name, yesYVal = positive)
    reference <- frame; reference[[y_name]] <- as.integer(frame[[y_name]] == positive)
    reference_fit <- glm(reformulate(setdiff(names(frame), y_name), y_name),
                          data = reference, family = binomial())
    assert_close(predict(fit, frame)$prediction, predict(reference_fit, frame, type = "response"))
  })
}
if (full_paper) {
  # Section 4.4 of arXiv:2411.04228v3: execute the published grid forms on
  # complete bundled datasets with an explicit seed. Corrected metrics need
  # not reproduce the article's historical numerical tables.
  run_check("paper COMPAS five-fold grid", {
    set.seed(2026)
    result <- dsldFairUtils(compas1, "two_year_recid", "race", "dsldFgrrm",
      unfairness = c(.9, .6, .1, .05, .005), yesYVal = "Yes", k_folds = 5)
    stopifnot(nrow(result) == 5L, all(is.finite(result$testAcc)))
    write.csv(result, file.path(output_directory, "paper-compas-grid.csv"), row.names = FALSE)
  })
  run_check("paper census five-fold grid", {
    set.seed(2026)
    result <- dsldFairUtils(svcensus, "wageinc", c("gender", "age"), "dsldQeFairKNN",
      deweightPars = list(occ = c(.9, .8, .5, .3, .1, .05, .01)), k_folds = 5,
      model_args = list(k = 25))
    stopifnot(nrow(result) == 7L, all(is.finite(result$testAcc)))
    write.csv(result, file.path(output_directory, "paper-census-grid.csv"), row.names = FALSE)
  })
}
writeLines(capture.output(sessionInfo()), file.path(output_directory, "session-info.txt"))
cat("Completed", length(results), "held-out fits;", length(failures), "failed checks.\n")
if (length(failures)) stop(paste(failures, collapse = "\n"), call. = FALSE)
