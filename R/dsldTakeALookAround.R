# Compare feature sets on a shared holdout so changes in error reflect changes
# in predictors, rather than a new random train/test split for every model.
dsldTakeALookAround <- function(data, yName, sName,
                                maxFeatureSetSize = (ncol(data) - 2),
                                holdout = floor(min(1000, 0.1 * nrow(data)))) {
  .dsld_check_data(data, c(yName, sName), complete = TRUE)
  data <- toNumericFactor(data)
  feature_names <- setdiff(names(data), c(yName, sName))
  if (length(maxFeatureSetSize) != 1L || !is.finite(maxFeatureSetSize) ||
      maxFeatureSetSize < 1L || maxFeatureSetSize != as.integer(maxFeatureSetSize) ||
      maxFeatureSetSize > length(feature_names)) {
    stop("maxFeatureSetSize must be an integer between 1 and the number of available predictors.")
  }
  if (is.null(holdout)) {
    train_indices <- test_indices <- seq_len(nrow(data))
  } else {
    if (length(holdout) != 1L || !is.finite(holdout) || holdout < 1L ||
        holdout >= nrow(data) || holdout != as.integer(holdout)) {
      stop("holdout must be NULL or a positive integer smaller than the number of rows.")
    }
    test_indices <- sample.int(nrow(data), holdout)
    train_indices <- setdiff(seq_len(nrow(data)), test_indices)
  }
  prediction_error <- function(predictor_names, response_name) {
    train_data <- data[train_indices, c(predictor_names, response_name), drop = FALSE]
    test_data <- data[test_indices, predictor_names, drop = FALSE]
    response <- data[[response_name]][test_indices]
    if (is.numeric(response)) {
      model <- qeML::qeLin(train_data, response_name, holdout = NULL)
      mean(abs(predict(model, test_data) - response))
    } else {
      model <- qeML::qeLogit(train_data, response_name, holdout = NULL)
      predictions <- predict(model, test_data)$predClasses
      mean(as.vector(predictions) != as.character(response))
    }
  }
  results <- list()
  for (size in seq_len(maxFeatureSetSize)) {
    feature_sets <- combn(feature_names, size, simplify = FALSE)
    for (features in feature_sets) {
      results[[length(results) + 1L]] <- data.frame(
        "Feature Names" = paste(features, collapse = ","),
        a = prediction_error(features, yName),
        b = prediction_error(c(features, sName), yName),
        c = prediction_error(features, sName), check.names = FALSE)
    }
  }
  do.call(rbind, results)
}
