# Cross-validation helpers. Each candidate uses the same held-out rows.
make_folds <- function(n, k = 5, strata = NULL) {
  if (length(k) != 1L || !is.finite(k) || k != as.integer(k) || k < 2L || k > n) {
    stop("k_folds must be an integer between 2 and the number of rows.")
  }
  fold_ids <- integer(n)
  if (is.null(strata)) {
    fold_ids[sample.int(n)] <- rep(seq_len(k), length.out = n)
  } else {
    if (length(strata) != n || anyNA(strata)) stop("Invalid stratification labels.")
    if (any(table(strata) < 2L)) stop("Each response class needs at least two rows for cross-validation.")
    offset <- 0L
    for (group in unique(strata)) {
      indices <- which(strata == group)
      shuffled <- indices[sample.int(length(indices))]
      fold_ids[shuffled] <- (seq_along(indices) - 1L + offset) %% k + 1L
      offset <- (offset + length(indices)) %% k
    }
  }
  split(seq_len(n), factor(fold_ids, levels = seq_len(k)))
}

get_fold_split <- function(data, folds, i) {
  stopifnot(i >= 1L, i <= length(folds))
  test_indices <- folds[[i]]
  list(train = data[-test_indices, , drop = FALSE],
       test = data[test_indices, , drop = FALSE])
}

.filter_model_args <- function(ftn_name, user_args) {
  if (is.null(user_args) || !length(user_args)) return(list())
  if (!is.list(user_args) || is.null(names(user_args)) ||
      any(!nzchar(names(user_args))) || anyDuplicated(names(user_args))) {
    stop("model_args must be a named list with unique argument names.")
  }
  reserved <- c("data", "yName", "sName", "sNames", "unfairness", "deweightPars", "yesYVal")
  if (any(names(user_args) %in% reserved)) {
    stop("model_args cannot override data, column names, response coding, or grid parameters.")
  }
  allowed <- names(formals(match.fun(ftn_name)))
  supported <- names(user_args) %in% allowed
  if (any(!supported)) {
    warning("Ignoring unsupported arg(s) for ", ftn_name, ": ",
            paste(names(user_args)[!supported], collapse = ", "), call. = FALSE)
  }
  user_args[supported]
}

dsldFairUtils <- function(data, yName, sName, dsldFTNName, unfairness = NULL,
                          deweightPars = NULL, yesYVal = NULL, k_folds = 5,
                          model_args = NULL) {
  edf_models <- c("dsldQeFairKNN", "dsldQeFairRF", "dsldQeFairRidgeLin", "dsldQeFairRidgeLog")
  fairml_models <- c("dsldFrrm", "dsldFgrrm", "dsldNclm", "dsldZlm", "dsldZlrm")
  if (length(dsldFTNName) != 1L || !dsldFTNName %in% c(edf_models, fairml_models)) {
    stop("Invalid dsldFTNName specified.")
  }
  .dsld_check_data(data, c(yName, sName), complete = TRUE)
  data <- toNumericFactor(data)
  classification <- is.factor(data[[yName]]) || !is.null(yesYVal)
  if (classification) {
    data[[yName]] <- factor(.dsld_binary_response(data[[yName]], yesYVal), levels = c(0, 1))
    yesYVal <- "1"
  }
  classification_models <- c("dsldQeFairRidgeLog", "dsldFgrrm", "dsldZlrm")
  regression_models <- c("dsldQeFairRidgeLin", "dsldFrrm", "dsldNclm", "dsldZlm")
  if ((classification && dsldFTNName %in% regression_models) ||
      (!classification && dsldFTNName %in% classification_models)) {
    stop("The selected model does not support this response type.")
  }
  is_edf <- dsldFTNName %in% edf_models
  if (is_edf) {
    if (!is.list(deweightPars) || !length(deweightPars) ||
        is.null(names(deweightPars)) || any(!nzchar(names(deweightPars))) ||
        anyDuplicated(names(deweightPars))) stop("Provide deweightPars as a named list of numeric vectors.")
    if (any(!vapply(deweightPars, function(values) {
      is.numeric(values) && length(values) > 0L && all(is.finite(values)) && all(values >= 0)
    }, logical(1)))) stop("Deweight grids must contain finite, nonnegative numbers.")
    parameter_grid <- expand.grid(deweightPars, KEEP.OUT.ATTRS = FALSE,
                                  stringsAsFactors = FALSE)
    .dsld_check_deweights(as.list(parameter_grid[1L, , drop = FALSE]),
                          setdiff(names(data), c(yName, sName)))
  } else {
    if (!is.numeric(unfairness) || !length(unfairness) ||
        any(!is.finite(unfairness)) || any(unfairness <= 0 | unfairness > 1)) {
      stop("Provide unfairness as numbers in (0, 1].")
    }
    parameter_grid <- data.frame(unfairness = unfairness)
  }
  extra_args <- .filter_model_args(dsldFTNName, model_args)
  folds <- make_folds(nrow(data), k_folds,
                      if (classification) data[[yName]] else NULL)
  feature_names <- s_correlations(data, sName, seq_len(nrow(data)))$feature
  results <- vector("list", nrow(parameter_grid))
  for (candidate in seq_len(nrow(parameter_grid))) {
    fold_errors <- numeric(length(folds))
    fold_correlations <- matrix(NA_real_, length(folds), length(feature_names),
                                 dimnames = list(NULL, feature_names))
    for (fold in seq_along(folds)) {
      split_data <- get_fold_split(data, folds, fold)
      train_data <- split_data$train
      test_data <- split_data$test
      call_args <- list(data = train_data, yName = yName)
      if (is_edf) {
        call_args$sNames <- sName
        call_args$deweightPars <- as.list(parameter_grid[candidate, , drop = FALSE])
      } else {
        call_args$sName <- sName
        call_args$unfairness <- parameter_grid$unfairness[candidate]
      }
      if (classification) call_args$yesYVal <- yesYVal
      model <- do.call(dsldFTNName, c(call_args, extra_args))
      prediction <- predict(model, test_data[, setdiff(names(test_data), yName), drop = FALSE])
      if (classification) {
        probabilities <- .dsld_positive_probability(prediction$preds)
        fold_errors[fold] <- mean(as.integer(test_data[[yName]] == yesYVal) !=
                                   as.integer(probabilities > 0.5))
      } else {
        fold_errors[fold] <- mean(abs(prediction$preds - test_data[[yName]]))
      }
      correlations <- prediction$correlations
      common_features <- intersect(feature_names, correlations$feature)
      fold_correlations[fold, common_features] <-
        correlations$correlation[match(common_features, correlations$feature)]
    }
    # Weight prediction errors by held-out sample count. Undefined correlations
    # (constant groups/predictions) stay missing and are not counted as zero.
    test_error <- stats::weighted.mean(fold_errors, lengths(folds))
    mean_correlations <- vapply(seq_along(feature_names), function(column) {
      values <- fold_correlations[, column]
      if (all(is.na(values))) NA_real_ else mean(values, na.rm = TRUE)
    }, numeric(1))
    names(mean_correlations) <- feature_names
    results[[candidate]] <- as.data.frame(c(as.list(parameter_grid[candidate, , drop = FALSE]),
      list(testAcc = test_error), as.list(mean_correlations)), check.names = FALSE)
  }
  result <- do.call(rbind, results)
  rownames(result) <- NULL
  result
}
