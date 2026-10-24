# Internal utilities shared by the R functions and Python interfaces.

getSuggestedLib <- function(pkgName) {
   if (!requireNamespace(pkgName,quietly=TRUE))
      stop("Package '", pkgName, "' is required for this function. Install it with install.packages('",
           pkgName, "').", call. = FALSE)
}

pr2file <- function(filename)
{
   origdev <- dev.cur()
   parts <- strsplit(filename,".",fixed=TRUE)
   nparts <- length(parts[[1]])
   suff <- parts[[1]][nparts]
   if (suff == "pdf") {
       pdf(filename)
   }
   else if (suff == "png") {
       png(filename,bg='white')
   }
   else jpeg(filename)
   devnum <- dev.cur()
   dev.set(origdev)
   dev.copy(which = devnum)
   dev.set(devnum)
   dev.off()
   dev.set(origdev)
}

# generates a "cartesian product" of factor levels from input factors
cartFactorLvls <- function(factorNames)
{
   theLevels <- lapply(factorNames,function(fName) levels(get(fName)))
   expand.grid(theLevels)
}

## needed for dsldLinear, dsldLogit -------------------------------------------
### selects 5 rows for comparison across each level of the sensitive variable
### randomly if the user doesn't supply data in the interactions case

dsldGetRow5 <- function(data, yName, sName) {
  row_indices <- sample.int(nrow(data), min(5L, nrow(data)))
  predictor_names <- setdiff(names(data), c(yName, sName))
  message("No sComparisonPts supplied; using rows: ",
          paste(row_indices, collapse = ", "))
  data[row_indices, predictor_names, drop = FALSE]
}

## needed for: python interfaces ----------------------------------------------
### convert data to factors and numeric as per user input
convert_cols <- function(data, cat_features = character(), num_features = character()) {
  if (length(intersect(cat_features, num_features))) stop("A column cannot be both categorical and numeric.")
  requested_columns <- c(cat_features, num_features)
  if (length(requested_columns)) .dsld_check_data(data, requested_columns)
  # If both vectors are missing or empty, return original data unchanged
  if ((missing(cat_features) || length(cat_features) == 0) &&
      (missing(num_features) || length(num_features) == 0)) {
    return(data)
  }

  data[] <- lapply(names(data), function(col) {
    if (col %in% cat_features) {
      factor(data[[col]])
    } else if (col %in% num_features) {
      as.numeric(if (is.factor(data[[col]])) as.character(data[[col]]) else data[[col]])
    } else {
      data[[col]]
    }
  })

  data
}

# Preserve the training encoding, including ordered and custom contrasts.
factor_levels <- function(data) {
  stopifnot(is.data.frame(data))
  factor_names <- names(Filter(is.factor, data))
  setNames(lapply(factor_names, function(name) {
    values <- levels(data[[name]])
    attr(values, "ordered") <- is.ordered(data[[name]])
    attr(values, "contrasts") <- attr(data[[name]], "contrasts")
    values
  }), factor_names)
}

### applies factor levels from each factor in dataset
apply_factor_levels <- function(test_data, train_levels, quiet = TRUE,
                                restore_contrasts = TRUE) {
  stopifnot(is.data.frame(test_data), is.list(train_levels))
  cols <- intersect(names(train_levels), names(test_data))
  if (!quiet) {
    skipped <- setdiff(names(train_levels), names(test_data))
    if (length(skipped)) message("Skipping missing columns: ", paste(skipped, collapse = ", "))
  }

  out <- test_data
  for (nm in cols) {
    levs <- train_levels[[nm]]
    v <- out[[nm]]
    v_chr <- as.character(v)                   # works for factor/char/anything coercible
    fac <- factor(v_chr, levels = levs,
                  ordered = isTRUE(attr(levs, "ordered"))) # unseen -> NA
    if (restore_contrasts) attr(fac, "contrasts") <- attr(levs, "contrasts")
    if (!quiet && all(is.na(fac))) {
      warning("Column '", nm, "' became all NA after applying training levels.")
    }
    out[[nm]] <- fac
  }
  out
}

## needed for fairML and EDF-Fair functions -----------------------------------
### converts integer cols to numeric and character cols to factors
toNumericFactor <- function(data) {
  stopifnot(is.data.frame(data))
  data[] <- lapply(data, function(column) {
    if (is.integer(column)) as.numeric(column)
    else if (is.character(column)) factor(column)
    else column
  })
  data
}

### computes correlation between predictions and one or more sensitive attributes
s_correlations <- function(data, sNames, predictions,
                           method = "pearson", sort_by_abs = TRUE) {
  stopifnot(is.data.frame(data))
  if (length(sNames) == 1L && grepl(",", sNames, fixed = TRUE)) {
    sNames <- trimws(strsplit(sNames, ",", fixed = TRUE)[[1]])
  }
  sNames <- unique(as.character(sNames[!is.na(sNames) & nzchar(sNames)]))
  if (length(predictions) != nrow(data)) {
    stop("`predictions` length (", length(predictions),
         ") must equal nrow(data) (", nrow(data), ").")
  }
  prediction_values <- as.numeric(predictions)
  indicator_blocks <- list()
  for (sensitive_name in sNames) {
    if (!sensitive_name %in% names(data)) {
      warning("Skipping missing column: ", sensitive_name, call. = FALSE)
      next
    }
    sensitive_values <- data[[sensitive_name]]
    if (is.character(sensitive_values)) sensitive_values <- factor(sensitive_values)
    if (is.factor(sensitive_values)) {
      # Equality preserves missing rows and unused levels. model.matrix() can
      # silently omit NA rows, misaligning the indicators with predictions.
      group_levels <- levels(sensitive_values)
      if (!length(group_levels)) next
      indicators <- matrix(vapply(group_levels, function(group) {
        as.numeric(sensitive_values == group)
      }, numeric(nrow(data))), nrow = nrow(data), ncol = length(group_levels))
      colnames(indicators) <- paste0(sensitive_name, "==", group_levels)
      indicator_blocks[[sensitive_name]] <- indicators
    } else if (is.numeric(sensitive_values) || is.logical(sensitive_values)) {
      indicator_blocks[[sensitive_name]] <- matrix(as.numeric(sensitive_values),
        ncol = 1L, dimnames = list(NULL, sensitive_name))
    } else {
      warning("Skipping unsupported type for ", sensitive_name, call. = FALSE)
    }
  }
  if (!length(indicator_blocks)) {
    return(data.frame(feature = character(), correlation = numeric()))
  }
  indicators <- do.call(cbind, indicator_blocks)
  correlations <- vapply(seq_len(ncol(indicators)), function(column) {
    valid <- is.finite(prediction_values) & is.finite(indicators[, column])
    if (sum(valid) < 2L || length(unique(prediction_values[valid])) < 2L ||
        length(unique(indicators[valid, column])) < 2L) return(NA_real_)
    cor(prediction_values[valid], indicators[valid, column], method = method)
  }, numeric(1))
  names(correlations) <- colnames(indicators)
  if (isTRUE(sort_by_abs)) {
    correlations <- correlations[order(abs(correlations), decreasing = TRUE,
                                       na.last = TRUE)]
  }
  data.frame(feature = names(correlations), correlation = unname(correlations),
             row.names = NULL)
}

# Shared validation keeps failures close to the caller rather than in a backend.
.dsld_check_data <- function(data, columns, complete = FALSE) {
  if (!is.data.frame(data) || !nrow(data)) stop("data must be a nonempty data frame.")
  if (anyDuplicated(names(data))) stop("data must have unique column names.")
  if (!is.character(columns) || !length(columns) || anyNA(columns) ||
      any(!nzchar(columns))) stop("Column names must be nonempty character strings.")
  missing_columns <- setdiff(columns, names(data))
  if (length(missing_columns)) stop("Missing column(s): ", paste(missing_columns, collapse = ", "))
  if (complete && (anyNA(data) || any(vapply(data, function(column) {
    is.numeric(column) && any(!is.finite(column))
  }, logical(1))))) stop("Model data must contain complete, finite values.")
  invisible(NULL)
}

.dsld_binary_response <- function(response, yesYVal) {
  if (length(yesYVal) != 1L || is.na(yesYVal)) {
    stop("Provide one non-missing yesYVal for a binary response.")
  }
  observed <- unique(as.character(response[!is.na(response)]))
  if (length(observed) != 2L || !as.character(yesYVal) %in% observed) {
    stop("The response must have two observed values, including yesYVal.")
  }
  as.integer(response == yesYVal)
}

.dsld_positive_probability <- function(predictions, positive_class = "1") {
  probabilities <- if (is.list(predictions)) predictions$probs else predictions
  if (is.null(probabilities)) stop("The model did not return class probabilities.")
  if (!is.null(dim(probabilities))) {
    if (ncol(probabilities) == 1L) probabilities <- probabilities[, 1L]
    else {
      if (!positive_class %in% colnames(probabilities)) {
        stop("Cannot identify positive-class probability column: ", positive_class)
      }
      probabilities <- probabilities[, positive_class]
    }
  }
  as.numeric(probabilities)
}

.dsld_check_deweights <- function(deweightPars, predictor_names) {
  if (is.null(deweightPars)) return(invisible(NULL))
  if (!is.list(deweightPars) || is.null(names(deweightPars)) ||
      any(!nzchar(names(deweightPars))) || anyDuplicated(names(deweightPars))) {
    stop("deweightPars must be a named list with unique predictor names.")
  }
  if (length(setdiff(names(deweightPars), predictor_names))) {
    stop("deweightPars may name only predictor columns, excluding Y and S.")
  }
  valid <- vapply(deweightPars, function(weight) {
    is.numeric(weight) && length(weight) == 1L && is.finite(weight) && weight >= 0
  }, logical(1))
  if (!all(valid)) stop("Deweights must be finite, nonnegative numbers.")
  invisible(NULL)
}

# Build formulas from literal column names rather than parsing user-supplied
# names as R expressions. This preserves spaces, operators, and backticks.
.dsld_formula <- function(response, predictors = NULL, intercept = TRUE) {
  right_side <- if (is.null(predictors)) quote(.) else if (length(predictors)) {
    Reduce(function(left, right) call("+", left, right), lapply(predictors, as.name))
  } else 1L
  if (!intercept) right_side <- call("-", right_side, 1L)
  stats::as.formula(call("~", as.name(response), right_side), env = parent.frame())
}

# Use the fitted model's terms and contrasts instead of reconstructing dummy
# column names by removing punctuation. This also respects reordered inputs.
.dsld_design_matrix <- function(model, new_data) {
  # The fitted model supplies contrasts explicitly below. Avoid model.frame's
  # warning when xlev rebuilds factors carrying redundant contrast attributes.
  for (name in names(Filter(is.factor, new_data))) attr(new_data[[name]], "contrasts") <- NULL
  model_terms <- stats::delete.response(stats::terms(model))
  model_frame <- stats::model.frame(model_terms, new_data, na.action = stats::na.pass,
                                    xlev = model$xlevels)
  stats::model.matrix(model_terms, model_frame, contrasts.arg = model$contrasts)
}

.dsld_lm_prediction <- function(model, new_data, covariance) {
  design <- .dsld_design_matrix(model, new_data)
  design <- design[, names(coef(model)), drop = FALSE]
  if (anyNA(coef(model))) stop("Predictions with standard errors require a full-rank fitted model.")
  standard_errors <- sqrt(pmax(0, rowSums((design %*% covariance) * design)))
  data.frame(row = seq_len(nrow(new_data)), prediction = as.numeric(predict(model, new_data)),
             standardError = standard_errors)
}

.dsld_factor_contrasts <- function(object, covariance) {
  model <- object[[1L]]$model
  sensitive_name <- object[[1L]]$sName
  group_levels <- model$xlevels[[sensitive_name]]
  if (length(group_levels) < 2L) stop("Group comparisons require at least two factor levels.")
  reference_rows <- stats::model.frame(model)[rep(1L, length(group_levels)), , drop = FALSE]
  reference_rows[[sensitive_name]] <- factor(group_levels, levels = group_levels,
    ordered = is.ordered(reference_rows[[sensitive_name]]))
  design <- .dsld_design_matrix(model, reference_rows)
  pairs <- combn(seq_along(group_levels), 2L)
  # Retain the established binary direction (second minus first).
  if (length(group_levels) == 2L) pairs <- pairs[2:1, , drop = FALSE]
  contrasts <- design[pairs[1L, ], , drop = FALSE] - design[pairs[2L, ], , drop = FALSE]
  contrasts <- contrasts[, names(coef(model)), drop = FALSE]
  if (anyNA(coef(model))) stop("Group comparisons require a full-rank fitted model.")
  estimates <- as.vector(contrasts %*% coef(model))
  standard_errors <- sqrt(pmax(0, rowSums((contrasts %*% covariance) * contrasts)))
  result <- data.frame(comparison = paste(group_levels[pairs[1L, ]], group_levels[pairs[2L, ]], sep = " - "),
    estimate = estimates, standardError = standard_errors,
    pValue = 2 * pnorm(abs(estimates / standard_errors), lower.tail = FALSE))
  names(result) <- c("Factors Compared", "Estimates", "Standard Errors", "P-Value")
  result
}

.dsld_group_differences <- function(predictions, probabilities = FALSE) {
  comparisons <- list()
  for (row_number in unique(predictions$row)) {
    row_predictions <- predictions[predictions$row == row_number, , drop = FALSE]
    pairs <- combn(seq_len(nrow(row_predictions)), 2L)
    for (pair in seq_len(ncol(pairs))) {
      first <- row_predictions[pairs[1L, pair], , drop = FALSE]
      second <- row_predictions[pairs[2L, pair], , drop = FALSE]
      comparison <- data.frame(comparison = paste(first$level, second$level, sep = " - "),
        row = row_number, estimate = first$prediction - second$prediction,
        standardError = sqrt(first$standardError^2 + second$standardError^2))
      if (probabilities) {
        comparison <- data.frame(comparison = comparison$comparison, row = row_number,
          first = first$prediction, second = second$prediction,
          difference = comparison$estimate, standardError = comparison$standardError)
        names(comparison) <- c("Factors Compared", "New Data Row", "Factor A", "Factor B",
                               "Difference in Estimates", "Standard Errors")
      } else names(comparison) <- c("Factors Compared", "New Data Row", "Estimates", "Standard Errors")
      comparisons[[length(comparisons) + 1L]] <- comparison
    }
  }
  do.call(rbind, comparisons)
}
