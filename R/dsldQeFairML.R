# Explicitly deweighted features (EDF): the sensitive columns are excluded
# from model fitting. Shared preprocessing is reused unchanged at prediction.
qeFairBase <- function(qeFUNC, data, yName, sNames, scaling, ...,
                       appendedItems = list()) {
  training_data <- fairScale(data, yName, sNames, scaling)
  if (length(appendedItems$distanceWeights)) {
    for (predictor in names(appendedItems$distanceWeights)) {
      training_data[[predictor]] <- training_data[[predictor]] * appendedItems$distanceWeights[[predictor]]
    }
  }
  base <- qeFUNC(training_data, yName, ...)
  model <- c(list(base = base), appendedItems)
  model$scalePars <- attr(training_data, "scalePars")
  model$FactorsInfo <- factor_levels(data)
  model$predictorNames <- setdiff(names(data), c(yName, sNames))
  model$encodedNames <- setdiff(names(training_data), yName)
  class(model) <- "dsldQeFair"
  model
}

.dsld_edf_data <- function(data, yName, sNames, deweightPars, yesYVal = NULL) {
  .dsld_check_data(data, c(yName, sNames), complete = TRUE)
  data <- toNumericFactor(data)
  predictor_names <- setdiff(names(data), c(yName, sNames))
  if (!length(predictor_names)) stop("At least one predictor other than Y and S is required.")
  .dsld_check_deweights(deweightPars, predictor_names)
  if (is.factor(data[[yName]]) || !is.null(yesYVal)) {
    .dsld_binary_response(data[[yName]], yesYVal)
    data[[yName]] <- droplevels(factor(data[[yName]]))
  } else if (!is.numeric(data[[yName]])) stop("Y must be numeric or a binary factor.")
  data
}

.dsld_edf_metrics <- function(model, data) {
  model$trainPreds <- predict.dsldQeFair(model,
    data[, setdiff(names(data), model$yName), drop = FALSE])
  if (isTRUE(model$classif)) {
    probabilities <- .dsld_positive_probability(model$trainPreds$preds)
    model$trainAcc <- mean(as.integer(data[[model$yName]] == model$yesYVal) !=
                            as.integer(probabilities > 0.5))
  } else {
    probabilities <- as.numeric(model$trainPreds$preds)
    model$trainAcc <- mean(abs(probabilities - data[[model$yName]]))
  }
  model$trainCorrs <- s_correlations(data, model$sNames, probabilities)
  model
}

dsldQeFairRF <- function(data, yName, sNames, deweightPars = NULL, nTree = 500,
                         minNodeSize = 10, mtry = floor(sqrt(ncol(data))),
                         yesYVal = NULL) {
  data <- .dsld_edf_data(data, yName, sNames, deweightPars, yesYVal)
  scaling <- FALSE
  classif <- is.factor(data[[yName]])
  expanded_weights <- expandDeweights(deweightPars, data[1L, , drop = FALSE])
  encoded_names <- setdiff(names(fairScale(data, yName, sNames)), yName)
  if (missing(mtry)) mtry <- min(mtry, length(encoded_names))
  weights <- setNames(rep(1, length(encoded_names)), encoded_names)
  weights[names(expanded_weights)] <- unlist(expanded_weights)
  if (!any(weights > 0)) stop("At least one predictor must have a positive RF weight.")
  model <- qeFairBase(qeML::qeRFranger, data, yName, sNames, scaling,
    nTree = nTree, minNodeSize = minNodeSize, mtry = mtry,
    yesYVal = yesYVal, holdout = NULL, deweightPars = expanded_weights,
    appendedItems = variablesAsList(yName, sNames, scaling, deweightPars, yesYVal, classif))
  model$func_call <- "dsldQeFairRF"
  if (classif) model$yName_info <- levels(data[[yName]])
  .dsld_edf_metrics(model, data)
}

dsldQeFairKNN <- function(data, yName, sNames, deweightPars = NULL,
                          yesYVal = NULL, k = 25, scaleX = TRUE) {
  data <- .dsld_edf_data(data, yName, sNames, deweightPars, yesYVal)
  if (length(k) != 1L || !is.finite(k) || k < 1L || k != as.integer(k) || k >= nrow(data)) {
    stop("k must be a positive integer smaller than the training sample size.")
  }
  scaling <- scaleX
  classif <- is.factor(data[[yName]])
  expanded_weights <- expandDeweights(deweightPars, data[1L, , drop = FALSE])
  model <- qeFairBase(qeML::qeKNN, data, yName, sNames, scaling,
    k = k, scaleX = FALSE, yesYVal = yesYVal, holdout = NULL,
    appendedItems = c(variablesAsList(yName, sNames, scaling, deweightPars, yesYVal, classif),
                      list(distanceWeights = expanded_weights)))
  model$func_call <- "dsldQeFairKNN"
  if (classif) model$yName_info <- levels(data[[yName]])
  .dsld_edf_metrics(model, data)
}

qeFairRidgeBase <- function(data, yName, sNames, deweightPars, yesYVal = NULL) {
  classif <- !is.null(yesYVal)
  scaling <- TRUE
  training_data <- ridgeModify(data, yName, sNames, deweightPars, yesYVal)
  scalePars <- attr(training_data, "scalePars")
  intercept_column <- NULL
  if (classif) {
    base <- qeML::qeLogit(training_data, yName, holdout = NULL, yesYVal = yesYVal)
  } else {
    # The penalty rows must not penalize the intercept. An explicit intercept
    # column is 1 for observations and 0 for the appended penalty rows.
    intercept_column <- utils::tail(make.unique(c(names(training_data), ".dsld_intercept")), 1L)
    training_data[[intercept_column]] <- c(rep(1, nrow(data)),
                                          rep(0, nrow(training_data) - nrow(data)))
    model_formula <- .dsld_formula(yName, setdiff(names(training_data), yName),
                                    intercept = FALSE)
    base <- lm(model_formula, data = training_data)
  }
  model <- c(list(base = base),
    variablesAsList(sNames, yName, deweightPars, scaling, scalePars, classif, yesYVal))
  model$FactorsInfo <- factor_levels(data)
  model$predictorNames <- setdiff(names(data), c(yName, sNames))
  model$encodedNames <- setdiff(names(training_data), c(yName, intercept_column))
  model$interceptColumn <- intercept_column
  class(model) <- "dsldQeFair"
  model
}

# Augment the design with one penalty row per encoded predictor. The logistic
# variant retains the original EDF pseudo-observation construction.
ridgeModify <- function(data, yName, sNames, deweightPars, yesYVal = NULL) {
  expanded_weights <- expandDeweights(deweightPars, data[1L, , drop = FALSE])
  training_data <- fairScale(data, yName, sNames, scaling = TRUE)
  predictor_names <- setdiff(names(training_data), yName)
  penalty_weights <- setNames(rep(0, length(predictor_names)), predictor_names)
  if (length(expanded_weights)) {
    penalty_weights[names(expanded_weights)] <- sqrt(unlist(expanded_weights))
  }
  penalty_rows <- as.data.frame(diag(penalty_weights, nrow = length(penalty_weights)), check.names = FALSE)
  names(penalty_rows) <- predictor_names
  if (is.null(yesYVal)) {
    penalty_rows[[yName]] <- 0
  } else {
    negative_label <- setdiff(levels(data[[yName]]), as.character(yesYVal))[1L]
    penalty_rows[[yName]] <- factor(rep(negative_label, nrow(penalty_rows)),
                                     levels = levels(data[[yName]]))
  }
  augmented_data <- rbind(training_data, penalty_rows)
  attr(augmented_data, "scalePars") <- attr(training_data, "scalePars")
  augmented_data
}

dsldQeFairRidgeLin <- function(data, yName, sNames, deweightPars = NULL) {
  data <- .dsld_edf_data(data, yName, sNames, deweightPars)
  if (!is.numeric(data[[yName]])) stop("RidgeLin requires a numeric response.")
  model <- qeFairRidgeBase(data, yName, sNames, deweightPars)
  .dsld_edf_metrics(model, data)
}

dsldQeFairRidgeLog <- function(data, yName, sNames, deweightPars = NULL, yesYVal) {
  data <- .dsld_edf_data(data, yName, sNames, deweightPars, yesYVal)
  model <- qeFairRidgeBase(data, yName, sNames, deweightPars, yesYVal)
  model$func_call <- "dsldQeFairRidgeLog"
  model$yName_info <- levels(data[[yName]])
  .dsld_edf_metrics(model, data)
}

predict.dsldQeFair <- function(object, newx, ...) {
  predictor_names <- object$predictorNames
  if (is.null(predictor_names)) predictor_names <- setdiff(names(newx), c(object$sNames, object$yName))
  .dsld_check_data(newx, predictor_names)
  predictors <- apply_factor_levels(newx[, predictor_names, drop = FALSE], object$FactorsInfo)
  .dsld_check_data(predictors, predictor_names, complete = TRUE)
  predictors <- scaleNewX(predictors, object$scaling, object$scalePars)
  if (!is.null(object$encodedNames)) predictors <- predictors[, object$encodedNames, drop = FALSE]
  if (!is.null(object$interceptColumn)) predictors[[object$interceptColumn]] <- 1
  if (length(object$distanceWeights)) {
    for (predictor in names(object$distanceWeights)) {
      predictors[[predictor]] <- predictors[[predictor]] * object$distanceWeights[[predictor]]
    }
  }
  predictions <- predict(object$base, predictors)
  if (is.list(predictions)) {
    probabilities <- .dsld_positive_probability(predictions)
    if (!is.null(object$yesYVal) && !is.null(object$yName_info)) {
      negative_label <- setdiff(object$yName_info, as.character(object$yesYVal))[1L]
      predictions$predClasses <- factor(ifelse(probabilities > 0.5, object$yesYVal, negative_label),
                                         levels = object$yName_info)
    }
  } else {
    probabilities <- as.numeric(predictions)
  }
  list(preds = predictions, correlations = s_correlations(newx, object$sNames, probabilities))
}

fairScale <- function(data, yName, sNames, scaling = FALSE) {
  predictors <- data[, setdiff(names(data), c(sNames, yName)), drop = FALSE]
  predictors <- as.data.frame(regtools::factorsToDummies(predictors, omitLast = TRUE))
  scale_parameters <- NULL
  if (scaling) {
    centers <- vapply(predictors, mean, numeric(1))
    scales <- vapply(predictors, stats::sd, numeric(1))
    scales[!is.finite(scales) | scales == 0] <- 1
    predictors <- as.data.frame(scale(predictors, center = centers, scale = scales))
    scale_parameters <- list(center = centers, scale = scales)
  }
  predictors[[yName]] <- data[[yName]]
  attr(predictors, "scalePars") <- scale_parameters
  predictors
}

scaleNewX <- function(newx, scaling = FALSE, scalePars = NULL) {
  predictors <- as.data.frame(regtools::factorsToDummies(newx, omitLast = TRUE))
  if (scaling) {
    if (is.null(scalePars)) stop("Training scale parameters are required for prediction.")
    predictors <- predictors[, names(scalePars$center), drop = FALSE]
    predictors <- as.data.frame(scale(predictors, center = scalePars$center, scale = scalePars$scale))
  }
  predictors
}

expandDeweights <- function(deweightPars, row1) {
  expanded_weights <- list()
  for (predictor in names(deweightPars)) {
    if (is.factor(row1[[predictor]])) {
      dummy_names <- names(data.frame(regtools::factorToDummies(row1[[predictor]],
                                                               predictor, omitLast = TRUE)))
      expanded_weights <- c(expanded_weights,
        setNames(rep(deweightPars[predictor], length(dummy_names)), dummy_names))
    } else expanded_weights <- c(expanded_weights, deweightPars[predictor])
  }
  expanded_weights
}

variablesAsList <- function(...) {
  variable_names <- vapply(as.list(substitute(list(...)))[-1L], as.character, character(1))
  setNames(list(...), variable_names)
}
