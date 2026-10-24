### dsld fairML wrappers

# base function for fairML wrappers --- they all follow the same format:
# converts the data into a format that the fairml models accept
# then puts the fairml model inside an object of the dsldFairML class which
# has its own predict function

fairmlBase <- function(fairmlFUNC, data, yName, sName, unfairness, ...) {

  .dsld_check_data(data, c(yName, sName), complete = TRUE)
  # data-prep
  data <- toNumericFactor(data)
  response <- data[,yName]
  predictors <- data[, !colnames(data) %in% c(yName, sName), drop = FALSE]
  sensitive <- data[, sName, drop = FALSE]

  # calls a fairml model function as the base for the dsldFairML object
  base <- fairmlFUNC(response = response, predictors = predictors,
                       sensitive = sensitive, unfairness = as.numeric(unfairness), ...)

  # save yName and sName to use in predict()
  model <- list(
    base        = base,
    yName       = yName,
    sName       = sName,
    predictorNames = setdiff(names(data), c(yName, sName)),
    FactorsInfo = factor_levels(data)
  )

  class(model) <- c("dsldFairML")
  model
}

# wrapper for Frrm()
dsldFrrm <- function(data, yName, sName, unfairness,
                     definition = "sp-komiyama", lambda = 0,
                     save.auxiliary = FALSE) {

  data <- toNumericFactor(data)

  model <- fairmlBase(fairml::frrm, data, yName, sName, unfairness,
                       definition = definition, lambda = as.numeric(lambda),
                       save.auxiliary = save.auxiliary)

  # training preds/corrs
  predictors <- data[, !colnames(data) %in% c(yName, sName), drop = FALSE]
  sensitive <- data[, sName, drop = FALSE]
  model$trainPreds <- predict(model$base, predictors, sensitive)
  model$trainAcc <- mean(abs(model$trainPreds - data[[yName]]))
  model$trainCorrs <- s_correlations(data, sName, model$trainPreds)
  model
}

# wrapper for Fgrrm()
dsldFgrrm <- function(data, yName, sName, unfairness,
                      definition = "sp-komiyama", family = "binomial",
                      lambda = 0, save.auxiliary = FALSE, yesYVal) {

  data <- toNumericFactor(data)
  data[[yName]] <- factor(.dsld_binary_response(data[[yName]], yesYVal), levels = c(0, 1))

  model <- fairmlBase(fairml::fgrrm, data, yName, sName, unfairness,
                        definition = definition, family = family, lambda = as.numeric(lambda),
                       save.auxiliary = save.auxiliary)

  # training preds/corrs
  predictors <- data[, !colnames(data) %in% c(yName, sName), drop = FALSE]
  sensitive <- data[, sName, drop = FALSE]
  model$trainPreds <- predict(model$base, predictors, sensitive)
  observed_class <- as.integer(data[[yName]] == 1)
  model$trainAcc <- mean(observed_class != (model$trainPreds > 0.5))
  model$trainCorrs <- s_correlations(data, sName, model$trainPreds)
  model
}

# wrapper for Nclm()
dsldNclm <- function(data, yName, sName, unfairness, covfun = cov,
                     lambda = 0, save.auxiliary = FALSE) {

  getSuggestedLib('cccp')
  data <- toNumericFactor(data)

  model <- fairmlBase(fairml::nclm, data, yName, sName, unfairness, covfun = covfun,
                       lambda = as.numeric(lambda), save.auxiliary = save.auxiliary)

  # training preds/corrs
  predictors <- data[, !colnames(data) %in% c(yName, sName), drop = FALSE]
  sensitive <- data[, sName, drop = FALSE]
  model$trainPreds <- predict(model$base, predictors, sensitive)
  model$trainAcc <- mean(abs(model$trainPreds - data[[yName]]))
  model$trainCorrs <- s_correlations(data, sName, model$trainPreds)
  model
}

# wrapper for Zlm()
dsldZlm <- function(data, yName, sName, unfairness) {

  getSuggestedLib('CVXR')
  data <- toNumericFactor(data)

  model <- fairmlBase(fairml::zlm, data, yName, sName, unfairness)

  # training preds/corrs
  predictors <- data[, !colnames(data) %in% c(yName, sName), drop = FALSE]
  model$trainPreds <- predict(model$base, predictors)
  model$trainAcc <- mean(abs(model$trainPreds - data[[yName]]))
  model$trainCorrs <- s_correlations(data, sName, model$trainPreds)
  model
}

# wrapper for Zlrm()
dsldZlrm <- function(data, yName, sName, unfairness, yesYVal) {

  getSuggestedLib('CVXR')
  data <- toNumericFactor(data)
  data[[yName]] <- factor(.dsld_binary_response(data[[yName]], yesYVal), levels = c(0, 1))

  model <- fairmlBase(fairml::zlrm, data, yName, sName, unfairness)

  # training preds/corrs
  predictors <- data[, !colnames(data) %in% c(yName, sName), drop = FALSE]
  model$trainPreds <- predict(model$base, predictors)
  observed_class <- as.integer(data[[yName]] == 1)
  model$trainAcc <- mean(observed_class != (model$trainPreds > 0.5))
  model$trainCorrs <- s_correlations(data, sName, model$trainPreds)
  model
}

### S3 methods summary() and predict()
summary.dsldFairML <- function(object,...){
  summary(object$base)
}

predict.dsldFairML <- function(object, newx,...) {
  newx <- toNumericFactor(newx)
  newx <- apply_factor_levels(newx, object$FactorsInfo)
  predictor_names <- object$predictorNames
  if (is.null(predictor_names)) {
    predictor_names <- setdiff(names(newx), c(object$yName, object$sName))
  }
  .dsld_check_data(newx, c(predictor_names, object$sName), complete = TRUE)
  predictors <- newx[, predictor_names, drop = FALSE]
  sensitive <- newx[, object$sName, drop = FALSE]
  if (inherits(object$base, c("zlm", "zlrm"))) {
    predictions <- predict(object$base, predictors)
  } else {
    predictions <- predict(object$base, predictors, sensitive)
  }
  list(preds = predictions,
       correlations = s_correlations(newx, object$sName, predictions))
}
