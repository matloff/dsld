### dsld fairML wrappers 

# base function for fairML wrappers --- they all follow the same format:
# converts the data into a format that the fairml models accept
# then puts the fairml model inside an object of the dsldFairML class which
# has its own predict function

fairmlBase <- function(fairmlFUNC, data, yName, sName, unfairness, ...) {
  
  # data-prep
  data <- toNumericFactor(data)
  response <- data[,yName]
  predictors <- data[,!colnames(data) %in% c(yName,sName)]
  sensitive <- data[,sName]
  
  # calls a fairml model function as the base for the dsldFairML object
  base <- fairmlFUNC(response = response, predictors = predictors, 
                       sensitive = sensitive, unfairness = unfairness, ...)
  
  # save yName and sName to use in predict()
  model <- list(
    base        = base,
    yName       = yName,
    sName       = sName,
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
  
  suppressWarnings({
    model = fairmlBase(fairml::frrm, data, yName, sName, unfairness,
                       definition, lambda, save.auxiliary)
  })
  
  # training preds/corrs
  predictors <- data[,!colnames(data) %in% c(yName, sName)]
  sensitive <- data[,sName]
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
  data[[yName]] <- as.factor(as.integer(data[[yName]] == yesYVal))
  
  suppressWarnings({
    model <- fairmlBase(fairml::fgrrm, data, yName, sName, unfairness,
                        definition, family, lambda, save.auxiliary)
  })
  
  # training preds/corrs
  predictors <- data[,!colnames(data) %in% c(yName, sName)]
  sensitive <- data[,sName]
  model$trainPreds <- predict(model$base, predictors, sensitive)
  test_y <- as.integer(data[[yName]] == 1)  
  model$trainAcc <- mean(test_y != round(model$trainPreds))
  model$trainCorrs <- s_correlations(data, sName, model$trainPreds)
  model
}

# wrapper for Nclm()
dsldNclm <- function(data, yName, sName, unfairness, covfun = cov, 
                     lambda = 0, save.auxiliary = FALSE) {
  
  getSuggestedLib('cccp')
  data <- toNumericFactor(data)
  
  suppressWarnings({
    model <- fairmlBase(fairml::nclm, data, yName, sName, unfairness, covfun, 
                        lambda, save.auxiliary)
  })
  
  # training preds/corrs
  predictors <- data[,!colnames(data) %in% c(yName, sName)]
  sensitive <- data[,sName]
  model$trainPreds <- predict(model$base, predictors, sensitive)
  model$trainAcc <- mean(abs(model$trainPreds - data[[yName]]))
  model$trainCorrs <- s_correlations(data, sName, model$trainPreds)
  model
}

# wrapper for Zlm()
dsldZlm <- function(data, yName, sName, unfairness) {
 
  getSuggestedLib('CVXR')
  data <- toNumericFactor(data)
  
  suppressWarnings({
    model <- fairmlBase(fairml::zlm, data, yName, sName, unfairness)
  })
  
  # training preds/corrs
  predictors <- data[,!colnames(data) %in% c(yName, sName)]
  sensitive <- data[,sName]
  model$trainPreds <- predict(model$base, predictors)
  model$trainAcc <- mean(abs(model$trainPreds - data[[yName]]))
  model$trainCorrs <- s_correlations(data, sName, model$trainPreds)
  model
}

# wrapper for Zlrm()
dsldZlrm <- function(data, yName, sName, unfairness, yesYVal) {
  
  getSuggestedLib('CVXR')
  data <- toNumericFactor(data)
  data[[yName]] <- as.factor(as.integer(data[[yName]] == yesYVal))
  
  suppressWarnings({
    model <- fairmlBase(fairml::zlrm, data, yName, sName, unfairness)
  })
  
  # training preds/corrs
  predictors <- data[,!colnames(data) %in% c(yName, sName)]
  sensitive <- data[,sName]
  model$trainPreds <- predict(model$base, predictors)
  test_y <- as.integer(data[[yName]] == 1)  
  model$trainAcc <- mean(test_y != round(model$trainPreds))
  model$trainCorrs <- s_correlations(data, sName, model$trainPreds)
  model
}

### S3 methods summary() and predict()
summary.dsldFairML <- function(object,...){
  summary(object$base)
}

predict.dsldFairML <- function(object, newx,...) {
  suppressWarnings({
    # data-prep
    newx <- toNumericFactor(newx)
    newx <- apply_factor_levels(newx, object$FactorsInfo)
    
    yName <- object$yName
    sName <- object$sName
    predictors <- newx[,!colnames(newx) %in% c(yName, sName)]
    sensitive <- newx[,sName]
    
    class <- class(object$base)[1]
    
    if (class %in% c("zlm", "zlrm")) {
      
      # zlm and zlrm have one less argument for prediction
      preds <- predict(object$base, predictors)
      cors  <- s_correlations(newx, sName, preds)
      return(list(preds = preds, correlations = cors))
      
    } else { 
      
      preds <- predict(object$base, predictors, sensitive)
      cors  <- s_correlations(newx, sName, preds)
      return(list(preds = preds, correlations = cors))
      
    }
  })
}
