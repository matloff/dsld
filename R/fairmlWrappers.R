# converts integer cols to numeric and character cols to factor for fairml wrappers
toNumericFactor <- function(data) {
  data[,unlist(lapply(data, is.integer))] <- 
    lapply(data[,unlist(lapply(data, is.integer))], as.numeric)
  data[,unlist(lapply(data, is.character))] <- 
    lapply(data[,unlist(lapply(data, is.character))], as.factor)
  data
}

# base function for fairML wrappers since they all follow the same format
# converts the data into a format that the fairml models accept
# then puts the fairml model inside an object of the dsldFairML class which
# has its own predict function
fairmlBase <- function(fairmlFUNC, data, yName, sName, unfairness, ...) {
  # fairml requires numeric and factor columns
  data <- toNumericFactor(data)
  
  response <- data[,yName]
  predictors <- data[,!colnames(data) %in% c(yName,sName)]
  sensitive <- data[,sName]
  
  # calls a fairml model function as the base for the dsldFairML object
  base <- fairmlFUNC(response = response, predictors = predictors, 
                       sensitive = sensitive, unfairness = unfairness, ...)
  
  # save yName and sName to use in predict()
  model <- list(base=base)
  model$yName <- yName
  model$sName <- sName
  class(model) <- c("dsldFairML")
  model
}

# wrapper for nclm
dsldNclm <- function(data, yName, sName, unfairness, covfun = cov, 
                     lambda = 0, save.auxiliary = FALSE) {
  getSuggestedLib('cccp')
  fairmlBase(fairml::nclm, data, yName, sName, unfairness, covfun, 
             lambda, save.auxiliary)
}

# wrapper for zlm
dsldZlm <- function(data, yName, sName, unfairness) {
  getSuggestedLib('CVXR')
  fairmlBase(fairml::zlm, data, yName, sName, unfairness)
}

# wrapper for zlrm
dsldZlrm <- function(data, yName, sName, unfairness) {
  getSuggestedLib('CVXR')
  fairmlBase(fairml::zlrm, data, yName, sName, unfairness)
}

# wrapper for frrm
dsldFrrm <- function(data, yName, sName, unfairness,
                     definition = "sp-komiyama", lambda = 0, 
                     save.auxiliary = FALSE) {
  fairmlBase(fairml::frrm, data, yName, sName, unfairness,
             definition, lambda, save.auxiliary)
}

# wrapper for fgrrm
dsldFgrrm <- function(data, yName, sName, unfairness,
                      definition = "sp-komiyama", family = "binomial", 
                      lambda = 0, save.auxiliary = FALSE) {
  fairmlBase(fairml::fgrrm, data, yName, sName, unfairness,
             definition, family, lambda, save.auxiliary)
}

### S3 methods summary() and predict()
summary.dsldFairML <- function(object,...){
  summary(object$base)
}

predict.dsldFairML <- function(object, newx,...) {
  # fairml requires char and factor cols
  newx <- toNumericFactor(newx)
  
  yName <- object$yName
  sName <- object$sName
  
  # convert dsld format into fairml format
  predictors <- newx[,!colnames(newx) %in% c(yName, sName)]
  sensitive <- newx[,sName]
  
  class <- class(object$base)[1]
  # call the fairml predict function
  if (class == "zlm" || class == "zlrm")
    # zlm and zlrm have one less argument for prediction
    predict(object$base, predictors)
  else
    predict(object$base, predictors, sensitive)
}
