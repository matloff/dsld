# converts int cols to numeric and chr cols to factor for fairml wrappers
toNumericFactor <- function(data) {
  data[,unlist(lapply(data, is.integer))] <- 
    lapply(data[,unlist(lapply(data, is.integer))], as.numeric)
  data[,unlist(lapply(data, is.character))] <- 
    lapply(data[,unlist(lapply(data, is.character))], as.factor)
  data
}

# base function for fairML wrappers since they all follow the same format
fairmlBase <- function(fairmlFUNC, data, yName, sName, unfairness, ...) {
  # fairml requires numeric and factor columns
  data <- toNumericFactor(data)
  
  response = data[,yName]
  predictors = data[,!colnames(data) %in% c(yName, sName)]
  sensitive = data[,colnames(data) %in% sName]
  
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

dsldNclm <- function(data, yName, sName, unfairness, covfun = cov, 
                     lambda = 0, save.auxiliary = FALSE) {
  fairmlBase(fairml::nclm, data, yName, sName, unfairness, covfun, 
             lambda, save.auxiliary)
}

dsldZlm <- function(data, yName, sName, unfairness) {
  fairmlBase(fairml::zlm, data, yName, sName, unfairness)
}

dsldZlrm <- function(data, yName, sName, unfairness) {
  fairmlBase(fairml::zlrm, data, yName, sName, unfairness)
}

dsldFrrm <- function(data, yName, sName, unfairness,
                     definition = "sp-komiyama", lambda = 0, 
                     save.auxiliary = FALSE) {
  fairmlBase(fairml::frrm, data, yName, sName, unfairness,
             definition, lambda, save.auxiliary)
}

dsldFgrrm <- function(data, yName, sName, unfairness,
                      definition = "sp-komiyama", family = "binomial", 
                      lambda = 0, save.auxiliary = FALSE) {
  fairmlBase(fairml::fgrrm, data, yName, sName, unfairness,
             definition, family, lambda, save.auxiliary)
}

summary.dsldFairML <- function(object){
  summary(object$base)
}

predict.dsldFairML <- function(object, newx) {
  newx <- toNumericFactor(newx)
  
  yName <- object$yName
  sName <- object$sName
  
  predictors <- newx[,!colnames(newx) %in% c(yName, sName)]
  sensitive <- newx[,colnames(newx) %in% sName]
  
  class <- class(object$base)[1]
  # call the fairml predict function
  if (class == "zlm" || class == "zlrm")
    # zlm and zlrm have one less argument for prediction
    predict(object$base, predictors)
  else
    predict(object$base, predictors, sensitive)
}

# --------- Tests ------------

# library(dsld)
# data(svcensus)
# data <- svcensus
# yName <- "wageinc"
# sName <- "gender"
# 
# model <- dsldFrrm(data, yName, sName, 0)
# summary(model)
# predict(model, data)
# 
# model <- dsldNclm(data, yName, sName, 0)
# summary(model)
# predict(model, data)
# 
# model <- dsldZlm(data, yName, sName, 0)
# summary(model)
# predict(model, data)
# 
# data <- fairml::compas
# yName <- "two_year_recid"
# sName <- "race"
# 
# model <- dsldFgrrm(data, yName, sName, 0)
# summary(model)
# predict(model, data)
# 
# model <- dsldZlrm(data, yName, sName, 0)
# summary(model)
# predict(model, data)