#dsldnclm(), a wrapper for fairml::nclm()
#usage:
#fairml::nclm(response, predictors, sensitive, unfairness, covfun, lambda = 0, 
#             save.auxiliary = FALSE)

dsldNclm <- function (data, yName, sName, unfairness, covfun = cov, 
                     lambda = 0, save.auxiliary = FALSE) 
{
  data <- fairmlConvert(data)
  
  r = data[,yName]
  p = data[,!colnames(data) %in% c(yName, sName)]
  s = data[,colnames(data) %in% sName]

  base <- fairml::nclm(response = r, predictors = p, 
               sensitive = s, unfairness = unfairness, covfun = covfun,
               lambda = lambda, save.auxiliary = save.auxiliary)
  
  model <- list(base=base)
  model$yName <- yName
  model$sName <- sName
  class(model) <- c("dsldNclm")
  model
}

predict.dsldNclm <- function(object, newx) {
  newx <- fairmlConvert(newx)
  
  yName <- object$yName
  sName <- object$sName
  preds <- predict(object$base, newx[,!colnames(newx) %in% c(yName, sName)], newx[,colnames(newx) %in% sName])
  preds
}

# ---- Test ----
# data <- fairml::compas
# yName <- "two_year_recid"
# sName <- "race"
#
# model <- dsldNclm(data, yName, sName, 0)
# predict(model, data)


