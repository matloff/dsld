#dsldZlrm(), a wrapper for fairml::zlrm()
#usage:
# zlrm(response, predictors, sensitive, unfairness)

dsldZlrm <- function(data, yName, sName, unfairness) {
  
  data <- fairmlConvert(data)
  
  r = data[,yName]
  p = data[,!colnames(data) %in% c(yName, sName)]
  s = data[,colnames(data) %in% sName]
  
  base <- fairml::zlrm(response = r, predictors = p, 
                        sensitive = s, unfairness = unfairness)
  model <- list(base=base)
  model$yName <- yName
  model$sName <- sName
  class(model) <- c("dsldZlrm")
  model
}

predict.dsldZlrm <- function(object, newx) {
  newx <- fairmlConvert(newx)
  
  yName <- object$yName
  sName <- object$sName
  preds <- predict(object$base, newx[,!colnames(newx) %in% c(yName, sName)])
  preds
}

# ---- Test ----
# data <- fairml::compas
# yName <- "two_year_recid"
# sName <- "race"

# model <- dsldZlrm(data, yName, sName, 0)
# predict(model, data)
