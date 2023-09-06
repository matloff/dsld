#dsldZlrm(), a wrapper for fairml::zlrm()
#usage:
# zlm(response, predictors, sensitive, unfairness)

dsldZlm <- function(data, yName, sName, unfairness) {
  r = data[,yName]
  p = data[,!colnames(data) %in% c(yName, sName)]
  s = data[,colnames(data) %in% sName]
  
  base <- fairml::zlm(response = r, predictors = p, 
                       sensitive = s, unfairness = unfairness)
  model <- list(base=base)
  model$yName <- yName
  model$sName <- sName
  class(model) <- c("dsldZlm")
  model
}

predict.dsldZlm <- function(object, newx) {
  yName <- model$yName
  sName <- model$sName
  preds <- predict(object$base, newx[,!colnames(newx) %in% c(yName, sName)])
  preds
}

# ---- Test ----
# data <- fairml::compas
# yName <- "two_year_recid"
# sName <- "race"

# model <- dsldZlm(data, yName, sName, 0)
# predict(model, data)
