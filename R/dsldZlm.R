#dsldZlrm(), a wrapper for fairml::zlrm()
#usage:
# zlm(response, predictors, sensitive, unfairness)

dsldZlm <- function(data, yName, sName, unfairness) {
  
  data <- fairmlConvert(data)
  
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

summary.dsldZlm <- function(object){
  return(summary(object$base))
}

predict.dsldZlm <- function(object, newx) {
  newx <- fairmlConvert(newx)
  
  yName <- object$yName
  sName <- object$sName
  preds <- predict(object$base, newx[,!colnames(newx) %in% c(yName, sName)])
  preds
}

# ---- Test ----
# data(svcensus)
# data <- svcensus
# yName <- "wageinc"
# sName <- "gender"
# model <- dsldZlm(data, yName, sName, 0)
# summary(model)
# newX <- data[1,]
# predict(model, newX)
