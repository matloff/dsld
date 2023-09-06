#dsldFrrm(), a wrapper for fairml::frrm()
#usage:
#fairml::frrm(response, predictors, sensitive, unfairness,
#     definition = "sp-komiyama", lambda = 0, save.auxiliary = FALSE)

dsldFrrm <- function(data, yName, sName, unfairness,
                     definition = "sp-komiyama", lambda = 0, 
                     save.auxiliary = FALSE) 
{
  data <- fairmlConvert(data)
  
  r = data[,yName]
  p = data[,!colnames(data) %in% c(yName, sName)]
  s = data[,colnames(data) %in% sName]

  base <- fairml::frrm(response = r, predictors = p, 
               sensitive = s, unfairness = unfairness, 
               definition = definition, lambda = lambda, 
               save.auxiliary = save.auxiliary)
  model <- list(base=base)
  model$yName <- yName
  model$sName <- sName
  class(model) <- c("dsldFrrm")
  model
}

summary.dsldFrrm <- function(object){
  return(summary(object$base))
}

predict.dsldFrrm <- function(object, newx) {
  newx <- fairmlConvert(newx)
  yName <- object$yName
  sName <- object$sName
  preds <- predict(object$base, newx[,!colnames(newx) %in% c(yName, sName)], newx[,colnames(newx) %in% sName])
  preds
}

# ---- Test ----
#data(svcensus)
#yName <- "wageinc"
#sName <- "gender"
#model <- dsldFrrm(svcensus, yName, sName, 0.5)
#summary(model)
#newX <- svcensus[1,]
#predict(model, newX)

