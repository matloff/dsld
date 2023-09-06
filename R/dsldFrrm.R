#dsldFrrm(), a wrapper for fairml::frrm()
#usage:
#fairml::frrm(response, predictors, sensitive, unfairness,
#     definition = "sp-komiyama", lambda = 0, save.auxiliary = FALSE)

dsldFrrm <- function(data, yName, sName, unfairness,
                     definition = "sp-komiyama", lambda = 0, 
                     save.auxiliary = FALSE) 
{
  # convert int col to numeric
  data[,unlist(lapply(svcensus, is.integer))] <- 
    lapply(data[,unlist(lapply(svcensus, is.integer))], as.numeric)
  
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

predict.dsldFrrm <- function(object, newx) {
  yName <- object$yName
  sName <- object$sName
  preds <- predict(object$base, newx[,!colnames(newx) %in% c(yName, sName)], newx[,colnames(newx) %in% sName])
  preds
}

# ---- Test ----
# data(svcensus)
# data <- svcensus
# yName <- "wageinc"
# sName <- "gender"

# model <- dsldFgrrm(data, yName, sName, 0)
# predict(model, data)

