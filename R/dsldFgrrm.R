#dsldFgrrm(), a wrapper for fairml::fgrrm()
#usage:
#fairml::fgrrm( response, predictors, sensitive, unfairness,
#               definition = "sp-komiyama", family = "binomial", lambda = 0,
#               save.auxiliary = FALSE)


dsldFgrrm <- function(data, yName, sName, unfairness,
                      definition = "sp-komiyama", family = "binomial", 
                      lambda = 0, save.auxiliary = FALSE) {
  # convert int col to numeric
  data[,unlist(lapply(svcensus, is.integer))] <- 
    lapply(data[,unlist(lapply(svcensus, is.integer))], as.numeric)
  
  r = data[,yName]
  p = data[,!colnames(data) %in% c(yName, sName)]
  s = data[,colnames(data) %in% sName]
  
  base <- fairml::fgrrm(response = r, predictors = p, 
                sensitive = s, unfairness = unfairness,
                definition = definition, family = family, 
                lambda = lambda, save.auxiliary = save.auxiliary)
  model <- list(base=base)
  model$yName <- yName
  model$sName <- sName
  class(model) <- c("dsldFgrrm")
  model
}

predict.dsldFgrrm <- function(object, newx) {
  yName <- object$yName
  sName <- object$sName
  preds <- predict(object$base, newx[,!colnames(newx) %in% c(yName, sName)], 
                   newx[,colnames(newx) %in% sName])
  preds
}

# ---- Test ----
# data <- fairml::compas
# yName <- "two_year_recid"
# sName <- "race"

# model <- dsldFgrrm(data, yName, sName, 0)
# predict(model, data)
