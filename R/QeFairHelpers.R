# called data2 in the edffair package. 
# factor types are split into binary columns for each level
# sNames is removed. all but yName may be scaled
fairScale <- function(data, yName, sNames, scaling=FALSE, scalePars=NULL) {
  predictors <- data[,!colnames(data) %in% c(sNames, yName)]
  response <- data[,yName]
  
  # expand factors
  predictors <- regtools::factorsToDummies(predictors)
  if (scaling) { # if user chooses to scale the data,
    if (is.null(scalePars)) { # default scaling
      predictors <- scale(predictors)
      # save scalePars in order to rescale newx in predict function
      scalePars <- list(
        center=attr(predictors,'scaled:center'),
        scale=attr(predictors,'scaled:scale')
      )
    } else {
      # called from predict, with scaledPars
      predictors <- scale(predictors, scalePars$center, scalePars$scale)
    }
  }
  data <- cbind(data.frame(predictors), response)
  colnames(data)[ncol(data)] <- yName
  
  attr(data, 'scalePars') <- scalePars
  data
}

# if one of the deweights is a factor, expand it into a dummy for all the qeFairs
expandDeweights <- function(deweightPars, row1) {
  pars <- list()
  for (item in names(deweightPars)) {
    if (is.factor(row1[,item])) {      # expand deweight if factor type
      names <- names(factorToDummies(row1[,item], item)[1,])
      expanded <- Map(\(x) deweightPars[[item]], names)
      pars <- append(pars, expanded)
    } else {                           # add deweight as usual
      pars <- append(pars, deweightPars[item])
    }
  }
  pars
}

sCorr <- function(model, data, yName, sNames) {
  # qeML functions have a lot of predict formats
  preds <- 
    if (is.list(model$holdoutPreds)) {
      probs <- model$holdoutPreds$probs
        if (is.matrix(probs))  
          probs[,1] 
        else probs
    }
    else
      model$holdoutPreds
  
  xNames <- setdiff(colnames(data), c(yName))
  holdout <- model$holdIdxs
  
  # add correlations per s level per s name one by one
  corrs <- NULL
  for (sName in sNames) {
    sCol <- data[holdout, sName]
    if (is.factor(sCol)) { # classification case
      corrs <- 
        c(corrs, 
          if(length(levels(sCol)) == 2)
            binaryScorr(preds, data, xNames, sName, holdout)
          else
            nonBinScorr(preds, data, xNames, sName, holdout)
          )
    } else { # non-classification case
      if (is.matrix(preds)) preds <- as.vector(preds)
      cor <- cor(preds, sCol)^2
      corrs <- c(corrs, setNames(cor, sName))
    }
  }
  
  corrs
} 

binaryScorr <- function(preds, data, xNames, sName, holdout) {
  formula <- formula(paste(sName, '~.'))
  model <- glm(formula, data[,xNames], family=binomial())
  sProbs <- model$fitted.values[holdout]
  cor <- cor(preds, sProbs)^2
  setNames(cor, sName)
}

nonBinScorr <- function(preds, data, xNames, sName, holdout) {
  model <- suppressWarnings(qeML::qeLogit(data[,xNames], sName, holdout=NULL))
  corrs <- NULL
  for (glmout in model$glmOuts) {
    sProbs <- glmout$fitted.values[holdout]
    cor <- cor(preds, sProbs)^2
    corrs <- c(corrs, cor)
  }
  setNames(corrs, levels(data[,sName])) 
}

# add holdout predictions and test accuracy to ridge regressions
predictHoldoutFair <- function(model, test, train) {
  yName <- model$yName
  data <- rbind(test, train)
  preds <- predict.dsldQeFair(model, test)
  predHold <- list(holdoutPreds=preds)
  if (model$classif) {
    predHold$testAcc <- mean(preds$predClasses != test[, yName])
    predHold$baseAcc <- 1 - max(table(data[,yName])) / nrow(data)
  } else {
    predHold$testAcc <- mean(abs(preds - test[,yName]))
    predHold$baseAcc <-  mean(abs(test[,yName] - mean(train[,yName])))
  }
  predHold
}

# utility to add variables to a list as an item w/ their own name
# ex. 'model$yName <- yName' for a list of variables
variablesAsList <- function(...) {
  names <- as.list(substitute(list(...)))[-1]
  values <- list(...)
  list <- setNames(values, names)
  list
}