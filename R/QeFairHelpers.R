# ------------- data scaling --------------
# called data2 in the edffair package. 
# factor types are split into binary columns for each level
# sNames is removed. all but yName may be scaled
#
# scaling - wether or not the data is scaled
#
fairScale <- function(data, yName, sNames, scaling=FALSE) {
  predictors <- data[,!colnames(data) %in% c(sNames, yName)]
  response <- data[,yName]
  
  # expand factors
  predictors <- regtools::factorsToDummies(predictors, omitLast = TRUE)
  if (scaling) { # if user chooses to scale the data,
    predictors <- scale(predictors)
    # save scalePars in order to rescale newx in predict function
    scalePars <- list(
      center=attr(predictors,'scaled:center'),
      scale=attr(predictors,'scaled:scale')
    )
  }
  # put it back together
  data <- cbind(data.frame(predictors), response)
  colnames(data)[ncol(data)] <- yName
  
  if (scaling) attr(data, 'scalePars') <- scalePars
  data
}

# converts the prediction data to match the data the format 
# that the model was trained on
#
# newx      - data to be converted before predicted on
# scaling   - was the model trained on scaled data
# scalePars - a list of (center, scale) that describes how the data was scaled
#
scaleNewX <- function(newx, scaling=FALSE, scalePars=NULL) {
  newx <- regtools::factorsToDummies(newx, omitLast = TRUE)
  
  center <- TRUE; scale <- TRUE
  if (scaling) { # if user chooses to scale the data,
    if (!is.null(scalePars)) { # default scaling
      center <- scalePars$center
      scale <- scalePars$scale
    }
    # save scalePars in order to rescale newx in predict function
    # called from predict, with scaledPars
    newx <- scale(newx, center=center, scale=scale)
  }
  newx
}

# --------- Expand Deweights --------------
# if one of the deweights is a factor, expand it into a dummy for all the qeFairs
#
# deweightPars - in the form of list(var_name=value)
# row1         - doesnt have to be 1 row, but the only thing this needs is
#                information on the data type and name of every column
#
expandDeweights <- function(deweightPars, row1) {
  pars <- list()
  for (item in names(deweightPars)) {
    if (is.factor(row1[,item])) {      # expand deweight if factor type
      names <- names(regtools::factorToDummies(row1[,item], item, omitLast = TRUE)[1,])
      expanded <- Map(\(x) deweightPars[[item]], names)
      pars <- append(pars, expanded)
    } else {                           # add deweight as usual
      pars <- append(pars, deweightPars[item])
    }
  }
  pars
}

# ----------------- S Corr ------------------
# calculates s correlation as described in EDF fair github but idk how it works
#
# xData     - data w/o the y column
#
sCorr <- function(model, xData, sNames) {
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
  
  holdIdxs <- model$holdIdxs
  
  # add correlations per s level per s name one by one
  corrs <- NULL
  for (sName in sNames) {
    sCol <- xData[holdIdxs, sName]
    if (is.factor(sCol)) { # classification case
      corrs <- 
        c(corrs, 
          if(length(levels(sCol)) == 2)
            binaryScorr(preds, xData, sName, holdIdxs)
          else
            nonBinScorr(preds, xData, sName, holdIdxs)
          )
    } else { # non-classification case
      if (is.matrix(preds)) preds <- as.vector(preds)
      cor <- cor(preds, sCol)^2
      corrs <- c(corrs, setNames(cor, sName))
    }
  }
  
  corrs
} 

# preds     - numeric vector of predictions the model made
# xData     - data w/o the y column
# sName     - one of the names of the sensitive column
# holdIdxs  - the row numbers of the holdout set 
binaryScorr <- function(preds, xData, sName, holdIdxs) {
  formula <- formula(paste(sName, '~.'))
  model <- glm(formula, xData, family=binomial())
  sProbs <- model$fitted.values[holdIdxs]
  cor <- cor(preds, sProbs)^2
  setNames(cor, sName)
}
nonBinScorr <- function(preds, xData, sName, holdIdxs) {
  model <- suppressWarnings(
    qeML::qeLogit(xData, sName, holdout=NULL))
  corrs <- NULL
  for (glmout in model$glmOuts) {
    sProbs <- glmout$fitted.values[holdIdxs]
    cor <- cor(preds, sProbs)^2
    corrs <- c(corrs, cor)
  }
  setNames(corrs, levels(xData[,sName])) 
}

# ------------------ Predict Holdout Fair -----------------------
# add holdout predictions and test accuracy to ridge regressions
# test must not be scaled. train should be
#
# model - model to make predictions with
# test  - data set to train the data on. must not be scaled
# train - the data set the model was trained on. 
#
predictHoldoutFair <- function(model, test, train) {
  yName <- model$yName
  preds <- predict.dsldQeFair(model, test)
  yCol <- c(test[,yName], train[,yName])
  predHold <- list(holdoutPreds=preds)
  if (model$classif) {
    predHold$testAcc <- mean(preds$predClasses != test[, yName])
    predHold$baseAcc <- 1 - max(table(yCol)) / length(yCol)
  } else {
    predHold$testAcc <- mean(abs(preds - test[,yName]))
    predHold$baseAcc <-  mean(abs(test[,yName] - mean(train[,yName])))
  }
  predHold
}

# ------------ Utils -----------------
# utility to add variables to a list as an item w/ their own name
# ex. 'model$yName <- yName' for a list of variables
#
# ...    - one or more variables to be turned into a list
variablesAsList <- function(...) {
  names <- as.list(substitute(list(...)))[-1]
  values <- list(...)
  list <- setNames(values, names)
  list
}