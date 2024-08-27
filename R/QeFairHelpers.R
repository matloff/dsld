# ------------- data scaling --------------
# factor types are split into binary columns for each level
# sNames and yName are removed in order for them to be scaled
# yName is added back.
# scaledPars are added as an attribute in order to rescale the newx in predict
#
# scaling - wether or not the data is scaled
#
# returns - a dataframe with a scalePars attribute
fairScale <- function(data, yName, sNames, scaling=FALSE) {
  predictors <- data[,!colnames(data) %in% c(sNames, yName)]
  response <- data[,yName]
  
  # expand factors
  predictors <- regtools::factorsToDummies(predictors, omitLast = TRUE)
  if (scaling) { 
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

# factor types are split into binary columns for each level
# newx is scaled to match how the training data was scaled
#
# newx      - data to be converted before predicted on
# scaling   - was the model trained on scaled data
# scalePars - a list of (center, scale) that describes how the data was scaled
#
# returns   - a dataframe
scaleNewX <- function(newx, scaling=FALSE, scalePars=NULL) {
  newx <- regtools::factorsToDummies(newx, omitLast = TRUE)
  
  if (scaling) { 
    center <- TRUE; scale <- TRUE
    
    if (!is.null(scalePars)) { 
      center <- scalePars$center
      scale <- scalePars$scale
    }
    
    newx <- scale(newx, center=center, scale=scale)
  }
  
  newx <- data.frame(newx) # this changes the colnames for some reason
  newx
}

# --------- Expand Deweights --------------
# if one of the deweights is a factor, expand it into a dummy for all the qeFairs
#
# deweightPars - in the form of list(var_name=value)
# row1         - doesnt have to be 1 row, but the only thing this needs is
#                information on the data type and name of every column
#
# returns      - an expanded list(var_value=value) 
expandDeweights <- function(deweightPars, row1) {
  pars <- list()
  for (item in names(deweightPars)) {  # loop through every item
    if (is.factor(row1[,item])) {      # expand deweight if factor type
      names <- names(
        data.frame(
          regtools::factorToDummies(row1[,item], item, omitLast = TRUE)
        )
      )
      expanded <- Map(\(x) deweightPars[[item]], names)
      pars <- append(pars, expanded)
    } else {                          
      pars <- append(pars, deweightPars[item])  # add deweight as usual
    }
  }
  pars
}

# ----------------- S Corr ------------------
# calculates s correlation as described in EDF fair github but idk how it works
#
# standardizes the various prediction formats that come with the various models
# then, calculates s correlation according to various cases:
# binary sName / nonbinary sName / continuous sName
#
# xData     - data w/o the y column
#
# returns   - a named numeric vector
sCorr <- function(model, xData, sNames) {
  # qeML functions have a lot of predict formats. 
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
    if (is.factor(sCol)) { 
      corrs <- # classification case
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

# I dont really know the math behind this
# but this gets the correlation between two numeric vectors, names it
#
# preds     - numeric vector of predictions the model made
# xData     - data w/o the y column
# sName     - one of the names of the sensitive column
# holdIdxs  - the row numbers of the holdout set 
#
# returns   - a named number (binary) or named numeric vector (non-binary)
binaryScorr <- function(preds, xData, sName, holdIdxs) {
  formula <- formula(paste(sName, '~.'))
  model <- glm(formula, xData, family=binomial())
  sProbs <- model$fitted.values[holdIdxs]
  cor <- cor(preds, sProbs)^2
  setNames(cor, sName)
}

# this gets the correlation for each level in sName, then names it
nonBinScorr <- function(preds, xData, sName, holdIdxs) {
  # warning with qeLogit(fairml::compas, "race")
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
#
# model   - model to make predictions with
# test    - data set to train the data on. must not be scaled
# train   - the data set the model was trained on. 
#
# returns - list with: 
#           holdoutPreds: numeric vector
#                testAcc: number 
#                baseAcc: number
predictHoldoutFair <- function(model, test, train) {
  yName <- model$yName
  rr <- test[,yName]             ### need to fix predictHoldFair()
  yCol <- c(rr, train[,yName])
  test <- test[ , !names(test) %in% c(yName)]
  preds <- predict.dsldQeFair(model, test)

  testInfo <- list(holdoutPreds=preds)
  if (model$classif) {
    testInfo$testAcc <- mean(preds$predClasses != rr)
    testInfo$baseAcc <- 1 - max(table(yCol)) / length(yCol)
  } else {
    testInfo$testAcc <- mean(abs(preds - rr))
    testInfo$baseAcc <-  mean(abs(rr - mean(train[,yName])))
  }
  testInfo
}

# ------------ Utils -----------------
# utility to add variables to a list as an item w/ their own name
# ex. 'model$yName <- yName' for a list of variables
#
# ...     - one or more variables to be turned into a list
#
# returns - a list
variablesAsList <- function(...) {
  names <- as.list(substitute(list(...)))[-1]
  values <- list(...)
  list <- setNames(values, names)
  list
}