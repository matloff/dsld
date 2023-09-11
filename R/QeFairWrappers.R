# -------- QeFairBase -----------
#
# base function to use in qefairRF and qefairKNN.  
# scales the data/expands factors, trains the base model, then adds additional
# information into the final model object
#
# qeFUNC        - a qeML model generation function
# data          - unmodified data to train the model on
# appendedItems - items to be appended to the model object.
#                 must have yName, sNames, scaling.
# ...           - additional parameters that are passed into the qeFUNC
#
qeFairBase <- function(qeFUNC, data, appendedItems, ...) {
  yName <- appendedItems$yName
  sNames <- appendedItems$sNames
  scaling <- appendedItems$scaling
  
  train <- fairScale(data, yName, sNames, scaling)
  
  base <- qeFUNC(train, yName, ...)
  
  transferredItems <- base[names(base) %in% 
                             c("classif", "holdIdxs", "holdoutPreds", "testAcc", "baseAcc")]
  model <- list(base=base)
  model <- append(model, append(appendedItems, transferredItems))
  model$scalePars <- attr(train, 'scalePars')
  
  if (!is.null(sNames) && !is.null(model$holdIdxs)) {
    xData <- data[,!colnames(data) %in% yName]
    model$corrs <- sCorr(model, xData, sNames)
  }
  class(model) <- c("dsldQeFair")
  model
}

# -------------- QeFairRF -------------
dsldQeFairRF <- function(data,yName,sNames,deweightPars=NULL, nTree=500,
                         minNodeSize=10, mtry = floor(sqrt(ncol(data))),
                         yesYVal=NULL,holdout=floor(min(1000,0.1*nrow(data)))) {
  scaling <- FALSE 
  appendedItems <- variablesAsList(sNames, yName, deweightPars, scaling)
  
  # expand deweights to match the expanded factors in the data in qeFairBase
  expandedDW <- expandDeweights(deweightPars, data[1,])
  model <- qeFairBase(qeML::qeRFranger, data, appendedItems, 
                      
                      nTree=nTree, minNodeSize=minNodeSize, mtry=mtry,
                      yesYVal=yesYVal, holdout=holdout, deweightPars = expandedDW)
  model
}

# rf <- dsldQeFairRF(fairml::compas, "two_year_recid", "race")
# predict.dsldQeFair(rf, fairml::compas[1,])

# ----------------- QeFairKNN ------------
dsldQeFairKNN <- function(data, yName, sNames, deweightPars=NULL, 
                          yesYVal=NULL,k=25,scaleX=TRUE,
                          holdout=floor(min(1000,0.1*nrow(data)))) {
  scaling <- scaleX
  appendedItems <- variablesAsList(sNames, yName, deweightPars, scaling)
  # expand deweights to match the expanded factors in the data in qeFairBase
  expandedDW <- expandDeweights(deweightPars, data[1,])
  expandVars <- names(expandedDW) 
  expandVals <- unlist(expandedDW) # to use as qeKNN pars
  
  model <- qeFairBase(qeML::qeKNN, data, appendedItems, 
                      
                      k=k, yesYVal=yesYVal, holdout=holdout,
                       expandVars=expandVars, expandVals=expandVals)
  model
}

# knn <- dsldQeFairKNN(fairml::compas, "two_year_recid", "race")
# predict.dsldQeFair(knn, fairml::compas[1,])

# --------------- QeFairRidgeBase -------------

# base function for the ridge models. 
# scales and expands the data, expands the deweightPars, 
# adds a diagonal matrix to the bottom of the data as described in the paper
# before training the model on it. then calculates its own test accuracy metrics
#
# linear - a logical value. if false, use linear model- if true, use logistic.
#
qeFairRidgeBase <- function(linear, data, yName, sNames, deweightPars, 
                            holdout, yesYVal) {
  classif <- !linear
  
  # data w/o sName and scaled xNames
  scaling <- TRUE
  scaledData <- fairScale(data, yName, sNames, scaling)
  scalePars <- attr(scaledData, 'scalePars')
  
  # expanded deweight pars
  expandDW <- expandDeweights(deweightPars, data[1,])
  
  # test and training sets to use in testAcc section
  if (!is.null(holdout)){
    holdIdxs <- sample(1:nrow(scaledData),holdout);
    test <- data[holdIdxs,];
    train <- scaledData[-holdIdxs,];
  } else train <- scaledData
  
  # in linear case: 0- in nonlinear: the no value
  blank <- if (linear) 0 else setdiff(levels(data[,yName]), yesYVal) 
  
  # perform formula described in edffair paper
  dataExtended <- ridgeModify(train, expandDW, blank)
  
  base <- 
    if (linear) 
      qeML::qeLin(dataExtended,yName,holdout=NULL)
    else
      qeML::qeLogit(dataExtended,yName,holdout=NULL, yesYVal=yesYVal)

  # add these variables as their own name in the model object
  appendedItems <- variablesAsList(
    sNames, yName, deweightPars, scaling, scalePars, classif)
  if (!linear) appendedItems$yesYVal <- yesYVal
  
  model <- list(base=base)
  model <- append(model, appendedItems)
  
  if (!is.null(holdout)) {
    model$holdIdxs <- holdIdxs
    model <- append(model, predictHoldoutFair(model, test, train))
    
    if (!is.null(sNames)) {
      xData <- data[,!colnames(data) %in% yName]
      model$corrs <- sCorr(model, xData, sNames)
    }
  }

  class(model) <- c("dsldQeFair")
  model
}

# modify the training data as described in the edf fair paper
# 
# train    - expanded factor + scaled training data
# expandDW - expanded factor deweights
# blank    - value to append to the y column to expand it. 0 in the linear case
#            the no value in the general case
#
ridgeModify <- function(train, expandDW, blank=0) {
  expandVars <- names(expandDW)
  expandVals <- unlist(expandDW)
  
  xNames <- colnames(train[,-ncol(train)])
  p <- ncol(train) - 1   # common length for how many predictors there are
  n <- nrow(train)       # common length for how many rows in the data
  
  # formula described in edffair paper
  D <- setNames(rep(0, p), xNames)                                        # new row of 0s for each col
  if (length(expandDW))                     
    D[expandVars] <- sqrt(expandVals)                                     # set deweighted cols to sqrt of deweight
  newx <- data.frame(diag(D))                                             # turn this vector into a diag matrix
  newxy <- cbind(newx, rep(blank, p))                                     # append a blank y column
  names(newxy) <- colnames(train)
  dataExtended <- rbind(train, newxy)                                     # append this to the bottom of the training data
  
  dataExtended
}

dsldQeFairRidgeLin <- function(data, yName, sNames, deweightPars = NULL, 
                                      holdout=floor(min(1000,0.1*nrow(data)))) {
  qeFairRidgeBase(linear=TRUE, data, yName, sNames, deweightPars, holdout)
}
# lin <- dsldQeFairRidgeLin(svcensus, "wageinc", "gender", deweightPars = list(occ=.4, age=.2))
# predict.dsldQeFair(lin, svcensus[1,])

# only works in the binary classification case
dsldQeFairRidgeLog <- function(data,yName,sNames,deweightPars=NULL,
                               holdout=floor(min(1000,0.1*nrow(data))),
                               yesYVal=levels(data[,yName])[2]) {
  qeFairRidgeBase(linear=FALSE, data, yName, sNames, deweightPars, holdout, yesYVal)
}

# log <- dsldQeFairRidgeLog(fairml::compas, "two_year_recid", "race")
# predict.dsldQeFair(log, fairml::compas[1,])

# -------- Predict -----------------
predict.dsldQeFair <- function(model, newx) {
  yName <- model$yName
  sNames <- model$sNames
  scaling <- model$scaling
  scalePars <- model$scalePars

  newx <- newx[,!colnames(newx) %in% c(sNames, yName)]
  # rescale the data according to how the training data was scaled in the model
  newx <- scaleNewX(newx, scaling, scalePars)
  
  suppressWarnings(predict(model$base, newx))
}
