# -------- QeFairBase -----------
#
# base function to use in qefairRF and qefairKNN.  
# The output object is a list, with the base item being a qeML model object.
# The rest of the function adds more items to that list.
# Those items may be from the base model itself, or from appendedItems.
# Also calculates s correlation
#
# The original data is scaled according to how the wrapper function specifies it
# It then saves how the data was scaled in order to rescale newx in predict
#
# qeFUNC        - a qeML model generation function
# data          - unmodified data to train the model on
# appendedItems - items to be appended to the model object.
#                 must have yName, sNames, scaling.
# ...           - additional parameters that are passed into the qeFUNC
#
qeFairBase <- function(qeFUNC, data, appendedItems, ...) {
  # extract important variables
  yName <- appendedItems$yName
  sNames <- appendedItems$sNames
  scaling <- appendedItems$scaling
  
  # scale data / expand factors, before training the model
  train <- fairScale(data, yName, sNames, scaling)
  base <- qeFUNC(train, yName, ...)
  
  # construct the model with items from base, and from appendedItems
  transferredItems <- base[names(base) %in% 
                  c("classif", "holdIdxs", "holdoutPreds", "testAcc", "baseAcc")]
  model <- list(base=base)
  model <- append(model, append(appendedItems, transferredItems))
  model$scalePars <- attr(train, 'scalePars')
  
  # add s correlation
  if (!is.null(sNames) && !is.null(model$holdIdxs)) {
    xData <- data[,!colnames(data) %in% yName]
    model$corrs <- sCorr(model, xData, sNames)
  }
  class(model) <- c("dsldQeFair")
  model
}

# -------------- QeFairRF -------------
#
# No scaling. Appends variables to the final model output via appendedItems
# Deweights are expanded to match the scaled data
#
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
#
# May be scaled. Appends variables to the final model output via appendedItems
# Deweights are expanded to match the scaled data
#
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
# The output object is a list, with the base item being a qeML model object.
# The rest of the function adds more items to that list.
# Also calculates s correlation, and test accuracies
#
# Creates its own training and testing sets 
# training set is modified w/ ridgeModify, according to EDF paper
# trains the base model with that training set. 
# appends additional variables via variables as list
# appends testAcc, baseAcc, holdoutPreds with the testing set via predictHoldoutFair
#
# linear - a logical value. if false, use linear model- if true, use logistic.
#
qeFairRidgeBase <- function(linear, data, yName, sNames, deweightPars, 
                            holdout, yesYVal) {
  # data w/o sName and scaled xNames. save scalePars for predict()
  scaling <- TRUE
  scaledData <- fairScale(data, yName, sNames, scaling)
  scalePars <- attr(scaledData, 'scalePars')

  # test and training sets to use in testAcc section
  if (!is.null(holdout)){
    holdIdxs <- sample(1:nrow(scaledData),holdout);
    test <- data[holdIdxs,];
    train <- scaledData[-holdIdxs,];
  } else train <- scaledData
  
  # in linear case: 0- in nonlinear: the no value
  yBlank <- if (linear) 0 else setdiff(levels(data[,yName]), yesYVal)
  expandDW <- expandDeweights(deweightPars, data[1,])
  # perform formula described in edffair paper
  dataExtended <- ridgeModify(train, expandDW, yBlank)
  
  base <- 
    if (linear) 
      qeML::qeLin(dataExtended,yName,holdout=NULL)
    else
      qeML::qeLogit(dataExtended,yName,holdout=NULL, yesYVal=yesYVal)

  # Append these variables to the model object
  classif <- !linear
  appendedItems <- variablesAsList(
    sNames, yName, deweightPars, scaling, scalePars, classif)
  if (!linear) appendedItems$yesYVal <- yesYVal
  
  # construct output model object
  model <- list(base=base)
  model <- append(model, appendedItems)
  
  # add test accuracy and s corr calculations
  if (!is.null(holdout)) {
    model$holdIdxs <- holdIdxs
    model <- append(model, predictHoldoutFair(model, test, train))
    
    if (!is.null(sNames)) {
      # sCorr requires data w/o the ycol
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
ridgeModify <- function(train, expandDW, yBlank=0) {
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
  newxy <- cbind(newx, rep(yBlank, p))                                    # append a blank y column
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
  # extract params from the model
  yName <- model$yName
  sNames <- model$sNames
  scaling <- model$scaling
  scalePars <- model$scalePars

  newx <- newx[,!colnames(newx) %in% c(sNames, yName)]
  # rescale the data according to how the training data was scaled in the model
  newx <- scaleNewX(newx, scaling, scalePars)
  predict(model$base, newx)
}
