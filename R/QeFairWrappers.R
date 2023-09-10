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
  # extract important variables
  yName <- appendedItems$yName
  sNames <- appendedItems$sNames
  scaling <- appendedItems$scaling
  # scale data / expand factors
  scaledData <- fairScale(data, yName, sNames, scaling)
  
  base <- qeFUNC(scaledData, yName, ...)
  
  # construct model
  model <- list(base=base)
  # append from base model + appendedItems
  model <- append(model, base[names(base) %in% 
                c("classif", "holdIdxs", "holdoutPreds", "testAcc", "baseAcc")])
  model <- append(model, appendedItems)
  model$scalePars <- attr(scaledData, 'scalePars')
  
  # add s correlation calculation
  if (!is.null(sNames) && length(model$holdIdxs) > 0) {
    model$corrs <- sCorr(base, data, yName, sNames)
  }
  class(model) <- c("dsldQeFair")
  model
}
# -------------- QeFairRF -------------
dsldQeFairRF <- function(data,yName,sNames,deweightPars=NULL, nTree=500,
                         minNodeSize=10, mtry = floor(sqrt(ncol(data))),
                         yesYVal=NULL,holdout=floor(min(1000,0.1*nrow(data)))) {
  scaling <- FALSE 
  # expand deweights to match the expanded factors in the data in qeFairBase
  expandedDW <- expandDeweights(deweightPars, data[1,])
  
  # these items will be in the model object later
  appendedItems <- variablesAsList(sNames, yName, deweightPars, scaling)
  model <- qeFairBase(qeML::qeRFranger, data, appendedItems, nTree=nTree,
                      minNodeSize=minNodeSize, mtry=mtry,
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
  # expand deweights to match the expanded factors in the data in qeFairBase
  expandedDW <- expandDeweights(deweightPars, data[1,])
  expandVars <- names(expandedDW) 
  expandVals <- unlist(expandedDW) # to use as qeKNN pars
  
  # these items will be in the model object later
  appendedItems <- variablesAsList(sNames, yName, deweightPars, scaling)
  model <- qeFairBase(qeML::qeKNN, data, appendedItems, k=k, yesYVal=yesYVal,
                      expandVars=expandVars,expandVals=expandVals, holdout=holdout)
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
  expandVars <- names(expandDW)
  expandVals <- unlist(expandDW)
  
  # test and training sets to use in testAcc section
  if(!is.null(holdout)) {
    holdIdxs <- sample(1:nrow(scaledData),holdout);
    test <- data[holdIdxs,];
    train <- scaledData[-holdIdxs,];
  } else train <- scaledData
  
  xNames <- colnames(train[,-ncol(train)])
  p <- ncol(train) - 1 # common length for how many predictors there are
  n <- nrow(train) # common length for how many rows in the data
  
  blank <- # in linear case: 0- in nonlinear: the no value
    if (linear) 0 else setdiff(levels(data[,yName]), yesYVal) 
  
  # formula described in edffair paper
  D <- setNames(rep(0, p), xNames)                # new row of 0s for each col
  if (!is.null(deweightPars))                     
    D[expandVars] <- sqrt(expandVals)             # set deweighted cols to sqrt of deweight
  newx <- data.frame(diag(D))                     # turn this vector into a diag matrix
  newxy <- cbind(newx, rep(blank, p))             # append a blank y column
  names(newxy) <- colnames(train)
  dataExtended <- rbind(train, newxy)             # append this to the bottom of the training data
  
  base <- 
    if (linear) 
      qeML::qeLin(dataExtended,yName,holdout=NULL)
    else
      qeML::qeLogit(dataExtended,yName,holdout=NULL, yesYVal=yesYVal)

  model <- list(base=base)
  # add these variables as their own name in the model object
  model <- append(model, variablesAsList(
    sNames, yName, deweightPars, scaling, scalePars, holdIdxs, classif))
  if (!linear) model$yesYVal <- yesYVal
  
  # add the test accuracy calculations
  if (!is.null(holdout)) {
    model <- append(model, predictHoldoutFair(model, test, train, data))
  }
  
  # add s correlation calculation
  if (!is.null(sNames) && !is.null(holdout)) {
    model$corrs <- sCorr(model, data, yName, sNames)
  }
  class(model) <- c("dsldQeFair")
  model
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
