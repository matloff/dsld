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
# ...           - additional parameters that are passed into the qeFUNC
# appendedItems - list of items to be appended to the model object.
#
# returns       - list with the class "dsldQeFair"
qeFairBase <- function(qeFUNC, data, yName, sNames, scaling, ...,
                       appendedItems = list()) {
  # scale data / expand factors, before training the model
  train <- fairScale(data, yName, sNames, scaling)
  base <- qeFUNC(train, yName, ...)
  
  # construct the model with items from base, and from appendedItems
  transferredItems <- base[names(base) %in% c("classif",
                                              "holdIdxs",
                                              "holdoutPreds",
                                              "testAcc",
                                              "baseAcc"
  )]
  
  model <- list(base = base)
  model <- append(model, transferredItems)
  model <- append(model, appendedItems)
  model$scalePars <- attr(train, "scalePars")
  
  # add s correlation
  if (!is.null(sNames) && !is.null(model$holdIdxs)) {
    xData <- data[, !colnames(data) %in% yName]
    model$corrs <- sCorr(model, xData, sNames)
  }
  
  # return s3 object
  class(model) <- c("dsldQeFair")
  return(model)
}

# -------------- QeFairRF -------------
#
# No scaling. Appends variables to the final model output via appendedItems
# Deweights are expanded to match the scaled data
#
dsldQeFairRF <- function(data, yName, sNames, deweightPars = NULL, nTree = 500,
                         minNodeSize = 10, mtry = floor(sqrt(ncol(data))),
                         yesYVal = NULL,
                         holdout = floor(min(1000, 0.1 * nrow(data)))) {
  # setup
  scaling <- FALSE
  appendedItems <- variablesAsList(yName, sNames, scaling, deweightPars)
  
  # expand deweights to match the expanded factors in the data in qeFairBase
  expandedDW <- expandDeweights(deweightPars, data[1, ])
  model <- qeFairBase(qeML::qeRFranger,
                      data,
                      yName,
                      sNames,
                      scaling,
                      nTree = nTree,
                      minNodeSize = minNodeSize,
                      mtry = mtry,
                      yesYVal = yesYVal,
                      holdout = holdout,
                      deweightPars = expandedDW,
                      appendedItems = appendedItems
  )
  
  return(model)
}

# rf <- dsldQeFairRF(fairml::compas, "two_year_recid", "race")
# predict.dsldQeFair(rf, fairml::compas[1,])

# ----------------- QeFairKNN ------------
#
# May be scaled. Appends variables to the final model output via appendedItems
# Deweights are expanded to match the scaled data
#
dsldQeFairKNN <- function(data, yName, sNames, deweightPars = NULL,
                          yesYVal = NULL, k = 25, scaleX = TRUE,
                          holdout = floor(min(1000, 0.1 * nrow(data)))) {
  # setup for easier calls
  scaling <- scaleX
  appendedItems <- variablesAsList(yName, sNames, scaling, deweightPars)
  
  # expand deweights to match the expanded factors in the data in qeFairBase
  expandedDW <- expandDeweights(deweightPars, data[1, ])
  expandVars <- names(expandedDW)
  expandVals <- unlist(expandedDW) # to use as qeKNN pars
  
  model <- qeFairBase(qeML::qeKNN,
                      data,
                      yName,
                      sNames,
                      scaling,
                      k = k,
                      yesYVal = yesYVal,
                      holdout = holdout,
                      expandVars = expandVars,
                      expandVals = expandVals,
                      appendedItems = appendedItems
  )
  
  return(model)
}

# knn <- dsldQeFairKNN(fairml::compas, "two_year_recid", "race")
# predict.dsldQeFair(knn, fairml::compas[1,])

# --------------- QeFairRidgeBase -------------
#
# base function for the ridge models. 
# Creates its own training and testing sets 
# training set is modified w/ ridgeModify, according to EDF paper
# trains a qeML model with that training set as the base of the output list
# appends additional variables via variables as list
# appends testAcc, baseAcc, holdoutPreds with the testing set via predictHoldoutFair
#
qeFairRidgeBase <- function(data, yName, sNames, deweightPars,
                            holdout = floor(min(1000, 0.1 * nrow(data))), yesYVal = 0) {
  
  linear <- yesYVal == 0     # if we're using the default yesYVal, assume its linear
  
  if (!is.null(holdout)) {
    holdIdxs <- sample(1:nrow(data), holdout)
    test <- data[holdIdxs,]
    train <- data[-holdIdxs,]
  } else {
    train <- data
  }
  
  # perform formula described in edffair paper
  dataExtended <- ridgeModify(train, yName, sNames, deweightPars, yesYVal)
  scalePars <- attr(dataExtended, "scalePars")
  
  base <- 
    if (linear) qeML::qeLin(dataExtended, yName, holdout = NULL)
  else        qeML::qeLogit(dataExtended, yName, holdout = NULL, yesYVal = yesYVal)
  
  # Append these variables to the model object
  classif <- !linear
  scaling <- TRUE
  appendedItems <- variablesAsList(
    sNames,
    yName,
    deweightPars,
    scaling,
    scalePars,
    classif,
    yesYVal
  )
  
  # construct output model object
  model <- list(base = base)
  model <- append(model, appendedItems)
  
  # add test accuracy and s corr calculations
  if (!is.null(holdout)) {
    model$holdIdxs <- holdIdxs
    testInfo <- predictHoldoutFair(model, test, train)
    model <- append(model, testInfo)
    
    if (!is.null(sNames)) {
      xData <- data[, !colnames(data) %in% yName]
      model$corrs <- sCorr(model, xData, sNames)
    }
  }
  
  # return s3 object
  class(model) <- c("dsldQeFair")
  return(model)
}

# modify the training data as described in the edf fair paper
# essentially, you append a diagonal matrix to the bottom of the training data
# with each value of the diagonal optionally having a deweighting value
# the yCol is expanded w/ 0s or a no
#
# yesYVal - the yes value in the binary yName. if 0, we're assuming its a linear model
#
ridgeModify <- function(data, yName, sNames, deweightPars, yesYVal = 0) {
  # setup expanddeweighting
  expandDW <- expandDeweights(deweightPars, data[1, ])
  expandVars <- names(expandDW)
  expandVals <- unlist(expandDW)
  
  data <- fairScale(data, yName, sNames, scaling = TRUE)
  
  xNames <- colnames(data[,-ncol(data)])
  noYVal <- setdiff(levels(data[,yName]), yesYVal)[[1]]
  yBlank <- if (yesYVal == 0) 0 else noYVal
  p <- ncol(data) - 1   # how many predictors
  
  # formula described in edffair paper
  D <- setNames(rep(0, p), xNames)
  if (!is.null(deweightPars)) {
    D[expandVars] <- sqrt(expandVals)
  }
  
  extension <- data.frame(diag(D))
  extension <- cbind(extension, rep(yBlank, p))
  
  names(extension) <- colnames(data)
  dataExtended <- rbind(data, extension)
  
  attr(dataExtended, "scalePars") <- attr(data, "scalePars")
  return(dataExtended)
}

dsldQeFairRidgeLin <- function(data, yName, sNames, deweightPars = NULL,
                               holdout = floor(min(1000, 0.1 * nrow(data)))) {
  # wrap call directly
  return(qeFairRidgeBase(data, yName, sNames, deweightPars, holdout))
}
# lin <- dsldQeFairRidgeLin(svcensus, "wageinc", "gender", deweightPars = list(occ=.4, age=.2))
# predict.dsldQeFair(lin, svcensus[1,])

# only works in the binary classification case
dsldQeFairRidgeLog <- function(data, yName, sNames, deweightPars = NULL,
                               holdout = floor(min(1000, 0.1 * nrow(data))),
                               yesYVal = levels(data[, yName])[2]) {
  # wrap call directly
  return(qeFairRidgeBase(data, yName, sNames, deweightPars, holdout, yesYVal))
}

# log <- dsldQeFairRidgeLog(fairml::compas, "two_year_recid", "race")
# predict.dsldQeFair(log, fairml::compas[1,])

# -------- Predict -----------------
predict.dsldQeFair <- function(object, newx, ...) {
  # extract params from the model
  #yName <- model$yName
  sNames <- object$sNames
  scaling <- object$scaling
  scalePars <- object$scalePars
  
  newx <- newx[, !colnames(newx) %in% c(sNames)]
  
  # rescale the data according to how the training data was scaled in the model
  newx <- scaleNewX(newx, scaling, scalePars)
  return(predict(object$base, newx))
}
