# -------------------------------- QeFairBase ----------------------------------
# base function to use in qefairRF and qefairKNN. 
# returns: list with the class "dsldQeFair"
qeFairBase <- function(qeFUNC, data, yName, sNames, scaling, ...,
                       appendedItems = list()) {
  
  # scale data / expand factors, before training the model
  train <- fairScale(data, yName, sNames, scaling)
  base <- qeFUNC(train, yName, ...)
  
  model <- list(base = base)
  model <- append(model, appendedItems)
  model$scalePars <- attr(train, "scalePars")
  model$FactorsInfo = factor_levels(data)

  # return s3 object
  class(model) <- c("dsldQeFair")
  return(model)
}

# ------------------------------------- QeFairRF -------------------------------

# No scaling. Appends variables to the final model output via appendedItems
# Deweights are expanded to match the scaled data

dsldQeFairRF <- function(data, yName, sNames, deweightPars = NULL, nTree = 500,
                         minNodeSize = 10, mtry = floor(sqrt(ncol(data))),
                         yesYVal = NULL) {
  # setup
  scaling <- FALSE
  appendedItems <- variablesAsList(yName, sNames, scaling, deweightPars)
  
  if (is.factor(data[[yName]]) && is.null(yesYVal)) {
    stop("Missing `yesYVal` for a factor response.")
  } 
  
  if (!is.factor(data[[yName]]) && !is.null(yesYVal)) {
    yesYVal = NULL
  } 
  
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
                      holdout = NULL,
                      deweightPars = expandedDW,
                      appendedItems = appendedItems
  )
  
  # training preds/corrs
  model$trainPreds <- predict.dsldQeFair(model, data[,!colnames(data) %in% c(yName)])
  
  if (!is.null(yesYVal)) {
    # classification case
    model$trainAcc <- mean(data[[yName]] != model$trainPreds$preds$predClasses)
    model$trainCorrs <- s_correlations(data, sNames, model$trainPreds$preds$probs[,2])
  } else {
    # regression case
    model$trainAcc <- mean(abs(model$trainPreds$preds - data[[yName]]))
    model$trainCorrs <- s_correlations(data, sNames, model$trainPreds$preds)
  }

  return(model)
}

# ------------------------------ QeFairKNN -------------------------------------

# May be scaled. Appends variables to the final model output via appendedItems
# Deweights are expanded to match the scaled data

dsldQeFairKNN <- function(data, yName, sNames, deweightPars = NULL,
                          yesYVal = NULL, k = 25, scaleX = TRUE) {
  # setup for easier calls
  scaling <- scaleX
  appendedItems <- variablesAsList(yName, sNames, scaling, deweightPars)
  
  # expand deweights to match the expanded factors in the data in qeFairBase
  expandedDW <- expandDeweights(deweightPars, data[1, ])
  expandVars <- names(expandedDW)
  expandVals <- unlist(expandedDW) # to use as qeKNN pars
  
  if (is.factor(data[[yName]]) && is.null(yesYVal)) {
    stop("Missing `yesYVal` for a factor response.")
  } 
  
  if (!is.factor(data[[yName]]) && !is.null(yesYVal)) {
    yesYVal = NULL
  } 
  
  model <- qeFairBase(qeML::qeKNN,
                      data,
                      yName,
                      sNames,
                      scaling,
                      k = k,
                      yesYVal = yesYVal,
                      holdout = NULL,
                      expandVars = expandVars,
                      expandVals = expandVals,
                      appendedItems = appendedItems
  )
  
  # training preds/corrs
  model$trainPreds <- predict.dsldQeFair(model, data[,!colnames(data) %in% c(yName)])

  if (!is.null(yesYVal)) {
    # classification case
    model$trainAcc <- mean(data[[yName]] != model$trainPreds$preds$predClasses)
    model$trainCorrs <- s_correlations(data, sNames, model$trainPreds$preds$probs)
  } else {
    # regression case
    model$trainAcc <- mean(abs(model$trainPreds$preds - data[[yName]]))
    model$trainCorrs <- s_correlations(data, sNames, model$trainPreds$preds)
  }
  
  return(model)
}

# -------------------------------- QeFairRidgeBase -----------------------------
#
# base function for the ridge models. 
# Creates its own training and testing sets 
# training set is modified w/ ridgeModify, according to EDF paper
# trains a qeML model with that training set as the base of the output list
# appends additional variables via variables as list
# appends testAcc, baseAcc, holdoutPreds with the testing set via predictHoldoutFair
#
qeFairRidgeBase <- function(data, yName, sNames, deweightPars, yesYVal = 0) {
  
  linear <- yesYVal == 0     # if we're using the default yesYVal, assume its linear
  
  # perform formula described in edffair paper
  dataExtended <- ridgeModify(data, yName, sNames, deweightPars, yesYVal)
  scalePars <- attr(dataExtended, "scalePars")
  
  base <- 
    if (linear) qeML::qeLin(dataExtended, yName, holdout = NULL)
  else          qeML::qeLogit(dataExtended, yName, holdout = NULL, yesYVal = yesYVal)
  
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
  model$FactorsInfo = factor_levels(data)
  
  # return s3 object
  class(model) <- c("dsldQeFair")
  return(model)
}

# modify the training data as described in the edf fair paper
# essentially, you append a diagonal matrix to the bottom of the training data
# with each value of the diagonal optionally having a deweighting value
# the yCol is expanded w/ 0s or a no
# yesYVal - the yes value in the binary yName. if 0,
# we're assuming its a linear model

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


### dsldQeFairRidgeLin
dsldQeFairRidgeLin <- function(data, yName, sNames, deweightPars = NULL) {
  model = qeFairRidgeBase(data, yName, sNames, deweightPars)
  model$trainPreds <- predict.dsldQeFair(model, data[,!colnames(data) %in% c(yName)])
  model$trainAcc <- mean(abs(model$trainPreds$preds - data[[yName]]))
  model$trainCorrs <- s_correlations(data, sNames, model$trainPreds$preds)
  return(model)
}


# dsldQeFairRidgeLog
# works in the binary classification case
dsldQeFairRidgeLog <- function(data, yName, sNames, deweightPars = NULL, yesYVal) {
  model = qeFairRidgeBase(data, yName, sNames, deweightPars, yesYVal)
  model$trainPreds <- predict.dsldQeFair(model, data[,!colnames(data) %in% c(yName)])
  model$trainAcc <- mean(data[[yName]] != model$trainPreds$preds$predClasses)
  model$trainCorrs <- s_correlations(data, sNames, model$trainPreds$preds$probs)
  return(model)
}

# ------------------------------- Predict() ------------------------------------
predict.dsldQeFair <- function(object, newx, ...) {
  # extract params from the model
  #yName <- model$yName
  sNames <- as.vector(object$sNames)
  scaling <- object$scaling
  scalePars <- object$scalePars
  
  
  newxNoS <- newx[, !colnames(newx) %in% c(sNames)]
  newxNoS <- apply_factor_levels(newxNoS, object$FactorsInfo)
  
  # rescale the data according to how the training data was scaled in the model
  newxNoS <- scaleNewX(newxNoS, scaling, scalePars)
  preds = predict(object$base, newxNoS)
  if (is.list(preds)) {
    if (is.null(dim(preds$probs))) {
      # it's a simple vector, already length n
      probs <- preds$probs
    } else {
      # it's a matrix/data frame with 2 columns (0/1)
      probs <- preds$probs[,1]
    }
    cors <- s_correlations(newx, sNames, probs)
  } else {
    cors = s_correlations(newx, sNames, preds)
  }
  return(list(preds = preds, correlations = cors))
}

### helper functions

# ------------- data scaling --------------
# factor types are split into binary columns for each level
# sNames and yName are removed in order for them to be scaled
# yName is added back.
# scaledPars are added as an attribute in order to rescale the newx in predict
# scaling - wether or not the data is scaled
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
# newx      - data to be converted before predicted on
# scaling   - was the model trained on scaled data
# scalePars - a list of (center, scale) that describes how the data was scaled
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
