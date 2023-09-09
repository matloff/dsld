
dsldQeFairRF <- function(data,yName,sNames,deweightPars=NULL, nTree=500,
                         minNodeSize=10,mtry = floor(sqrt(ncol(data))),
                         yesYVal=NULL,holdout=floor(min(1000,0.1*nrow(data)))) {
  # remove sName from data and expand factors
  scaling <- FALSE
  scaledData <- fairScale(data, yName, sNames, scaling)
  expandDW <- expandDeweights(deweightPars, data[1,])
  
  base <- qeML::qeRFranger(scaledData, yName, nTree=nTree,minNodeSize=10,
                   mtry=floor(sqrt(ncol(data)))+1,yesYVal=yesYVal,
                   holdout=holdout, deweightPars = expandDW)
  
  # add base model into dsldQeFair object with special predict function
  model <- list(base = base)
  
  # transfer these objects from the base model into objects in the main model
  model <- append(model, base[names(base) %in% 
                 c("classif", "holdIdxs", "holdoutPreds", "testAcc", "baseAcc")])
  # add these variables as objects of their own name to the model                
  model <- append(model, variablesAsList(
                  sNames, yName, deweightPars, scaling))
  
  # add s correlation calculation
  if (!is.null(sNames) && !is.null(holdout)) {
    model$corrs <- sCorr(base, data, yName, sNames)
  }
  class(model) <- c("dsldQeFair")
  model
}

# rf <- dsldQeFairRF(fairml::compas, "two_year_recid", "race")
# predict.dsldQeFair(rf, fairml::compas[1,])

dsldQeFairKNN <- function(data, yName, sNames, deweightPars=NULL, 
                          yesYVal=NULL,k=25,scaleX=TRUE,
                          holdout=floor(min(1000,0.1*nrow(data)))) {
  # data w/o sName, expanded factors, may or may not be scaled
  scaling <- scaleX
  scaledData <- fairScale(data, yName, sNames, scaling)
  scalePars <- attr(scaledData, 'scalePars')
  
  # expand deweights to match deweightpars
  expandDW <- expandDeweights(deweightPars, data[1,])
  expandVars <- names(expandDW)
  expandVals <- unlist(expandDW)
  
  base <- qeML::qeKNN(scaledData, yName, k=k, yesYVal=yesYVal,
                      expandVars=expandVars,expandVals=expandVals,
                      holdout=holdout)
  
  # add base model into dsldQeFair object with special predict function
  model <- list(base=base)
  
  # transfer these objects from the base model into objects in the main model
  model <- append(model, base[names(base) %in% 
                  c("classif", "factorsInfo", "holdIdxs")])
  # add these variables as objects of their own name to the model   
  model <- append(model, variablesAsList(
                  sNames, yName, deweightPars, scaling, scalePars))
  
  # add s correlation calculation
  if (!is.null(sNames) && !is.null(holdout)) {
    model$corrs <- sCorr(base, data, yName, sNames)
  }
  
  class(model) <- c("dsldQeFair")
  model
}

# knn <- dsldQeFairKNN(fairml::compas, "two_year_recid", "race")
# predict.dsldQeFair(knn, fairml::compas[1,])

qeFairRidgeBase <- function(general, data, yName, sNames, deweightPars, 
                            holdout, yesYVal) {
  classif <- general
  
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
    test <- scaledData[holdIdxs,];
    train <- scaledData[-holdIdxs,];
  } else train <- scaledData
  
  xNames <- colnames(train[,-ncol(train)])
  p <- ncol(train) - 1 # common length for how many predictors there are
  n <- nrow(train) # common length for how many rows in the data
  
  blank <- # in general case: the no value- in linear case: 0
    if (general) levels(data[,yName])[!levels(data[,yName]) %in% yesYVal] else 0
  
  # formula described in edffair paper
  temp <- setNames(rep(0, p), xNames)
  if (!is.null(deweightPars))
    temp[expandVars] <- sqrt(expandVals)
  newx <- data.frame(diag(temp))
  newxy <- cbind(newx, rep(blank, p))
  names(newxy) <- colnames(train)
  dataExtended <- rbind(train, newxy)
  
  base <- 
    if (general) 
      qeML::qeLogit(dataExtended,yName,holdout=NULL, yesYVal=yesYVal)
    else
      qeML::qeLin(dataExtended,yName,holdout=NULL)
  
  model <- list(base=base)
  # add these variables as their own name in the model object
  model <- append(model, variablesAsList(
    sNames, yName, deweightPars, scaling, scalePars, holdIdxs, classif))
  if (general) model <- append(model, variablesAsList(yesYVal))
  
  # add the test accuracy calculations
  if (!is.null(holdout)) {
    model <- append(model, predictHoldoutFair(model, test, train))
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
  qeFairRidgeBase(general=FALSE, data, yName, sNames, deweightPars, holdout)
}
# lin <- dsldQeFairRidgeLin(svcensus, "wageinc", "gender", deweightPars = list(occ=.4, age=.2))
# predict.dsldQeFair(lin, svcensus[1,])

# only works in the binary classification case
dsldQeFairRidgeLog <- function(data,yName,sNames,deweightPars=NULL,
                               yesYVal=levels(data[,yName])[2],
                               holdout=floor(min(1000,0.1*nrow(data)))) {
  qeFairRidgeBase(general=TRUE, data, yName, sNames, deweightPars, holdout, yesYVal)
}

# log <- dsldQeFairRidgeLog(fairml::compas, "two_year_recid", "race")
# predict.dsldQeFair(log, fairml::compas[1,])

predict.dsldQeFair <- function(model, newx) {
  yName <- model$yName
  sNames <- model$sNames
  scaling <- model$scaling
  scalePars <- model$scalePars
  
  # rescale the data according to how the training data was scaled in the model
  newx <- fairScale(newx, yName, sNames, scaling, scalePars)
  newx <- newx[,!colnames(newx) %in% yName]
  
  suppressWarnings(predict(model$base, newx))
}

# utility to add variables to a list as an item w/ their own name
# ex. 'model$yName <- yName' for a list of variables
variablesAsList <- function(...) {
  names <- as.list(substitute(list(...)))[-1]
  values <- list(...)
  list <- setNames(values, names)
  list
}
