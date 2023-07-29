#dsldQeFairRidgeLog(), a wrapper for EDFfair::qeFairRidgeLog()
#usage:
#EDFfair::qeFairRidgeLog(data, yName, deweightPars, sensNames = sName, yesYVal, holdout)

dsldQeFairRidgeLog<- function(data, yName, deweightPars, sName = NULL, yesYVal = 0,
                              holdout = floor(min(1000,0.1 * nrow(data)))){
  #EDFfair::qeFairRidgeLog(data, yName, deweightPars, sName, yesYVal, holdout)
  require(qeML)
  if (!require('gtools')) install.packages('gtools'); library('gtools')
  
  if (yesYVal == 0) stop('missing yesYVal')
  
  yLevels <- levels(data[,yName])
  if (length(yLevels) != 2)
    stop('handles the 2-class setting only')
  
  # data prep
  data <- na.exclude(data)
  scaling <- 'scale'
  prepData(scaling=scaling)
  names(data2)[ncol(data2)] <- yName
  dataNonSens <- data2
  yCol <- ncol(dataNonSens)
  namesX <- colnames(xm)
  
  if (!is.null(holdout)) {
    dataNonSensFull <- dataNonSens
    splitData(holdout,dataNonSens)
    dataNonSensTrn <- trn
  } else dataNonSensTrn <- dataNonSens
  
  # set up the lambdas
  p <- ncol(dataNonSensTrn) - 1
  dataExtended <- rbind(dataNonSensTrn,dataNonSensTrn[1:p,])
  n <- nrow(dataNonSensTrn)
  newRows <- (n+1):(n+p)
  tmp <- rep(0,p)
  names(tmp) <- namesX
  tmp[deweightNames] <- sqrt(deweightVals)
  newx <- as.data.frame(diag(tmp))
  names(newx) <- namesX
  dataY <- dataNonSens[,yName]
  whichYesY <- which(levels(dataY) == yesYVal)
  whichNoY <- which(levels(dataY) != yesYVal)
  noYVal <- levels(dataY)[whichNoY]
  newy <- rep(noYVal,p)
  y <- as.character(dataNonSensTrn[,yCol])
  yExtended <- as.factor(c(y,newy))
  dataExtended[newRows,-yCol] <- newx
  dataExtended[,yCol] <- yExtended
  names(dataExtended)[yCol] <- yName
  
  fairLogOut <- qeLogit(dataExtended,yName,holdout=NULL)
  tmp <- data[,yName]
  fairLogOut$whichYesY <- whichYesY
  
  fairLogOut$yName <- yName
  fairLogOut$yLevels <- yLevels
  fairLogOut$yesYVal <- yesYVal
  fairLogOut$deweightPars <- deweightPars
  fairLogOut$sName <- sName
  fairLogOut$factorsInfo <- factorsInfo
  fairLogOut$trainRow1 <- trainRow1
  fairLogOut$scalePars <- scalePars
  fairLogOut$classif <- TRUE
  class(fairLogOut) <- c('qeFairRidgeLog')
  
  if (!is.null(holdout)) {      
    # need to turn off scaling in the case of predicting holdouts, as
    # they have already been scaled
    fairLogOut$scaling <- 'none'  
    fairLogOut$factorsInfo <- NULL
    predictHoldoutFair(fairLogOut)
    fairLogOut$holdIdxs <- holdIdxs
  }
  
  fairLogOut$scaling <- scaling
  
  if (!is.null(sName) && !is.null(holdout)) {
    data2 <- data1
    fairLogOut$corrs <- corrsens(data,yName,fairLogOut,sName)
  }
  
  fairLogOut
}

# processNewx means to apply factorsToDummies() and scaling; it should
# be TRUE for an "external" prediction, FALSE for predicting a holdlout
# set
predict.dsldQeFairRidgeLog <- function(object,newx)
{
  
  # newx can include the sensitive variables, as prepNewx will remove
  # them in present
  
  processNewx <- is.null(attr(newx,'noNeedPrepNewx'))
  if (processNewx) newx <- prepNewx(object,newx)
  if (is.vector(newx)) {
    nr <- 1
  } else{
    nr <- nrow(newx)
  }
  newx <- as.data.frame(newx)
  tmp <- object$glmOuts[[object$whichYesY]]
  class(tmp) <- c('glm','lm')
  preds <- predict(tmp,newx,type='response')
  preds
}

#Example: 1
#NOTE: EXAMPLE DOESN'T WORK BUT CALLS THE EDF FUNCTION
#TODO: test examples after adding functions to namespace
#library(dsld)
#data(svcensus)
#dsldQeFairRidgeLog(svcensus, 'gender', 1, yesYVal=1)
