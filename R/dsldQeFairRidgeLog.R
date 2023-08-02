### Currently In-Progress; makes use of the EDFFAIRMisc.R functions
dsldQeFairRidgeLog <- function(data,yName,deweightPars,sensNames=NULL,
                           yesYVal=0,holdout=floor(min(1000,0.1*nrow(data))))
{
  sensNames = sName
  require(qeML)
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
  fairLogOut$sensNames <- sensNames
  fairLogOut$factorsInfo <- factorsInfo
  fairLogOut$trainRow1 <- trainRow1
  fairLogOut$scalePars <- scalePars
  fairLogOut$classif <- TRUE
  class(fairLogOut) <- c('qeFairRidgeLog', 'qeLogit')

  
  if (!is.null(holdout)) {      
    fairLogOut$scaling <- 'none'  
    fairLogOut$factorsInfo <- NULL
    predictHoldoutFair(fairLogOut)
    fairLogOut$holdIdxs <- holdIdxs
  }
  
  fairLogOut$scaling <- scaling
  
  if (!is.null(sensNames) && !is.null(holdout)) {
    data2 <- data1
    fairLogOut$corrs <- corrsens(data,yName,fairLogOut,sensNames)
  }
  
  fairLogOut
  
}

# processNewx means to apply factorsToDummies() and scaling; it should
# be TRUE for an "external" prediction, FALSE for predicting a holdlout
# set
predict.dsldQeFairRidgeLog <- function(object,newx)
{
  processNewx <- is.null(attr(newx,'noNeedPrepNewx'))
  if (processNewx) newx <- prepNewx(object,newx)
  if (is.vector(newx)) {
    nr <- 1
  } else{
    nr <- nrow(newx)
  }
  newx <- as.data.frame(newx)
  #tmp <- object$glmOuts[[object$whichYesY]]
  class(object) <- c('qeLogit','glm')
  preds <- predict(object,newx,type='response')
  return(preds)
}

# ------------------------------------------------------------------------------------------------
#Example: 1
#load('/Users/adityamittal/Desktop/Year_two/Spring_2023/ECS_189G/hw2/law.school.admissions.rda')                        
#drop <- c('fulltime','cluster')
#law.school.admissions <- law.school.admissions[, !(names(law.school.admissions) %in% drop)]
#law.school.admissions$bar <- as.integer(as.logical(law.school.admissions$bar))
#law.school.admissions$bar <- as.factor(law.school.admissions$bar)
#z <- dsldQeFairRidgeLog(data=law.school.admissions,yName='bar',deweightPars=list(fam_inc=0.2),sensNames='gender', yesYVal = '1')
