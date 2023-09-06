# ------------------------ qeFairRidgeLin --------------------------------------
dsldQeFairRidgeLin <- function(data,yName,sName,deweightPars=NULL,
                           holdout=floor(min(1000,0.1*nrow(data))))
{
  
  require(qeML)
  
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
  # for now, dataNonSensTrn[1:p,] will be placeholder
  dataExtended <- rbind(dataNonSensTrn,dataNonSensTrn[1:p,])
  n <- nrow(dataNonSensTrn)
  newRows <- (n+1):(n+p)
  tmp <- rep(0,p)
  names(tmp) <- namesX
  tmp[deweightNames] <- sqrt(deweightVals)
  newx <- as.data.frame(diag(tmp))
  names(newx) <- namesX
  dataY <- dataNonSens[,yName]
  newy <- rep(0,p)
  yExtended <- c(dataY,newy)
  dataExtended[newRows,-yCol] <- newx
  dataExtended[,yCol] <- yExtended
  names(dataExtended)[yCol] <- yName
  
  fairLinOut <- qeLin(dataExtended,yName,noBeta0=TRUE,holdout=NULL)
  tmp <- data[,yName]
  fairLinOut$yBar <- mean(tmp)
  fairLinOut$yName <- yName
  fairLinOut$deweightPars <- deweightPars
  fairLinOut$sName <- sName
  fairLinOut$factorsInfo <- factorsInfo
  fairLinOut$trainRow1 <- getRow1(data1,yName)
  fairLinOut$trainRow1 <- trainRow1
  fairLinOut$scalePars <- scalePars
  fairLinOut$classif <- FALSE
  class(fairLinOut) <- c('dsldQeFairRidgeLin','qeLin')
  
  if (!is.null(holdout)) {      
    # need to turn off scaling in the case of predicting holdouts, as
    # they have already been scaled
    fairLinOut$scaling <- 'none'  
    fairLinOut$factorsInfo <- NULL
    predictHoldoutFair(fairLinOut)
    fairLinOut$holdIdxs <- holdIdxs
  }
  
  fairLinOut$scaling <- scaling
  
  if (!is.null(sName) && !is.null(holdout)) {
    data2 <- data1
    fairLinOut$corrs <- corrsens(data,yName,fairLinOut,sName)
  }
  
  fairLinOut
  
}

# ------------------------ predict.dsldQeFairRidgeLin --------------------------
# processNewx means to apply factorsToDummies() and scaling; it should
# be TRUE for an "external" prediction, FALSE for predicting a holdlout
# set
predict.dsldQeFairRidgeLin <- function(object,newx,processNewx=FALSE)
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
  class(object) <- c('qeLin','lm')
  preds <- predict(object,newx,FALSE)
  preds + object$yBar
}


# ------------------------ Example 1 -------------------------------------------
#library(dsld)
#data("svcensus")
#z <- dsldQeFairRidgeLin(data=svcensus,yName='wageinc',deweightPars=list(occ=0.2),sName='gender')
#z$testAcc
