# qeFair*() arguments:
# data:  dataframe, training set; class labels col is a factor; other columns may be factors
# yName: column name for outcome variable; vector indicates regression, factor classification 
# selectProbs: probabilities that specified features will be selected
# sensNames: sensitive variables to be excluded from the ML analysis possible algorithm-specific options
# holdout: size of holdout set, if any

# value: see individual functions below
# predict() arguments:
# object:  output from q*()
# newx:  data frame of points to be predicted possible options

# value: R list with components as follows:

# classification case:
# ypreds:  R factor instance of predicted class labels, one element for each row of newx 
# conditprobs:  vector/matrix of class probabilities; in the 2-class case, a vector, 
# the probabilities of Y = 1

# regression case: vector of predicted values

# -------------------------------  qeFairRF()  ---------------------------------
dsldQeFairRF <- function(data,yName,deweightPars,sensNames=NULL,
                     nTree=500,minNodeSize=10,mtry = floor(sqrt(ncol(data))),
                     yesYVal=NULL,holdout=floor(min(1000,0.1*nrow(data))))
{
  require(qeML)
  
  prepData(1,scaling='none')
  
  rfout <- qeRFranger(data2,'y',
                      nTree=nTree,minNodeSize=minNodeSize,mtry=mtry,
                      deweightPars,yesYVal=yesYVal,holdout=holdout)
  
  fairRFout <- list(rfout=rfout)
  fairRFout$classif <- rfout$classif
  fairRFout$deweightPars <- deweightPars
  fairRFout$sensNames <- sensNames
  fairRFout$trainRow1 <- trainRow1
  fairRFout$factorsInfo <- factorsInfo
  class(fairRFout) <- c('qeFairRF')
  fairRFout$holdIdxs <- rfout$holdIdxs
  fairRFout$holdoutPreds <- rfout$holdoutPreds
  fairRFout$testAcc <- rfout$testAcc
  fairRFout$baseAcc <- rfout$baseAcc
  fairRFout$confusion <- rfout$confusion
  fairRFout$scaling <- 'none'
  
  if (!is.null(sensNames) && !is.null(holdout)) {
    fairRFout$corrs <- corrsens(data,yName,rfout,sensNames)
    if (fairRFout$classif)
      fairRFout$sensConfusion <- calcSensConfusion(data,data1,yName,
                                                   fairRFout$holdIdxs,fairRFout$holdoutPreds,sensNames)
  }
  
  fairRFout
}

predict.dsldQeFairRF <- function(object,newx)
{
  newx <- prepNewx(object,newx)
  rfout <- object$rfout
  classif <- object$classif
  predict(rfout,newx)
}

# Example 1
#library(dsld)
#data("svcensus")
#z <- dsldQeFairRF(data=svcensus,yName='wageinc',deweightPars=list(occ=0.2),sensNames='gender')
#z$testAcc
