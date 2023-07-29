dsldQeFairRF <- function(data,yName,deweightPars,sensNames=NULL,
   nTree=500,minNodeSize=10,mtry = floor(sqrt(ncol(data))),
   yesYVal=NULL,holdout=floor(min(1000,0.1*nrow(data))))
{
   require(qeML)
   if (!require('gtools')) install.packages('gtools'); library('gtools')

   prepData(1,scaling='none')

   rfout <- qeRFranger(data2,'y',
      nTree=nTree,minNodeSize=minNodeSize,mtry=mtry,
      deweightPars,yYesName=yesYVal,holdout=holdout)

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
 

# library(dsld)
# data(svcensus)
# dsldQeFairRF(svcensus, "wageinc", svcensus["age", "edu", "occ"],"gender")
# Currently not working. Not quite sure what argument deweightPars is used for and what is its data type?
