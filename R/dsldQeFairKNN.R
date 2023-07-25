dsldQeFairKNN <- function(data, yName, deweightPars, sName=NULL,
                      yesYVal=NULL, k=25, scaleX=TRUE, holdout=floor(min(1000,0.1*nrow(data)))) {
   require(qeML)

   scaling <- if(scaleX) 'scale' else 'none'
   prepData(scaling=scaling)

   nonSensNames <- setdiff(names(data),sensNames)
   data1 <- data[nonSensNames]

   y <- data[yName][,1]
   classif <- is.factor(y)
   if (classif) classNames <- levels(y)

   deweightNames <- names(deweightPars)
   deweightVals <- unlist(deweightPars)
   expandVars <- deweightNames
   expandVals <- deweightVals 

   knnout <- qeKNN(data1,yName,k,yesYVal=yesYVal,
      expandVars=expandVars,expandVals=expandVals,,
      holdout=holdout)

   srout <- list(knnout=knnout)
   srout$factorsInfo <- knnout$factorsInfo
   srout$classif <- classif
   srout$deweightNames <- deweightNames
   srout$deweightVals <- deweightVals
   srout$sensNames <- sensNames
   srout$trainRow1 <- trainRow1
   class(srout) <- c('qeFairKNN')
   srout$scalePars <- scalePars
   srout$yesYVal <- yesYVal
   if (!is.null(yesYVal)) {
      lvlsY <- levels(data1[,yName])
      noYVal <- lvlsY[3 - which(lvlsY==yesYVal)]
      srout$noYVal <- noYVal
   }
   if (!is.null(holdout)){
      if (classif) tst[,ycol] <- as.integer(tst[,ycol] == yesYVal)
      predictHoldoutFair(srout)
      srout$corrs <- corrsens(data,yName,srout,sensNames)
   }
   srout
}

predict.dsldQeFairKNN <- function(object,newx,needsSetup=TRUE)
{

   # remove the sensitive variables, if any
   sens <- object$sensNames
   nonsens <- setdiff(colnames(newx),sens)
   newx <- newx[,nonsens]

   if (needsSetup && !is.null(object$factorsInfo)) 
      newx <- factorsToDummies(newx,TRUE,object$factorsInfo)

   if (needsSetup) {
      sps <- object$scalePars
      newx <- scale(newx,center=sps$ctr,scale=sps$scl)
   }
   knnout <- object$knnout

   # have already scaled and dealt with factors, so turn that off
   if (needsSetup) {
      knnout$scalePars <- NULL
      knnout$factorsInfo <- NULL
   }
   
   preds <- predict(knnout,newx)
   # if (knnout$classfic) {
   #    ifelse(preds >= 0.5,knnout$yesYVal,knnout$noYVal)
   # } else as.vector(preds)
   as.vector(preds)
}

# Currently running into some issues when calling the function cannot find function getRow1(data, yName) 
# from prepData in EDF_Misc.R

# library(dsld)
# data(svcensus)
#	dsldQeFairKNN(svcensus, "wageinc", NULL, "gender")
