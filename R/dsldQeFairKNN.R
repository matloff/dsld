### DsldQeFairKNN:: Works with the predict() function!!!

### ------------------------------- dsldQeFairKNN ------------------------------
dsldQeFairKNN <- function(data, yName, sName, deweightPars, scaleX = TRUE, yesYVal= NULL,
                          k=25, holdout=floor(min(1000,0.1*nrow(data)))) {
  
  ### I'm wrapping the qeKNN function and using expandVars/expandVals args.
  
  # remove sName
  scol <- data[,sName]
  data <- data[,!names(data) %in% c(sName)]
  
  ### qeML has an issue with de-weighting categorical data; factorsToDummies encodes the data by
  ### level and then 'expandVars' doesn't know what the actual column is being referred to.
  ### Ex: dataset used 'svcensus'.
  ### ExpandVars = 'occ' which is an R factor. factortoDummies will convert occ to many columns
  ### "occ.100,occ.101,occ.106.... etc", so 'occ' itself will no longer be a column.
  ### The following chunk of codes reformats the deweightPars argument and converts them into 
  ### readable format used by qeKNN so they can be used properly in case that the covariate 
  ### is either numeric or categorical.
  
  cName <- names(deweightPars)                                                  # create the expandVars & expandVals argument from deweightPars
  cVal <- c()
  for (i in 1:length(deweightPars)) {
    cVal <- c(cVal, deweightPars[[i]])
  }
  
  l <- length(cName)
  expandPars <- c()
  expandVals <- c()
  
  for (i in 1:l) {
    cI <- cName[i]
    cValI <- cVal[i]
    if (!is.numeric(data[,cI])) {
      cGroups <- factorToDummies(data[, cI], cI, omitLast = TRUE)
      expandPars <- c(expandPars,colnames(cGroups))
      expandVals <- c(expandVals, rep(cValI, ncol(cGroups)))
    } else {
      expandPars <- c(expandPars,cI)
      expandVals <- c(expandVals,cValI)
    }
  } 
  
  classif <- is.factor(data[[yName]])
  if (classif) classNames <- levels(data[[yName]])
  
  # run qeKNN 
  knn <- qeKNN(data = data,yName = yName,k = k, 
               scaleX = scaleX, expandVars = expandPars, expandVals = expandVals,
               yesYVal = yesYVal, holdout = holdout)
  
  knnOut <- list(knnOut=knn)                                                    # create instance of qeKNN (to be used in predict)
  knnOut$testAcc <- knn$testAcc                                                 # add useful information
  knnOut$baseAcc <- knn$baseAcc
  knnOut$deweightNames <- expandPars
  knnOut$deweightVals <- expandVals
  knnOut$factorsInfo <- knn$factorsInfo
  knnOut$classif <- classif
  knnOut$sName <- sName
  knnOut$yName <- yName
  knnOut$holdIdxs <- knn$holdIdxs
  knnOut$holdoutPreds <- knn$holdoutPreds
  knnOut$scol <- scol
  knnOut$data <- data
  df <- cbind(data,scol)                                                        # data-frame to be used in corrsens()
  colnames(df)[colnames(df) == "scol"] = sName
  knnOut$Sdata <- df
  knnOut$scaleX <- scaleX
  knnOut$yesYVal <- yesYVal
  knnOut$corrsens <- corrsens(df, yName, knnOut,sName)
  class(knnOut) <- c('dsldQeFairKNN')
  return(knnOut)
}

### ---------------------------- predict() -------------------------------------
predict.dsldQeFairKNN <- function(object,newx,needsSetup=TRUE)
{
  # removes sName if the user has provided it
  sName <- object$sName
  yName <- object$yName
  
  if (sName %in% names(newx)) {
    newx <- newx[, !(names(newx) %in% sName), drop = FALSE]
  }
  
  # uses instance of qeKNN set in 1st element of qeFairKNN that is used in predict.qeKNN() w/ the deweighted params.
  predict_object <- object[[1]]
  preds <- predict(predict_object, newx)
  return(preds)
}

### -------------------------- working example ---------------------------------
# data(svcensus)
# k1 = dsldQeFairKNN(data = svcensus,yName = 'wageinc', sName = 'gender', deweightPars = list(occ=1))     # no deweighting done
# k1$deweightNames
# k1$deweightVals
# k1$holdoutPreds
# k1$holdIdxs
# k1$sName
# k1$data
# k1$testAcc
# k1$baseAcc
# k1$corrsens

# k1[[1]]$expandVars
#k1[[1]]$expandVals

# k2 = dsldQeFairKNN(data = svcensus,yName = 'wageinc', sName = 'gender', deweightPars = list(occ=0.1))   # de-weight 'occ' = 0.1
# k2$testAcc
# k2$deweightNames
# k2$deweightVals

# k2[[1]]$expandVars
# k2[[1]]$expandVals

# k3 = dsldQeFairKNN(data = svcensus,yName = 'wageinc', sName = 'gender', deweightPars = list(occ=0.1, educ=0.9))   # multiple proxies
# k3$testAcc
# k3$deweightNames
# k3$deweightVals

# k3[[1]]$expandVars
# k3[[1]]$expandVals

## predict() example
#newData <- data.frame(age = c(18, 60), educ = c("zzzOther", 'zzzOther'),
#                      wkswrkd = c(50, 50), occ = c("106", "106"), gender = c('male','male'))

#predict(k1,newData)
#predict(k2,newData)
#predict(k3,newData)
