### ------------------------------- dsldQeFairRF ------------------------------
dsldQeFairRF <- function(data,yName, sName=NULL,deweightPars, scaleX = TRUE, nTree=500,
                         minNodeSize=10,mtry = floor(sqrt(ncol(data))),
                         yesYVal=NULL,holdout=floor(min(1000,0.1*nrow(data)))) {
  scol <- data[,sName]
  data <- data[,!names(data) %in% c(sName)]
  
  classif <- is.factor(data[[yName]])
  if (classif) classNames <- levels(data[[yName]])
  
  rf <- qeRFranger(data = data,yName = yName, nTree=nTree,minNodeSize=10,
                   mtry=floor(sqrt(ncol(data)))+1,yesYVal=yesYVal,
                   holdout=holdout, deweightPars = deweightPars)
  
  rfOut <- list(rfOut=rf)                                                       # create instance of qeRF (to be used in predict)
  rfOut$testAcc <- rf$testAcc                                                   # add useful information
  rfOut$baseAcc <- rf$baseAcc
  rfOut$trainAcc <- rf$trainAcc
  rfOut$deweightPars <- deweightPars
  rfOut$classif <- classif
  rfOut$sName <- sName
  rfOut$yName <- yName
  rfOut$holdIdxs <- rf$holdIdxs
  rfOut$holdoutPreds <- rf$holdoutPreds
  rfOut$scol <- scol
  rfOut$data <- data
  df <- cbind(data,scol)                                                        # data-frame to be used in corrsens()
  colnames(df)[colnames(df) == "scol"] = sName
  rfOut$Sdata <- df
  rfOut$yesYVal <- yesYVal
  rfOut$corrsens <- corrsens(df, yName, rfOut,sName)
  class(rfOut) <- c('dsldQeFairRF')
  return(rfOut)
}

### ---------------------------- predict() -------------------------------------
predict.dsldQeFairRF <- function(object,newx,needsSetup=TRUE)
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

# working example::
# data(svcensus)
# z1 <- dsldQeFairRF(data=svcensus,yName='wageinc',deweightPars = list(occ =0.1),sName='gender')
# z1$testAcc
# z1$corrsens

# z2 <- dsldQeFairRF(data=svcensus,yName='wageinc',deweightPars = list(occ =0.9),sName='gender')
# z2$testAcc
# z2$corrsens

# predict()
#newData <- data.frame(age = c(18, 60), educ = c("zzzOther", 'zzzOther'), 
#                      wkswrkd = c(50, 50), occ = c("106", "106"), gender = c('male','male'))

#predict(z1,newData)
#predict(z2,newData)
