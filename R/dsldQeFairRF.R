### ------------------------------- dsldQeFairRF ------------------------------
dsldQeFairRF <- function(data,yName,expandVars,expandVals, scaleX = TRUE, sName=NULL,
                         nTree=500,minNodeSize=10,mtry = floor(sqrt(ncol(data))),
                         yesYVal=NULL,holdout=floor(min(1000,0.1*nrow(data)))) {
  
  original_data <- data
  scol <- original_data[,sName]
  data <- data[,!names(data) %in% c(sName)] 
  scaled_data = dsldScaleData(data = data, yName = yName, scaleX = scaleX, 
                              expandVars = expandVars, expandVals = expandVals)

  classif <- is.factor(data[[yName]])
  if (classif) classNames <- levels(data[[yName]])
  rfout <- qeRFranger(data = scaled_data,yName = yName,
                      nTree=nTree,minNodeSize=minNodeSize,mtry=mtry,yesYVal=yesYVal,
                      holdout=holdout)
  
  rfout$classif <- rfout$classif
  rfout$deweightPars <- expandVars
  rfout$deweightVal <- expandVals
  rfout$sName <- sName
  rfout$scol <- scol
  rfout$data <- data
  rfout$scaled_data <- scaled_data
  rfout$scaleX <- scaleX
  rfout$yesYVal <- yesYVal
  rfout$corrsens <- corrsens(original_data, yName, rfout,sName)
  class(rfout) <- c('dsldQeFairRF','qeRF')
  return(rfout)
}

predict.dsldQeFairRF <- function(object,newx)
{
  newx <- prepNewx(object,newx)
  rfout <- object$rfout
  classif <- object$classif
  predict(rfout,newx)
}

# --------------------------------- Working Example -----------------------
# z1 <- dsldQeFairRF(data=svcensus,yName='wageinc',expandVars ='occ', expandVals = 0.1,sName='gender')
# z1$testAcc
# z1$corrsens

# z2 <- dsldQeFairRF(data=svcensus,yName='educ',expandVars ='occ', expandVals = 0.001,sName='gender')
# z2$testAcc
# z2$corrsens
