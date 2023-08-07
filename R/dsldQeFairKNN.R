### ------------------------------- dsldQeFairKNN ------------------------------
dsldQeFairKNN <- function(data, yName, sName, expandVars = NULL, expandVals = NULL, 
                          scaleX = TRUE, yesYVal= NULL,k=25,
                          holdout=floor(min(1000,0.1*nrow(data)))) {
  original_data <- data
  scol <- original_data[,sName]
  data <- data[,!names(data) %in% c(sName)] 
  scaled_data = dsldScaleData(data = data, yName = yName, scaleX = scaleX, 
                              expandVars = expandVars, expandVals = expandVals)
  
  classif <- is.factor(data[[yName]])
  if (classif) classNames <- levels(data[[yName]])
  
  knnOut <- qeKNN(data = scaled_data,yName = yName,k = k, 
                  scaleX = FALSE, yesYVal = yesYVal, holdout = holdout)
  
  knnOut$classif <- classif
  knnOut$deweightNames <- expandVars
  knnOut$deweightVals <- expandVals
  knnOut$sName <- sName
  knnOut$scol <- scol
  knnOut$data <- data
  knnOut$scaled_data <- scaled_data
  knnOut$scaleX <- scaleX
  knnOut$yesYVal <- yesYVal
  knnOut$corrsens <- corrsens(original_data, yName, knnOut,sName)
  class(knnOut) <- c('dsldQeFairKNN','qeKNN')
  return(knnOut)
}

# Working example:
# library(dsld)
# data("svcensus")
# knn1 = dsldQeFairKNN(data = svcensus,yName = 'wageinc',sName = 'gender', expandVars = 'occ', expandVals = 0.1, scaleX = TRUE)
# knn1$testAcc
# knn1$corrsens

# knn2 = dsldQeFairKNN(data = svcensus,yName = 'educ',sName = 'gender', expandVars = 'occ', expandVals = 0.1, scaleX = TRUE)
# knn2$testAcc
# knn2$corrsens


# data("law.school.admissions")
# a2 <- dsldQeFairKNN(data = law.school.admissions,yName = 'lsat',sName = 'race1', expandVars = 'fam_inc', expandVals = 0.1, scaleX = TRUE)
# a2$testAcc
# a2$corrsens
