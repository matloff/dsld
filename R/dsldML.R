
# like dsldLinear and dsldLogit, but for machine learning (i.e.
# nonparametric) prediction algorithms

# args:

#    data, yName, sName as usual 

#    sComparisonPts as in the with-interactions case of dsldLinear()
#    (nonparametric case necessarily has interactions)

#    qeMLftnName is, e.g. 'qeKNN'; opts is an R list of optional arguments
#    for that function

dsldML<-function(data,yName,sName,qeMLftnName,sComparisonPts='rand5',opts=NULL){

  ycol <- which(names(data) == yName)
  scol <- which(names(data) == sName)
  slevels <- levels(data[,scol])
  
  factors_info = factor_levels(data)
  
  if (sComparisonPts=='rand5'){
    rows <- sample(nrow(data), 5)
    reducedData <- data[rows, ]
    columns <- c(yName, sName)
    sComparisonPts <- reducedData[, !(names(reducedData) %in% columns)]
    sComparisonPts <- apply_factor_levels(sComparisonPts, factors_info)
  }
  
  sComparisonPts <- apply_factor_levels(sComparisonPts, factors_info)
  
  # called from lapply(), calling the QE function on the subset of data
  # corresponding to the specified level of the sensitive variable S
  do1Slevel <- function(sLevel) 
  {
    subData <- data[data[,scol]==sLevel,]
    subData <- subData[,-scol]
    opts[['data']] <- subData
    opts[['yName']] <- yName
    do.call(qeMLftnName,opts)
  }
  
  qeOut <- lapply(slevels,do1Slevel)
  names(qeOut) <- slevels

  testAccs <- sapply(qeOut,function(qeo) qeo$testAcc)
  res <- list(testAccs = testAccs)

  tmp <- sComparisonPts
  for (sl in slevels) {
    # predicted values are the values of the estimated regression
    # function, just what we want
    preds <- predict(qeOut[[sl]],sComparisonPts)
    if (qeOut[[1]]$classif) {
      if (is.null(preds$probs)) stop('ML function does not return "probs"')
      preds <- preds$probs
    } else preds <- as.vector(preds)
    tmp[[sl]] <- preds
  }

  res$comparisons <- tmp
  
  return(res) 
}


