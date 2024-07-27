
# like dsldLinear and dsldLogit, but for machine learning prediction algorithms

# args:

#    dataName (must be quoted!), yName (must be called from quote()),
#    sName as usual 

#    sComparisonPts as in the with-interactions case of dsldLinear()

#    qeMLftnName is, e.g. 'qeKNN'); opts is an R list of optional arguments
#    for that function

dsldML<-function(data,yName,sName,sComparisonPts='rand5',
   qeMLftnName,opts=NULL,holdout=NULL){

  # args checking
  # if (!inherits(yName,'name')) stop('specify yName via quote()')
  
  
  # execute params
  data <- get(dataName)
  ycol <- which(names(data) == yName)
  scol <- which(names(data) == sName)
  slevels <- levels(data[,scol])
  
  if (sComparisonPts=='rand5'){
    rows <- sample(nrow(data), 5)
    reducedData <- data[rows, ]
    columns <- c(yName, sName)
    sComparisonPts <- reducedData[, !(names(reducedData) %in% columns)]
  }

  # called from lapply(), calling the QE function on the subset of data
  # corresponding to the specified level of the sensitive variable S
  do1Slevel <- function(sLevel) 
  {
    subData <- data[data[,scol]==sLevel,]
    subData <- subData[,-scol]
    qeMLftn <- get(qeMLftnName)
    opts[['data']] <- data
    opts[['yName']] <- yName
    opts[['sName']] <- sName
    do.call(qeMLftn,opts)
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


