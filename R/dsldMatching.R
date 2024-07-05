
# dsld wrapper for Matching::Match; optional propensFtn must be either
# 'glm' for logit or 'knn' for qeKNN

dsldMatchedATE <- function(data,yName,sName,yesSVal,yesYVal=NULL,k=NULL,
   propensFtn=NULL) 
{
   getSuggestedLib("Matching")

   ycol <- which(names(data) == yName)
   y <- data[,ycol]

   if (is.factor(y)) {
      yLvls <- levels(y)
      if (length(yLvls) != 2)
         stop('factor Y can only be dichotomous')
      yNum <- as.integer(y == yesYVal)
      dichotY <- TRUE
   } else {
      yNum <- y
      dichotY <- FALSE
   }


   scol <- which(names(data) == sName)
   s <- data[,scol]
   sLog <- (s == yesSVal)
   sNum <- as.integer(sLog)
   
   x <- data[,-c(ycol,scol)]
   if (!allNumeric(x))
      xNum <- factorsToDummies(x,omitLast=TRUE,dfOut=TRUE)

   if (!is.null(propensFtn)) {
      if (propensFtn == 'glm') {
         matchVals <- glm(sNum ~ xNum,family=binomial)$fitted.values
      } else {  # qeKNN 
         tmp <- qeKNN(data[,-ycol],sName,yesYVal=yesSVal,k=k)
         matchVals <- tmp$regests
      }
      X <- matchVals
   } else X <- xNum



   matchOut <- Matching::Match(Y=y,Tr=sLog,X=xNum,estimand='ATE',ties=FALSE)
   matchOut

}
