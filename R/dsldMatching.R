

# finds the estimated mean difference between`the matched Y pairs in the
# treated/nontreated (exposed and nonn-exposed) groups, with covariates
# X in 'data' other than the yName and sName columns

# sName here is the "treatment" or "exposure," S

# dsld wrapper for Matching::Match; optional propensFtn must be either
# 'glm' for logit or 'knn' for qeKNN

# in that optional case, we estimate P(S = 1 | X), either by a logistic
# or k-NN model

# due to the fact that various function calls require different argument
# types, we may generate several different versions of a variable; e.g.
# S is a factor but we also need logical and numeric versions

dsldMatchedATE <- function(data,yName,sName,yesSVal,yesYVal=NULL,
   propensFtn=NULL,k=NULL) 
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
   else xNum <- as.matrix(x)

   if (!is.null(propensFtn)) {
      if (propensFtn == 'glm') {
         matchVals <- glm(sNum ~ xNum,family=binomial)$fitted.values
      } else {  # qeKNN 
         tmp <- qeKNN(data[,-ycol],sName,yesYVal=yesSVal,k=k,holdout=NULL)
         matchVals <- tmp$regests
      }
      xNum <- matchVals
   } 

   matchOut <- Matching::Match(Y=y,Tr=sLog,X=xNum,estimand='ATE',ties=FALSE)
   matchOut

}


