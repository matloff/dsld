

# dsld wrapper for Matching::Match; optional propensFtn must be either
# 'lm', 'glm' for logit, or a qeML function (default arguments only)

dsldMatchedATE <- function(data,yName,sName,yesSVal,yesYVal=NULL,
   propensFtn=NULL) 
{
   getSuggestedLib("Matching")

   ycol <- which(names(data) == yName)
   y <- data[,ycol]

   if (is.factor(y)) {
      yLvls <- levels(y)
      if (length(yLvls) != 2)
         stop('factor Y can only be dichotomous')
      if (is.null(yesYVal))
         stop('must specify yesYVal')
      y <- as.integer(y == yesYVal)
      dichotY <- TRUE
   } else dichotY <- FALSE

   scol <- which(names(data) == sName)
   s <- data[,scol]
   s <- as.integer(s == yesSVal)
   
   x <- data[,-c(ycol,scol)]
   if (!allNumeric(x))
      x <- factorsToDummies(x,omitLast=TRUE,dfOut=TRUE)
   if (!is.null(propensFtn)) {
      if (propensFtn == 'lm') {
         tmp <- lm(y ~ x)
      } else if (propensFtn == 'glm') {
         tmp <- glm(y ~ x,family=binomial)
      } else {  # qeML function
         frmls <- names(formals(propensFtn))
         if (!('yesYVal' %in% frmls)) {
            tmp <- do.call(propensFtn,
               list(data=data[,-scol],yName=yName,holdout=NULL))
         } else {
            tmp <- do.call(propensFtn,
               list(data=data[,-scol],yName=yName,holdout=NULL,yesYVal=yesYVal))
         }
         x <- predict(tmp,data[,-c(ycol,scol)])
         if (is.matrix(x)) x <- as.vector(x)
         # yesSVal doesn't really matter here, just need values for
         # matching
         if (dichotY) {
            if (propensFtn != 'qeKNN') {
               x <- x$probs[,1]
            } else x <- x$probs
         }
      }
   }

   matchOut <- Matching::Match(Y=y,Tr=s,X=x,estimand='ATE',ties=FALSE)
   matchOut

}
