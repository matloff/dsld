
# dsld wrapper for Matchinf::Match; optional propensFtn must be either
# 'lm' or 'glm' logit

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
      } else {
         tmp <- glm(y ~ x,family=binomial)
      }
      x <- tmp$fitted.values
   }

   matchOut <- Match(Y=y,Tr=s,X=x,estimand='ATE')

   matchOut

}
