
# ad hoc aid in deciding which covariates one should treat as
# confounders

# we want to find variables C that are correlated with both Y and S

# based on qeRF, which uses the 'randomForests' package

# note: using permutation method, measuring deterioration in accuracy

dsldCHunting <- function(data,yName,sName) 
{

   ycol <- which(names(data) == yName)
   y <- data[,ycol]
   s <- data[,scol]
   scol <- which(names(data) == sName)
   dataNoS <- data[,-scol]  # for predicting Y
   dataNoY <- data[,-ycol]  # for predicting S

   impY <- qeRF(dataNoS,yName)$importance
   impS <- qeRF(dataNoY,sName)$importance
   if (is.numeric(y)) impY1 <- impY[,1] else 
      if(is.factor(y)) impY1 <- impY[,length(levels(y))+1] else 
         stop('Y must be numeric or an R factor')
   if (is.factor(s)) impS1 <- impS[,length(levels(s))+1] else 
         stop('S must be numeric or an R factor')

}
