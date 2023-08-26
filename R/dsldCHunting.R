
# ad hoc aid in deciding which covariates one should treat as
# confounders

# we want to find variables C that are correlated with both Y and S

# based on qeRF, which uses the 'randomForests' package

# note: using permutation method, measuring deterioration in accuracy

dsldCHunting <- function(data,yName,sName) 
{

   ycol <- which(names(data) == yName)
   scol <- which(names(data) == sName)
   y <- data[,ycol]
   s <- data[,scol]
   dataNoS <- data[,-scol]  # for predicting Y
   dataNoY <- data[,-ycol]  # for predicting S

   impY <- qeRF(dataNoS,yName)$importance
   impS <- qeRF(dataNoY,sName)$importance
   if (is.numeric(y)) impY1 <- impY[,1] else 
      if(is.factor(y)) impY1 <- impY[,length(levels(y))+1] else 
         stop('Y must be numeric or an R factor')
   if (is.factor(s)) impS1 <- impS[,length(levels(s))+1] else 
         stop('S must be numeric or an R factor')
   impY1 <- sort(impY1,decreasing=TRUE)
   impS1 <- sort(impS1,decreasing=TRUE)
   res <- list(impForY=impY1,impForS=impS1)
   nmsY <- names(impY1)
   nmsS <- names(impS1)
   res$inCommon <- list()
   for (i in 1:min(10,ncol(data)-2)) {
      res$inCommon[[i]] <- intersect(nmsY[1:i],nmsS[1:i])
   }
   res


}
