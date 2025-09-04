
# ad hoc aid in deciding which covariates one should treat as
# confounders

# we want to find variables C that are correlated with both Y and S

# based on qeRF, which uses the 'randomForests' package; its output
# includes a variable importance measure

# importance here uses the permutation method, measuring deterioration
# in prediction accuracy resulting from shuffling the given data column;
# the greater the deterioration, the more important the variable

# 'intersectDepth' specifies the number of prediction sets for each of Y
# and S to examine for intersection; in datasets with many predictors,
# this probably should be set to a larger value, or else each
# intersection may be null

dsldCHunting <- function(data, yName, sName, intersectDepth = 10) {

   ycol <- which(names(data) == yName)
   scol <- which(names(data) == sName)
   y <- data[, ycol]
   s <- data[, scol]

   dataNoS <- data[, -scol]  # for predicting Y
   dataNoY <- data[, -ycol]  # for predicting S

   impY <- qeML::qeRF(dataNoS, yName)$importance
   impS <- qeRF(dataNoY, sName)$importance

   # the 'importance' output format has several different cases, which
   # must be dealt with separately in extracting the actual importance
   # vector
   nlevsY <- length(levels(y))
   if (is.numeric(y) || nlevsY == 2) 
       impY1 <- impY[, 1]
   else if (is.factor(y)) {
       impY1 <- impY[, nlevsY + 1]
   }
   else stop("Y must be numeric or an R factor")
   if (!is.factor(s)) stop("S must be an R factor")
   nlevsS <- length(levels(s))
   if (nlevsS == 2) impS1 <- impS[, 1] else impS1 <- impS[, nlevsS + 1]

   # larger values mean higher importance
   impY1 <- sort(impY1, decreasing = TRUE)
   impS1 <- sort(impS1, decreasing = TRUE)

   # start assembling output
   res <- list(impForY = impY1, impForS = impS1)
   nmsY <- names(impY1)
   nmsS <- names(impS1)
   res$inCommon <- list()

   # for each i, find the "top i" set of confounders, defined as being
   # highly correlated with both Y and S
   for (i in 1:min(intersectDepth, ncol(data) - 2)) {
      res$inCommon[[i]] <- intersect(nmsY[1:i], nmsS[1:i])
   }

   return(res)
}


# ad hoc aid in deciding which covariates one should treat as
# proxies

# we want to find variables O that are correlated with S; S need not be
# binary/categorical

# based on cor(), using Kendall's Tau in order to acccomdate binary
# variables (0,1 valued), and to mitigate effects of outliers

dsldOHunting <- function(data,yName,sName) 
{
  
  ycol <- which(names(data) == yName)
  scol <- which(names(data) == sName)
  
  sdumms <- regtools::factorsToDummies(data[,scol,drop=FALSE])
  odumms <- regtools::factorsToDummies(data[,-c(ycol,scol),drop=FALSE])
  
  cor(sdumms,odumms,method='kendall')
  
}


