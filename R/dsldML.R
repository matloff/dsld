
# like dsldLinear and dsldLogit, but for machine learning prediction algorithms

dsldML <- function(dataName,yName,sName,sComparisonPts='rand5',
   qeMLftnName,opts=NULL)
{

   if (!inherits(yName,'name')) stop('specify yName via quote()')

   data <- get(dataName)
   ycol <- which(names(data) == yName)
   scol <- which(names(data) == sName)
   slevels <- levels(data[,scol])

   do1Slevel <- function(sLevel) 
   {
      subData <- data[data[,scol]==sLevel,]
      subData <- subData[,-scol]
      cmd <- buildQEcall(qeMLftnName,'subData',yName,opts)
      evalr(cmd)
   }

   qeOut <- lapply(slevels,do1Slevel)
   names(qeOut) <- slevels

   if (sComparisonPts=='rand5') 
      sComparisonPts <- data[sample(1:nrow(data),5),-c(ycol,scol)]

   tmp <- sComparisonPts
   for (sl in slevels) {
      preds <- predict(qeOut[[sl]],sComparisonPts)
      if (qeOut[[1]]$classif) {
         if (is.null(preds$probs)) stope('ML function does not return "probs"')
         preds <- preds$probs
      } else preds <- as.vector(preds)
      tmp[[sl]] <- preds
   }
   ### preds <- sapply(qeOut,function(qeo) predict(qeo,sComparisonPts))
   ### cbind(sComparisonPts,as.data.frame(preds))
   tmp

}

