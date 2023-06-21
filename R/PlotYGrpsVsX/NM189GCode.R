
# plots Y against X, with restrictions condits

# arguments:

#   data, yName: as in qeML functions
#   xName: name of a numeric column
#   sName: name of the sensitive variable, an R factor
#   conditDisparity: an R vector; each component is a
#      character string for an R logial expression,
#      representing a desired condition; these must NOT
#      involve sName
#   qeFtn: reg function (defaults only)
#   minS: minimum size fo an S group to be retained in the analysis
#   yLim: a 2-element vector specifying the lower and upper vertical plot limits 
#   useLoess: if TRUE, do Loess smoothing on the regression values

conditDisparity <- function(data,yName,sName,xName,condits,qeFtn,
   minS=50,yLim=NULL,useLoess=TRUE) 
{

   if (is.null(yLim)) yLim <- c(0,max(data[[yName]]))

   # obtain the restricted data
   if (length(condits) > 1) 
      condits <- paste(condits,collapse=' & ')
   cmd <- sprintf('smallData <- subset(data,%s)',condits)
   evalr(cmd)
   
   # won't use the restricting variables anymore
   smallData <- smallData[c(yName,xName,sName)]

   sCol <- which(names(smallData) == sName)

   # group the data by S level, but excluding levels with few datapoints
   s <- smallData[[sName]]
   groupByS <- split(smallData,s)
   sizes <- sapply(groupByS,nrow)
   tiny <- which(sizes < minS)
   if (length(tiny) > 0) groupByS <- groupByS[-tiny]

   # plot one curve for each (remaining) S level
   clrs <- c('black','red','blue','green','yellow','brown','purple')
   sLevels <- names(groupByS)  # not levels(s), if some have been excluded
   for (i in 1:length(sLevels)) { 

      tmp <- groupByS[[i]][,-sCol]
      xs <- unique(tmp[[xName]])
      xsdf <- as.data.frame(xs)
      names(xsdf) <- xName

      # fit ML model
      qeOut <- qeFtn(tmp,yName,holdout=NULL)
      regs <- predict(qeOut,xsdf)
   
      # plot
      xs <- as.vector(xs)
      oxs <- order(xs)
      xs <- xs[oxs]
      regs <- as.vector(regs)
      regs <- regs[oxs]
      plotdf <- data.frame(xs,regs)
      if (useLoess) 
         regs <- loess(regs ~ xs,plotdf)$fitted
      if (i == 1) {
         plot(xs,regs,type='l',lty='solid',ylim=yLim,col=clrs[i])
      } else {
         points(xs,regs,type='l',lty='solid',col=clrs[i])
      }
   }
   
}

