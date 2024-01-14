#dsldQeSU(), a wrapper for EDFfair::qeSU()
#usage:
#EDFfair::qeSU(data, yName, deweightPars, sensNames,lambda, yesYVal, holdout)

dsldQeSU <- function(data, yName, deweightPars, 
                     sName, lambda, yesYVal=NULL,
                     holdout=floor(min(1000,0.1*nrow(data))))

  # EDFfair::qeSU(data=data,yName=yName,deweightPars=deqeightPars,
  #           sensNames=sName,lambda=lambda, yesYVal=yesYVal,
  #           holdout=holdout)
  {

   # These libraries were recently added to the DESCRIPTION file
   # require(qeML)
   # require(fairml)

   data <- na.exclude(data)

   unfairness <- deweightPars$unfairness
   y <- data[,yName]
   classif <- is.factor(y)
   if (classif) {
      if (is.null(yesYVal))
         stop('must set yesYVal for classification case')
      ## y <- (y == yesYVal)
      ## data[,yName] <- as.double(as.integer(y))
      suFtn <- fgrrm
   } else suFtn <- frrm

   if (!is.null(holdout)) {
      holdIdxs <- sample(1:nrow(data),holdout)
      trn <- data[-holdIdxs,]
      tst <- data[holdIdxs,]
   } else {
      trn <- data
      holdIdxs <- NULL
   } 

   # where are they in the columns?
   allNames <- names(trn)
   findidx <- function(sn) grep(sn,allNames)
   sensCols <- sapply(sName,findidx)
   if (length(sName) > 1)
      stop('currently only one S variable is allowed')
   yCol <- which(names(trn) == yName)
   xCols <- setdiff(1:ncol(trn),union(yCol,sensCols))
   sensVar <- trn[,sensCols]

   # fairml quirk: integer isn't considered numeric
   for (i in 1:ncol(trn)) {
      trnCol <- trn[,i]
      if (is.integer(trnCol)) trn[,i] <- as.double(trnCol)
   }

   suOut <- 
      suFtn(trn[,yCol],trn[,xCols],trn[,sensCols],unfairness,lambda=lambda)

   # fill in info somehow lost by suFtn
   s1classes <- class(sensVar)
   names(s1classes) <- 'S1'
   suOut$data$senstive$classes <- s1classes
   suOut$data$sensitive$levels$S1 <- levels(sensVar)

   suOut$unfairness <- unfairness
   suOut$sName <- sName
   suOut$nonsName <- setdiff(names(data),sName)
   suOut$holdIdxs <- holdIdxs

   classif <- identical(suFtn,fgrrm)
   suOut$classif <- classif
   attr(data,'classif') <- classif
   suOut$holdIdxs <- holdIdxs
   suOut$yesYVal <- yesYVal
   suOut$noYVal <- setdiff(levels(data[,yCol]),yesYVal)

   suOut$sensCols <- sensCols
   suOut$xCols <- xCols
   suOut$yCol <- yCol
   suOut$scaling <- 'none'
   suOut$trainRow1 <- getRow1(data[suOut$nonsName],yName)

   class(suOut) <- c('qeSU',class(suOut))

   if (!is.null(holdout)) {
      preds <- predict(suOut,tst[,xCols],tst[,sensCols])
      suOut$holdoutPreds <- preds
      if (!classif) 
         suOut$testAcc <- mean(abs(tst[,yCol] - preds))
      else {
         predClasses <- round(preds)
         predClasses <- 
            ifelse(predClasses,suOut$yesYVal,suOut$noYVal)
         suOut$testAcc <- mean(predClasses != tst[,yCol])
      }
      suOut$holdIdxs <- holdIdxs
   } else suOut$holdIdxs <- NULL

   if (!is.null(sName) && !is.null(holdout)) {
      suOut$corrs <- corrsens(data,yName,suOut,sName)
   }

   suOut
}

predict.qeSU <- function(object,newx,newsens)
{
   ## processNewx <- is.null(attr(newx,'noNeedPrepNewx'))
   ## if (processNewx) newx <- prepNewx(object,newx)

   # fairml quirk: integer isn't considered numeric
   for (i in 1:ncol(newx)) {
      newxCol <- newx[,i]
      if (is.integer(newxCol)) newx[,i] <- as.double(newxCol)
   }
   if (is.integer(newsens)) newsens <- as.double(newsens)

   class(object) <- class(object)[-1]
   predict(object,newx,newsens)
}


#No examples found