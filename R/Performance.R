# investigate Fairness-Utility Tradeoff

# runs a single hyperparameter value (unfairness or deweightPars), on a
# single train/test partition; use regtools::fineTuning to do many
# hyperparameter values or many partitions

# arguments

#   data,yName,sName: the usual
#   dsldFtnName: one of the 'dsldF*' functions wrapping fairML or
#                one of the 'dsldQeFair*' functions wrapping EDF;
#                Y,S must be binary (factor) or continuous
#   yesYVal,yesSVal: the usual
#   unfairness: use this if calling a fairML function
#   deweightPars: use this if calling an EDF function
#   holdout: as in qeML etc.

# value: 

#    testAcc as in qeML 

#    corr(T,W)^2 in lieu of corr(Yhat,S)^2, where

#       T =  P(Y = 1 | X,S) in binary Y case; else T = Yhat
#       W =  P(S = 1 | X) in binary S case; else W = S

# examples

#   tradeoff(svcensus,'wageinc','gender','dsldFrrm',0.2,yesSVal='male') 
#   race1 <- lsabw$race1
#   race1 <- as.character(lsabw$race1)
#   lsabw$race1 <- as.factor(race1)
#   tradeoff(lsabw,'bar','race1','dsldQeFairRidgeLog',
#      deweightPars=list(fam_inc=0.1),yesYVal='TRUE',yesSVal='white')

tradeoff <- function(data,yName,sName,dsldFtnName,
   unfairness=NULL,deweightPars=NULL,yesYVal=NULL,yesSVal=NULL,
   holdout = floor(min(1000, 0.1 * nrow(data)))) 
{

   if (dsldFtnName == 'dsldFgrrm')
      stop('dsldFgrrm may not be used here at this time')
   # train, test etc.
   allrows <- 1:nrow(data)
   tstrows <- sample(allrows,holdout)
   trnrows <- setdiff(allrows,tstrows)
   trn <- data[trnrows,]
   tst <- data[tstrows,]
   ycol <- which(names(data) == yName)
   scol <- which(names(data) == sName)
   allcols <- 1:ncol(data)
   xcols <- setdiff(allcols,c(ycol,scol))
   trnx <- trn[,xcols]
   tstx <- tst[,xcols]
   trnxs <- trn[,c(xcols,scol)]
   trnxnos <- trn[,xcols]
   tstxs <- tst[,c(xcols,scol)]
   trny <- trn[,ycol]
   tsty <- tst[,ycol]
   trns <- trn[,scol]
   if (is.factor(trns) && length(levels(trns)) > 2)
      stop('S must be numeric or binary')
   tsts <- tst[,scol]

   classif <- is.factor(trny)

   # fit model on training set
   if (substring(dsldFtnName,1,10) == 'dsldQeFair') {
      fitted <- do.call(dsldFtnName,
         list(data=trn,yName=yName,sName=sName,deweightPars=deweightPars))
   } else {
      pars <- list(data=trn,yName=yName,sName=sName,unfairness=unfairness)
      # if (!is.null(yesYVal)) pars$yesYVal <- yesYVal
      # if (!is.null(yesSVal)) pars$yesSVal <- yesSVal
      fitted <- do.call(dsldFtnName,pars)
   }

   # predict holdout
   ypreds <- predict(fitted,tstxs)

   # find T, W
   T <- ypreds
   if (!is.null(T$probs)) T <- T$probs
   if (is.factor(trns)) {
      tmp <- qeLogit(trnxs,sName,holdout=NULL,yesYVal=yesSVal)
      W <- predict(tmp,tstx)$probs
   } else W <- tsts
   T <- as.vector(T)
   W <- as.vector(W)

   # 'fitted' has no testAcc, must generate it
   if (!classif) {
      testAcc <- mean(abs(tsty - ypreds))
   } else {
      if (!is.null(ypreds$probs)) ypreds <- ypreds$probs
      preds10 <- as.integer(ypreds > 0.5)
      tsty10 <- as.integer(tsty == yesYVal)
      testAcc <- mean(preds10 != tsty10)
   }

   c(testAcc,cor(T,W)^2) 

}

