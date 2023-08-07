### I've created functions to aid the qeFairFTN()

### Code taken from matloff/qeML 
newMultCols <- function (x,cols,vals) {
  partx <- x[,cols,drop=FALSE]
  nvals <- length(vals)
  x[,cols] <- partx %*% diag(vals,nrow=nvals,ncol=nvals)
  return(x)
}

#---------------------------------- dsldScaleData ------------------------------

### This function will rescale the data if user desires; it will also deweight the selected covariate.
dsldScaleData <- function(data, yName, scaleX = TRUE, expandVars = NULL, expandVals = NULL) {
  y <- data[,yName]
  x <- data[,!names(data) %in% c(yName)]
  
  
  if (!is.numeric(x)) {
    x <- regtools::factorsToDummies(x,omitLast=TRUE)
    factorsInfo <- attr(x,'factorsInfo') 
  } else factorsInfo <- NULL
  
  xm <- as.matrix(x)
  
  if (scaleX) {
    xm <- scale(xm)
    ctr <- attr(xm,'scaled:center')
    scl <- attr(xm,'scaled:scale')
    scalePars <- list(ctr=ctr,scl=scl)
  } else scalePars <- NULL
  
  if (!is.null(expandVars)) {
    dta <- xm[,grep(expandVars, colnames(xm), value = TRUE),drop=FALSE]
    dta <- rbind(expandVals,dta)
    dta <- as.data.frame(dta)
    tmp <- regtools::factorsToDummies(dta,omitLast=TRUE)
    expandVars <- colnames(tmp)
    expandVals <- tmp[1,]
    
    for (i in 1:length(expandVars)) {
      j <- which(expandVars[i] == colnames(x))
      expandVars[i] <- j
    }
    expandVars <- as.numeric(expandVars)
    xm <- newMultCols(xm,expandVars,expandVals)
  }
  xm <- as.data.frame(xm)
  #df = cbind(xm,y)
  xm[[yName]] <- y
  return(xm)
}

#data("svcensus")
#df1 = dsldScaleData(data = svcensus, yName = 'wageinc',scaleX = FALSE,expandVars = 'occ', expandVals = NULL)
#df2 = dsldScaleData(data = svcensus, yName = 'wageinc',scaleX = FALSE,expandVars = 'wkswrkd', expandVals = 0.1)
#df3 = dsldScaleData(data = svcensus, yName = 'educ',scaleX = FALSE,expandVars = 'wkswrkd', expandVals = 0.1)

#------------------------------------ Corrsens ---------------------------------
corrsens <- function(data,yName,fittedObject,sensNames=NULL) 
{
  classif <- fittedObject$classif
  if (is.null(classif)) classif <- attr(data,'classif')
  
  preds <- 
    if (classif && is.numeric(fittedObject$holdoutPreds))
      fittedObject$holdoutPreds
  else if(classif && is.list(fittedObject$holdoutPreds))
    fittedObject$holdoutPreds$probs[,1]
  else 
    fittedObject$holdoutPreds
  
  xNames <- setdiff(names(data),c(yName,sensNames))
  allX <- setdiff(names(data),c(yName))
  holdIdxs <- fittedObject$holdIdxs
  
  corrs <- NULL  
  nCorrs <- 0
  
  for (sensNm in sensNames) {
    sens <- data[sensNm][holdIdxs,]
    if (is.factor(sens)) {
      lvls <- levels(sens)
      sens <- as.numeric(sens)
      nLvls <- length(lvls)
      if (nLvls == 2) {  # sens is dichotomous
        # set up R formula to be used in glm() call
        frml <- paste0(sensNm,' ~ .')
        frml <- as.formula(frml)
        tmp <- glm(frml,data[allX],family=binomial())
        sensProbs <- tmp$fitted.values[holdIdxs]
        corrs <- c(corrs,cor(preds,sensProbs)^2)
        nCorrs <- nCorrs + 1
        names(corrs)[nCorrs] <- sensNm
      } else {  
        tmp <- qeLogit(data[allX],sensNm,holdout=NULL)
        for (i in 1:length(tmp$glmOuts)) {
          glmout <- tmp$glmOuts[[i]]
          sens <- glmout$fitted.values[holdIdxs]
          corrs <- c(corrs,cor(preds,sens)^2)
          nCorrs <- nCorrs + 1
          nm <- paste0(sensNm,'.',lvls[i])
          names(corrs)[nCorrs] <- nm
        }
      }
    } else {
      if (is.matrix(preds)) preds <- as.vector(preds)
      corrs <- c(corrs,cor(preds,sens)^2)
      nCorrs <- nCorrs + 1
      names(corrs)[nCorrs] <- sensNm
    }
  }
  return(corrs)
}

#--------------------------------- PrepNewX ----------------------------------
dsldPrepNewX <- function(dsldObj, new_data) {
  data <- dsldObj$data
  char_cols <- sapply(new_data, is.character)
  new_data[char_cols] <- lapply(new_data[char_cols], as.factor)
  for (i in 1:length(char_cols)) {
    if (char_cols[i] == TRUE) {
      currlevels <- levels(data[[names(char_cols[i])]]) 
      new_data[[names(char_cols[i])]] <- factor(new_data[[names(char_cols[i])]], levels = currlevels)
    }
  }
  return(new_data)
}


#------------------------------------ prepData ---------------------------------
# common prep for all qeFair*(); here the S and C sets are as in the
# paper, S being the set of sensitive variables, to be excluded from the
# analysis, and C being the set of variables that we will use but
# deweight, due to assumed correlation with S

# to be called from a qeFair*() function that has 'data', 'yName', 
# 'sensNames' and 'deweightPars' as (some of) its arguments

# the argument zzz is just a placeholder; apparently gtools:defmacro()
# needs at least one argument other than expr

# value:  this is a macro, setting the following

#    nonSensNames:  original data names, minus those in S
#    data1:  original 'data' without S columns
#    x:  "X" part of data1, run through factorsToDummies()
#    factorsInfo:  info on factors, attribute of output of
#       factorsToDummies(); note that omitLast is set to FALSE
#    xm: scaled version of x; uses mmscale() instead of scale(), to avoid
#       inordinate influence of dummies that are mostly 1s or mostly 0s
#    minmax:  attributes produced by mmscale()
#    deweightNames, deweightVals: names of the C variables,
#       and their deweighting parameters AFTER factorsToDummies()
#       if any variables in S are factors
#    y:  "Y" vector/factor from original 'data'
#    data2: as.data.frame(cbind(x,y))
#    trainRow1:  needed for names used in later prediction, standard qeML

# note that prepData() does not call splitData(); this is assumed to be
# done by the qeML function

prepData <- defmacro(zzz,scaling='mmscale',expr=
                       {
                         ycol <- which(names(data) == yName)
                         y <- data[,ycol]
                         
                         nonSensNames <- setdiff(names(data),sensNames)
                         data1 <- data[nonSensNames]
                         
                         ycol <- which(names(data1) == yName)
                         x <- data1[,-ycol]
                         x <- factorsToDummies(x,omitLast=TRUE)
                         factorsInfo <- attr(x,'factorsInfo')
                         
                         # scaling
                         if (!is.null(scaling)) {
                           if (scaling == 'mmscale') {
                             xm <- mmscale(x)
                             scalePars <- attr(xm,'minmax')
                           } else {  # assumed to be ordinary R scale()
                             xm <- scale(x)
                             scalePars <- list(
                               ctr=attr(xm,'scaled:center'),
                               scl=attr(xm,'scaled:scale')
                             )
                           }
                         }
                         
                         xm.df <- as.data.frame(xm)
                         data2 <- cbind(xm.df,y)
                         names(data2)[1:ncol(xm)] <- colnames(x)
                         
                         deweightPars <- expandDeweightPars(data,yName,deweightPars)
                         deweightNames <- names(deweightPars)
                         deweightVals <- unlist(deweightPars)
                         
                         trainRow1 <- getRow1(data1,yName)
                       }
)

#------------------------------------ getRow1 ----------------------------------

getRow1 <- function (data, yName)                                                   
{                                                                                   
  ycol <- which(names(data) == yName)                                             
  data[1, -ycol]                                                                  
}


#------------------------------------ splitData --------------------------------
# requireNamespace(gtools)
# standard split into training, test sets

# arguments:
#    holdout: holdout set size
#    data: XY data frame

# globals/value:
#    tst: the generated holdout set
#    data: the correspondingly reduced training set
#    holdIdxs: indices of the holdout set in original ata

splitData <- defmacro(holdout,data,
                      expr={
                        nHold <- holdout;
                        # cat('holdout set has ',nHold, 'rows\n');
                        idxs <- sample(1:nrow(data),nHold);
                        tst <- data[idxs,,drop=FALSE];
                        trn <- data[-idxs,,drop=FALSE];
                        data <- data[-idxs,,drop=FALSE];
                        holdIdxs <- idxs
                      }
)

#------------------------------------ predictHoldoutFair  ----------------------
# modified version of qeML::predictHoldout()
# globals:  trn, tst, yName, preds
# res is the return value of qeFair*()

predictHoldoutFair <- defmacro(res,
                               expr={
                                 ycol <- which(names(tst) == yName);
                                 ycolData <- which(names(data) == yName);
                                 tstx <- tst[,-ycol,drop=FALSE];
                                 attr(tstx,'noNeedPrepNewx') <- TRUE;
                                 preds <- predict(res,tstx);
                                 res$holdoutPreds <- preds;
                                 if (res$classif) {
                                   if (is.numeric(preds)) {
                                     probs <- preds
                                     tmp <- round(preds)
                                     # predClasses <- res$yLevels[tmp+1]
                                     predClasses <- ifelse(tmp,yesYVal,noYVal)
                                   }
                                   preds <- list(probs=probs,predClasses=predClasses)
                                   res$testAcc <- mean(preds$predClasses != tst[,ycol])
                                   res$baseAcc <- 1 - max(table(data[,ycolData])) / nrow(data)
                                   # res$confusion <- regtools::confusion(tst[,ycol],preds$predClasses)
                                   doOneConfMatrix <- function(sensName) 
                                   {
                                     tmp <- sensName
                                     sens <- data[[tmp]][idxs]
                                     table(tst[,ycol],preds$predClasses,sens)
                                   }
                                   # res$sensConfusion <- lapply(sensNames,doOneConfMatrix)
                                 } else {
                                   res$testAcc <- mean(abs(preds - tst[,ycol]))
                                   trnYcol <- which(names(trn) == yName)
                                   res$baseAcc <-  mean(abs(tst[,ycol] - mean(trn[,trnYcol])))
                                 }
                               }
)

#------------------------------------ expandDeweightPars() ---------------------
# if a variable in the sensitive set S is an R factor, it will
# eventually be converted to dummies, and its deweighting parameter
# should be expand to each of them

expandDeweightPars <- function(data,yName,deweightPars) 
{
  yCol <- which(names(data) == yName)
  dataX <- data[,-yCol]
  colnamesX <- colnames(dataX)
  newPars <- list()
  parNames <- names(deweightPars)
  # for each factor in dataX, is it in deweightPars?; if so, expand
  # deweightPars
  dataXNames <- names(dataX)
  for (i in 1:ncol(dataX)) {
    xName <- dataXNames[i]
    if (xName %in% parNames) {
      if (is.numeric(dataX[,i])) {
        newPars[xName] <- deweightPars[xName]
      } else {
        lvls <- levels(dataX[,i])
        lvls <- lvls[-length(lvls)]  # omit redundant dummy
        expandedNames <- paste0(xName,'.',lvls)
        newPars[expandedNames] <- deweightPars[[colnamesX[i]]]
      }
    }
  }
  deweightPars <- newPars
  deweightPars
}



prepNewx <- function(object,newx) 
{
  nonSensNames <- setdiff(names(newx),object$sensNames)
  newx <- newx[nonSensNames]
  newx <- qeML:::setTrainFactors(object,newx)
  newx <- 
    factorsToDummies(newx,omitLast=TRUE,factorsInfo=object$factorsInfo)
  cnames <- colnames(newx)
  sps <- object$scalePars
  if (object$scaling != 'none') {
    newx <- 
      if(object$scaling == 'mmscale') {
        mmscale(newx,sps)
      } else {
        scale(newx,center=sps$ctr,scale=sps$scl)
      }
  }
  
  colnames(newx) <- cnames
  newx
}

