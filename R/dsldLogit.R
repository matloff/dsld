### ------------------------ DSLDLogit -----------------------------------------
### creates the dsldLogit object
dsldLogit <- function(data, yName, sName, sComparisonPts = NULL, interactions = FALSE, yesYVal) {
  
  dsldModel <- list()
  data[[yName]] <- ifelse(data[[yName]] == yesYVal, 1, 0)
  
  # user wants interactions #
  if (interactions) {

    # generate interactions data if not provided / stop if erroneous
    if (is.null(sComparisonPts)) {
      sComparisonPts <- dsldGetRow5(data,yName, sName)
    } else if (!is.data.frame(sComparisonPts)) {
      stop(paste("Error: sComparisonPts must be a dataframe or equivalent"))
    } 
  
    # split data into list of dataframes by each level of sName #
    dataSplit <- split(data, data[[sName]])
    dataNames <- names(dataSplit)
    
    # loop and create model for each level in sName #
    for (name in dataNames) {
      # initialize instance of dsldDiffModel #
      dsldDiffModel <- list()
      
      # get data for each specific S factor & drop sensitive column #
      diffData <- dataSplit[[name]]
      drop <- c(sName)
      diffData <- diffData[, !(names(diffData) %in% drop)]
      
      # create the model #
      diffModel <- glm(formula = as.formula(paste(yName, "~ .")),
                       family = "binomial", data = diffData)
      
      # setup individual instance of dsldDiffModel 
      dsldDiffModel <- c(
        dsldDiffModel,
        yName,
        sName,
        list(diffModel),
        list(sComparisonPts),
        list(summary(diffModel)),
        list(coef(diffModel)),
        list(diffData),
        list(factor_levels(data))
      )
      names(dsldDiffModel) <- c("yName", "sName", "model", "newData",
                                "summary", "coef", "data", "FactorsInfo")
      class(dsldDiffModel) <- "dsldDiffModel"
      
      # add instance into output list: dsldModel #
      dsldModel[[name]] <- dsldDiffModel
    }
  } else {
    # initialize instance of dsldDiffModel #
    dsldDiffModel <- list()
    
    # create model #
    diffModel <- glm(formula = as.formula(paste(yName, "~ .")),
                     family = "binomial", data = data)
    
    # setup instance of dsldDiffModel #
    dsldDiffModel <- c(dsldDiffModel,
                       yName,
                       sName,
                       list(diffModel),
                       list(summary(diffModel)),
                       list(coef(diffModel)),
                       list(data),
                       list(factor_levels(data))
    )
    names(dsldDiffModel) <- c("yName", "sName", "model", "summary",
                              "coef", "data", "FactorsInfo")
    
    # add instance into dsldModel
    dsldModel[[sName]] <- dsldDiffModel
  }
  class(dsldModel) <- "dsldGLM"
  return(dsldModel)
}

# ----------------------- Auxiliary Functions ---------------------------------#
coef.dsldGLM <- function(object,...) {
  # merge & return coefficients #
  mergedCoef <- lapply(object, function(x) x$coef)
  return(mergedCoef)
}

vcov.dsldGLM <- function(object,...) {
  # merge & return coefficients #
  mergedCoef <- lapply(object, function(x) vcov(x$model))
  return(mergedCoef)
}

dsldGetData <- function(object) {
  # merge & return datasets #
  mergedData <- lapply(object, function(x) x$data)
  return(mergedData)
}

### #------------------------- dsldDiffSLog function --------------------------#
dsldDiffSLog <- function(object, sComparisonPts = NULL) {
  # naming
  dsldGLM <- object
  
  # get sName and yName from the output of dsldLogistic #
  sName <- dsldGLM[[1]]$sName
  yName <- dsldGLM[[1]]$yName
  
  # diffS results when interaction == FALSE in dsldLinear #
  if (length(dsldGLM) == 1) {
    # extract pairwise combination of [dummy level in glm - factor levels]
    # from summary output
    data <- dsldGetData(dsldGLM)[[1]]
    model <- dsldGLM[[1]]$model
    C <- vcov(model)
    c <- coef(model)
    
    # get all values containing sName levels from summary(model) #
    rowsWithRace <- grep(sName, rownames(coef(summary(model))))
    regularS <- summary(model)$coefficients[rowsWithRace, ]
    
    # for the case when we have only two levels in S; ex: male/female #
    if (length(levels(data[[sName]])) == 2) {
      estimate <- regularS[1]
      standardError <- regularS[2]
      pVal <- regularS[4]
      sPairs <- combn(levels(data[[sName]]), 2)
      a <- sPairs[1]
      b <- sPairs[2]
      indexVal <- sprintf("%s - %s", b, a)
      df <- data.frame(indexVal, estimate, standardError, pVal)
      names(df) <- c("Factors Compared", "Estimates", 
         "Standard Errors", "P-Value")
      return(df)
    }
    
    # extract estimates and standard errors #
    estimates <- regularS[, 1]
    standardErrors <- regularS[, 2]
    pVal <- regularS[, 4]
    
    # create dataframe #
    df <- data.frame(estimates, standardErrors, pVal)
    df$estimates <- -df$estimates
    
    # extract other pairwise combinations of levels (not including dummy) #
    featureNames <- colnames(vcov(model))
    combinationMatrix <- combn(featureNames, 2)
    
    # remove all columns that do not have sName #
    matchingCols <- which(apply(combinationMatrix, 2,
                                function(col) all(grepl(sName, col))))
    finalResult <- combinationMatrix[, matchingCols, drop = FALSE]
    
    # loops through each pair #
    for (j in 1:dim(finalResult)[2]) {
      # create i-th pair of pairwise combinations #
      val <- finalResult[, j]
      a <- val[1]
      b <- val[2]
      
      # create vector of 0's length of coef(z) #
      vectorLength <- length(c)
      rt <- rep(0, vectorLength)
      
      # put 1 on the first element #
      aIndex <- which(names(c) == a)
      rt[aIndex] <- 1
      
      # put -1 on the second element #
      bIndex <- which(names(c) == b)
      rt[bIndex] <- -1
      
      aValue <- c[aIndex]
      bValue <- c[bIndex]
      
      # get estimates & standard errors #
      estimates <- aValue - bValue
      standardErrors <- sqrt((t(rt) %*% C %*% rt))
      
      tStatistic <- (estimates) / standardErrors
      degOfFreedom <- nrow(data) - 1 # degrees of freedom
      pVal <- 2 * pt(abs(tStatistic), df = degOfFreedom,
                     lower.tail = FALSE)
      
      tempDF <- data.frame(estimates, standardErrors, pVal)
      df <- rbind(df, tempDF)
    }
    
    # get names of sName comparisons #
    sPairs <- combn(levels(data[[sName]]), 2)
    test <- c()
    for (i in 1:dim(sPairs)[2]) {
      val <- sPairs[,i]
      a <- val[1]
      b <- val[2]
      indexVal <- sprintf("%s - %s", a, b)
      test <- c(test, indexVal)
    }
    
    # create final data-frame #
    df <- cbind(test, df)
    df <- data.frame(df, row.names = NULL)
    names(df) <- c("Factors Compared", "Estimates", "Standard Errors",
                   "P-Value")
    return(df)
    
  } else {
    # raise error if the user doesn't input new data #
    if (is.null(sComparisonPts)) {
      stop("Please enter the sComparisonPts input to compare for interactions")
    }
    
    if (!is.data.frame(sComparisonPts)) {
      stop(paste("Error: sComparisonPts must be a dataframe or equivalent"))
    } 
    
    if (!is.null(sComparisonPts)) {
      sComparisonPts <- apply_factor_levels(sComparisonPts, object[[1]]$FactorsInfo)
    }
    
    # naming
    xNew <- sComparisonPts
    
    # get vector of all levels in sName #
    sNames <- names(dsldGLM)
    df <- data.frame()
    
    # loop through each level of S name to compute estimates and standard errors
    for (i in sNames) {
      data <- dsldGLM[[i]]$data
      model <- dsldGLM[[i]]$model
      predictions <- predict(model, xNew, type = "response", se.fit = TRUE)
      pred <- predictions$fit
      se <- predictions$se.fit
      level <- row <- prediction <- standardError <-  NULL
      tempDF <- data.frame(level = i, row = 1:nrow(xNew), 
         prediction = pred, standardError = se)
      df <- rbind(df, tempDF)
    }
    
    # compute difference in estimates between each pair factor level 
    # for each row
    uniqueElements <- sort(unique(df$row))
    pairwiseDF <- data.frame()
    
    for (i in uniqueElements) {
      rowData <- subset(df, row == i)
      charVec <- as.character(rowData$level)
      combinationMatrix <- combn(charVec, 2)
      for (j in 1:dim(combinationMatrix)[2]) {
        val <- combinationMatrix[, j]
        a <- val[1]
        b <- val[2]
        aData <- subset(rowData, level == a) 
        a3 <- aData[3]
        bData <- subset(rowData, level == b)
        b3 <- bData[3]
        indexVal <- sprintf("%s - %s", a, b)
        estimatedDiff <- aData$prediction - bData$prediction
        standardError <- sqrt(((aData$standardError) ^ 2) +
                                ((bData$standardError) ^ 2))
        tempDF <- data.frame(indexVal, i, a3,b3, estimatedDiff,
                             standardError)
        names(tempDF) <- c("Factors Compared", "New Data Row", 
           'Factor A','Factor B', "Difference in Estimates", "Standard Errors")
        pairwiseDF <- rbind(pairwiseDF, tempDF)
      }
    }
    return(pairwiseDF)
  }
}

## ------------------------------ summary() ------------------------------
summary.dsldGLM <- function(object,...) {
  diffS <- list()
  # get sName and yName from the output of dsldLogistic #
  sName <- object[[1]]$sName
  yName <- object[[1]]$yName
  
  sNames <- names(object)
  newData <- object[[1]]$newData
  
  if (length(object) == 1) {
    data <- dsldGetData(object)[[1]]
    summary_output <- summary(object[[1]]$model)
    coef <- summary_output$coefficients[, 1]
    std_err <- summary_output$coefficients[, 2]
    pValues <- summary_output$coefficients[, 4]
    
    # Create dataframe
    df <- data.frame(
      Covariate = row.names(summary_output$coefficients),
      Estimate = coef,
      `Standard Error` = std_err,
      PValue = pValues,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    
    diffS[['Summary Coefficients']] <- df
    diffS[['Sensitive Factor Level Comparisons']] <- dsldDiffSLog(object)
    
    return(diffS)
  } else { 
    # loop through each level of S name to compute estimates and standard errors
    for (i in sNames) {
      data <- object[[i]]$data
      summaryOutput <- summary(object[[i]]$model)
      coef <- summaryOutput$coefficients[, 1]
      stdErr <- summaryOutput$coefficients[, 2]
      pValues <- summaryOutput$coefficients[, 4]
      
      df <- data.frame(
        Covariate = row.names(summaryOutput$coefficients),
        Estimate = coef,
        `Standard Error` = stdErr,
        PValue = pValues,
        stringsAsFactors = FALSE,
        row.names = NULL
      )
      diffS[[i]] <- df
    }
    diffS[['Sensitive Factor Level Comparisons']] <- dsldDiffSLog(object,
                                                                  newData)
    return(diffS)
  }
}

# ---------------------------- add predict() -----------------------------------
predict.dsldGLM <- function(object, xNew,...){
  df <- data.frame()
  yName = object[[1]]$yName
  if (length(object) == 1) {
    data <- object[[1]]$data
    model <- object[[1]]$model
    xNew <- apply_factor_levels(xNew, object[[1]]$FactorsInfo)
    predictions <- predict(model, xNew, type = "response", se.fit = TRUE)
    pred <- predictions$fit
    se <- predictions$se.fit
    tempDF <- data.frame(row = 1:nrow(xNew), prediction = pred, standardError = se)
    df <- rbind(df, tempDF)
    return (df)
  } else {
    sNames <- names(object)
    for (i in sNames) {         # loop through each level of S name to compute estimates and standard errors
      data <- object[[i]]$data
      model <- object[[i]]$model
      xNew <- apply_factor_levels(xNew, object[[1]]$FactorsInfo)
      predictions <- predict(model, xNew, type = "response", se.fit = TRUE)
      pred <- predictions$fit
      se <- predictions$se.fit
      tempDF <- data.frame(level = i, row = 1:nrow(xNew), prediction = pred, standardError = se)
      df <- rbind(df, tempDF)
    }
    return (df)
  }
}
