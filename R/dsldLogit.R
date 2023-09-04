### --------------------------- DSLDCheckData ----------------------------------
dsldCheckData <- function(data1, data2, yName) {
  data1 <- data1[, !(names(data1) == yName), drop = FALSE]
  missingCols <- setdiff(names(data1), names(data2))
  if (length(missingCols) > 0) {
    stop(paste("Invalid column(s) in sComparisonPts:", paste(missingCols, collapse = ", ")))
  }
  
  if (!identical(sort(names(data1)), sort(names(data2)))) {
    stop("Error: Column names do not match")
  }
  data2 <- data2[names(data1)]
  
  char <- sapply(data2, is.character)
  for (colName in names(data2)[char]) {
    if (colName %in% names(data1) && is.factor(data1[[colName]])) {
      levels_data1 <- levels(data1[[colName]])
      data2[[colName]] <- factor(data2[[colName]], levels = levels_data1)
      
      invalid_levels <- !data2[[colName]] %in% levels_data1
      if (any(invalid_levels)) {
        stop(paste("Invalid", colName, "level(s) in sComparisonPts:", 
                   paste(data2[[colName]][invalid_levels], collapse = ", ")))
      }
    }
  }
  return(data2)
}

### ------------------------ DSLDLogit -----------------------------------------
dsldLogit <- function(data, yName, sName, sComparisonPts = NULL, interactions = FALSE, yesYVal) {
  
  dsldModel <- list()
  data[[yName]] <- ifelse(data[[yName]] == yesYVal, 1, 0)
  
  # user wants interactions #
  if (interactions) {
    
    # raise error if user doesn't input sComparisonPts #
    if (is.null(sComparisonPts)) {
      sComparisonPts = dsldGetRow5(svcensus,yName, sName)
    }
    if (!is.data.frame(sComparisonPts)) {
      stop(paste("Error: sComparisonPts must be a dataframe"))
    }
    
    tempData <- data[, !(names(data) %in% sName)]
    newData <- dsldCheckData(tempData, sComparisonPts, yName)
    
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
        list(newData),
        list(summary(diffModel)),
        list(coef(diffModel)),
        list(diffData)
      )
      names(dsldDiffModel) <- c("yName", "sName", "model", "newData",
                                "summary", "coef", "data")
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
                       list(data)
    )
    names(dsldDiffModel) <- c("yName", "sName", "model", "summary",
                              "coef", "data")
    
    # add instance into dsldModel
    dsldModel[[sName]] <- dsldDiffModel
  }
  class(dsldModel) <- "dsldGLM"
  return(dsldModel)
}

# ------------------------------------- Test Run dsldLogit ---------------------
#library(fairml)
#data(law.school.admissions)                      
#drop <- c('fulltime','cluster')
#law.school.admissions <- law.school.admissions[, !(names(law.school.admissions) %in% drop)]
#newData <- data.frame(age = c(18,18), decile1 = c(5,5),decile3 = c(4,4), lsat = c(44,44), fam_inc = c(3,3), ugpa = c(3.5, 3.5), race1 = c('asian', 'black')) 
#log1 <- dsldLogit(law.school.admissions,'bar','gender', newData, TRUE, 'TRUE'); log1 # we are predicting lsat score 
#log2 <- dsldLogit(data = law.school.admissions,yName = 'bar',sName = 'gender', interactions = FALSE,yesYVal = 'FALSE'); log2
# ------------------------------------------------------------------------------

# ----------------------- Auxiliary Functions ---------------------------------#
coef.dsldGLM <- function(dsldGLM) {
  # merge & return coefficients #
  mergedCoef <- lapply(dsldGLM, function(x) x$coef)
  return(mergedCoef)
}

# coef(log1) 

vcov.dsldGLM <- function(dsldGLM) {
  # merge & return coefficients #
  mergedCoef <- lapply(dsldGLM, function(x) vcov(x$model))
  return(mergedCoef)
}

# vcov(log1)

dsldGetData <- function(dsldGLM) {
  # merge & return datasets #
  mergedData <- lapply(dsldGLM, function(x) x$data)
  return(mergedData)
}

# dsldGetData(log1)

#------------------------- dsldDiffSLog function ------------------------------#
dsldDiffSLog <- function(dsldGLM, sComparisonPts) {
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
      names(df) <- c("Factors Compared", "Estimates", "Standard Errors",
                     "P-Value")
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
    
  } else {
    # raise error if the user doesn't input new data #
    if (is.null(sComparisonPts)) {
      stop("Please enter the sComparisonPts input to compare for interactions")
    }
    
    if (!is.data.frame(sComparisonPts)) {
      stop(paste("Error: sComparisonPts must be a dataframe"))
    } 
    
    tempData <- dsldGLM[[1]]$data
    xNew <- dsldCheckData(tempData, sComparisonPts, yName)
    
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
      tempDF <- data.frame(level = i, row = 1:nrow(xNew), prediction = pred, standardError = se)
      df <- rbind(df, tempDF)
    }
    
    # compute difference in estimates between each pair factor level for each row
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
        names(tempDF) <- c("Factors Compared", "New Data Row", 'Factor A','Factor B', "Difference in Estimates",
                           "Standard Errors")
        pairwiseDF <- rbind(pairwiseDF, tempDF)
      }
    }
    return(pairwiseDF)
  }
}

# ---------------------------- Test runs  -------------------------------------#
# newData <- data.frame(age = c(18,18), decile1 = c(5,5),decile3 = c(4,4), lsat = c(25,25), fam_inc = c(3,3), ugpa = c(3.5, 3.5), race1 = c('asian', 'black')) 
# dsldDiffSLog(log1, newData) # run with interactions 
# dsldDiffSLog(log2, newData) # run with interactions 
# -----------------------------------------------------------------------------#

summary.dsldGLM <- function(dsldGLM) {
  diffS <- list()
  # get sName and yName from the output of dsldLogistic #
  sName <- dsldGLM[[1]]$sName
  yName <- dsldGLM[[1]]$yName
  
  sNames <- names(dsldGLM)
  newData <- dsldGLM[[1]]$newData
  
  if (length(dsldGLM) == 1) {
    data <- dsldGetData(dsldGLM)[[1]]
    summary_output <- summary(dsldGLM[[1]]$model)
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
    diffS[['Sensitive Factor Level Comparisons']] <- dsldDiffSLog(dsldGLM)
    
    return(diffS)
  } else { 
    # loop through each level of S name to compute estimates and standard errors
    for (i in sNames) {
      data <- dsldGLM[[i]]$data
      summaryOutput <- summary(dsldGLM[[i]]$model)
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
    diffS[['Sensitive Factor Level Comparisons']] <- dsldDiffSLog(dsldGLM,
                                                                  newData)
    return(diffS)
  }
}

# ------------------------------- Test run -------------------------------------
# summary(log1)
# summary(log2)
