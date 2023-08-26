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
#' ::: Descripton :::
#' @brief The dsldLogit function fits a logistic model to the response variable,
#'      yName, using all other available covariates in the user provided
#'      dataset. The user may select for full interactions across the sensitive
#'      variable, sName, in which case the function will fit m separate models,
#'      where m is the number of levels of sName.
#'
#'      The function produces an instance of the `dsldLM` class (an S3
#'      object).
#'
#'      The output of dsldLogistic will store a list of useful traits pertaining
#'      the Logistic model; the following useful information will be stored:
#'          1. yName & sName; [character] @ yName, @ sName
#'          2. Model; [character] @ model
#'          3. (Full Interactions only) New data input by user;
#'             [dataframe] @ data
#'          3. Summary Output of model; [character] @ summary
#'          4. Coef of beta parameters; [character] @ coef
#'          5. Data used in the model (useful to see for interactions);
#'             [dataframe] @ data
#'
#' ::: Arguments :::
#' @param data: dataset used to train the model [dataframe]
#' @param yName: name of the response column [character]
#' @param sName: name of the sensitive column [character]
#' @param sComparisonPts: new test cases to compute Y | X 

dsldLogit <- function(data, yName, sName, sComparisonPts = NULL, interactions = FALSE, yesYVal) {
  
  dsldModel <- list()
  data[[yName]] <- ifelse(data[[yName]] == yesYVal, 1, 0)
  
  # user wants interactions #
  if (interactions) {
    
    # raise error if user doesn't input sComparisonPts #
    if (is.null(sComparisonPts)) {
      stop(paste("Please enter the sComparisonPts input to compare for ",
                 "interactions in summary()"))
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
#' ::: Description ::
#' @brief coef() is a polymorphic method that takes in an object of the
#'      'dsldGLM' class. The function provides m regression coefficients
#'      of the model, where m is the number of levels of sName.
#'
#' ::: Arguments :::
#' @param dsldGLM: an instance of the dsldGLM s3 object.
#'
coef.dsldGLM <- function(dsldGLM) {
  # merge & return coefficients #
  mergedCoef <- lapply(dsldGLM, function(x) x$coef)
  return(mergedCoef)
}

# coef(log1) 

#' ::: Description ::
#' @brief vcov() is a polymorphic method that takes in an object of the
#'      'dsldLM' class. The function provides m variance-covariance coeffs
#'      of the model, where m is the number of levels of sName.
#'
#' ::: Arguments :::
#' @param dsldGLM: an instance of the dsldGLM s3 object.
#'
vcov.dsldGLM <- function(dsldGLM) {
  # merge & return coefficients #
  mergedCoef <- lapply(dsldGLM, function(x) vcov(x$model))
  return(mergedCoef)
}

# vcov(log1)

#' ::: Description ::
#' @brief the dsldGetData() function takes in an object of the 'dsldGLM'
#'      class. The function provides m dataset(s) used to train the Logistic
#'      model, where m is the number of levels of sName.
#'
#' ::: Arguments :::
#' @param dsldGLM: an instance of the dsldGLM s3 object.
#'
dsldGetData <- function(dsldGLM) {
  # merge & return datasets #
  mergedData <- lapply(dsldGLM, function(x) x$data)
  return(mergedData)
}

# dsldGetData(log1)

#------------------------- dsldDiffSLog function ------------------------------#
#' ::: Description ::
#' @brief The dsldDiffS() function helps users quantify possible evidence of
#'      discrimination between S levels. For the no-interactions case,
#'      dsldDiffS compares differences in regression coefficients between each
#'      pairs of S levels. For the full-interactions case, dsldDiffS now
#'      requires an argument, in data-frame form, of new test cases where
#'      difference in mean Y at that X value will be compared between each pair
#'      of S levels.
#'
#'      For no-interaction case, dsldDiffS returns a data frame with 4 columns:
#'      1. Pairs of S level names
#'      2. Estimates of the differences
#'      3. Associated standard errors
#'      4. P-values
#'      There will be one row for each pair of S levels.
#'
#'      For full-interactions case, dsldDiffs returns a data frame with 3
#'      columns:
#'      1. Col. number of diffs argument
#'      2. Estimate of the difference in mean Y at that X value
#'      3. Associated std. err.
#'      There will be one row for each pair of S levels.
#'
#' ::: Arguments :::
#' @param dsldGLM: output from dsldLogistic() function
#' @param newData: new test cases to be provided; required for
#'      full-interactions case

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

#' ::: Description ::
#' @brief summary() is a polymorphic method that takes in an object of the 'dsldGLM' 
#'      class. The function provides m summaries of the model, where m is the number 
#'      of levels of sName. Additionally, the summary function also report differences 
#'      across S levels.
#' 
#' ::: Arguments :::
#' @param dsldGLM: an instance of the dsldGLM s3 object that output summary objects.
#'
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
