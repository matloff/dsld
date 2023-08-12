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
### -------------------------- DSLD Linear -------------------------------------
#' ::: Descripton :::
#' @brief The dsldLinear function fits a linear model to the response variable,
#'      yName, using all other available covariates in the user provided
#'      dataset. The user may select for full interactions across the sensitive
#'      variable, sName, in which case the function will fit m separate models,
#'      where m is the number of levels of sName.
#'
#'      The function produces an instance of the `dsldLM` class (an S3
#'      object).
#'
#'      The output of dsldLinear will store a list of useful traits pertaining
#'      the linear model; the following useful information will be stored:
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
#' @param interactions: specifies whether or not to consider interactions;
#'      Defaults to FALSE [boolean]
#' @param newData: new test cases to compute Y | X ; REQUIRED when
#'      interactions = TRUE [dataframe]
#' @param sandwich: whether or not to use sandwich variance estimator; defaults
#'      to FALSE [logical]

dsldLinear <- function(data, yName, sName, interactions = FALSE, 
                       sComparisonPts = NULL, useSandwich = FALSE) {
  # load if needed
  if (useSandwich) {
    library(sandwich)
  }
  
  # create final output list to by populated with results #
  dsldModel <- list()
  
  # user wants interactions #
  if (interactions) {
    
    # raise error if user doesn't input proper Data #
    if (is.null(sComparisonPts)) {
      stop(paste("Please enter the sComparisonPts argument to compare for ",
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
      diffModel <- lm(formula = as.formula(paste(yName, "~ .")),
                      data = diffData)
      
      if (useSandwich) {
        covMatrix <- sandwich(diffModel)
      } else {
        covMatrix <- vcov(diffModel)
      }
      
      # setup individual instance of dsldDiffModel #
      dsldDiffModel <- c(dsldDiffModel,
                         yName,
                         sName,
                         list(diffModel),
                         list(newData),
                         list(summary(diffModel)),
                         list(coef(diffModel)),
                         list(covMatrix),
                         list(diffData)
      )
      names(dsldDiffModel) <- c("yName", "sName", "model", "newData",
                                "summary", "coef", "covarianceMatrix", "data")
      class(dsldDiffModel) <- "dsldDiffModel"
      
      # add instance into output list: dsldModel #
      dsldModel[[name]] <- dsldDiffModel
    }
    
    # user selects interactions == FALSE #
  } else {
    # initialize instance of dsldDiffModel #
    dsldDiffModel <- list()
    
    # create model #
    diffModel <- lm(formula = as.formula(paste(yName, "~ .")), data = data)
    if (useSandwich) {
      covMatrix <- sandwich(diffModel)
    } else {
      covMatrix <- vcov(diffModel)
    }
    
    # setup instance of dsldDiffModel #
    dsldDiffModel <- c(dsldDiffModel,
                       yName,
                       sName,
                       list(diffModel),
                       list(summary(diffModel)),
                       list(coef(diffModel)),
                       list(covMatrix),
                       list(data)
    )
    names(dsldDiffModel) <- c("yName", "sName", "model", "summary",
                              "coef", "covarianceMatrix", "data")
    
    # add instance into dsldModel
    dsldModel[[sName]] <- dsldDiffModel
  }
  
  # finalize dsldModel #
  class(dsldModel) <- "dsldLM"
  return(dsldModel)
}

# -------------------- Test Run dsldLinear ------------------------------------#
# data(svcensus)
# newData <- data.frame(age = c(18,60), educ = c("zzzOther",'zzzOther'),wkswrkd = c(50,50), occ = c("106", "106"))  
# lin1 <- dsldLinear(svcensus,'wageinc','gender', interactions = TRUE, newData); lin1   
# lin11 <- dsldLinear(svcensus,'wageinc','gender', interactions = TRUE, newData, useSandwich = TRUE); lin11   
# lin2 <- dsldLinear(svcensus,'wageinc','gender', interactions = FALSE); lin2
# lin22 <- dsldLinear(svcensus,'wageinc','gender', interactions = FALSE, useSandwich = TRUE); lin22
# -----------------------------------------------------------------------------#

# ----------------------- Auxiliary Functions ---------------------------------#

#' ::: Description ::
#' @brief coef() is a polymorphic method that takes in an object of the
#'      'dsldLM' class. The function provides m regression coefficients
#'      of the model, where m is the number of levels of sName.
#'
#' ::: Arguments :::
#' @param dsldLM: an instance of the dsldLM s3 object.

coef.dsldLM <- function(dsldLM) {
  # merge & return coefficients #
  mergedCoef <- lapply(dsldLM, function(x) x$coef)
  return(mergedCoef)
}


# coef(lin1) # test run
# coef(lin11)

# added vcov generic
vcov.dsldLM <- function(dsldLM) {
  # merge & return coefficients #
  mergedCoef <- lapply(dsldLM, function(x) x$covarianceMatrix)
  return(mergedCoef)
}

# vcov(lin1)
# vcov(lin11)

#' ::: Description ::
#' @brief the dsldGetData() function takes in an object of the 'dsldLM'
#'      class. The function provides m dataset(s) used to train the linear
#'      model, where m is the number of levels of sName.
#'
#' ::: Arguments :::
#' @param dsldLM: an instance of the dsldLM s3 object.

dsldGetData <- function(dsldLM) {
  # merge & return datasets #
  mergedData <- lapply(dsldLM, function(x) x$data)
  return(mergedData)
}


# dsldGetData(lin1)
# dsldGetData(lin11)

#------------------------- dsldDiffS function ---------------------------------#
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
#' @param dsldLM: output from dsldLinear() function
#' @param newData: new test cases to be provided; required for
#'      full-interactions case

dsldDiffSLin <- function(dsldLM, sComparisonPts = NULL) {
  library(regtools)
  # get sName and yName from the output of dsldLinear #
  sName <- dsldLM[[1]]$sName
  yName <- dsldLM[[1]]$yName
  
  # diffS results when interaction == FALSE in dsldLinear #
  if (length(dsldLM) == 1) {
    # extract pairwise combination of [dummy level in glm - factor levels]
    # from summary output
    data <- dsldGetData(dsldLM)[[1]]
    model <- dsldLM[[1]]$model
    C <- dsldLM[[1]]$covarianceMatrix
    se_robust <- sqrt(diag(C))
    c <- coef(model)
    
    # get all values containing sName levels from summary(model) #
    rowsWithRace <- grep(sName, rownames(coef(summary(model))))
    regularS <- summary(model)$coefficients[rowsWithRace, ]
    standardErrors <- se_robust[rowsWithRace]
    
    # for the case when we have only two levels in S; ex: male/female #
    if (length(levels(data[[sName]])) == 2) {
      estimate <- regularS[1]
      standardError <- standardErrors
      testStat <- estimate / standardError
      pVal <- 2 * (1 - pnorm(abs(testStat)))
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
    testStat <- estimates / standardErrors
    pVal <- 2 * (1 - pnorm(abs(testStat)))
    
    # create dataframe #
    df <- data.frame(estimates, standardErrors, pVal)
    df$estimates <- -df$estimates
    
    # extract other pairwise combinations of levels (not including dummy) #
    featureNames <- colnames(C)
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
      testStat <- (estimates) / standardErrors
      pVal <- 2 * (1 - pnorm(abs(testStat)))
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
  } else { # with interactions
    
    # raise error if the user doesn't input new data #
    if (is.null(sComparisonPts)) {
      stop("Please enter the sComparisonPts argument to compare for interactions")
    }
    if (!is.data.frame(sComparisonPts)) {
      stop(paste("Error: sComparisonPts is not a dataframe"))
    } 
    tempData <- dsldLM[[1]]$data
    xNew <- dsldCheckData(tempData, sComparisonPts, yName)
    
    # get vector of all levels in sName #
    sNames <- names(dsldLM)
    df <- data.frame()
    
    # loop through each level of S name to compute estimates and
    # standard errors
    for (i in sNames) {
      data <- dsldLM[[i]]$data
      colName <- names(data)
      colName <- colName[colName != yName]
      model <- dsldLM[[i]]$model
      C <- (dsldLM[[i]]$covarianceMatrix)
      u_names <- names(coef(dsldLM[[i]]$model))
      for (j in 1:nrow(xNew)) {
        row <- xNew[j, ]
        if (!is.data.frame(row)) {
          row <- data.frame(x = row)
          colnames(row) <- colName
        }
        if (!is.numeric(row)) {
          x <- regtools::factorsToDummies(row,omitLast=FALSE)
        } else {
          x <- as.vector(row)
        }
        full_u <- x[1,]
        cleaned_vector1_names <- names(full_u)
        cleaned_vector1_names <- gsub("\\.", "", cleaned_vector1_names)
        matched_names <- intersect(cleaned_vector1_names, u_names)
        subset_values <- full_u[match(matched_names, cleaned_vector1_names)]
        subset_values <- c(1,subset_values)
        pred <- predict(model,row)
        standard_error <- sqrt(t(subset_values) %*% C %*% subset_values)
        tempDf <- data.frame(level = i, row = j, prediction = pred, standardError = standard_error)
        df <- rbind(df, tempDf)
      }
    }
    # compute difference in estimates between each pair factor level for
    # each row
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
        aData <- subset(rowData, level == a) # error, needs fix
        bData <- subset(rowData, level == b)
        indexVal <- sprintf("%s - %s", a, b)
        estimatedDiff <- aData$prediction - bData$prediction
        standardError <- sqrt(((aData$standardError) ^ 2) +
                                ((bData$standardError) ^ 2))
        tempDF <- data.frame(indexVal, i, estimatedDiff,
                             standardError)
        names(tempDF) <- c("Factors Compared", "New Data Row", "Estimates",
                           "Standard Errors")
        pairwiseDF <- rbind(pairwiseDF, tempDF)
      }
    }
    return(pairwiseDF)
  }
}

# ---------------------------- Test runs  -------------------------------------#
# educ_data <- data.frame(age = c(18,60), educ = c("zzzOther", "zzzOther"), wkswrkd = c(50, 50),occ = c("106", "106")) 
# dsldDiffSLin(lin1, educ_data) # run with interactions 
# dsldDiffSLin(lin11, educ_data)
# -----------------------------------------------------------------------------#

#' ::: Description ::
#' @brief summary() is a polymorphic method that takes in an object of the 'dsldLM' 
#'      class. The function provides m summaries of the model, where m is the number 
#'      of levels of sName. Additionally, the summary function also report differences 
#'      across S levels.
#' 
#' ::: Arguments :::
#' @param dsldLM: an instance of the dsldLM s3 object that output summary objects.

summary.dsldLM <- function(dsldLM) {
  diffS <- list()
  
  # get sName and yName from the output of dsldLinear #
  sName <- dsldLM[[1]]$sName
  yName <- dsldLM[[1]]$yName
  
  if (length(dsldLM) == 1) {
    data <- dsldGetData(dsldLM)[[1]]
    summaryOutput <- summary(dsldLM[[1]]$model)
    coef <- summaryOutput$coefficients[, 1]
    covMatrix = dsldLM[[1]]$covarianceMatrix
    stdErr <- sqrt(diag(covMatrix))
    testStat <- coef / stdErr
    pValues <- 2 * (1 - pnorm(abs(testStat)))
    
    # Create dataframe
    df <- data.frame(
      Covariate = row.names(summaryOutput$coefficients),
      Estimate = coef,
      StandardError = stdErr,
      PValue = pValues,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    
    diffS[['Summary Coefficients']] <- df
    diffS[['Sensitive Factor Level Comparisons']] <- dsldDiffSLin(dsldLM)
    
    return(diffS)
  } else {
    sNames <- names(dsldLM)
    newData <- dsldLM[[1]]$newData
    
    # loop through each level of S name to compute estimates and standard
    # errors
    for (i in sNames) {
      data <- dsldLM[[i]]$data
      summaryOutput <- summary(dsldLM[[i]]$model)
      coef <- summaryOutput$coefficients[, 1]
      covMatrix = dsldLM[[i]]$covarianceMatrix
      stdErr <- sqrt(diag(covMatrix))
      testStat <- coef / stdErr
      pValues <- 2 * (1 - pnorm(abs(testStat)))
      
      df <- data.frame(
        Covariate = row.names(summaryOutput$coefficients),
        Estimate = coef,
        StandardError = stdErr,
        PValue = pValues,
        stringsAsFactors = FALSE,
        row.names = NULL
      )
      
      diffS[[i]] <- df
    }
    diffS[['Sensitive Factor Level Comparisons']] <- dsldDiffSLin(dsldLM,
                                                                  newData)
    return(diffS)
  }
}

# Test runs --------------------------------------------------------------------
# summary(lin1)
# summary(lin11)
# summary(lin2)
# summary(lin22)
