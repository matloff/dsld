### -------------------------- DSLD Linear -------------------------------------
### creates the dsldLinear model; useful to call summary(), predict() etc.
dsldLinear <- function(data, yName, sName, interactions = FALSE, 
                       sComparisonPts = NULL, useSandwich = FALSE) { 
  # setup
  dsldModel <- list()
  
  # branch on interactions
  if (interactions) {
    # generate interactions data if not provided / stop if erroneous
    if (is.null(sComparisonPts)) {
      sComparisonPts <- dsldGetRow5(data,yName, sName)
    } else if (!is.data.frame(sComparisonPts)) {
      stop(paste("Error: sComparisonPts must be a dataframe or equivalent"))
    } 
    
    # setup interactions data
    dataSplit <- split(data, data[[sName]])
    dataNames <- names(dataSplit)
    
    for (name in dataNames) {
      dsldDiffModel <- list()
      diffData <- dataSplit[[name]]
      drop <- c(sName)
      diffData <- diffData[, !(names(diffData) %in% drop)]
      diffModel <- lm(
        formula = as.formula(paste(yName, "~ .")),
        data = diffData
      )
      
      # sandwich branch for covariance matrix
      if (useSandwich) {
        getSuggestedLib('sandwich')
        covMatrix <- sandwich::sandwich(diffModel)
      } else {
        covMatrix <- vcov(diffModel)
      }
      
      # generate diff model s3 object with attributes & names
      dsldDiffModel <- c(
        dsldDiffModel,
        yName,
        sName,
        list(diffModel),
        list(sComparisonPts),
        list(summary(diffModel)),
        list(coef(diffModel)),
        list(covMatrix),
        list(diffData),
        list(factor_levels(data))
        
      )
      names(dsldDiffModel) <- c(
        "yName",
        "sName",
        "model",
        "newData",
        "summary",
        "coef",
        "covarianceMatrix",
        "data",
        "FactorsInfo"
      )
      class(dsldDiffModel) <- "dsldDiffModel"
      
      # add diff model to dsldLM object
      dsldModel[[name]] <- dsldDiffModel
    }
  } else {
    # setup diff model object
    dsldDiffModel <- list()
    diffModel <- lm(
      formula = as.formula(paste(yName, "~ .")),
      data = data
    )
    
    # branch covariance matrix on sandwich
    if (useSandwich) {
      getSuggestedLib('sandwich')
      covMatrix <- sandwich::sandwich(diffModel)
    } else {
      covMatrix <- vcov(diffModel)
    }
    
    # generate diff model s3 object with attributes & names
    dsldDiffModel <- c(dsldDiffModel,
                       yName,
                       sName,
                       list(diffModel),
                       list(summary(diffModel)),
                       list(coef(diffModel)),
                       list(covMatrix),
                       list(data),
                       list(factor_levels(data))
    )
    names(dsldDiffModel) <- c(
      "yName",
      "sName",
      "model",
      "summary",
      "coef",
      "covarianceMatrix",
      "data",
      "FactorsInfo"
    )
    
    # add diff model to dsldLM object
    dsldModel[[sName]] <- dsldDiffModel
  }
  
  # finalize dsldLM s3 object & return
  class(dsldModel) <- "dsldLM"
  return(dsldModel)
}

# ----------------------- Auxiliary Functions ---------------------------------#
coef.dsldLM <- function(object,...) {
  # merge & return coefficients #
  mergedCoef <- lapply(object, function(x) x$coef)
  return(mergedCoef)
}

vcov.dsldLM <- function(object,...) {
  # merge & return covariance matrix #
  mergedCov <- lapply(object, function(x) x$covarianceMatrix)
  return(mergedCov)
}

dsldGetData <- function(object) {
  # merge separated datasets & return #
  mergedData <- lapply(object, function(x) x$data)
  return(mergedData)
}

# removing for now, too complicated -- NM July 20, 2024
# restored, NM, Aug 4 2024
#------------------------- dsldDiffS function ---------------------------------#
# computes the differences in predicted values across levels 
# of sensitive variables
# result is included in the output of the summary() function.
#
dsldDiffSLin <- function(object, sComparisonPts = NULL) {
  # naming 
  dsldLM <- object
  
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
    
    if (!is.null(sComparisonPts)) {
      sComparisonPts <- apply_factor_levels(sComparisonPts, object[[1]]$FactorsInfo)
    }
    
    # change naming 
    xNew <- sComparisonPts
    
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
        level <- row <- prediction <- standardError <-  NULL
        tempDf <- data.frame(level = i, row = j, prediction = pred, 
           standardError = standard_error)
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

#------------------------- summary function ---------------------------------#
# the goal of the summary is to generate all attributes a user may want to
# inspect with a given dsldLM object, which ends up as a list of these key
# underlying attributes (sensitive comparison (interactions, etc.), and more)

summary.dsldLM <- function(object,...) {
  diffS <- list()
  
  # get sName and yName from the output of dsldLinear #
  sName <- object[[1]]$sName
  yName <- object[[1]]$yName
  
  if (length(object) == 1) {
    data <- dsldGetData(object)[[1]]
    summaryOutput <- summary(object[[1]]$model)
    coef <- summaryOutput$coefficients[, 1]
    covMatrix <- object[[1]]$covarianceMatrix
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
    diffS[['Sensitive Factor Level Comparisons']] <- dsldDiffSLin(object)
    
    return(diffS)
  } else {
    sNames <- names(object)
    newData <- object[[1]]$newData
    
    # loop through each level of S name to compute estimates and standard
    # errors
    for (i in sNames) {
      data <- object[[i]]$data
      summaryOutput <- summary(object[[i]]$model)
      coef <- summaryOutput$coefficients[, 1]
      covMatrix <- object[[i]]$covarianceMatrix
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
    diffS[['Sensitive Factor Level Comparisons']] <- dsldDiffSLin(object,
                                                                  newData)
    return(diffS)
  }
}

### ---------------- predict() method ----------------------------------------##
### produces predictions for data supplied in dsldLinear
predict.dsldLM <- function(object, xNew,...) {
  df <- data.frame()
  yName = object[[1]]$yName
  if (length(object) == 1) {
    xNew <- apply_factor_levels(xNew, object[[1]]$FactorsInfo)
    data <- object[[1]]$data
    colName <- names(data)
    colName <- colName[colName != yName]
    model <- object[[1]]$model
    C <- (object[[1]]$covarianceMatrix)
    u_names <- names(coef(object[[1]]$model))
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
      tempDf <- data.frame(row = j, prediction = pred, standardError = standard_error)
      df <- rbind(df, tempDf)
    }
    return(df)
  }
  else {
    xNew <- apply_factor_levels(xNew, object[[1]]$FactorsInfo)
    sNames <- names(object)
    for (i in sNames) {
      data <- object[[i]]$data  
      colName <- names(data)
      colName <- colName[colName != yName]
      model <- object[[i]]$model
      C <- (object[[i]]$covarianceMatrix)
      u_names <- names(coef(object[[i]]$model))
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
    return(df)
  }
}
