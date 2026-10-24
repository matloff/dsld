### ------------------------ DSLDLogit -----------------------------------------
### creates the dsldLogit object
dsldLogit <- function(data, yName, sName, sComparisonPts = NULL, interactions = FALSE, yesYVal) {

  .dsld_check_data(data, c(yName, sName))
  data <- toNumericFactor(data)
  .dsld_binary_response(data[[yName]], yesYVal)
  if (!is.factor(data[[sName]]) || nlevels(droplevels(data[[sName]])) < 2L)
    stop("sName must identify a factor with at least two observed groups.")
  if (any(table(data[[sName]]) == 0L)) data[[sName]] <- droplevels(data[[sName]])
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
      diffData <- diffData[, !(names(diffData) %in% drop), drop = FALSE]

      # create the model #
      diffModel <- glm(formula = .dsld_formula(yName),
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
    diffModel <- glm(formula = .dsld_formula(yName),
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

### #------------------------- dsldDiffSLog function --------------------------#
dsldDiffSLog <- function(object, sComparisonPts = NULL) {
  if (length(object) == 1L) return(.dsld_factor_contrasts(object, vcov(object[[1L]]$model)))
  if (!is.data.frame(sComparisonPts)) stop("Supply a data frame of sComparisonPts for interactions.")
  .dsld_group_differences(predict.dsldGLM(object, sComparisonPts), probabilities = TRUE)
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
predict.dsldGLM <- function(object, xNew, ...) {
  if (!is.data.frame(xNew) || !nrow(xNew)) stop("xNew must be a nonempty data frame.")
  predictions <- lapply(seq_along(object), function(index) {
    group_model <- object[[index]]
    new_data <- apply_factor_levels(xNew, group_model$FactorsInfo, restore_contrasts = FALSE)
    prediction <- predict(group_model$model, new_data, type = "response", se.fit = TRUE)
    result <- data.frame(row = seq_len(nrow(new_data)), prediction = as.numeric(prediction$fit),
                         standardError = as.numeric(prediction$se.fit))
    if (length(object) > 1L) result <- cbind(level = names(object)[index], result)
    result
  })
  do.call(rbind, predictions)
}
