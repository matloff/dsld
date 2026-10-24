### -------------------------- DSLD Linear -------------------------------------
### creates the dsldLinear model; useful to call summary(), predict() etc.
dsldLinear <- function(data, yName, sName, interactions = FALSE,
                       sComparisonPts = NULL, useSandwich = FALSE) {
  .dsld_check_data(data, c(yName, sName))
  data <- toNumericFactor(data)
  if (!is.numeric(data[[yName]])) stop("dsldLinear requires a numeric response.")
  if (!is.factor(data[[sName]]) || nlevels(droplevels(data[[sName]])) < 2L)
    stop("sName must identify a factor with at least two observed groups.")
  if (any(table(data[[sName]]) == 0L)) data[[sName]] <- droplevels(data[[sName]])
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
      diffData <- diffData[, !(names(diffData) %in% drop), drop = FALSE]
      diffModel <- lm(
        formula = .dsld_formula(yName),
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
      formula = .dsld_formula(yName),
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
  if (length(object) == 1L) {
    return(.dsld_factor_contrasts(object, object[[1L]]$covarianceMatrix))
  }
  if (!is.data.frame(sComparisonPts)) stop("Supply a data frame of sComparisonPts for interactions.")
  .dsld_group_differences(predict.dsldLM(object, sComparisonPts))
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
predict.dsldLM <- function(object, xNew, ...) {
  if (!is.data.frame(xNew) || !nrow(xNew)) stop("xNew must be a nonempty data frame.")
  predictions <- lapply(seq_along(object), function(index) {
    group_model <- object[[index]]
    new_data <- apply_factor_levels(xNew, group_model$FactorsInfo, restore_contrasts = FALSE)
    result <- .dsld_lm_prediction(group_model$model, new_data, group_model$covarianceMatrix)
    if (length(object) > 1L) result <- cbind(level = names(object)[index], result)
    result
  })
  do.call(rbind, predictions)
}
