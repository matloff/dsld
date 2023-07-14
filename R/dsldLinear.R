# -------------------------- dsldLinear ---------------------------------------#
#' ::: Descripton :::
#' @brief The dsldLinear function fits a linear model to the response variable,
#'      yName, using all other available covariates in the user provided
#'      dataset. The user may select for full interactions across the sensitive
#'      variable, sName, in which case the function will fit m separate models,
#'      where m is the number of levels of sName.
#'
#'      The function produces an instance of the `dsldLinear` class (an S3
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
#'
dsldLinear <- function(data, yName, sName, interactions = FALSE,
    newData = NULL) {
    # create final output list to by populated with results #
    dsldModel <- list()

    # user wants interactions #
    if (interactions) {
        # raise error if user doesn't input newData #
        if (is.null(newData)) {
            stop(paste("Please enter the newData input to compare for ",
                "interactions in summary()"))
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
                family = "gaussian", data = diffData)

            # setup individual instance of dsldDiffModel #
            dsldDiffModel <- c(dsldDiffModel,
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

    # user selects interactions == FALSE #
    } else {
        # initialize instance of dsldDiffModel #
        dsldDiffModel <- list()

        # create model #
        diffModel <- glm(formula = as.formula(paste(yName, "~ .")),
            family = "gaussian", data = data)

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

    # finalize dsldModel #
    class(dsldModel) <- "dsldLinear"
    return(dsldModel)
}

# -------------------- Test Run dsldLinear ------------------------------------#
#library(qeML)
#svcensus <- read.csv("~/Desktop/Dsld_Package/svcensus.csv")
#svcensus$occ <- as.factor(svcensus$occ)
#svcensus$educ <- as.factor(svcensus$educ)
#svcensus$gender <- as.factor(svcensus$gender)
#newData <- data.frame(age = c(18,60), educ = c("zzzOther",'zzzOther'),wkswrkd = c(50,50), occ = c("106","106"))       # compare genders across different age // early vs late career 
#lin1 = dsldLinear(svcensus,'wageinc','gender', interactions = TRUE, newData); lin1
#lin2 = dsldLinear(svcensus,'wageinc','gender', interactions = FALSE); lin2
# -----------------------------------------------------------------------------#

# ------------------- Test Run dsldLinear -------------------------------------#
#load('/Users/adityamittal/Desktop/Year_two/Spring_2023/ECS_189G/hw2/law.school.admissions.rda')                        # law schools data, predict undergrad gpa
#drop <- c('fulltime', 'bar','cluster')
#law.school.admissions <- law.school.admissions[, !(names(law.school.admissions) %in% drop)]
#lin_1 <- dsldLinear(law.school.admissions,'ugpa','gender', interactions = TRUE)                                        # will raise error since we haven't specified new data
#lin_2 <- dsldLinear(law.school.admissions,'ugpa','gender', interactions = FALSE)
# -----------------------------------------------------------------------------#


# ----------------------- Auxiliary Functions ---------------------------------#

#' ::: Description ::
#' @brief coef() is a polymorphic method that takes in an object of the
#'      'dsldLinear' class. The function provides m regression coefficients
#'      of the model, where m is the number of levels of sName.
#'
#' ::: Arguments :::
#' @param dsld_obj: an instance of the dsldLinearModel s3 object.
#'
coef.dsldLinear <- function(dsldLM) {
    # merge & return coefficients #
    mergedCoef <- lapply(dsldLM, function(x) x$coef)
    return(mergedCoef)
}


#coef(lin1) # test run
#coef(lin2)


#' ::: Description ::
#' @brief the dsldGetData() function takes in an object of the 'dsldLinear'
#'      class. The function provides m dataset(s) used to train the linear
#'      model, where m is the number of levels of sName.
#'
#' ::: Arguments :::
#' @param dsldLM: an instance of the dsldLinearModel s3 object.
#'
dsldGetData <- function(dsldLM) {
    # merge & return datasets #
    mergedData <- lapply(dsldLM, function(x) x$data)
    return(mergedData)
}


#dsldGetData(lin1)
#dsldGetData(lin2)


#------------------------- dsldDiffS function ---------------------------------#
#' ::: Description ::
#' @brief dsldValidateData() is an indirect helper function for dsldDiffS()
#'      full-interactions case. The function takes in the newData argument
#'      from dsldDiffS() and validates if the user has entered appropriate
#'      entries for newData.
#'
#' ::: Arguments :::
#' @param newData: user inputted data for incoming new test cases.
#' @param model: linear model fitted by output of dsldLinear().
#'
dsldValidateData <- function(newData, model) {
    #  check to see if columns in newData exist in the original data #
    missingCols <- setdiff(names(newData), names(model$model))
    if (length(missingCols) > 0) {
        stop(paste("Invalid column(s):", paste(missingCols,
            collapse = ", ")))
    }

    # Check if categorical variables entries are valid #
    categoricalVars <- names(model$model)[sapply(model$model, is.factor)]
    for (i in 1:nrow(newData)) {
        curRow <- newData[i, ]

        for (var in categoricalVars) {
            levels <- unique(model$model[[var]])
            if (!(curRow[[var]] %in% levels)) {
                stop(paste("Invalid", var, "level in row", i, "."))
            }
        }
    }

    # return the data back if everything is good #
    return(newData)
}


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
#'
dsldDiffS <- function(dsldLM, newData = NULL) {
    # get sName and yName from the output of dsldLinear #
    sName <- dsldLM[[1]]$sName
    yName <- dsldLM[[1]]$yName

    # diffS results when interaction == FALSE in dsldLinear #
    if (length(dsldLM) == 1) {
        # extract pairwise combination of [dummy level in glm - factor levels]
        # from summary output
        data <- dsldGetData(dsldLM)[[1]]
        model <- dsldLM[[1]]$model
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
        return(df)
    } else { # with interactions
        # raise error if the user doesn't input new data #
        if (is.null(newData)) {
            stop("Please enter the newData input to compare for interactions")
        }

        # get vector of all levels in sName #
        sNames <- names(dsldLM)
        df <- data.frame()

        # loop through each level of S name to compute estimates and
        # standard errors
        for (i in sNames) {
            data <- dsldLM[[i]]$data
            model <- dsldLM[[i]]$model
            xNew <- dsldValidateData(newData, model)

            predictions <- predict(model, xNew, type = "response",
                se.fit = TRUE)
            pred <- predictions$fit
            se <- predictions$se.fit
            tempDF <- data.frame(level = i, row = 1:nrow(xNew),
                prediction = pred, standardError = se)
            df <- rbind(df, tempDF)
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
                names(tempDF) <- c("Factors Compared", "Row", "Estimates",
                    "Standard Errors")
                pairwiseDF <- rbind(pairwiseDF, tempDF)
            }
        }

        return(pairwiseDF)
    }
}


# ---------------------------- Test runs  -------------------------------------#
# educ_data <- data.frame(age = c(18,60), educ = c("16", "16"),
#       wkswrkd = c(50, 50), occ = c("106", "106")) # compare genders across
#                                                   # different age and early
#                                                   # vs late career
# dat1 <- dsldDiffS(lin1, educ_data) # run with interactions 
# View(dat1)

# dat2 <- dsldDiffS(lin2) # no interactions case
# View(dat2)
# -----------------------------------------------------------------------------#

#' ::: Description ::
#' @brief summary() is a polymorphic method that takes in an object of the 'dsldLinear' 
#'      class. The function provides m summaries of the model, where m is the number 
#'      of levels of sName. Additionally, the summary function also report differences 
#'      across S levels.
#' 
#' ::: Arguments :::
#' @param dsld_obj: an instance of the dsldLinearModel s3 object that output summary objects.
#'
summary.dsldLinear <- function(dsldLM) {
    diffS <- list()

    # get sName and yName from the output of dsldLinear #
    sName <- dsldLM[[1]]$sName
    yName <- dsldLM[[1]]$yName

    if (length(dsldLM) == 1) {
        data <- dsldGetData(dsldLM)[[1]]
        summary_output <- summary(dsldLM[[1]]$model)
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
        diffS[['Sensitive Factor Level Comparisons']] <- dsldDiffS(dsldLM)

        return(diffS)
    } else {
        sNames <- names(dsldLM)
        newData <- dsldLM[[1]]$newData

        # loop through each level of S name to compute estimates and standard
        # errors
        for (i in sNames) {
            data <- dsldLM[[i]]$data
            summary_output <- summary(dsldLM[[i]]$model)
            coef <- summary_output$coefficients[, 1]
            std_err <- summary_output$coefficients[, 2]
            pValues <- summary_output$coefficients[, 4]

            df <- data.frame(
                Covariate = row.names(summary_output$coefficients),
                Estimate = coef,
                `Standard Error` = std_err,
                PValue = pValues,
                stringsAsFactors = FALSE,
                row.names = NULL
            )

            diffS[[i]] <- df
        }
        diffS[['Sensitive Factor Level Comparisons']] <- dsldDiffS(dsldLM,
            newData)
        return(diffS)
    }
}

# interactions_summary <- summary(lin2); interactions_summary
# no_interactions_summary <- summary(lin1); no_interactions_summary
