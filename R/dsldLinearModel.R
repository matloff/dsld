# Task 2: linear/generalized linear models:
# IN PROGRESS - Brainstorming collection of functions that can be used for statistical inferences


# ------------ Ideas ------------ #
#' - Want to include for single effects of each S levels AND for differences between S levels
#' - Initial Function: Creates model, outputs S3 list 'dsld' object. This will contain summaries of 
#'      the linear models. Allows for interactions vs. no interactions.
#' - Plot function: Diagnostic plots - could be useful to see how the model diagnostics hold up
#' - Comparison functions <- not sure how to implement


# ------------ Class Design ------------ #
#' The class hierarchy will be as such:
#' - dsldDiffModel  :: stores a list of traits such as summary, formula, coefficients, etc. that shows the linear 
#'                     model produced for each interacting level in the sensitive variable. Stores the following:
#'      1. formula to go into the model (yName ~.,); [character] @ formula
#'      2. summary output of model; [character] @ summary
#'      3. coef of beta parameters; [list] @ coef
#'      4. data used in the model (useful to see for interactions); [dataframe] @ data
#'
#' - dsldLinModel   :: stores a list of dsldDiffModels


# ------------ Linear Model ------------ #
#' ::: Descripton :::
#' @brief This function produces an instance of the `dsldLinModel` class that 
#'      houses a separate instance of the `dsldDiffModel` class for each 
#'      unique, interactive level in the sensitive column specified. The end 
#'      result is a linear model that with or without interactions that 
#'      predicts over the inputted data with respect to yName and sName.
#' 
#' ::: Arguments :::
#' @param data: dataset to model over, will be split according to each level 
#'      in the final outputted `dsldLinModel` object [dataframe]
#' @param yName: name of the predictive column [character]
#' @param sName: name of the sensitive column [character]
#' @param interactions: specifies whether or not to consider interactions. 
#'      Defaults to TRUE [boolean]
#' 
dsldLinModel <- function(data, yName, sName, interactions=TRUE) {
    # setup linear model #
    # library requirements
    library(sandwich)
    library(qeML)

    # initialize class
    dsldLinModel = list()

    # interactions #
    if (interactions == TRUE) {
        # split data by sensitive level
        dataSplit = split(data, data[[sName]])
        dataNames = names(dataSplit)
        
        # populate linear model for each level
        for (name in dataNames) {
            # initialize instance of dsldDiffModel
            dsldDiffModel <- list()
            
            # data for this level, drop sensitive column
            diffData <- dataSplit[[i]]
            drop <- c(sName)
            diffData <- diffData[, !(names(diffData) %in% drop)]
            
            # get formula & diff model
            formula <- as.formula(paste(yName, "~ ."))
            diffModel <- lm(formula, data=diffData)
            
            # setup instance of dsldDiffModel
            dsldDiffModel <- c(dsldDiffModel, formula, list(summary(diffModel)), list(coef(diffModel)), list(diffData))
            names(dsldDiffModel) <- c("formula", "summary", "coef", "data")
            class(dsldDiffModel) <- "dsldDiffModel"

            # add instance into dsldLinModel
            dsldLinModel[[name]] <- dsldDiffModel
        }
    } 
    else {
        # initialize instance of dsldDiffModel
        dsldDiffModel <- list()
        
        # data for non-interactive
        diffData <- data
        
        # get formula & diff model
        formula <- as.formula(paste(yName, "~ ."))
        diffModel <- lm(formula, data=diffData)
        
        # setup instance of dsldDiffModel
        dsldDiffModel <- c(temp_list, formula, list(summary(diffModel)), list(coef(diffModel)), list(diffData))
        names(dsldDiffModel) <- c("formula", "summary", "coef", "data")
        class(dsldDiffModel) <- "dsldDiffModel"

        # add instance into dsldLinModel
        dsldLinModel[[sName]] <- dsldDiffModel
    }
    
    # finalize dsldLinModel #
    class(dsldLinModel) <- "dsldLinModel"
    return(dsldLinModel)
}


# ------------ Linear Model Testing ------------ #
x <- dsldLinModel(data = pef, yName = 'wageinc', sName = 'sex', interactions = TRUE)
x # creates a list of two that contains useful info of the linear model. 
x$`1`
x$`2`


# ------------ Polymorphic Methods for the Linear Model ------------ #
#' Defining some basic polymorphic methods for the linear model
#'  - str()     :: in string form, accesses the summary for the model
#'  - print()   :: will print the str()
#'  - summary() :: prints out a new level for each statistically independent portion of data
#'  - plot()    :: plots the linear model give the data it stores

#' ::: Description ::
#' @brief Polymorphic method that overrides the str() function, returning a string representation of the 
#'      dsld linear model object. For now, the string representation will be a simple summary printed out 
#'      as accessed through the summary() method, but in the future this may be adjusted to provide a simpler 
#'      set of information for users to look at (intercepts, std-err, etc.) in a table-like format.
#' 
#' ::: Arguments :::
#' @param dsldModel: an instance of the dsldLinearModel s3 object to convert to string representation
#' 
str.dsldLinModel <- function(dsldModel) {
    # return summaries for now #
    return(summary(dsldModel))
}

#' ::: Description ::
#' @brief Polymorphic method that overrides the print() function, printing a string representation of the 
#'      dsld linear model object. For now, the string representation will be a simple summary printed out 
#'      as accessed through the summary() method, but in the future this may be adjusted to provide a simpler 
#'      set of information for users to look at (intercepts, std-err, etc.) in a table-like format.
#' 
#' ::: Arguments :::
#' @param dsldModel: an instance of the dsldLinearModel s3 object to print
#' 
print.dsldLinModel <- function(dsldModel) {
    # print string representation #
    print(str(dsldModel))
}

#' ::: Description ::
#' @brief Polymorphic method that overrides the summary() method, extracting information regarding 
#'      standard errors with regards to sensitive variables (this can be for each level and the differences 
#'      between each level).
#' 
#' ::: Arguments :::
#' @param dsldModel: an instance of the dsldLinearModel s3 object to summarize
#' 
summary.dsldLinModel <- function(dsldModel) {
    # return all summaries #
    # store summaries
    summaries <- ""

    # for each interaction level
    for (level in names(dsldModel)) {
        # add level and summary
        summaries <- paste(summaries, "\n :::: ", level, " :::: \n")
        summaries <- paste(summaries, summary(dsldModel[[level]]$summary))
    }

    # return concatenated summaries
    return(summaries)
}


#' ::: Description ::
#' @brief Polymorphic method that overrides the str() function, returning a string representation of the 
#'      dsld linear model object. For now, the string representation will be a simple summary printed out 
#'      as accessed through the summary() method, but in the future this may be adjusted to provide a simpler 
#'      set of information for users to look at (intercepts, std-err, etc.) in a table-like format.
#' 
#' ::: Arguments :::
#' @param dsldModel: an instance of the dsldLinearModel s3 object to print
#' 
plot.dsldLinModel <- function(dsldModel) {
    # variable handling #
    data <- dsldModel$data
    linModel <- dsldModel$model

    xData <- data[dsldModel$xCols, ]
    yData <- data[dsldModel$yCol, ]

    # plotting #
    # create plot
    plot(
        xData, 
        yData, 
        type='l', 
        lty='solid',  
        col="black",
        xlab=dsldModel$xCols, 
        ylab=dsldMode$yCol, 
        main="DSLD Linear Model Plot"
    )

    # plot predictive model
    points(xData, dsldModel$pred, type='l', lty='solid', col="red")
}

