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
# Task 2: linear/generalized linear models:
# IN PROGRESS - Brainstorming collection of functions that can be used for statistical inferences

library(qeML)

# DSLD Collection of functions to be used ::

# Linear ---------------------------------------------------------------------------------------

library(sandwich)
library(qeML)

dsldLinModel <- function(data, yName, sName, interactions = TRUE) {
  # setup linear model #
  
  # initialize class
  dsldLinModel <- list()
  
  # interactions #
  if (interactions == TRUE) {
    # split data by sensitive level
    dataSplit <- split(data, data[[sName]])
    dataNames <- names(dataSplit)
    
    # populate linear model for each level
    for (name in dataNames) {
      # initialize instance of dsldDiffModel
      dsldDiffModel <- list()
      
      # data for this level, drop sensitive column
      diffData <- dataSplit[[name]]
      drop <- c(sName)
      diffData <- diffData[, !(names(diffData) %in% drop)]
      
      # get formula & diff model
      formula <- as.formula(paste(yName, "~ ."))
      diffModel <- lm(formula, data = diffData)
      
      # setup instance of dsldDiffModel
      dsldDiffModel <- c(dsldDiffModel, formula,
                         list(summary(diffModel)), list(coef(diffModel)),
                         list(diffData))
      names(dsldDiffModel) <- c("formula", "summary", "coef", "data")
      
      # add instance into dsldLinModel
      dsldLinModel[[name]] <- dsldDiffModel
    }
  } else {
    # initialize instance of dsldDiffModel
    dsldDiffModel <- list()
    
    # data for non-interactive
    diffData <- data
    
    # get formula & diff model
    formula <- as.formula(paste(yName, "~ ."))
    diffModel <- lm(formula, data = diffData)
    
    # setup instance of dsldDiffModel
    dsldDiffModel <- c(dsldDiffModel, formula, list(summary(diffModel)),
                       list(coef(diffModel)), list(diffData))
    names(dsldDiffModel) <- c("formula", "summary", "coef", "data")
    
    # add instance into dsldLinModel
    dsldLinModel[[sName]] <- dsldDiffModel
  }
  
  # finalize dsldLinModel #
  class(dsldLinModel) <- "dsld"
  return(dsldLinModel)
}


# Test runs 
data(pef)
x <- dsldLinModel(data = pef, yName = 'wageinc', sName = 'sex', interactions = TRUE)


# some other polymorphic functions  ------------------------------------------------------------
summary.dsld <- function(dsld_obj) {
  result <- lapply(dsld_obj, function(x) x$summary)
  return(result)
}
#summary(x)

coef.dsld <- function(dsld_obj) {
  result <- lapply(dsld_obj, function(x) x$coef)
  return(result)
}
#coef(x)

get_data <- function(dsld_obj) {
  result <- lapply(dsld_obj, function(x) x$temp_data)
  return(result)
}
#get_data(x)

dsld_is_valid_name <- function(name, list) {
  name %in% names(list)
}
                   
# this function is intended to compare effects across S level  -----------------------------------
dsldCompareDifferencesOfEffects <- function(dsld_obj, xName, data) {
  
  # Prompt the user to enter two component names
  component1_name <- ""
  component2_name <- ""
  
  while (!dsld_is_valid_name(component1_name, dsld_obj)) {
    component1_name <- as.character(readline("Enter the name of the first S level: "))
  }
  
  while (!dsld_is_valid_name(component2_name, dsld_obj)) {
    component2_name <- as.character(readline("Enter the name of the second S level: "))
  }
  
  # Subset the list to include only the specified components
  subsetted_list <- dsld_obj[c(component1_name, component2_name)]
  
  # Subset the list to include only the specified components
  subsetted_list <- dsld_obj[c(component1_name, component2_name)]
  class(subsetted_list) <- 'dsld'
  
  summary_list <- summary(subsetted_list)
  
  
  # What to do when xName is numeric
  if (is.numeric(data[[xName]])) {
    coefficient_xName <- sapply(summary_list, function(summary_output) summary_output$coefficients[xName, "Estimate"])
    se_xName <- sapply(summary_list, function(summary_output) summary_output$coefficients[xName, "Std. Error"])
  
    difference_xName <- diff(coefficient_xName)
    standard_error_difference <- sqrt(sum(unlist(se_xName)^2))
    
    # Print the results
    cat("Difference in coefficients (age_female - age_male):", round(difference_xName, 2), "\n")
    cat("Standard error for the difference:", round(standard_error_difference, 2), "\n")
    
    estimate = difference_xName
    standard_error = standard_error_difference
    
    my_list <- list(estimate, standard_error)
    names(my_list) <- c("Estimate of difference", "Standard error of difference")
    
  } else { # This part is currently incomplete, trying to figure out best way to make it work well for user purposes.
    
    coefficient_xName <- sapply(summary_list, function(summary_output) {
      summary_output$coefficients[grepl(xName, rownames(summary_output$coefficients)), "Estimate"]
    })
    
    se_xName <- sapply(summary_list, function(summary_output) {
      summary_output$coefficients[grepl(xName, rownames(summary_output$coefficients)), "Std. Error"]
    })
    
    # Compute the difference in coefficients
    difference_in_coefficients <- apply(coefficient_xName, 1, diff)
    standard_error_difference <- sqrt(sum(se_xName^2))
    
    print(coefficient_xName)
    print(se_xName)
    print(difference_in_coefficients)
    print(standard_error_difference)
  
    cat("Difference in coefficients (education_female - education_male):", round(difference_in_coefficients, 2), "\n")
    cat("Standard error for the difference:", round(standard_error_difference, 2), "\n")
    
    estimate = difference_in_coefficients
    standard_error = standard_error_difference
    
    my_list <- list(estimate, standard_error)
    names(my_list) <- c("Estimate of difference", "Standard error of difference")
    
  }
  return(my_list)
}

# Test run
b <- dsldCompareDifferencesOfEffects(x, 'age', pef)
b

# CI interval
dsldConfidenceInterval <- function(estimates, confidence_level) {
  # Extract point estimate and standard error from the list
  point_estimate <- estimates[[1]]
  standard_error <- estimates[[2]]
  
  # Calculate the critical value based on the confidence level
  z_value <- qnorm((1 + confidence_level) / 2)
  
  # Calculate the margin of error
  margin_of_error <- z_value * standard_error
  
  # Calculate the lower and upper bounds of the confidence interval
  lower_bound <- point_estimate - margin_of_error
  upper_bound <- point_estimate + margin_of_error
  
  # Create the confidence interval as a named vector
  confidence_interval <- c(lower = lower_bound, upper = upper_bound)
  
  return(confidence_interval)
}

# test run
bt <- dsldConfidenceInterval(b, 0.95)
bt
                       
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

    xData <- data[dsldModel$xCols, ]
    yData <- data[dsldModel$yCol, ]

    # plotting #
    # create plot
    plot(
        xData,
        yData,
        type = 'l',
        lty = 'solid',
        col = "black",
        xlab = dsldModel$xCols,
        ylab = dsldModel$yCol,
        main = "DSLD Linear Model Plot"
    )

    # plot predictive model
    points(xData, dsldModel$pred, type = "l", lty = "solid", col = "red")
}

