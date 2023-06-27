# Task 2: linear/generalized linear models:
# IN PROGRESS - Brainstorming collection of functions that can be used for statistical inferences

### IDEAS:: want to include for single effects of each S levels AND for differences between S levels

# Initial Function: Creates model, outputs S3 list 'dsld' object. This will contain summaries of 
# the linear models. Allows for interactions vs. no interactions.

# Plot function: Diagnostic plots - could be useful to see how the model diagnostics hold up

# comparison functions <- not sure how to implement

library(sandwich) # load libraries 
library(qeML)

# DSLD Collection of functions to be used ::
# Linear ---------------------------------------------------------------------------------------
dsldLinModel <- function(data, yName, sName, interactions = TRUE) {
  # This functions creates a linear model with/without interactions based on user input of yName, sName, and Data
  # Will return a list of lists 3 objects:
  # 1. formula to go into the model (yName ~.,)
  # 2. summary output of model
  # 3. coef of beta parameters
  # 3. data used in the model (useful to see for interactions)
 
  dsld = list()  # initialize empty list 

  # case if interactions == TRUE
  if (interactions == TRUE) {
    data_split = split(data,data[[sName]]) # split data by S level 
    x = names(data_split) # get names 
    
    # loop through each S level
    for (i in x) { 
      temp_list <- list() # initialize second list, to be populated into main DSLD list 
      
      # Get temp_data
      temp_data <- data_split[[i]] 
      drop <- c(sName)
      temp_data = temp_data[,!(names(temp_data) %in% drop)]
      
      # get formula & summary output 
      formula <- as.formula(paste(yName, "~ ."))
      temp_model <- lm(formula, data = temp_data)
      
      temp_list <- c(temp_list, formula, list(summary(temp_model)), list(coef(temp_model)), list(temp_data))
      names(temp_list) <- c("formula", "summary", "coef", "temp_data")
      dsld[[i]] <- temp_list
    }
  } 
  # case if interactions == FALSE 
  else {
    
    # get data
    temp_data <- data 
    
    # get formula & summary output 
    formula <- as.formula(paste(yName, "~ ."))
    temp_model <- lm(formula, data = temp_data)
    
    # populate list
    temp_list <- c(temp_list, formula, list(summary(temp_model)),list(coef(temp_model)), list(temp_data))
    names(temp_list) <- c("formula", "summary", "coef", "temp_data")
    dsld[[1]] <- temp_list
  }
  
  class(dsld) <- 'dsld'
  return(dsld)
}

# Test runs 
x <- dsldLinModel(data = pef, yName = 'wageinc', sName = 'sex', interactions = TRUE)
x # creates a list of two that contains useful info of the linear model. 
x$`1`
x$`2`


# -------------------------------------------------------------------------------------------------------------
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
str.dsldLinearModel <- function(dsldModel) {
    # return summary #
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
print.dsldLinearModel <- function(dsldModel) {
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
summary.dsldLinearModel <- function(dsldModel) {
    # return summary #
    return(dsldModel$summary)
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
plot.dsldLinearModel <- function(dsldModel) {
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

