# Task 2: linear/generalized linear models:
# Function to be written, we're currently brainstorming things users can utilize.

dsldModel <- function(data, yName, sName, xName, family) {
  return(NULL)
}

# DSLD Function

### What to include::
### Thinking of outputting an S3 object that can be called on w/ other functions maybe
# summary outputs
###### - include model
###### - the output itself
### want to include for single effects of each S levels AND for differences between S levels


#load car package
library(car)
library(sandwich)
library("lmtest")
library(qeML)


# Collection of functions maybe

# First fct: dsldCreateModel <- this fct. outputs an S3 Objects of terms
# currently:: output is summary obj
dsldCreateModel <- function(data, yName, sName, function_type, interactions=TRUE) {
    dsld <- list()
    if (interactions == TRUE) {
        data_split <- split(data, data[[sName]])
        x <- names(data_split) # get list attributes, i.e. names
        for (i in x) { # loop through each component of list; i is the name of the list at each iteration
            temp_data <- data_split[[i]] # get subset of data for i-th iteration
            response_var <- temp_data[[yName]]
            drop <- c(sName, yName)
            temp_data <- temp_data[, !(names(temp_data) %in% drop)]
            temp_model <- glm(formula=response_var ~ ., family=function_type, data=temp_data)
            dsld[[i]] <- summary(temp_model)
        }
    } else {
        response_var <- data[[yName]]
        drop <- c(yName)
        data <- data[, !(names(data) %in% drop)]
        temp_model <- glm(formula=response_var ~ ., family=function_type, data=data)
        dsld[["summary"]] <- summary(temp_model)
    }

    # return class #
    class(dlsd) <- "dsldLinearModel"

    return(dsld)
}


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


# Test runs
# regression
data(pef)
dsldCreateModel(data=pef, yName='wageinc', sName='sex', function_type='gaussian', interactions=TRUE)
dsldCreateModel(data=pef, yName='wageinc', sName='sex', function_type='gaussian', interactions=FALSE)

# binomial output
load('/Users/adityamittal/Desktop/Year_two/Spring_2023/ECS_189G/packages/fairml/data/german.credit.rda') # german credit
View(german.credit)
dsldCreateModel(data=german.credit, yName='Credit_risk', sName='Gender', function_type='binomial', interactions=TRUE)

# poisson
data(mtcars)
View(mtcars)



# Random test code
# No interaction --------------------------------------------------------------------
data(pef) 
z <- glm(formula=wageinc ~ ., family="gaussian", data=pef) # thought: output can be summary outputs
summary(z)
tt <- list()
x <- summary(z)
tt[[1]] <- x
tt

# here gender pay gap can be analyzed, simple when there aren't interactions 
# CI
# Hypothesis testings (advised against, but ig we'll show how to do them)

# comparing single effects of each S level <- this may be a separate function
b <- coef(z)
C <- vcov(z)
?vcov()
u <- c(0,0,0,0,1,0,-1,0,0,0,0)
xx <- t(u) %*% C %*% u

# comparing for differences between S levels


# potential plots <- separate function?
colnames(vcov(z))

# Full Interaction -------------------------------------------------------------------
data_split <- split(pef,pef[['sex']])
data_split


# male data
pefm <- data_split$`1`
drop <- c("sex")
pefm <- pefm[,!(names(pefm) %in% drop)]

# female data
peff <- data_split$`2`
drop <- c("sex")
peff <- peff[,!(names(peff) %in% drop)]


m <- glm(formula=wageinc ~ ., family="gaussian", data=pefm)
f <- glm(formula=wageinc ~ ., family="gaussian", data=peff)

summary(m)
summary(f)

# Partial Interaction -------------------------------------------------------------------
t <-lm(wageinc ~. + age:sex, data=pef) # in this case, would we not have the sex term as well???
summary(t)
