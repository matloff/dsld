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
dsldCreateModel <- function(data, yName, sName, function_type, interactions = TRUE) {
  dsld = list()
  if (interactions == TRUE) {
    data_split = split(data,data[[sName]])
    x = names(data_split) # get list attributes, i.e. names
    for (i in x) { # loop through each component of list; i is the name of the list at each iteration
      temp_data <- data_split[[i]] # get subset of data for i-th iteration
      response_var <- temp_data[[yName]]
      drop <- c(sName, yName)
      temp_data = temp_data[,!(names(temp_data) %in% drop)]
      temp_model <- glm(formula = response_var ~ ., family = function_type, data = temp_data)
      dsld[[i]] <- summary(temp_model)
    }
  } else{
    response_var <- data[[yName]]
    drop <- c(yName)
    data = data[,!(names(data) %in% drop)]
    temp_model <- glm(formula = response_var ~ ., family = function_type, data = data)
    dsld[[1]] <- summary(temp_model)
  }
  return(dsld)
}

dsldCreateModel(data = pef, yName = 'wageinc', sName = 'sex', function_type = 'gaussian', interactions = TRUE)
dsldCreateModel(data = pef, yName = 'wageinc', sName = 'sex', function_type = 'gaussian', interactions = FALSE)


# No interaction --------------------------------------------------------------------
data(pef) 
z <- glm(formula = wageinc ~ ., family = "gaussian", data = pef) # thought: output can be summary outputs
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
data_split = split(pef,pef[['sex']])
data_split


# male data
pefm <- data_split$`1`
drop <- c("sex")
pefm = pefm[,!(names(pefm) %in% drop)]

# female data
peff <- data_split$`2`
drop <- c("sex")
peff = peff[,!(names(peff) %in% drop)]


m <- glm(formula = wageinc ~ ., family = "gaussian", data = pefm)
f <- glm(formula = wageinc ~ ., family = "gaussian", data = peff)

summary(m)
summary(f)

# Partial Interaction -------------------------------------------------------------------
t <-lm(wageinc ~. + age:sex, data=pef) # in this case, would we not have the sex term as well???
summary(t)
