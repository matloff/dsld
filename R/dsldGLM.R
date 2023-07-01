# ------------ Class Design ------------ #
#' The class hierarchy will be as such:
#' - dsldDiffModel  :: stores a list of traits such as summary, formula, coefficients, etc. that shows the linear 
#'                     model produced for each interacting level in the sensitive variable. Stores the following:
#'      1. formula to go into the model (yName ~.,); [character] @ formula
#'      2. summary output of model; [character] @ summary
#'      3. coef of beta parameters; [list] @ coef
#'      4. data used in the model (useful to see for interactions); [dataframe] @ data

# -------------------------- dsldGLM ------------------------ #
#' ::: Descripton :::
#' @brief This function produces an instance of the `dsld` class that 
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
#' @param family_type: type of model user wants to fit - enter 'gaussian' for linear; 'binomial' for logistic; 'poisson' for poisson
#' @param interactions: specifies whether or not to consider interactions. 
#'      Defaults to TRUE [boolean]

# dsldGLM
dsldGLM <- function(data, yName, sName, family_type, interactions = TRUE) {
  # create list
  dsldModel <- list()
  
  # user selects for interactions #
  if (interactions == TRUE) {
    
    # split data by sensitive level
    dataSplit <- split(data, data[[sName]])
    dataNames <- names(dataSplit)
    
    # populate model for each level
    for (name in dataNames) {
      
      # initialize instance of dsldDiffModel
      dsldDiffModel <- list()
      
      # data for this level, drop sensitive column
      diffData <- dataSplit[[name]]
      drop <- c(sName)
      diffData <- diffData[, !(names(diffData) %in% drop)]
      
      # get formula & diff model
      formula <- as.formula(paste(yName, "~ ."))
      diffModel <- glm(formula = formula, family = family_type, data = diffData)
      
      # setup instance of dsldDiffModel
      dsldDiffModel <- c(dsldDiffModel, formula,
                         list(summary(diffModel)), list(coef(diffModel)),
                         list(diffData))
      names(dsldDiffModel) <- c("formula", "summary", "coef", "data")
      class(dsldDiffModel) <- "dsldDiffModel"
      
      # add instance into dsldModel
      dsldModel[[name]] <- dsldDiffModel
    }
    
    # user does not select for interactions #
  } else {
    # initialize instance of dsldDiffModel
    dsldDiffModel <- list()
    
    # data for non-interactive
    diffData <- data
    
    # get formula & diff model
    formula <- as.formula(paste(yName, "~ ."))
    diffModel <- glm(formula = formula, family = family_type, data = diffData)
    
    # setup instance of dsldDiffModel
    dsldDiffModel <- c(dsldDiffModel, formula, list(summary(diffModel)),
                       list(coef(diffModel)), list(diffData))
    names(dsldDiffModel) <- c("formula", "summary", "coef", "data")
    class(dsldDiffModel) <- "dsldDiffModel"
    
    # add instance into dsldModel
    dsldModel[[sName]] <- dsldDiffModel
  }
  
  # finalize dsldModel #
  class(dsldModel) <- "dsldGLM"
  return(dsldModel)
  
}

# ------------------------------------- Test Run DsldGLM ----------------------------------------------------
#load('/Users/adityamittal/Desktop/Year_two/Spring_2023/ECS_189G/hw2/law.school.admissions.rda') 
#law.school.admissions$bar <- as.integer(law.school.admissions$bar)
#law.school.admissions$bar <- as.factor(law.school.admissions$bar)
#str(law.school.admissions)
#View(law.school.admissions) # view data
#log_model <- dsldGLM(data = law.school.admissions, yName = 'bar', sName = 'gender', family_type = 'binomial', interactions = TRUE)
#log_model

#another run w/ Compas data
#load('/Users/adityamittal/Desktop/Year_two/Spring_2023/ECS_189G/hw3/compas.rda') # load dataset 
#log_model_2 <- dsldGLM(data = compas, yName = 'two_year_recid', sName = 'race', family_type = 'binomial', interactions = TRUE)
#log_model_2
# ------------------------------------------------------------------------------------------------------------

# -------------------------------- Auxillary Functions -------------------------------------------------------

#' ::: Description ::
#' @brief Polymorphic method that overrides the summary() method, extracting information regarding 
#'      standard errors with regards to sensitive variables (this can be for each level and the differences 
#'      between each level).
#' 
#' ::: Arguments :::
#' @param dsldModel: an instance of the dsldLinearModel s3 object to summarize 

summary.dsldGLM <- function(dsld_obj) {
  result <- lapply(dsld_obj, function(x) x$summary)
  return(result)
}

#summary(log_model)


coef.dsldGLM<- function(dsld_obj) {
  result <- lapply(dsld_obj, function(x) x$coef)
  return(result)
}

#coef(log_model)

dsldGetData <- function(dsld_obj) {
  result <- lapply(dsld_obj, function(x) x$data)
  return(result)
}

# dsldGetData(log_model)
