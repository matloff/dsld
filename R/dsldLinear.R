# ------------ Class Design ------------ #
#' The class hierarchy will be as such:
#' - dsldDiffModel  :: stores a list of traits such as summary, formula, coefficients, etc. that shows the linear 
#'                     model produced for each interacting level in the sensitive variable. Stores the following:
#'      1. formula to go into the model (yName ~.,); [character] @ formula
#'      2. summary output of model; [character] @ summary
#'      3. coef of beta parameters; [list] @ coef
#'      4. data used in the model (useful to see for interactions); [dataframe] @ data

# -------------------------- dsldLinear ------------------------ #
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
#' @param interactions: specifies whether or not to consider interactions. 
#'      Defaults to TRUE [boolean]

# dsldLinear
dsldLinear <- function(data, yName, sName, interactions = TRUE) {
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
      diffModel <- glm(formula = formula, family = 'gaussian', data = diffData)
      
      # setup instance of dsldDiffModel
      dsldDiffModel <- c(dsldDiffModel, formula,
                         list(summary(diffModel)), list(coef(diffModel)),
                         list(diffData))
      names(dsldDiffModel) <- c("formula", "summary", "coef", "data")
      
      # class(dsldDiffModel) <- "dsldDiffModel"
      
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
    diffModel <- glm(formula = formula, family = 'gaussian', data = diffData)
    
    # setup instance of dsldDiffModel
    dsldDiffModel <- c(dsldDiffModel, formula, list(summary(diffModel)),
                       list(coef(diffModel)), list(diffData))
    names(dsldDiffModel) <- c("formula", "summary", "coef", "data")
    # class(dsldDiffModel) <- "dsldDiffModel"
    
    # add instance into dsldModel
    dsldModel[[sName]] <- dsldDiffModel
  }
  
  # finalize dsldModel #
  class(dsldModel) <- "dsldLinear"
  return(dsldModel)
  
}

# -------------------- Test Run dsldLinear --------------------------------------------------
#library(qeML)
#data(pef)
#lin_model_1 <- dsldLinear(data = pef, yName = 'wageinc', sName = 'sex', interactions = TRUE)
#lin_model_1

#lin_model_2 <- dsldLinear(data = pef, yName = 'wageinc', sName = 'sex', interactions = FALSE)
#lin_model_2
# --------------------------------------------------------------------------------

# -------------------------------- Auxillary Functions -------------------------------------------------------

#' ::: Description ::
#' @brief Polymorphic method that overrides the summary() method, extracting information regarding 
#'      standard errors with regards to sensitive variables (this can be for each level and the differences 
#'      between each level).
#' 
#' ::: Arguments :::
#' @param dsldModel: an instance of the dsldLinearModel s3 object to summarize

summary.dsldLinear <- function(dsld_obj) {
  result <- lapply(dsld_obj, function(x) x$summary)
  return(result)
}

# summary(lin_model_1) 

coef.dsldLinear <- function(dsld_obj) {
  result <- lapply(dsld_obj, function(x) x$coef)
  return(result)
}

# coef(lin_model_1)

dsldGetData <- function(dsld_obj) {
  result <- lapply(dsld_obj, function(x) x$data)
  return(result)
}
                   
# dsldGetData(lin_model_1)

### ------------------------- dsldDiffS function -----------------------------------------

dsldDiffS <- function(dsldObj, xName = NULL) {
  # Load libraries
  library(multcomp)
  
  # no interactions
  if (length(dsldObj) == 1) {
    
    # I'm kinda re-creating the model here, I don't really want to do that - to be updated in dsldLinear later
    for (i in 1:length(dsldObj)) {
      model <- glm(dsldObj[[i]]$formula, data = dsldObj[[i]]$data)
    }
    
    # This function gives the necessary info
    glht_result <- glht(model, linfct = mcp(educ = "Tukey"))
    summary_glht <- summary(glht_result)
    
    # Create a dataframe with the results
    pairwise_df <- data.frame(
      Estimate = summary_glht$test$coefficients,
      Std_Error = summary_glht$test$sigma,
      P_Value = summary_glht$test$pvalues
    )
    
    return(pairwise_df)
  } 
  
  # yes interactions 
  else {
    return('Still making this part :)')
  }
}

# run without interactions
# dsldDiffS(lin_model_2)
                   
