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
                   
