# -------------------------- dsldLinear ------------------------ #
#' ::: Descripton :::
#' @brief This function produces an instance of the `dsldLinear` class (an S3 object) that 
#'      houses a separate instances of the `dsldDiffModel` class for each 
#'      unique, interactive level of the sensitive column specified. In particular dsldLinear fits 
#'      a linear model with the response variable, yName, with or without interactions that depending on user specifications.
#'      The output of the dsldDiff model will store a list of traits such as model, model summary, model coefficients, model data.
#'      1. Model formula as.formula(yName ~.); [character] @ formula
#'      2. Summary Output of model; [character] @ summary
#'      3. Coef of beta parameters; [list] @ coef
#'      4. Data used in the model (useful to see for interactions); [dataframe] @ data
#'      
#' ::: Arguments :::
#' @param data: dataset to model over, will be split according to each level 
#'      in the final outputted `dsldLinModel` object [dataframe]
#' @param yName: name of the predictive column [character]
#' @param sName: name of the sensitive column [character]
#' @param interactions: specifies whether or not to consider interactions; Defaults to TRUE [boolean]
# -------------------------- dsldLinear ------------------------ #

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
      dsldDiffModel <- c(dsldDiffModel, 
                         yName, 
                         sName, 
                         formula,
                         list(summary(diffModel)), 
                         list(coef(diffModel)),
                         list(diffData))
      names(dsldDiffModel) <- c("yName", "sName", "formula", "summary", "coef", "data")
      
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
    dsldDiffModel <- c(dsldDiffModel, 
                       yName, 
                       sName, 
                       formula, 
                       list(summary(diffModel)),
                       list(coef(diffModel)), 
                       list(diffData))
    names(dsldDiffModel) <- c("yName", "sName", "formula", "summary", "coef", "data")
    class(dsldDiffModel) <- "dsldDiffModel"
    
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
#lin1 = dsldLinear(pef,'wageinc','sex', interactions = TRUE); lin1
#lin2 = dsldLinear(pef,'wageinc','occ', interactions = FALSE); lin2
# -------------------------------------------------------------------------------------------

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

# summary(lin1) # test run

coef.dsldLinear <- function(dsld_obj) {
  result <- lapply(dsld_obj, function(x) x$coef)
  return(result)
}

# coef(lin1) # test run

dsldGetData <- function(dsld_obj) {
  result <- lapply(dsld_obj, function(x) x$data)
  return(result)
}

# dsldGetData(lin1) test run


### ------------------------- dsldDiffS function -----------------------------------------

dsldDiffS <- function(dsldObj, new_data = NULL) {
  
  # get sName and yName from the output #
  sName <- dsldObj[[1]]$sName
  yName <- dsldObj[[1]]$yName 
  
  # case with no interactions #
  if (length(dsldObj) == 1) {
    
    # extract [dummary - factor levels] from summary #
    data <- dsldGetData(dsldObj)[[1]]
    model <- glm(as.formula(paste(yName, "~ .")), family = 'gaussian', data = data)
    c <- coef(model)
    C <- vcov(model)
    
    # get factors levels containing s #
    rows_with_race <- grep(sName, rownames(coef(summary(model))))
    regularS <- summary(model)$coefficients[rows_with_race, ]
    
    # for the case when we have two levels in S #
    if (length(levels(data[[sName]])) == 2) {
      estimate <- regularS[1]
      standard_error <- regularS[2]
      sPairs <- combn(levels(data[[sName]]),2)
      a <- sPairs[1]
      b <- sPairs[2]
      index_val = sprintf("%s - %s", a,b)
      df <- data.frame(index_val, estimate, standard_error)
      names(df) <- c("Factors Compared", "Estimates", "Standard Errors")
      return(df)
    }
    
    # extract estimates and standard errors #
    estimates <- regularS[,1]
    standard_errors <- regularS[,2]
    
    # create dataframe #
    df <- data.frame(estimates, standard_errors)
    
    # extract other pairwise combinations #
    feature_names = colnames(vcov(model))
    combination_matrix = combn(feature_names, 2) 
    
    # remove all columns containing "Intercept" #
    drop_value <- "(Intercept)" 
    drop_columns <- apply(combination_matrix, 2, function(col) any(col %in% drop_value)) 
    result <- combination_matrix[, !drop_columns] 
    
    # remove all columns that do not have sName #
    matching_columns <- which(apply(result, 2, function(col) all(grepl(sName, col))))
    result_final <- result[, matching_columns, drop = FALSE]
    
    # loops through each pair #
    for (j in 1:dim(result_final)[2]) {
      
      # create i-th pair of pairwise combinations #
      val <- result_final[,j]                   
      a <- val[1]                                 
      b <- val[2]                                     
      
      # create vector of 0's length of coef(z) #
      vector_length <- length(c)                     
      rt <- rep(0, vector_length) 
      
      # put 1 on the first element #
      a_index <- which(names(c) == a)                
      rt[a_index] <- 1
      
      # put -1 on the second element #
      b_index <- which(names(c) == b)                
      rt[b_index] <- -1
      
      value_a <- c[a_index]
      value_b <- c[b_index]
      
      # get estimates & standard errors # 
      estimates = value_a - value_b
      standard_errors = sqrt((t(rt) %*% C %*% rt))
      temp_df <- data.frame(estimates, standard_errors)
      df <- rbind(df, temp_df)
    }
    
    # get names of sName comparisons #
    sPairs <- combn(levels(data[[sName]]),2)
    test <- c()
    for (i in 1:dim(sPairs)[2]) {
      val <- sPairs[,i]
      a <- val[1]
      b <- val[2]
      index_val = sprintf("%s - %s", a,b)
      test <- c(test, index_val)
    }
    
    # create final data-frame #
    df <- cbind(test, df)
    df <- data.frame(df, row.names = NULL)
    names(df) <- c("Factors Compared", "Estimates", "Standard Errors")
    return(df)
  } 
  
  # case with interactions #
  else {
    if (is.null(new_data)) {
      stop("Please enter the new_data input to compare for interactions")
    }
    return('still working on this')
  }
  return('1')
}

# ---------------------------- Test runs  --------------------------------------
# dat1 <- dsldDiffS(lin1) # run with interactions 
# dat2 <- dsldDiffS(lin2) # run without interactions
# View(dat2)

# dsldConfidenceInterval function
dsldConfidenceInterval <- function(data, confidence_level) {
  # Calculate z-score based on alpha level
  z <- qnorm((1 + confidence_level) / 2)
  
  # Calculate lower and upper bounds for each row
  lower_bound <- data$Estimates - z * data$`Standard Errors`
  upper_bound <- data$Estimates + z * data$`Standard Errors`

  # Create a new dataframe with factor name, lower bound, and upper bound
  result <- data.frame(
    factor_name = data$`Factors Compared`,
    lower_bound = lower_bound,
    upper_bound = upper_bound
  )
  return(result)
}

# --------------------------- Test run -----------------------------------------
# dsldConfidenceInterval(dat2, 0.95) # test run with no interactions 
