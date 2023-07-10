# -------------------------- dsldLinear ---------------------------------------#
#' ::: Descripton :::
#' @brief The dsldLinear function fits a linear model to the response variable, 
#'        yName, using all other available covariates in the user provided dataset. 
#'        The user may select for full interactions across the sensitive variable, 
#'        sName, in which case the function will fit m separate models, where m is 
#'        the number of levels of sName.
#'        
#'        The function produces an instance of the `dsldLinear` class (an S3 object) 
#'        that houses a several instances of the `dsldDiffModel` class for each unique, 
#'        interactive level of the sensitive column specified.
#'       
#'        The output of dsldLinear will store a list of useful traits pertaining the linear model;
#'        the following useful information will be stored: 
#'        1. yName & sName; [character] @ yName, @ sName
#'        2. Summary Output of model; [character] @ summary
#'        3. Coef of beta parameters; [list] @ coef
#'        4. Data used in the model (useful to see for interactions); [dataframe] @ data
#'        
#' ::: Arguments :::
#' @param data: dataset used to train the model [dataframe]
#' @param yName: name of the response column [character]
#' @param sName: name of the sensitive column [character]
#' @param interactions: specifies whether or not to consider interactions; Defaults to TRUE [boolean]
# -------------------------- dsldLinear ---------------------------------------#

dsldLinear <- function(data, yName, sName, interactions = TRUE) {
  
  # create output list to by populated with information #
  dsldModel <- list()
  
  # user selects interactions == TRUE #
  if (interactions == TRUE) {
    
    # split data into list of dataframes by each level of sName #
    dataSplit <- split(data, data[[sName]])
    dataNames <- names(dataSplit)
    
    # loop and populate model for each level in sName #
    for (name in dataNames) {
      
      # initialize instance of dsldDiffModel #
      dsldDiffModel <- list()
      
      # get data for each specific S factor & drop sensitive column #
      diffData <- dataSplit[[name]]
      drop <- c(sName)
      diffData <- diffData[, !(names(diffData) %in% drop)]
      
      # create the model #
      diffModel <- glm(formula = as.formula(paste(yName, "~ .")), family = 'gaussian', data = diffData)
      
      # setup instance of dsldDiffModel #
      dsldDiffModel <- c(dsldDiffModel, yName, sName, list(summary(diffModel)), list(coef(diffModel)), list(diffData))
      names(dsldDiffModel) <- c("yName", "sName", "summary", "coef", "data")
      class(dsldDiffModel) <- "dsldDiffModel"
      
      # add instance into output list: dsldModel #
      dsldModel[[name]] <- dsldDiffModel
    }
    
    # user selects interactions == TRUE #
  } else {
    
    # initialize instance of dsldDiffModel #
    dsldDiffModel <- list()
    
    # create model #
    diffModel <- glm(formula = as.formula(paste(yName, "~ .")), family = 'gaussian', data = data)
    
    # setup instance of dsldDiffModel #
    dsldDiffModel <- c(dsldDiffModel, yName, sName, list(summary(diffModel)), list(coef(diffModel)), list(data))
    names(dsldDiffModel) <- c("yName", "sName", "summary", "coef", "data")
    
    # add instance into dsldModel
    dsldModel[[sName]] <- dsldDiffModel
  }
  
  # finalize dsldModel #
  class(dsldModel) <- "dsldLinear"
  return(dsldModel)
  
}

# -------------------- Test Run dsldLinear ------------------------------------#
#library(qeML)
#pef <- read.csv("~/Desktop/Dsld_Package/pef.csv"); View(pef)
#pef$occ <- as.factor(pef$occ)
#pef$educ <- as.factor(pef$educ)
#pef$gender <- as.factor(pef$gender)
#lin1 = dsldLinear(pef,'wageinc','gender', interactions = TRUE); lin1
#lin2 = dsldLinear(pef,'wageinc','gender', interactions = FALSE); lin2
# -----------------------------------------------------------------------------#

# ------------------- Test Run dsldLinear -------------------------------------#
#load('/Users/adityamittal/Desktop/Year_two/Spring_2023/ECS_189G/hw2/law.school.admissions.rda') # law schools data, predict undergrad gpa
#drop <- c('fulltime', 'bar','cluster')
#law.school.admissions <- law.school.admissions[, !(names(law.school.admissions) %in% drop)]
#View(law.school.admissions) # view data
#lin_1 <- dsldLinear(law.school.admissions,'ugpa','gender', interactions = TRUE); lin_1
#lin_2 <- dsldLinear(law.school.admissions,'ugpa','gender', interactions = FALSE); lin_2
# -----------------------------------------------------------------------------#

# -------------------------------- Auxiliary (Helpful) Functions --------------#

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

#summary(lin1) # test run
#summary(lin2) # test run

coef.dsldLinear <- function(dsld_obj) {
  result <- lapply(dsld_obj, function(x) x$coef)
  return(result)
}

#coef(lin1) # test run
#coef(lin2)

dsldGetData <- function(dsld_obj) {
  result <- lapply(dsld_obj, function(x) x$data)
  return(result)
}

#dsldGetData(lin1)
#dsldGetData(lin2)

#------------------------- dsldDiffS function ---------------------------------#

# this function is helpful for the interactions case to make sure new_data is a valid entry from the user. 
dsldValidateData <- function(new_data, model) {
  
  #  check to see if columns in new_data exist in the original data #
  missing_columns <- setdiff(names(new_data), names(model$model))
  if (length(missing_columns) > 0) {
    stop(paste("Invalid column(s):", paste(missing_columns, collapse = ", ")))
  }
  
  # Check if categorical variables entries are valid #
  categorical_vars <- names(model$model)[sapply(model$model, is.factor)]
  for (i in 1:nrow(new_data)) {
    current_row <- new_data[i, ]
    
    for (var in categorical_vars) {
      levels <- unique(model$model[[var]])
      if (!(current_row[[var]] %in% levels)) {
        stop(paste("Invalid", var, "level in row", i, "."))
      }
    }
  }
  
  # return the data back if everything is good #
  return(new_data)
}

dsldDiffS <- function(dsldObj, new_data = NULL) {
  
  # get sName and yName from the output of dsldLinear #
  sName <- dsldObj[[1]]$sName
  yName <- dsldObj[[1]]$yName
  
  # diffS results when interaction == FALSE in dsldLinear #
  if (length(dsldObj) == 1) {
    
    # we can extract [dummy level in glm output - factor levels] from summary output #
    data <- dsldGetData(dsldObj)[[1]]
    model <- glm(as.formula(paste(yName, "~ .")), family = 'gaussian', data = data)
    C <- vcov(model); c <- coef(model)
    
    # get all values containing sName levels from summary(model) #
    rows_with_race <- grep(sName, rownames(coef(summary(model))))
    regularS <- summary(model)$coefficients[rows_with_race, ]
    
    # for the case when we have only two levels in S; ex: male/female #
    if (length(levels(data[[sName]])) == 2) {
      estimate <- regularS[1]
      standard_error <- regularS[2]
      p_val <- regularS[4]
      sPairs <- combn(levels(data[[sName]]),2)
      a <- sPairs[1]
      b <- sPairs[2]
      index_val = sprintf("%s - %s", b,a)
      df <- data.frame(index_val, estimate, standard_error, p_val)
      names(df) <- c("Factors Compared", "Estimates", "Standard Errors","P-Value")
      return(df)
    }
    
    # extract estimates and standard errors #
    estimates <- regularS[,1]
    standard_errors <- regularS[,2]
    p_val <- regularS[,4]
    
    # create dataframe #
    df <- data.frame(estimates, standard_errors, p_val); df$estimates <- -df$estimates
    
    # extract other pairwise combinations #
    feature_names = colnames(vcov(model))
    combination_matrix = combn(feature_names, 2) 
    
    # remove all columns that do not have sName #
    matching_columns <- which(apply(combination_matrix, 2, function(col) all(grepl(sName, col))))
    result_final <- combination_matrix[, matching_columns, drop = FALSE]
    
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
      
      t_statistic <- (estimates) / standard_errors
      degrees_of_freedom <- nrow(data) - 1 # degrees of freedom
      p_val <- 2 * pt(abs(t_statistic), df = degrees_of_freedom, lower.tail = FALSE)
      
      temp_df <- data.frame(estimates, standard_errors, p_val)
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
    names(df) <- c("Factors Compared", "Estimates", "Standard Errors", "P-Value")
    return(df)
  } 
  
  # diffS results when interactions == TRUE in dsldLinear #
  else {
    
    # raise error if the user doesn't input new data #
    if (is.null(new_data)) {
      stop("Please enter the new_data input to compare for interactions")
    }
    
    # get vector of all levels in sName #
    sNames <- names(dsldObj)
    df = data.frame()
    
    # loop through each level of S name to compute estimates and standard errors #
    for (i in sNames) {
      data = dsldObj[[i]]$data
      model <- glm(as.formula(paste(yName, "~ .")), family = 'gaussian', data = data)
      X_new <- dsldValidateData(new_data, model)
      predictions = predict(model, X_new, type="response", se.fit =TRUE)
      pred <- predictions$fit
      se <- predictions$se.fit
      temp_df <- data.frame(level = i, row = 1:nrow(X_new), prediction = pred, standard_error = se)
      df <- rbind(df, temp_df)
    }

    # compute difference in estimates between each pair factor level for each row #
    unique_elements <- sort(unique(df$row)) 
    pairwise_df = data.frame()
    for (i in unique_elements) {  
      row_dat = subset(df, row == i)
      character_vector <- as.character(row_dat$level)
      combination_matrix = combn(character_vector, 2) 
      for (j in 1:dim(combination_matrix)[2]) {
        val <- combination_matrix[,j]                   
        a <- val[1]                                 
        b <- val[2] 
        a_dat = subset(row_dat, level == a)
        b_dat = subset(row_dat, level == b)
        index_val = sprintf("%s - %s", a,b)
        estimated_difference = a_dat$prediction - b_dat$prediction
        standard_error = sqrt(((a_dat$standard_error)^2) + ((b_dat$standard_error)^2))
        temp_df <- data.frame(index_val, i, estimated_difference, standard_error)
        names(temp_df) <- c("Factors Compared", "Row", "Estimates", "Standard Errors")
        pairwise_df <- rbind(pairwise_df, temp_df)
      }
    }
    return(pairwise_df)
  }
}

# ---------------------------- Test runs  -------------------------------------#
#X_New <- data.frame(age = c(22,45), educ = c("zzzOther",'zzzOther'), occ = c("141","141"),wkswrkd = c(40,40)) # compare genders across different age // early vs late career 
#dat1 <- dsldDiffS(lin1, X_New) # run with interactions 
#View(dat1)

#dat2 <- dsldDiffS(lin2) # no interactions case
#View(dat2)
                                    
# --------------------  dsldConfidenceInterval function -----------------------#
dsldConfidenceInterval <- function(data, confidence_level) {
  # get z value #
  z <- qnorm((1 + confidence_level) / 2)
  
  # Calculate lower and upper bounds for each row #
  lower_bound <- data$Estimates - (z * data$`Standard Errors`)
  upper_bound <- data$Estimates + (z * data$`Standard Errors`)

  # Create a new dataframe with factor name, lower bound, and upper bound #
  result <- data.frame(
    factor_name = data$`Factors Compared`,
    lower_bound = lower_bound,
    upper_bound = upper_bound
  )
  return(result)
}

# dsldConfidenceInterval(dat1,0.95) # test run with interactions 
# dsldConfidenceInterval(dat2,0.95) # test run with no interactions 
