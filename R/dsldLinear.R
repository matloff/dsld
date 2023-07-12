# -------------------------- dsldLinear ---------------------------------------#
#' ::: Descripton :::
#' @brief The dsldLinear function fits a linear model to the response variable,
#'      yName, using all other available covariates in the user provided
#'      dataset. The user may select for full interactions across the sensitive
#'      variable, sName, in which case the function will fit m separate models,
#'      where m is the number of levels of sName.
#'
#'      The function produces an instance of the `dsldLinear` class (an S3
#'      object).
#'
#'      The output of dsldLinear will store a list of useful traits pertaining
#'      the linear model; the following useful information will be stored:
#'          1. yName & sName; [character] @ yName, @ sName
#'          2. Model; [character] @ model
#'          3. (Full Interactions only) New data input by user;
#'             [dataframe] @ data
#'          3. Summary Output of model; [character] @ summary
#'          4. Coef of beta parameters; [character] @ coef
#'          5. Data used in the model (useful to see for interactions);
#'             [dataframe] @ data
#'
#' ::: Arguments :::
#' @param data: dataset used to train the model [dataframe]
#' @param yName: name of the response column [character]
#' @param sName: name of the sensitive column [character]
#' @param interactions: specifies whether or not to consider interactions;
#'      Defaults to FALSE [boolean]
#' @param new_data: new test cases to compute Y | X ; REQUIRED when
#'      interactions = TRUE [dataframe]
#' 
# -------------------------- dsldLinear ---------------------------------------#
dsldLinear <- function(data, yName, sName, interactions = FALSE, new_data = NULL) {
  # create final output list to by populated with results #
  dsldModel <- list()

  # user selects interactions == TRUE #
  if (interactions == TRUE) {
    
    # raise error if user doesn't input new_data # 
    if (is.null(new_data)) {
      stop("Please enter the new_data input to compare for interactions in summary()")
    }
    
    # split data into list of dataframes by each level of sName #
    dataSplit <- split(data, data[[sName]])
    dataNames <- names(dataSplit)
    
    # loop and create model for each level in sName #
    for (name in dataNames) {
      
      # initialize instance of dsldDiffModel #
      dsldDiffModel <- list()
      
      # get data for each specific S factor & drop sensitive column #
      diffData <- dataSplit[[name]]
      drop <- c(sName)
      diffData <- diffData[, !(names(diffData) %in% drop)]
      
      # create the model #
      diffModel <- glm(formula = as.formula(paste(yName, "~ .")), family = 'gaussian', data = diffData)
      
      # setup individual instance of dsldDiffModel #
      dsldDiffModel <- c(dsldDiffModel, yName, sName, list(diffModel), list(new_data), list(summary(diffModel)), list(coef(diffModel)), list(diffData))
      names(dsldDiffModel) <- c("yName", "sName", "model", "new_data", "summary", "coef", "data")
      
      class(dsldDiffModel) <- "dsldDiffModel"
      
      # add instance into output list: dsldModel #
      dsldModel[[name]] <- dsldDiffModel
    }
    
    # user selects interactions == FALSE #
  } else {
    
    # initialize instance of dsldDiffModel #
    dsldDiffModel <- list()
    
    # create model #
    diffModel <- glm(formula = as.formula(paste(yName, "~ .")), family = 'gaussian', data = data)
    
    # setup instance of dsldDiffModel #
    dsldDiffModel <- c(dsldDiffModel, yName, sName, list(diffModel), list(summary(diffModel)), list(coef(diffModel)), list(data))
    names(dsldDiffModel) <- c("yName", "sName", "model", "summary", "coef", "data")
    
    # add instance into dsldModel
    dsldModel[[sName]] <- dsldDiffModel
  }
  
  # finalize dsldModel #
  class(dsldModel) <- "dsldLinear"
  return(dsldModel)
  
}

# -------------------- Test Run dsldLinear ------------------------------------#
#library(qeML)
#pef <- read.csv("~/Desktop/Dsld_Package/pef.csv")
#pef$occ <- as.factor(pef$occ)
#pef$educ <- as.factor(pef$educ)
#pef$gender <- as.factor(pef$gender)
#new_data <- data.frame(age = c(18,60), educ = c("zzzOther",'zzzOther'),wkswrkd = c(50,50), occ = c("106","106"))       # compare genders across different age // early vs late career 
#lin1 = dsldLinear(pef,'wageinc','gender', interactions = TRUE, new_data); lin1
#lin2 = dsldLinear(pef,'wageinc','gender', interactions = FALSE); lin2
# -----------------------------------------------------------------------------#

# ------------------- Test Run dsldLinear -------------------------------------#
#load('/Users/adityamittal/Desktop/Year_two/Spring_2023/ECS_189G/hw2/law.school.admissions.rda')                        # law schools data, predict undergrad gpa
#drop <- c('fulltime', 'bar','cluster')
#law.school.admissions <- law.school.admissions[, !(names(law.school.admissions) %in% drop)]
#lin_1 <- dsldLinear(law.school.admissions,'ugpa','gender', interactions = TRUE)                                        # will raise error since we haven't specified new data
#lin_2 <- dsldLinear(law.school.admissions,'ugpa','gender', interactions = FALSE)
# -----------------------------------------------------------------------------#


# ----------------------- Auxiliary Functions ---------------------------------#

#' ::: Description ::
#' @brief coef() is a polymorphic method that takes in an object of the 'dsldLinear' 
#'      class. The function provides m regression coefficients of the model, where m 
#'      is the number of levels of sName.
#' 
#' ::: Arguments :::
#' @param dsld_obj: an instance of the dsldLinearModel s3 object.

coef.dsldLinear <- function(dsld_obj) {
  result <- lapply(dsld_obj, function(x) x$coef)
  return(result)
}

#coef(lin1) # test run
#coef(lin2)

#' ::: Description ::
#' @brief the dsldGetData() function takes in an object of the 'dsldLinear' class. 
#'      The function provides m dataset(s) used to train the linear model, where m 
#'      is the number of levels of sName.
#' 
#' ::: Arguments :::
#' @param dsld_obj: an instance of the dsldLinearModel s3 object. 

dsldGetData <- function(dsld_obj) {
  result <- lapply(dsld_obj, function(x) x$data)
  return(result)
}

#dsldGetData(lin1)
#dsldGetData(lin2)

#------------------------- dsldDiffS function ---------------------------------#

#' ::: Description ::
#' @brief dsldValidateData() is an indirect helper function for dsldDiffS() 
#'      full-interactions case. The function takes in the new_data argument from 
#'      dsldDiffS() and validates if the user has entered appropriate entries for 
#'      new_data. 
#' 
#' ::: Arguments :::
#' @param new_data: user inputted data for incoming new test cases.
#' @param model: linear model fitted by output of dsldLinear().

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

#' ::: Description ::
#' @brief The dsldDiffS() function helps users quantify possible evidence of discrimination 
#'      between S levels. For the no-interactions case, dsldDiffS compares differences in regression 
#'      coefficients between each pairs of S levels. For the full-interactions case, dsldDiffS 
#'      now requires an argument, in data-frame form, of new test cases where difference in mean Y at 
#'      that X value will be compared between each pair of S levels.
#'
#'      For no-interaction case, dsldDiffS returns a data frame with 4 columns:
#'      1. Pairs of S level names
#'      2. Estimates of the differences
#'      3. Associated standard errors
#'      4. P-values
#'      There will be one row for each pair of S levels.
#'
#'      For full-interactions case, dsldDiffs returns a data frame with 3 columns:
#'      1. Col. number of diffs argument
#'      2. Estimate of the difference in mean Y at that X value
#'      3. Associated std. err.
#'      There will be one row for each pair of S levels.
#' 
#' ::: Arguments :::
#' @param dsldObj: output from dsldLinear() function 
#' @param new_data: new test cases to be provided; required for full-interactions case 
#'
dsldDiffS <- function(dsldObj, new_data = NULL) {
  # get sName and yName from the output of dsldLinear #
  sName <- dsldObj[[1]]$sName
  yName <- dsldObj[[1]]$yName
  
  # diffS results when interaction == FALSE in dsldLinear #
  if (length(dsldObj) == 1) {
    
    # extract pairwise combination of [dummy level in glm - factor levels] from summary output #
    data <- dsldGetData(dsldObj)[[1]]
    model <- dsldObj[[1]]$model
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
    
    # extract other pairwise combinations of levels (not including dummy) #
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
    df <- data.frame()
    
    # loop through each level of S name to compute estimates and standard errors #
    for (i in sNames) {
      data = dsldObj[[i]]$data
      model <- dsldObj[[i]]$model
      X_new <- dsldValidateData(new_data, model)
      predictions = predict(model, X_new, type="response", se.fit =TRUE)
      pred <- predictions$fit
      se <- predictions$se.fit
      temp_df <- data.frame(level = i, row = 1:nrow(X_new), prediction = pred, standard_error = se)
      df <- rbind(df, temp_df)
    }
    
    # compute difference in estimates between each pair factor level for each row #
    unique_elements <- sort(unique(df$row)) 
    pairwise_df <- data.frame()
    for (i in unique_elements) {  
      row_dat <- subset(df, row == i)
      character_vector <- as.character(row_dat$level)
      combination_matrix <- combn(character_vector, 2) 
      for (j in 1:dim(combination_matrix)[2]) {
        val <- combination_matrix[,j]                   
        a <- val[1]                                 
        b <- val[2] 
        a_dat <- subset(row_dat, level == a)
        b_dat <- subset(row_dat, level == b)
        index_val <- sprintf("%s - %s", a,b)
        estimated_difference <- a_dat$prediction - b_dat$prediction
        standard_error <- sqrt(((a_dat$standard_error)^2) + ((b_dat$standard_error)^2))
        temp_df <- data.frame(index_val, i, estimated_difference, standard_error)
        names(temp_df) <- c("Factors Compared", "Row", "Estimates", "Standard Errors")
        pairwise_df <- rbind(pairwise_df, temp_df)
      }
    }
    return(pairwise_df)
  }
}

# ---------------------------- Test runs  -------------------------------------#
# educ_data <- data.frame(age = c(18,60), educ = c("16",'16'),wkswrkd = c(50,50), occ = c("106","106")) # compare genders across different age // early vs late career 
# dat1 <- dsldDiffS(lin1, educ_data) # run with interactions 
# View(dat1)

# dat2 <- dsldDiffS(lin2) # no interactions case
# View(dat2)
# -----------------------------------------------------------------------------#

#' ::: Description ::
#' @brief summary() is a polymorphic method that takes in an object of the 'dsldLinear' 
#'      class. The function provides m summaries of the model, where m is the number 
#'      of levels of sName. Additionally, the summary function also report differences 
#'      across S levels. 
#' 
#' ::: Arguments :::
#' @param dsld_obj: an instance of the dsldLinearModel s3 object that output summary objects.

summary.dsldLinear <- function(dsldObj) {
  
  diffS <- list() 
  
  # get sName and yName from the output of dsldLinear #
  sName <- dsldObj[[1]]$sName
  yName <- dsldObj[[1]]$yName
  
  if (length(dsldObj) == 1) {
    
    data <- dsldGetData(dsldObj)[[1]]
    summary_output <- summary(dsldObj[[1]]$model)
    coef <- summary_output$coefficients[,1 ]
    std_err <- summary_output$coefficients[,2]
    p_values <- summary_output$coefficients[,4]
    
    # Create dataframe
    df <- data.frame(
      Covariate = row.names(summary_output$coefficients),
      Estimate = coef,
      `Standard Error` = std_err,
      PValue = p_values,
      stringsAsFactors = FALSE, 
      row.names = NULL
    )
    
    diffS[['Summary Coefficients']] <- df
    diffS[['Sensitive Factor Level Comparisons']] <- dsldDiffS(lin2)
    return(diffS)
    
  } else {
    
    sNames <- names(dsldObj)
    new_data <- dsldObj[[1]]$new_data
    
    # loop through each level of S name to compute estimates and standard errors #
    for (i in sNames) {
      data = dsldObj[[i]]$data
      summary_output <- summary(dsldObj[[i]]$model)
      coef <- summary_output$coefficients[,1 ]
      std_err <- summary_output$coefficients[,2]
      p_values <- summary_output$coefficients[,4]
      
      df <- data.frame(
        Covariate = row.names(summary_output$coefficients),
        Estimate = coef,
        `Standard Error` = std_err,
        PValue = p_values,
        stringsAsFactors = FALSE, 
        row.names = NULL
      )
      
      diffS[[i]] <- df
    }
    diffS[['Sensitive Factor Level Comparisons']] <- dsldDiffS(lin1, new_data)
    return(diffS)
  }
}

# interactions_summary <- summary(lin2); interactions_summary
# no_interactions_summary <- summary(lin1); no_interactions_summary
