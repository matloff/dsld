#' ::: Description :::
#' @brief This function returns a dataframe containing 4 columns:
#' 1. Character string consisting of the names of the given feature set. 
#' 2. Prediction accuracy for yName of this feature set. 
#' 3. Prediction accuracy for yName of this feature set plus sName. 
#' 4. Prediction accuracy for sName of this feature set.
#'
#' ::: Arguments :::
#' @param data: dataset given in dataframe format
#' @param yName: as in qeML functions, response variable
#' @param sName: name of the sensitive variable, an R factor
#' @param maxFeatureSetSize: maximum number of combinations of features to be included; default argument set as: maxFeatureSetSize = (ncol(data) - 2)

dsldTakeALookAround <- function(data,yName,sName, maxFeatureSetSize = (ncol(data) - 2)) {
  
  # args checking #
  if (maxFeatureSetSize > (ncol(data) - 2)) { 
    stop("maxFeatureSetSize too large!")                            # Send error message if user enters invalid maxFeatureSetSize
  }
  if (!is.data.frame(data)) {
    stop("data must be a dataframe.")
  }
  
  # load libraries #
  library(qeML)
  if (!require('Kendall')) install.packages('Kendall'); library('Kendall')
  
  # subset dataset to remove sName and yName
  max_features_data = data[,!names(data) %in% c(yName, sName)]

  # get names of feature set 
  feature_names = colnames(max_features_data) 
  
  # initialize empty vectors to populate with test accuracy scores
  col_names = c() 
  MSE_Y = c()
  MSE_YS = c()
  MSE_S = c()
  
  # run for loop to get all possible combinations of features up to maxFeatureSetSize
  for (i in 1:maxFeatureSetSize) { 
    combination_matrix = combn(feature_names, i)                    # create combination matrix containing i-features
    
    # run second for loop across each column of the combination_matrix
    for (j in 1:dim(combination_matrix)[2]) {       
      
      # create vector of feature set names across each run - compute 1.
      current_features = combination_matrix[,j]                     # get feature names on the jth loop
      names = toString(current_features)                            # convert to string
      names = gsub(" ","",names)                                    # remove spaces between the characters
      col_names = c(col_names, names)                               # append feature names string into vector 
      
      # create dataframes to compute test accuracies 
      feature_data_Y = data[,c(current_features, yName)]            #dataframe with feature set and Y 
      feature_data_Y_S = data[,c(current_features, yName,sName)]    # dataframe with feature set, S and Y
      feature_data_S = data[,c(current_features, sName)]            # dataframe with feature set and S
      
      # get part 2. and 3.
      # check whether Y is continuous 
      if (is.numeric(data[[yName]])) {                  
        a = qeLin(feature_data_Y,yName)$testAcc                     # get prediction accuracy for Y of this feature set
        MSE_Y = c(MSE_Y, a)                                         # append test accuracy into vector 
        
        b = qeLin(feature_data_Y_S,yName)$testAcc                   # get prediction accuracy for Y of the feature set PLUS s
        MSE_YS = c(MSE_YS, b)
      } 
      # Y is discrete
      else { 
        a = qeLogit(feature_data_Y,yName)$testAcc                   # get prediction accuracy for Y of this feature set
        MSE_Y = c(MSE_Y, a)
        
        b = qeLogit(feature_data_Y_S,yName)$testAcc                 # get prediction accuracy for Y of this feature set PLUS s
        MSE_YS = c(MSE_YS, b)
      }
      
      # get 4.
      # check whether sName is continuous
      if (is.numeric(data[[sName]])) {                        
        c = qeLin(feature_data_S,sName)$testAcc                     # get prediction accuracy of S from the feature set
        MSE_S = c(MSE_S, c)
      } 
      # if sName is discrete
      else { 
        c = qeLogit(feature_data_S,sName)$testAcc                   # get prediction accuracy of S from the feature set
        MSE_S = c(MSE_S, c)
      }
    }
  }
  
  # create dataframe
  df <- data.frame(col_names, MSE_Y, MSE_YS, MSE_S) 
  colnames(df)[1] = "Feature Names"
  colnames(df)[2] = "a"
  colnames(df)[3] = "b"
  colnames(df)[4] = "c"
  return(df)
}



