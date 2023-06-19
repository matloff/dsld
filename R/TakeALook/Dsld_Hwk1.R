### Homework 1  - Under Construction

library(qeML) # Load necessary libraries
library(Kendall)
library(docstring)

takeALookAround <- function(data,yName,sName, maxFeatureSetSize = (ncol(data) - 2)) {
  #' This function returns a dataframe containing 4 columns:
  #' 1. Character string consisting of the names of the given feature set. 
  #' 2. Prediction accuracy for yName of this feature set. 
  #' 3. Prediction accuracy for yName of this feature set plus sName. 
  #' 4. Prediction accuracy for sName of this feature set. 
  #' yName and sName are the names of columns in the data; default argument for maxFeatureSetSize = (ncol(data) - 2).
  
  # Send error message if user enters invalid maxFeatureSetSize
  if (maxFeatureSetSize > (ncol(data) - 2)) { 
    stop("maxFeatureSetSize too large!")
  }
  
  max_features_data = data[,!names(data) %in% c(yName, sName)] # subset dataset to remove sName and yName
  feature_names = colnames(max_features_data) # get feature set from the column names of our subsetted data
  
  # initialize empty vectors to populate with data
  col_names = c() 
  MSE_Y = c()
  MSE_YS = c()
  MSE_S = c()
  
  for (i in 1:maxFeatureSetSize) { # run for loop to get all possible combinations of features up to maxFeatureSetSize
    combination_matrix = combn(feature_names, i) # create combination matrix containing i-features
    
    for (j in 1:dim(combination_matrix)[2]) { # run for loop to get test accuracies of features across all columns of combination matrix
      current_features = combination_matrix[,j] # get feature names on the jth loop
      names = toString(current_features) # convert to string
      names = gsub(" ","",names) # remove spaces between the characters
      col_names = c(col_names, names) # append feature names string into vector 
      
      feature_data_Y = data[,c(current_features, yName)] # create dataframe with current feature set and Y 
      feature_data_Y_S = data[,c(current_features, yName,sName)] # create dataframe with current feature set, S and Y
      feature_data_S = data[,c(current_features, sName)] # create dataframe with current feature set and S
      
      # get part (a) and (b)
      if (is.numeric(data[[yName]])) { # check whether Y is continuous 
        a = qeLin(feature_data_Y,yName)$testAcc # get prediction accuracy for Y of this feature set
        MSE_Y = c(MSE_Y, a) # append test accuracy into vector 
        
        b = qeLin(feature_data_Y_S,yName)$testAcc # get prediction accuracy for Y of the feature set PLUS s
        MSE_YS = c(MSE_YS, b)
      } else { # Y is discrete
        a = qeLogit(feature_data_Y,yName)$testAcc # get prediction accuracy for Y of this feature set
        MSE_Y = c(MSE_Y, a)
        
        b = qeLogit(feature_data_Y_S,yName)$testAcc # get prediction accuracy for Y of this feature set PLUS s
        MSE_YS = c(MSE_YS, b)
      }
      
      # get part(c)
      if (is.numeric(data[[sName]])) { # check whether sName is continuous 
        c = qeLin(feature_data_S,sName)$testAcc # get prediction accuracy of S from the feature set
        MSE_S = c(MSE_S, c)
      } else { # if sName is discrete
        c = qeLogit(feature_data_S,sName)$testAcc # get prediction accuracy of S from the feature set
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

# run the code
data(pef)
takeALookAround(pef, 'wageinc','sex',4) # run w maxFeatureSetSize to get all possible combinations
takeALookAround(pef, 'wageinc','sex')
takeALookAround(pef, 'wageinc','sex',1) # run w smaller maxFeatureSetSize
takeALookAround(pef, 'wageinc','sex',5) # returns error if maxFeatureSetSize too large
takeALookAround(pef, 'educ','occ',4) # with factor Y