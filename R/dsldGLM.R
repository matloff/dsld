# Function to be written, we're currently brainstorming things user can use.

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


dsldLin <- function(data, yName, sName, xName) {
  return(NULL)
}

