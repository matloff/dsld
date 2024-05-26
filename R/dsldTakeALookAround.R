### -------------------------- dsldTakeALookAround ---------------------------
dsldTakeALookAround <- function(data, yName, sName,
                                maxFeatureSetSize = (ncol(data) - 2),
                                holdout = floor(min(1000, 0.1 * nrow(data)))) {
    # load libraries
    getSuggestedLib("qeML")

    # args checking #
    if (maxFeatureSetSize > (ncol(data) - 2)) {
        stop("maxFeatureSetSize too large!")    # error on invalid size
    }
  
    if (!is.data.frame(data)) {
        stop("data must be a dataframe or equivalent")       # error on types
    }
    
    # subset dataset to remove sName and yName
    max_features_data <- data[, !names(data) %in% c(yName, sName)]

    # get names of feature set 
    feature_names <- colnames(max_features_data)
    
    # initialize empty vectors to populate with test accuracy scores
    col_names <- c()
    MSE_Y <- c()
    MSE_YS <- c()
    MSE_S <- c()
    
    # run for loop to get all possible combinations of features up to maxFeatureSetSize
    for (i in 1:maxFeatureSetSize) { 
        # create combination matrix containing i-features
        combination_matrix <- combn(feature_names, i)
        
        # run second for loop across each column of the combination_matrix
        for (j in 1:dim(combination_matrix)[2]) {
            # create vector of feature set names across each run - compute 1.
            current_features <- combination_matrix[,j]                     # get feature names on the jth loop
            names <- toString(current_features)                            # convert to string
            names <- gsub(" ","",names)                                    # remove spaces between the characters
            col_names <- c(col_names, names)                               # append feature names string into vector 
            
            # create dataframes to compute test accuracies 
            feature_data_Y <- data[,c(current_features, yName)]            # dataframe with feature set and Y 
            feature_data_Y_S <- data[,c(current_features, yName,sName)]    # dataframe with feature set, S and Y
            feature_data_S <- data[,c(current_features, sName)]            # dataframe with feature set and S
            
            # get part 2. and 3.
            # check whether Y is continuous
            if (is.numeric(data[[yName]])) {
                a <- qeLin(feature_data_Y, yName, holdout)$testAcc          # get prediction accuracy for Y of this feature set
                MSE_Y <- c(MSE_Y, a)                                        # append test accuracy into vector 
                
                b <- qeLin(feature_data_Y_S, yName, holdout)$testAcc        # get prediction accuracy for Y of the feature set PLUS s
                MSE_YS <- c(MSE_YS, b)
            } 
            # Y is discrete
            else { 
                a <- qeLogit(feature_data_Y, yName, holdout)$testAcc        # get prediction accuracy for Y of this feature set
                MSE_Y <- c(MSE_Y, a)
                
                b <- qeLogit(feature_data_Y_S, yName, holdout)$testAcc      # get prediction accuracy for Y of this feature set PLUS s
                MSE_YS <- c(MSE_YS, b)
            }
            
            # get 4.
            # check whether sName is continuous
            if (is.numeric(data[[sName]])) {
                c <- qeLin(feature_data_S, sName, holdout)$testAcc          # get prediction accuracy of S from the feature set
                MSE_S <- c(MSE_S, c)
            } 
            # if sName is discrete
            else {
                c <- qeLogit(feature_data_S, sName, holdout)$testAcc        # get prediction accuracy of S from the feature set
                MSE_S <- c(MSE_S, c)
            }
        }
    }
    
    # create dataframe
    df <- data.frame(col_names, MSE_Y, MSE_YS, MSE_S)
    colnames(df)[1] <- "Feature Names"
    colnames(df)[2] <- "a"
    colnames(df)[3] <- "b"
    colnames(df)[4] <- "c"
    return(df)
}

# Test runs
# Example 1: We investigate the predictive accuracy for a continuous Y,'wageinc', using the default arguments for maxFeatureSetSize = 4
# data(svcensus)
# dsldTakeALookAround(svcensus, 'wageinc', 'gender', 4)

# Example 2:  We investigate the predictive accuracy for a categorical Y, 'educ', using the default arguments for maxFeatureSetSize = 4
# data(svcensus)
# dsldTakeALookAround(svcensus, 'educ', 'occ')

# Example 3:  We investigate the predictive accuracy for a continuous Y, 'wageinc', using the maxFeatureSetSize = 1
# data(svcensus)
# dsldTakeALookAround(svcensus, 'wageinc', 'gender', 1)
