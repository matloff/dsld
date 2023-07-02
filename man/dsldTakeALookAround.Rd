\name{dsldTakeALookAround}
\alias{dsldTakeALookAround}

\title{DSLDTakeALookAround}

\description{
    The dsldTakeALookAround function is an exploratory tool for users to see which features in a dataset are 
    good predictors of Y, the response variable, but may also be highly correlated with S, the sensitive variable.  

    The return value will be a data frame with 4 columns: 
    1. The first will be a character string consisting of the names of the given feature set. 
    2. Prediction accuracy for Y of this feature set. 
    3. Prediction accuracy for Y of this feature set plus S. (i.e. how much are we giving up by NOT using S?)
    4. Prediction accuracy for S of this feature set. (the better the accuracy, the less fairness value there is in omitting S from prediction model)
    There will be one row in the d.f. for each feature set. All possible feature sets of size up to maxFeatureSetSize will be tested.

    The utility of this function comes from the following use-case: 
    For instance, we wish to predict Y from a feature vector X, where X = (X1,...Xp) which includes a sensitive variable 
    S such as race or gender. The main objective is to eliminate the effect of S on our predictions. One might naively think 
    that we can achieve this goal by simply removing S from X. However, this typically does not work, since there may be other 
    variables in X that are highly correlated with S. In such a situation, S still will have an effect on our predictions. The 
    function dsldTakeALookAround will help users explore the predictive accuracy of different combinations feature sets (up to maxFeatureSetSize)
    for Y and also see its correlation with the sensitive variable S. 

    This function uses the qeML package to compute the respective predictive test accuracies. More specifically, it uses the qeLin() for numeric Y or 
    S, qeLogit() for the categorical case. 
}

\usage{
    dsldTakeALookAround <- function(data,yName,sName, maxFeatureSetSize = (ncol(data) - 2))
}

\arguments{
    \item{data}{dataset in dataframe form}
    \item{yName}{as in qeML functions, the response variable}
    \item{sName}{name of the sensitive variable, an R factor}
    \item{maxFeatureSetSize}{maximum number of combinations of features to be included in the dataframe; 
    default argument set as: maxFeatureSetSize = (ncol(data) - 2)}
}

\author{
    N. Matloff, A. Ashok, S. Martha, A. Mittal
}

\examples{
#   The dsldTakeALookAround function aims to provide a seamless experience, requiring minimal effort to utilize its
#   capabilities. Once the package is successfully downloaded and installed, the function can be leveraged by simply 
#   employing similar commands shown in the examples.
    
#   The examples are conducted using the 'pef' dataset. 

#   Example 1: We investigate the predictive accuracy for a continuous Y,'wageinc', using the default arguments for maxFeatureSetSize = 4
    data(pef)
    dsldTakeALookAround(pef, 'wageinc', 'gender', 4)

#   Example 2:  We investigate the predictive accuracy for a categorical Y, 'educ', using the default arguments for maxFeatureSetSize = 4
    data(pef)
    dsldTakeALookAround(pef, 'educ', 'occ')

#   Example 3:  We investigate the predictive accuracy for a continuous Y, 'wageinc', using the maxFeatureSetSize = 1
    data(pef)
    dsldTakeALookAround(pef, 'wageinc', 'gender', 1)
}

