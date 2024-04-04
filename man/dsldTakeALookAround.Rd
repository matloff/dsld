\name{dsldTakeALookAround}
\alias{dsldTakeALookAround}

\title{dsldTakeALookAround}

\description{
    Evaluates feature sets for predicting Y while considering correlation with sensitive variable S.
}

\usage{
dsldTakeALookAround(data, yName, sName, maxFeatureSetSize = (ncol(data) - 2), 
holdout = floor(min(1000,0.1*nrow(data))))
}

\arguments{
    \item{data}{
        Data frame comprising the input data.
    }
    \item{yName}{
        Name of the response variable column. 
    }
    \item{sName}{
        Name of the sensitive attribute column.
    }
    \item{maxFeatureSetSize}{
        Maximum number of combinations of features to be included in the dataframe; 
        default argument set as: maxFeatureSetSize = (ncol(data) - 2).
    }
    \item{holdout}{
        If not NULL, form a holdout set of the specified size. After fitting to the 
        remaining data, evaluate accuracy on the test set.
    }
}

\details{
    This function provides an exploratory tool to see which features in a dataset are 
    good predictors of Y, the response variable, but may also be highly correlated with 
    S, the sensitive variable. 

    The return value will be a data frame with 4 columns: \cr
    1. The first will be a character string consisting of the names of the given 
       feature set. \cr
    2. Prediction accuracy for Y of this feature set. 
    3. Prediction accuracy for Y of this feature set plus S. (i.e. how much are
       we giving up by NOT using S?) \cr
    4. Prediction accuracy for S of this feature set. (the better the accuracy, 
       the less fairness value there is in omitting S from prediction model). \cr
       There will be one row in the d.f. for each feature set. All possible 
       feature sets of size up to maxFeatureSetSize will be tested.\cr

    This function uses the qeML package to compute the respective predictive 
    test accuracies. More specifically, it uses the qeLin() for numeric Y or S, 
    qeLogit() for the categorical case. 
}

\author{
    N. Matloff, A. Ashok, S. Martha, A. Mittal
}

\examples{
    \donttest{
    # Example 1: investigate predictive accuracy for a continuous Y,
    # 'wageinc', using the default arguments for maxFeatureSetSize = 4
    data(svcensus)
    dsldTakeALookAround(svcensus, 'wageinc', 'gender', 4)

    # Example 2:  We investigate the predictive accuracy for a categorical Y, 
    # 'educ', using the default arguments for maxFeatureSetSize = 4
    dsldTakeALookAround(svcensus, 'educ', 'gender')
    }
}
