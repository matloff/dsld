\name{dsldTakeALookAround}
\alias{dsldTakeALookAround}

\title{dsldTakeALookAround}

\description{

Evaluate feature sets for predicting Y while considering the
Fairness-Utility Tradeoff.
}

\usage{
dsldTakeALookAround(data, yName, sName, maxFeatureSetSize = (ncol(data) - 2), 
    holdout = floor(min(1000,0.1*nrow(data))))
}

\arguments{
    \item{data}{
        Data frame.
    }
    \item{yName}{
        Name of the response variable column. 
    }
    \item{sName}{
        Name of the sensitive attribute column.
    }
    \item{maxFeatureSetSize}{
        Maximum number of combinations of features to be 
        included in the data frame. 
        
    }
    \item{holdout}{
        If not NULL, form a holdout set of the specified size. After fitting to the 
        remaining data, evaluate accuracy on the test set.
    }
}

\details{

This function provides a tool for exploring feature combinations to use
in predicting an outcome Y from features X and a sensitive variable S. 

The features in X will first be considered singly, then doubly and so
on, up though feature combination size \code{maxFeatureSetSize}. Y is
prediction from X either a linear model (numeric Y) or logit
(dichotomous Y).

The accuracy (based on qeML holdout) will be computed for each of these
cases: (a) Y predicted from the given feature combination C, (b) Y
predicted from the given feature combination C plus S, and (c) S predicted
from C. The difference between columns 'a' and 'b' shows the sacrifice
in utility stemming from not using S in our prediction of Y. (Due to
sampling variation, it is possible for column 'b' to be larger than
'a'.) The value in column 'c' shows fairness, the smaller the fairer.

}

\author{
    N. Matloff, A. Ashok, S. Martha, A. Mittal
}

\examples{
\donttest{
# investigate predictive accuracy for a continuous Y,
# 'wageinc', using the default arguments for maxFeatureSetSize = 4
data(svcensus)
dsldTakeALookAround(svcensus, 'wageinc', 'gender', 4)

# investigate the predictive accuracy for a categorical Y, 
# 'educ', using the default arguments for maxFeatureSetSize = 4
dsldTakeALookAround(svcensus, 'educ', 'gender')}
}

\value{Data frame whose first column consists of the variable names,
followed by columns 'a', 'b' and 'c' as described in 'details'.}

