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
        A positive integer smaller than the sample size. The same holdout rows
        are used for every feature set and outcome comparison. If NULL,
        report in-sample error instead.
    }
}

\details{

This function provides a tool for exploring feature combinations to use
in predicting an outcome Y from features X and a sensitive variable S.

The features in X will first be considered singly, then doubly and so
on, up though feature combination size \code{maxFeatureSetSize}. Y is
prediction from X either a linear model (numeric Y) or logit
(dichotomous Y).

The prediction error (using a shared holdout) will be computed for each of these
cases: (a) Y predicted from the given feature combination C, (b) Y
predicted from the given feature combination C plus S, and (c) S predicted
from C. The difference between columns 'a' and 'b' shows the sacrifice
in utility stemming from not using S in our prediction of Y. (Due to
sampling variation, it is possible for column 'b' to be larger than
'a'.) Column 'c' measures how predictable S is from C: lower error means
S is easier to predict and may indicate stronger proxies. It is not a
fairness score in which smaller is better.

}

\author{
    N. Matloff, A. Ashok, S. Martha, A. Mittal
}

\examples{
\donttest{
# investigate predictive accuracy for a continuous Y,
# 'wageinc', using the default arguments for maxFeatureSetSize = 4
data(svcensus)
set.seed(2026)
svcensus <- svcensus[sample.int(nrow(svcensus), 1000), ]
dsldTakeALookAround(svcensus, 'wageinc', 'gender', 4)

# investigate the predictive accuracy for a categorical Y,
# 'educ', using the default arguments for maxFeatureSetSize = 4
dsldTakeALookAround(svcensus, 'educ', 'gender')}
}

\value{Data frame whose first column consists of the variable names,
followed by columns 'a', 'b' and 'c' as described in 'details'.}

