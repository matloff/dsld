\name{dsldQeFairRidgeLog}
\alias{dsldQeFairRidgeLog}

\title{dsldQeFairRidgeLog}

\description{ 
  Fair Logistic Ridge Regression. Intended for binary classification cases.
}

\usage{
  dsldQeFairRidgeLog(data, yName, sNames, deweightPars = NULL, holdout =
                   floor(min(1000, 0.1 * nrow(data))), yesYVal =
                   levels(data[, yName])[2])
}

\arguments{
    \item{data}{
        Data frame
    }
    \item{yName}{
        A string representing a binary factor reponse column
    }
    \item{sNames}{
        A vector or single string, representing the sensitive column(s)
    }
    \item{deweightPars}{
        E.g list(age=0.2, gender=0.5), used to deweight proxy variables. 
        Higher values mean more deweighting.
    }
    \item{holdout}{
        How many rows to use as the holdout/testing set. Can be NULL.
        The testing set is used to calculate s correlation and test accuracy.
    } 
    \item{yesYVal}{
        Y value to be considered "yes," to be coded 1 rather than 0. By default
        this is the 2nd factor level.
    }
}

\author{
    N. Matloff, S. Martha, A. Mittal, B. Ouattara, B. Zarate, J. Tran
}

\details{
    The algorithm used to perform ridge regression is described in the paper
    described in Matloff's EDFfair package. Test Accuracy is the misclassification rate.  
    Corrs is the correlation between predicted Y across every level of S.
}

\examples{
  data(compas1) # load dataset

  # deweight "race" column
  log <- dsldQeFairRidgeLog(compas1, "two_year_recid", "race", 
                          list(decile_score=0.1), yesYVal = "Yes")

  log$testAcc # accuracy on holdout set

  log$corrs # compute correlation between predicted Y and sensitive variables

  predict(log, compas1[1,]) # generate predictions for new cases
}
