\name{dsldQeFairRidgeLog}
\alias{dsldQeFairRidgeLog}

\title{dsldQeFairRidgeLog}

\description{ 
  Fair Logistic Ridge Regression
  Only works with binary classifications
}

\usage{
  dsldQeFairRidgeLog(data,yName,sNames,deweightPars=NULL,
                     yesYVal=levels(data[,yName])[2],
                     holdout=floor(min(1000,0.1*nrow(data))))
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
    \item{yesYVal}{
        Y value to be considered "yes," to be coded 1 rather than 0. By default
        this is the 2nd factor level.
    }
    \item{holdout}{
        How many rows to use as the holdout/testing set. Can be NULL.
        The testing set is used to calculate s correlation and test accuracy.
    } 
}

\author{
    N. Matloff, S. Martha, A. Mittal, B. Ouattara, B. Zarate, J. Tran
}

\details{
    The algorithm used to perform ridge regression is described in the paper
    described in Matloff's EDFfair package.
    testAcc is in mean absolute prediction error. corrs is the correlation
    on every level of S.
}

\examples{
  log <- dsld::dsldQeFairRidgeLog(fairml::compas, "two_year_recid", "race", 
      list(decile_score=1))
  predict(log, fairml::compas[1,])
}
