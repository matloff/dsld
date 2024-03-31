\name{dsldQeFairKNN}
\alias{dsldQeFairKNN}

\title{DSLDQeFairKNN}

\description{ 
  Fair K-Nearest Neighbors.
}

\usage{
  dsldQeFairKNN(data, yName, sNames, deweightPars=NULL, 
                yesYVal=NULL,k=25,scaleX=TRUE,
                holdout=floor(min(1000,0.1*nrow(data))))
}

\arguments{
    \item{data}{
        Data frame
    }
    \item{yName}{
        A string representing a reponse column
    }
    \item{sNames}{
        A vector or single string, representing the sensitive column(s)
    }
    \item{deweightPars}{
        Values for de-emphasizing variables in a split, e.g. 
        'list(age=0.2,gender=0.5)'. Lower values means more deweighting.
    }
    \item{scaleX}{
        Scale the features. Defaults to TRUE.
    }
    \item{yesYVal}{
        Y value to be considered "yes," in the binary case;
        to be coded 1 rather than 0.
    }
    \item{k}{
        Number of nearest neighbors. In functions other than qeKNN for which 
        this is an argument, it is the number of neighbors to use in finding 
        conditional probabilities via knnCalib.
    } 
    \item{holdout}{
        How many rows to use as the holdout/testing set. Can be NULL.
        The testing set is used to calculate s correlation and test accuracy.
    } 
}

\author{
    N. Matloff, A. Mittal, J. Tran
}

\details{
    The algorithm used to perform ridge regression is described in the paper
    described in Matloff's EDFfair package. Depending on regression vs. classification, 
    test Accuracy is mean absolute prediction error or the misclassification rate.  
    Corrs is the correlation between predicted Y across every level of S.
}

\examples{  
  data(compas1) # load dataset

  # deweight "decile score" column with "race" as the sensitive variable
  knnOut <- dsldQeFairKNN(compas1, "two_year_recid", "race", 
              list(decile_score=.1), yesYVal = "Yes")

  knnOut$testAcc # accuracy on holdout set

  knnOut$corrs # compute correlation between predicted Y and sensitive variables

  predict(knnOut, compas1[1,]) # generate predictions for new cases
}
        
