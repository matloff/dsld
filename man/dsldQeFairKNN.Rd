\name{dsldQeFairKNN}
\alias{dsldQeFairKNN}

\title{DSLDQeFairKNN}

\description{ 
  To-do
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
        Y value to be considered "yes," to be coded 1 rather than 0.
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
    To-do
}

\examples{  
  knn <- dsld::dsldQeFairKNN(fairml::compas, "two_year_recid", "race", 
              list(decile_score=.2), yesYVal = "Yes")
  predict(knn, fairml::compas[1,])
}
        
