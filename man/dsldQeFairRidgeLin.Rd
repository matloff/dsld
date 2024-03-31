\name{dsldQeFairRidgeLin}
\alias{dsldQeFairRidgeLin}

\title{dsldQeFairRidgeLin}

\description{ 
  Fair Linear Ridge Regression
}

\usage{
  dsldQeFairRidgeLin(data, yName, sNames, deweightPars = NULL, 
                     holdout=floor(min(1000,0.1*nrow(data))))
}

\arguments{
    \item{data}{
        Data frame
    }
    \item{yName}{
        A string representing a numeric reponse column
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
}

\author{
    N. Matloff, S. Martha, A. Mittal, B. Ouattara, B. Zarate, J. Tran
}

\details{
    The algorithm used to perform ridge regression is described in the paper
    described in Matloff's EDFfair package. Test Accuracy is in mean absolute prediction error. 
    Corrs is the correlation between predicted Y across every level of S.
}

\examples{  
  data(svcensus) # load dataset

  # deweight "occupation" and "age" columns
  lin <- dsldQeFairRidgeLin(svcensus, "wageinc", "gender", 
      deweightPars = list(occ=.4, age=.2))

  lin$testAcc # compute accuracy on holdout set

  lin$corrs # compute correlation between predicted Y and sensitive variables

  predict(lin, svcensus[1,]) # generate predictions for new cases
}
        
