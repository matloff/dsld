\name{dsldQeFairKNN}
\alias{dsldQeFairKNN}

\title{DSLDQeFairKNN}

\description{ 
  To-do
}

\usage{
 dsldQeFairKNN <- function(data, yName, sName, expandVars = NULL, expandVals = NULL, 
                          scaleX = TRUE, yesYVal= NULL,k=25, holdout=floor(min(1000,0.1*nrow(data))))
}

\arguments{
    \item{data}{
        to-do
    }
    \item{yName}{
        A numeric vector, the response variable.
    }
    \item{sName}{
        to-do
    }
    \item{expandVars}{
        to-do
    }
    \item{expandVals}{
        to-do
    }
    \item{scaleX}{
        to-do
    }
    \item{yesYVal}{
        to-do
    }
    \item{k}{
        to-do
    } 
    \item{holdout} {
        to-do
    } 
}

\author{
    N. Matloff, A. Mittal
}

\details{
    To-do
}

\examples{  
  # Working example:
  data("svcensus")
  knn1 = dsldQeFairKNN(data = svcensus,yName = 'wageinc',sName = 'gender', expandVars = 'occ', expandVals = 0.1, scaleX = TRUE)
  knn1$testAcc
  knn1$corrsens

  knn2 = dsldQeFairKNN(data = svcensus,yName = 'educ',sName = 'gender', expandVars = 'occ', expandVals = 0.1, scaleX = TRUE)
  knn2$testAcc
  knn2$corrsens
}
        
