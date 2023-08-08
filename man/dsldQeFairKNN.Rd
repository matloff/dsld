\name{dsldQeFairKNN}
\alias{dsldQeFairKNN}

\title{DSLDQeFairKNN}

\description{ 
  To-do
}

\usage{
  dsldQeFairKNN <- function(data, yName, sName = NULL,deweightPars, scaleX = TRUE, yesYVal= NULL,
                          k=25, holdout=floor(min(1000,0.1*nrow(data))))
}

\arguments{
    \item{data}{
        dataset in dataframe form
    }
    \item{yName}{
        a string, the response variable [character]
    }
    \item{sName}{
        a string, the sensitive variable (an R factor) [character]
    }
    \item{deweightPars}{
        Values for de-emphasizing variables in a split, e.g. 'list(age=0.2,gender=0.5)'
    }
    \item{scaleX}{
        Scale the features. Defaults to TRUE.
    }
    \item{yesYVal}{
        Y value to be considered "yes," to be coded 1 rather than 0.
    }
    \item{k}{
        Number of nearest neighbors. In functions other than qeKNN for which this is an argument, 
        it is the number of neighbors to use in finding conditional probabilities via knnCalib.
    } 
    \item{holdout} {
        If not NULL, form a holdout set of the specified size. After fitting to the remaining data, evaluate accuracy on the test set.
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

  # deweight 'occ' to 0.1
  k1 = dsldQeFairKNN(data = svcensus,yName = 'wageinc', sName = 'gender', deweightPars = list(occ=0.1))

  # View params:
  k1$testAcc
  k1$corrsens
  k1$holdoutPreds
  k1$deweightNames
  k1$deweightVals

  # predict()
  newData <- data.frame(age = c(18, 60), educ = c("zzzOther", 'zzzOther'), wkswrkd = c(50, 50), occ = c("106", "106"), gender = c('male','male'))
  predict(k1,newData)

  # deweight 'occ' to 0.5, 'age' to 0.8
  k2 = dsldQeFairKNN(data = svcensus,yName = 'wageinc', sName = 'gender', deweightPars = list(occ=0.5, age = 0.8))

  # View params:
  k2$testAcc
  k2$corrsens
  k2$holdoutPreds
  k2$deweightNames
  k2$deweightVals
  predict(k2,newData)
}
        
