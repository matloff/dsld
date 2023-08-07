\name{dsldQeFairRF}
\alias{dsldQeFairRF}

\title{DSLDQeFairRF}

\description{ 
  To-do
}

\usage{
dsldQeFairRF <- function(data,yName, sName=NULL,deweightPars, nTree=500,
                         minNodeSize=10,mtry = floor(sqrt(ncol(data))),
                         yesYVal=NULL,holdout=floor(min(1000,0.1*nrow(data))))
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
    \item{nTree}{
        Number of trees.
    }
    \item{minNodeSize}{
        Minimum number of data points in a tree node.
    }
    \item{mtry}{
        Number of variables randomly tried at each split.
    }
    \item{yesYVal}{
        Y value to be considered "yes," to be coded 1 rather than 0.
    } 
    \item{holdout}{
        If not NULL, form a holdout set of the specified size. After fitting to the remaining data, evaluate accuracy on the test set.
    } 
}

\author{
    N. Matloff, A.Mittal
}

\details{
    To-do
}

\examples{  
  # Working example:
  data("svcensus")

  # deweight 'occ' to 0.1
  z1 <- dsldQeFairRF(data=svcensus,yName='wageinc',deweightPars = list(occ =0.1),sName='gender')
  z1$testAcc
  z1$corrsens
  z1$deweightPars

  # predict()
  newData <- data.frame(age = c(18, 60), educ = c("zzzOther", 'zzzOther'), wkswrkd = c(50, 50), occ = c("106", "106"), gender = c('male','male'))
  predict(z1,newData)

  # deweight 'occ' to 0.5, 'age' to 0.8
  z2 <- dsldQeFairRF(data=svcensus,yName='wageinc',deweightPars = list(occ =0.5,age =0.8),sName='gender')
  z2$testAcc
  z2$corrsens
  z2$deweightPars
  predict(z2, newData)
}
        
