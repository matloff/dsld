\name{dsldQeFairRF}
\alias{dsldQeFairRF}

\title{DSLDQeFairRF}

\description{ 
  Fair Random Forest w/ deweighted nodes
}

\usage{
dsldQeFairRF <- function(data,yName,sNames,deweightPars=NULL, nTree=500,
                         minNodeSize=10,mtry = floor(sqrt(ncol(data))),
                         yesYVal=NULL,holdout=floor(min(1000,0.1*nrow(data))))
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
        Values for de-emphasizing variables in a split, e.g. 'list(age=0.2,gender=0.5)'
        Lower values means more deweighting.
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
        How many rows to use as the holdout/testing set. Can be NULL.
        The testing set is used to calculate s correlation and test accuracy.
    } 
}

\author{
    N. Matloff, A.Mittal, J. Tran
}

\details{
    To-do
}

\examples{  
  rf <- dsld::dsldQeFairRF(fairml::compas, "two_year_recid", "race")
  predict(rf, fairml::compas[1,])
}
        
