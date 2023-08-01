\name{dsldQeFairRF}
\alias{dsldQeFairRF}

\title{dsldQeFairRF}

\description{ 
  To-do
}

\usage{
  dsldQeFairRF <- function(data,yName,deweightPars,sensNames=NULL,
                     nTree=500,minNodeSize=10,mtry = floor(sqrt(ncol(data))),
                     yesYVal=NULL,holdout=floor(min(1000,0.1*nrow(data))))
}

\arguments{
    \item{data}{
        to-do
    }
    \item{yName}{
        A numeric vector, the response variable.
    }
    \item{deweightPars}{
        to-do
    }
    \item{holdout}{
        to-do
    }
    \item{nTree}{
        to-do
    }
    \item{minNodeSize}{
        to-do
    }
    \item{mtry}{
        to-do
    }
    \item{holdout}{
        to-do
    } 
}

\author{
    N. Matloff
}

\details{
    To-do
}

\examples{  
  # To-do
  library(dsld)
  data("svcensus")
  z <- dsldQeFairRF(data=svcensus,yName='wageinc',deweightPars=list(occ=0.2),sensNames='gender')
  z$testAcc
}
        
