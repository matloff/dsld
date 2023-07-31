\name{dsldQeFairRidgeLin}
\alias{dsldQeFairRidgeLin}

\title{dsldQeFairRidgeLin}

\description{ 
  To-do
}

\usage{
  dsldQeFairRidgeLin <- function(data,yName,deweightPars,sensNames=NULL, holdout=floor(min(1000,0.1*nrow(data))))
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
    \item{sensNames}{
        to-decorrelated
    }
    \item{holdout}{
        to-decorrelated
    } 
}

\author{
    N. Matloff, S. Martha, A. Mittal, B. Ouattara, B. Zarate
}

\details{
    To-do
}

\examples{  
  # will continue to improve :)
  library(dsld)
  data("svcensus")
  z <- dsldQeFairRidgeLin(data=svcensus,yName='wageinc',deweightPars=list(occ=0.2),sensNames='gender')
  z$testAcc
  z$corrs
}
        
