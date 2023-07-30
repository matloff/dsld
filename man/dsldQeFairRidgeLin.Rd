\name{dsldQeFairRidgeLin}
\alias{dsldQeFairRidgeLin}

\title{DSLDQeFairRidgeLin}

\description{ 
  To-do
}

\usage{
  dsldQeFairRidgeLin <- function(data,yName,deweightPars,sensNames=NULL, holdout=floor(min(1000,0.1*nrow(data))))
}

\arguments{
  To-do
}

\author{
    N. Matloff, S. Martha, A. Mittal, B. Ouattara, B. Zarate
}

\examples{  
  # will continue to improve :)
  library(dsld)
  data("svcensus")
  z <- dsldQeFairRidgeLin(data=svcensus,yName='wageinc',deweightPars=list(occ=0.2),sensNames='gender')
  z$testAcc
  z$corrs
}

\details{
    To-do
}
        
