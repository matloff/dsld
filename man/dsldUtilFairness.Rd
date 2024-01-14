\name{dsldUtilFairness}
\alias{dsldUtilFairness}

\title{dsldUtilFairness}
\description{
    Generates 2 plots depicting the Utility against D and Fairness against D.
}

\usage{
    dsldUtilFairness <- function(data, yName, cName, sName, count = 5, 
                                deweight_increment = 0.1)
}

\arguments{
    \item{data}{
        dataset in dataframe form
    }
    \item{yName}{
        name of the response column [character]
    }
    \item{cName}{
        name of proxy variable to deweight [character]
    }
    \item{sName}{
        name of the sensitive variable, an R factor [character]
    }
    \item{count}{
        Number of times to run the qeKNN function call [numeric]
    }
    \item{deweight_increment}{
        Increment to weight the proxy by [numeric]
    }
}

\author{
    N. Matloff, A. Mittal, T. Abdullah
}

\examples{
    library(dsld)
    data(svcensus)

    # example w/ categorical cName
    dsld::dsldUtilFairness(data = svcensus,yName ='wageinc', cName = 'occ', 
        sName = 'gender')

    # example w/ continuous cName
    dsld::dsldUtilFairness(data = svcensus,yName ='wageinc', cName = 'wkswrkd', 
        sName = 'gender', deweight_increment = 0.05)
}

