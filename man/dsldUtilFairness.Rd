\name{dsldUtilFairness}
\alias{dsldUtilFairness}

\title{dsldUtilFairness}
\description{
    Generate plots depicting Utility against D and Fairness against D.
}

\usage{
dsldUtilFairness(data, yName, cName, sName, count = 5, 
    deweight_increment = 0.1)
}

\arguments{
    \item{data}{
        Data frame comprising the input data.
    }
    \item{yName}{
        Name of the response variable column.
    }
    \item{cName}{  
        Name of proxy variable column to deweight.
    }
    \item{sName}{
        Name of the sensitive attribute column.
    }
    \item{count}{
        Number of times to run the qeKNN function call.
    }
    \item{deweight_increment}{
        Increment to deweight the proxy by.
    }
}

\author{
    N. Matloff, A. Mittal, T. Abdullah
}

\examples{
data(svcensus)
\donttest{
# example w/ categorical cName
dsldUtilFairness(data = svcensus,yName ='wageinc', cName = 'occ', 
    sName = 'gender')

# example w/ continuous cName
dsldUtilFairness(data = svcensus,yName ='wageinc', cName = 'wkswrkd', 
    sName = 'gender', deweight_increment = 0.05)}
}
