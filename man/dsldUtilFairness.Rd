\name{dsldUtilFairness}
\alias{dsldUtilFairness}

\title{dsldUtilFairness}
\description{
    Generates 2 plots depicting the Utility against D and Fairness against D.
}

\usage{
    dsldUtilFairness <- function(data, yName, cName, sName, count)
}

\arguments{
    \item{data}{
        dataset in dataframe form
    }
    \item{yName}{
        as in qeML functions, predictive variable, must be numeric
    }
    \item{cName}{
        
    }
    \item{sName}{
        name of the sensitive variable, an R factor
    }
    \item{count}{
        Number of times to run the qeKNN function call
    }
}

\author{
    N. Matloff, A. Mittal, T. Abdullah
}

\examples{
    library(regtools); library(Kendall); library(qeML)
    data(pef)
    dsldUtilFairness(pef, 'wageinc', 'occ', 'sex', 5)
}

