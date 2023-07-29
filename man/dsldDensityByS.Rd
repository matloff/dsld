\name{dsldDensityByS}
\alias{dsldDensityByS}
\title{Graphs densities of a response variable, grouped by a sensitive variable}

\usage{
    dsldDensityByS(data, cName, sName)
}

\arguments{
    \item{data}{
       [dataframe] datasetwith at least 1 numerical column and 1 factor column
    }
    \item{cName}{
        [character] possible confounding variable column, an R numeric
    }
    \item{sName}{
        [character] name of the sensitive variable column, an R factor
    }
}

\description{
    Graphs densities of a response variable, grouped by a sensitive variable. 
    Has sliders to control the bandwidth of the density.
}

\author{
    N. Matloff, T. Abdullah, A. Ashok, J. Tran
}

\examples{
    # Example 1
    library(dsld)
    data(svcensus)
    dsld::dsldDensityByS(svcensus, cName = "wageinc", sName = "educ")
}