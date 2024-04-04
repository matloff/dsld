\name{dsldDensityByS}
\alias{dsldDensityByS}
\title{dsldDensityByS}

\description{
    Graphs densities of a response variable, grouped by a sensitive variable. 
    Has sliders to control the bandwidth of the density.
}

\usage{
dsldDensityByS(data, cName, sName, graphType = "plotly", fill =
    FALSE)
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
    \item{graphType}{
        [character] type of graph created. Defaults to "plotly".
    }
    \item{fill}{
        [logical] to fill the graph
    }
}

\author{
    N. Matloff, T. Abdullah, A. Ashok, J. Tran
}

\examples{
    data(svcensus)
    dsld::dsldDensityByS(svcensus, cName = "wageinc", sName = "educ")
}
