\name{dsldDensityByS}
\alias{dsldDensityByS}
\title{dsldDensityByS}

\description{
    Graphs densities of a response variable, grouped by a sensitive variable. 
    Similar to \code{dsldConfounders}, but includes sliders to control the 
    bandwidth of the density estimate (analogous to controlling the bin
    width in a histogram).
}

\usage{
dsldDensityByS(data, cName, sName, graphType = "plotly", fill = FALSE)
}

\arguments{
    \item{data}{
       Datasetwith at least 1 numerical column and 1 factor column
    }
    \item{cName}{
        Possible confounding variable column, an R numeric
    }
    \item{sName}{
        Name of the sensitive variable column, an R factor
    }
    \item{graphType}{
        Type of graph created. Defaults to "plotly".
    }
    \item{fill}{
        To fill the graph. Defaults to "FALSE".
    }
}

\author{
    N. Matloff, T. Abdullah, A. Ashok, J. Tran
}

\value{No value; plot.}

\examples{
data(svcensus)
dsldDensityByS(svcensus, cName = "wageinc", sName = "educ")
}
