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
       Data frame with a numeric column and a factor or character grouping column.
    }
    \item{cName}{
        Possible confounding variable column, an R numeric
    }
    \item{sName}{
        Name of the sensitive variable column, a factor or character vector.
    }
    \item{graphType}{
        Type of graph created. Defaults to "plotly".
    }
    \item{fill}{
        Fill density curves in base graphics. The interactive figure has its own fill control.
    }
}

\author{
    N. Matloff, T. Abdullah, A. Ashok, J. Tran
}

\value{A plotly widget for \code{graphType = "plotly"}; invisible NULL after
drawing with base graphics for \code{graphType = "plot"}.}

\details{Missing groups and non-finite numeric observations are omitted. Each
displayed group needs at least two finite observations. The interactive bandwidth
slider updates all groups together.}

\examples{
data(svcensus)
dsldDensityByS(svcensus, cName = "wageinc", sName = "educ")
}
