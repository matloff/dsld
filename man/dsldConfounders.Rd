\name{dsldConfounders}
\alias{dsldConfounders}
\title{dsldConfounders}

\description{
Plots estimated densities of all continuous features X, conditioned on a
specified categorical feature C. 
}

\usage{
dsldConfounders(data, sName, graphType = "plotly", fill = FALSE)
}

\arguments{
    \item{data}{Dataframe, at least 2 columns.}
    \item{sName}{
        Name of the categorical column, an R factor.  In discrimination 
        contexts, Typically a sensitive variable.
    }
    \item{graphType}{
        Either "plot" or "plotly", for static or interactive graphs.
        The latter requires the \pkg{plotly} package.
    }
    \item{fill}{
        Only applicable to graphType = "plot" case. Setting to true
        will color each line down to the x-axis.
    }
}

\author{
    N. Matloff, T. Abdullah, A. Ashok, J. Tran
}

\value{No value; plot.}

\examples{
\donttest{
data(svcensus)
dsldConfounders(svcensus, "educ")
}
}
