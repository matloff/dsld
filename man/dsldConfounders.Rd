\name{dsldConfounders}
\alias{dsldConfounders}
\title{dsldConfounders}

\description{
    Analyzes confounding variables in a dataframe via density plots.
}

\usage{
dsldConfounders(data, sName, graphType = "plotly", fill = FALSE)
}

\arguments{
    \item{data}{Dataframe, at least 2 columns.}
    \item{sName}{
        Name of the sensitive variable column, an R factor.
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

\details{

This function is designed to assess possible confounding between a
sensitive variable S and the other variables in a data frame. By
iterating every column in a dataframe, it analyzes each column against
the sensitive feature in the context of finding confounding variables.
Each curve is a density estimate (smoothed histogram) that will
represent one level of S, so if there is no confounding, the curves will
be identical.

Depending on the type of data stored in the column (numeric,
categorical), `dsldConfounders` will dispatch to `dsldDensityByS()` to
graph numeric data in the specified format or `dsldFrequencyByS()` to
tally distribution for categorical data in a dataframe.  }

\author{
    N. Matloff, T. Abdullah, A. Ashok, J. Tran
}

\examples{
data(svcensus)
dsldConfounders(svcensus, "educ")
}
