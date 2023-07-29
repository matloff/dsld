\name{dsldConfounders}
\alias{dsldConfounders}
\title{dsldConfounders}

\description{
    This function is designed to assess possible confounding between a
    sensitive variable and the other variables in a data frame. By iterating
    every column in a dataframe, it analyzes each column agins the sensitive
    feature in the context of finding confounding variables. Depending on the
    type of data stored in the column (numeric, categorical), `dsldConfounders`
    will dispatch to `dsldDensityByS()` to graph numeric data in the specified
    format or `dsldFrequencyByS()` to tally distribution for categorical data
    in a dataframe.
}

\usage{
    dsldConfounders(data, sName, graphType = "plotly", fill = FALSE)
}

\arguments{
    \item{data}{
        [dataframe] dataset with labeled column names, at least 2 columns
    }
    \item{sName}{
        [character] name of the sensitive variable column, an R factor
    }
    \item{graphType}{
        [character] either "plot" or "plotly".
        "plot" : static graphs are made with R's default plot() function. 
        "plotly" : interactable graphs are made with the plotly package.
    }
    \item{fill}{
        [logical] only applicable to graphType = "plot" case. Setting to true
        will color each line down to the x-axis.
    }
}

\author{
    N. Matloff, T. Abdullah, A. Ashok, J. Tran
}

\examples{
    # Example 1
    library(dsld)
    data(svcensus)
    dsldConfounders(svcensus, "educ")
}

