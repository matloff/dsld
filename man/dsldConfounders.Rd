\name{dsldConfounders}
\alias{dsldConfounders}
\title{dsldConfounders}

\description{
    Goes through every column in a data frame displays each one's relation
    to a sensitive variable. It will generate a table for catagorical columns, 
    and a line graph for numerical columns. This function is designed to 
    assess possible confounding between a sensitive variable and the other 
    variables in a data frame.
}

\usage{
    dsldConfounders(data, sName, graphType = "plotly", fill = FALSE)
}

\arguments{
    \item{data}{
        dataframe with labeled column names
    }
    \item{sName}{
        name of the sensitive variable column, an R factor
    }
    \item{graphType}{
        either "plot" or "plotly".
        "plot" : static graphs are made with R's default plot() function. 
        "plotly" : interactable graphs are made with the plotly package.
    }
    \item{fill}{
        only applicable to graphType = "plot" case. Setting to true will color
        each line down to the x-axis.
    }
}

\author{
    T. Abdullah, A. Ashok, J. Tran
}

\examples{
    # Example 1
    library(dsld)
    data(svcensus)
    dsldConfounders(svcensus, "educ")
}

