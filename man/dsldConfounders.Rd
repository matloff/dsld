\name{dsldConfounders}
\alias{dsldConfounders}
\title{dsldConfounders}

\description{
    WORK IN PROGRESS
}

\usage{
    dsldConfounders(data, sName = NULL)
}

\arguments{
    \item{data}{
        dataframe with labeled column names
    }
    \item{sName}{
        name of the sensitive variable column, an R factor
    }
}

\author{
    T. Abdullah, A. Ashok, J. Tran
}

\examples{
    # Example 1
    data(svcensus)
    dsldConfounders(svcensus, "educ")
}

