\name{dsldLinearModel}
\alias{dsldLinModel}

\title{DSLD Linear Model}
\description{
    This function produces an instance of the `dsldLinModel` class that 
    houses a separate instance of the `dsldDiffModel` class for each 
    unique, interactive level in the sensitive column specified. The end 
    result is a linear model that with or without interactions that 
    predicts over the inputted data with respect to yName and sName.
}

\usage{
    dsldLinModel <- function(data, yName, sName, interactions=TRUE)
}

\arguments{
    \item{data}{dataset to model over, will be split according to each level 
    in the final outputted `dsldLinModel` object [dataframe]}
    \item{yName}{as in qeML functions, predictive variable [character]}
    \item{sName}{name of the sensitive variable, an R factor [character]}
    \item{interactions}{specifies whether or not to consider interactions. 
    Defaults to TRUE [boolean]}
}

\author{
    N. Matloff, A. Ashok, A. Mittal
}

\examples{
    # empty for now
}