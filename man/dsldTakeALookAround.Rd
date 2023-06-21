\name{dsldTakeALookAround}
\alias{dsld}

\title{DSLDTakeALookAround}
\description{
    Plots Y against X, with restrictions `condits` placed upon the dataset
    prior to predictive modeling on the dataset with `qeFtn` to extract underlying 
    patterns with respect to different variables. Not unlike Simpson's paradox, but 
    more specific to numeric data.

    The utility of this function comes from its ability to determine marginal effects 
    that might depend on the values of the other features. In other words, statistics that 
    generalize across a whole group may fail to take into account specifics about underlying 
    differences between subgroups (i.e. statistics about RECIDIVISM, a Y variable, relative to 
    AGE, an X variable, may fail to take into account different relationships across RACE, a 
    sensitive variable). This is shown in the first example given.
}

\usage{
    dsldTakeALookAround <- function(data,yName,sName, maxFeatureSetSize = (ncol(data) - 2))
}

\arguments{
\item{data}{dataset in dataframe form}
\item{yName}{as in qeML functions, response variable}
\item{sName}{name of the sensitive variable, an R factor}
\item{maxFeatureSetSize}{maximum # of combinations of features to be returned; 
default argument set as: maxFeatureSetSize = (ncol(data) - 2)}
}

\author{
    N. Matloff, A. Ashok, S. Martha, A. Mittal
}

\examples{
example 1:
    data(pef)
    dsldTakeALookAround(pef, 'wageinc','sex',4) # run w maxFeatureSetSize to get all possible combinations
}

\examples{
example 2:
    data(pef)
    dsldTakeALookAround(pef, 'educ','occ') # with factor Y & all possible combinations of maxFeatureSetSize are shown
}
