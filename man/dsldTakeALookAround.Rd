\name{dsldTakeALookAround}
\alias{dsld}

\title{DSLDTakeALookAround}

\description{
    dsldTakeALookAround function helps to predict Y from a feature vector X, where X = (X1,...Xp) 
    which includes a sensitive variable S such as race or gender. The main objective is to eliminate 
    the effect of S on our predictions.
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
    dsldTakeALookAround(pef, 'wageinc','sex',4) # continous Y; run with maxFeatureSetSize to get all possible combinations

example 2:
    data(pef)
    dsldTakeALookAround(pef, 'educ','occ') # factor Y; all possible combinations of maxFeatureSetSize are shown

example 3: 
    data(pef)
    dsldTakeALookAround(pef, 'wageinc','sex',1) # run with smaller maxFeatureSetSize
}

