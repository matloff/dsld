\name{dsldConditDisparity}
\alias{dsld}

\title{DSLDConditDisparity}
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
    dsldConditDisparity <- function(data, yName, sName, xName, condits, qeFtn=qeKNN,
                            minS=50, yLim=NULL, useLoess=TRUE)
}

\arguments{
\item{data}{dataset in dataframe form}
\item{yName}{as in qeML functions, predictive variable}
\item{xName}{name of a numeric column to take comparisons with}
\item{sName}{name of the sensitive variable, an R factor}
\item{condits}{an R vector; each component is a character string for an R logical 
expression representing a desired condition; these must NOT involve sName}
\item{qeFtn}{reg function (defaults only), defaults to qeKNN}
\item{minS}{minimum size for an S group to be retained in the analysis, defaults to 50}
\item{yLim}{a 2-element vector specifying the lower and upper vertical plot limits, defaults to fit data}
\item{useLoess}{if TRUE, do Loess smoothing on the regression values, defaults to TRUE}
}

\author{
    N. Matloff, A. Ashok, S. Martha, A. Mittal
}

\examples{
example 1:
    data(compas)
    compas$two_year_recid <- as.numeric(compas$two_year_recid == 'Yes')
    conditDisparity(compas,'two_year_recid', 'race', 'age', 
                    c('priors_count <= 4','decile_score>=6'), qeKNN)
}

