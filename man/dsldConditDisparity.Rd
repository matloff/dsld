\name{dsldConditDisparity}
\alias{dsld}

\title{DSLDConditDisparity}
\description{
    Plots Y against X, with restrictions `condits` placed upon the dataset
    prior to predictive modeling on the dataset with `qeFtn` to extract underlying 
    patterns.
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

}

