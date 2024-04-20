\name{dsldConditDisparity}
\alias{dsldConditDisparity}
\title{dsldConditDisparity}

\description{
    Plots (estimated) mean Y against X, separately for each level of S,
    with restrictions \code{condits}. May reveal Simpson's Paradox-like
    differences not seen in merely plotting mean Y against X.
}

\usage{
dsldConditDisparity(data, yName, sName, xName, condits,
    qeFtn = qeKNN, minS = 50, useLoess = TRUE)
}

\arguments{
   \item{data}{Dataframe or equivalent.}
   \item{yName}{As in qeML functions, predicted variable, must be numeric 
      or dichtomous R factor.}
   \item{sName}{Name of the sensitive variable S, an R factor}
   \item{xName}{Name of a numeric column for the X-axis.}
   \item{condits}{An R vector; each component is a character 
      string for an R logical expression representing a desired 
      condition involving \code{names(data) other than S and Y.}}
   \item{qeFtn}{qeML predictive function function (only default arguments
      will be used.)}
   \item{minS}{Minimum size for an S group to be retained in the analysis.}
   \item{useLoess}{if TRUE, do loess smoothing on the fitted regression values.}
}

\author{
    N. Matloff, A. Ashok, S. Martha, A. Mittal
}

\examples{
data(compas)
compas$two_year_recid <- as.numeric(compas$two_year_recid == "Yes")
dsldConditDisparity(compas,"two_year_recid", "race", "age", 
    c("priors_count <= 4","decile_score>=6"), qeKNN)

dsldConditDisparity(compas,"two_year_recid", "race", "age", 
    c("priors_count <= 4","decile_score>=6"), qeGBoost)

dsldConditDisparity(compas,"two_year_recid","sex","age",
    c("priors_count <= 4","decile_score>=6"),qeKNN)
}
