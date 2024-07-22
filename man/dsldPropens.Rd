\name{dsldMatchedATE}
\alias{dsldMatchedATE}

\title{dsldMatchedATE}

\description{
Wrapper for \code{Matching::Match}.
}

\usage{
dsldMatchedATE(data,yName,sName,yesSVal,yesYVal=NULL,k=NULL,propensFtn=NULL)
}

\arguments{
    \item{data}{Data frame}
    \item{yName}{Name of the response variable column.}
    \item{sName}{ Name of the sensitive attribute column. The
       attribute must be dichotomous.}
    \item{yesSVal}{S value to be considered "yes," to be coded 
       1 rather than 0.}
    \item{yesYVal}{Y value to be considered "yes," to be coded 
       1 rather than 0.}
    \item{propensFtn}{Either 'glm' (logistic), or 'knn'.}
}

\value{

Object of class 'Match'. See documentation in the 
\pkg{Matching} package.
}

\details{

This is a \pkg{dsld} wrapper for \code{Matching::Match}. 

Finds the estimated mean difference between`the matched Y pairs in the
treated/nontreated (exposed and nonn-exposed) groups, with covariates
X in 'data' other than the yName and sName columns

sName here is the "treatment" or "exposure," S

in that optional case, we estimate P(S = 1 | X), either by a logistic
or k-NN model
}

\author{
    N. Matloff
}

\examples{

data(lalonde)
ll <- lalonde
ll$treat <- as.factor(ll$treat)
ll$re74 <- NULL
ll$re75 <- NULL
summary(dsldMatchedATE(ll,'re78','treat','1')) 
summary(dsldMatchedATE(ll,'re78','treat','1',propensFtn='glm'))
summary(dsldMatchedATE(ll,'re78','treat','1',propensFtn='knn',k=15))
}

