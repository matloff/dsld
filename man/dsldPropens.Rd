\name{dsldMatchedATE}
\alias{dsldMatchedATE}

\title{dsldMatchedATE}

\description{
Causal inference via matching models.
Wrapper for \code{Matching::Match}.
}

\usage{
dsldMatchedATE(data,yName,sName,yesSVal,yesYVal=NULL,
   propensFtn=NULL,k=NULL)
}

\arguments{
    \item{data}{Data frame.}
    \item{yName}{Name of the response variable column.}
    \item{sName}{ Name of the sensitive attribute column. The
       attribute must be dichotomous.}
    \item{yesSVal}{S value to be considered "yes," to be coded 
       1 rather than 0.}
    \item{yesYVal}{Y value to be considered "yes," to be coded 
       1 rather than 0.}
    \item{propensFtn}{Either 'glm' (logistic), or 'knn'.}
    \item{k}{Number of nearest neighbors if \code{propensFtn='knn'.}}
}

\value{

Object of class 'Match'. See documentation in the 
\pkg{Matching} package.
}

\details{

This is a \pkg{dsld} wrapper for \code{Matching::Match}. 

Matched analysis is typically applied to measuring "treatment effects,"
but is often applied in situations in which the "treatment," S here, is
an immutable attribute such as race or gender. The usual issues
concerning observational studies apply.

The function \code{dsldMatchedATE} finds the estimated mean difference
between the matched Y pairs in the treated/nontreated (exposed and
non-exposed) groups, with covariates X in \code{data} other than the
\code{yName} and \code{sName} columns.

In the propensity model case, we estimate P(S = 1 | X), either by a logistic
or k-NN model.
}

\author{
    N. Matloff
}

\examples{

data(lalonde,package='Matching')
ll <- lalonde
ll$treat <- as.factor(ll$treat)
ll$re74 <- NULL
ll$re75 <- NULL
summary(dsldMatchedATE(ll,'re78','treat','1')) 
summary(dsldMatchedATE(ll,'re78','treat','1',propensFtn='glm'))
summary(dsldMatchedATE(ll,'re78','treat','1',propensFtn='knn',k=15))
}

