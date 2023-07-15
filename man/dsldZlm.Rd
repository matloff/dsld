\name{dsldZlm}
\alias{dsldZlm}
\title{zlm in dsld}
\description{Linear and logistic regression models enforcing fairness by bounding the covariance between sensitive attributes and predictors.
}
\usage{
dsldZlm(response, predictors, sensitive, unfairness)
}
\arguments{
  \item{response}{
a numeric vector, the response variable.
}
  \item{predictors}{
a numeric matrix or a data frame containing numeric and factor columns; the predictors.
}
  \item{sensitive}{
a numeric matrix or a data frame containing numeric and factor columns; the sensitive attributes.
}
  \item{unfairness}{
a positive number in [0, 1], how unfair is the model allowed to be. A value of 0 means the model is completely fair, while a value of 1 means the model is not constrained to be fair at all.
}
}
\details{
dsldZlm() and dsldZlrm() define fairness as statistical parity.

Estimation minimizes the log-likelihood of the regression models under the constraint that the correlation between each sensitive attribute and the fitted values (on the linear predictor scale, in the case of logistic regression) is smaller than unfairness in absolute value. Both models include predictors as explanatory variables; the variables sensitive only appear in the constraints.

The only difference between dsldZlm() and dsldZlmOrig(), and between dsldZlrm() and dsldZlrmOrig(), is that the latter uses the original constraint on the covariances of the individual sensitive attributes from Zafar et al. (2019).
}
\value{
dsldZlm() and dsldZlm.orig() return an object of class c("zlm", "fair.model"). dsldZlrm() and dsldZlrmOrig() return an object of class c("zlrm", "fair.model").
}
\references{
Zafar BJ, Valera I, Gomez-Rodriguez M, Gummadi KP (2019). "Fairness Constraints: a Flexible Approach for Fair Classification". Journal of Machine Learning Research, 30:1–42.
https://www.jmlr.org/papers/volume20/18-262/18-262.pdf~
}
\author{
Marco Scutari
}
\examples{
#ADD EXAMPLES
}

