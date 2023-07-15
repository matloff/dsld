\name{dsldZlmOrig}
\alias{dsldZlmOrig}
\title{zlmOrig in dsld}
\description{Linear and logistic regression models enforcing fairness by bounding the covariance between sensitive attributes and predictors.
}
\usage{
dsldZlmOrig(response, predictors, sensitive, max.abs.cov)
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
  \item{max.abs.cov}{
a non-negative number, the original bound on the maximum absolute covariance from Zafar et al. (2019).
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

