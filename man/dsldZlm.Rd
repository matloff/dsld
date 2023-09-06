\name{dsldZlm}
\alias{dsldZlm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  dsldZlm
}
\description{
    This is a wrapper for the zlrm function from the fairML package
    (link in references).
}
\usage{
dsldZlm(data, yName, sName, unfairness)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{data}{
        The data, in data frame or matrix form.
    }
    \item{yName}{
        Column name indicating a binary outcome variable
    }
    \item{sName}{
        A string vector of column names, the sensitive attributes.
    }
    \item{unfairness}{
        A positive number in [0, 1], how unfair is the model allowed to be. 
        A value of 0 means the model is completely fair, while a value of 1 means 
        the model is not constrained to be fair at all.
    }
}
\details{
From fairml:

zlm() and zlrm() define fairness as statistical parity.

Estimation minimizes the log-likelihood of the regression models under the constraint that the correlation between each sensitive attribute and the fitted values (on the linear predictor scale, in the case of logistic regression) is smaller than unfairness in absolute value. Both models include predictors as explanatory variables; the variables sensitive only appear in the constraints.
}
\value{
  An object of class "dsldZlm", which has a $data attribute that contains
    an object of class c("zlm", "fair.model").
}
\references{
https://cran.r-project.org/web/packages/fairml/index.html
}
\author{
  Jonathan Tran
}
\examples{
data <- fairml::compas
yName <- "two_year_recid"
sName <- "race"

model <- dsldZlm(data, yName, sName, 0)
predict(model, data)
}

