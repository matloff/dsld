\name{dsldZlrm}
\alias{dsldZlrm}

\title{
  dsldZlrm
}

\description{
    This is a wrapper for the zlrm function from the fairML package
    (link in references).
}

\usage{
dsldZlrm(data, yName, sName, unfairness)
}

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
Via fairml:

zlm() and zlrm() define fairness as statistical parity.

Estimation minimizes the log-likelihood of the regression models under the 
constraint that the correlation between each sensitive attribute and the fitted 
values (on the linear predictor scale, in the case of logistic regression) is 
smaller than unfairness in absolute value. Both models include predictors as 
explanatory variables; the variables sensitive only appear in the constraints.
}

\value{
  An object of class "dsldZlrm", which has a $data attribute that contains
    an object of class c("zlrm", "fair.model").
}

\references{
https://cran.r-project.org/web/packages/fairml/index.html
}

\author{
  N. Matloff, J. Tran
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
    Komiyama J, Takeda A, Honda J, Shimao H (2018). "Nonconvex Optimization for
    Regression with Fairness Constraints". Proceedints of the 35th International
    Conference on Machine Learning (ICML), PMLR \strong{80}:2737--2746. \cr
    \code{http://proceedings.mlr.press/v80/komiyama18a/komiyama18a.pdf}
}

\examples{
  data(compas1) # load dataset

  yName <- "two_year_recid"
  sName <- "age"

  # unfairness parameter set at 0.2
  zlrmOut <- dsldZlrm(compas1, yName, sName, 0.2)

  # create predictions for future cases
  predict(zlrmOut, compas1[c(1:10),])
}

