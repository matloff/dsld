\name{dsldZlrm}
\alias{dsldZlrm}

\title{dsldZlrm}

\description{
    This is a wrapper for the Zlrm function from the freqparcoord 
    package (link in references).
}

\usage{
    dsldZlrm(yName, xName, sName, unfairness)
}
\arguments{
    \item{yName}{
        A numeric vector, the response variable.
    }
    \item{xName}{
        A numeric matrix or a data frame containing numeric and factor columns; 
        the predictors.
    }
    \item{sName}{
        A numeric matrix or a data frame containing numeric and factor columns; 
        the sensitive attributes.
    }
    \item{unfairness}{
        A positive number in [0, 1], how unfair is the model allowed to be. 
        A value of 0 means the model is completely fair, while a value of 1 means 
        the model is not constrained to be fair at all.
    }
    \item{max.abs.cov}{
        A non-negative number, the original bound on the maximum absolute 
        covariance from Zafar et al. (2019).
    }
}

\details{
    dsldZlm() and dsldZlrm() define fairness as statistical parity.

    Estimation minimizes the log-likelihood of the regression models under the constraint that the correlation between each sensitive attribute and the fitted values (on the linear predictor scale, in the case of logistic regression) is smaller than unfairness in absolute value. Both models include predictors as explanatory variables; the variables sensitive only appear in the constraints.

    The only difference between dsldZlm() and dsldZlmOrig(), and between dsldZlrm() and dsldZlrmOrig(), is that the latter uses the original constraint on the covariances of the individual sensitive attributes from Zafar et al. (2019).
}

\value{
    dsldZlm() and dsldZlmOrig() return an object of class c("zlm", "fair.model"). 
    dsldZlrm() and dsldZlrmOrig() return an object of class c("zlrm", "fair.model").
}

\references{
    https://cran.r-project.org/web/packages/fairml/index.html
}

\author{
  S. Martha, A. Mittal, B. Ouattara, B. Zarate
}

\examples{
    # Example 1
    data(adult)
    # short-hand variable names.
    r = adult[, "income"]
    s = adult[, c("sex", "race")]
    p = adult[, setdiff(names(adult), c("income", "sex", "race"))]
    ## Not run:
    m = dsldZlrm(response = r, sensitive = s, predictors = p, unfairness = 0.05)
    summary(m)

    #Error: ‘there is no package called ‘CVXR’’
    #solution: Install 'CVXR' through CRAN
    #Error: Does not halt
    #After force quitting: 'Warning message: glm.fit: fitted probabilities numerically 0 or 1 occurred'

    # Example 2
    data(bank)
    # remove loans with unknown status, the corresponding coefficient is NA in glm().
    bank = bank[bank$loan != "unknown", ]
    # short-hand variable names.
    r = bank[, "subscribed"]
    s = bank[, c("age")]
    p = bank[, setdiff(names(bank), c("subscribed", "age"))]
    m = dsldZlrm(response = r, sensitive = s, predictors = p, unfairness = 0.05)
    summary(m)
}

