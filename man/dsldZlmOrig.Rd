\name{dsldZlmOrig}
\alias{dsldZlmOrig}

\title{dsldZlmOrig}

\description{
    This is a wrapper for the ZlmOrig function from the fairML package  
    (link in references).
}

\usage{
    dsldZlmOrig(yName, xName, sName, max.abs.cov)
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
    dsldZlm() and dsldZlm.orig() return an object of class c("zlm", "fair.model"). 
    dsldZlrm() and dsldZlrmOrig() return an object of class c("zlrm", "fair.model").
}

\references{
    https://cran.r-project.org/web/packages/fairml/index.html
}

\author{
    S. Martha, A. Mittal, B. Ouattara, B. Zarate
}

\examples{
    #ADD EXAMPLES
}

