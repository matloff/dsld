\name{dsldFairML Wrappers}
\alias{dsldFrrm}
\alias{dsldFgrrm}
\alias{dsldNclm}
\alias{dsldZlm}
\alias{dsldZlrm}
\alias{predict.dsldFairML}
\alias{summary.dsldFairML}

\title{dsldFairML Wrappers}

\description{
    Fair machine learning models: estimation and prediction. The following 
    functions provide wrappers for some functions in the \pkg{fairML}
    package. 
}
\usage{
dsldFrrm(data, yName, sName, unfairness, definition = "sp-komiyama", 
   lambda = 0, save.auxiliary = FALSE)
dsldFgrrm(data, yName, sName, unfairness, definition = "sp-komiyama", 
   family = "binomial", lambda = 0, save.auxiliary = FALSE)
dsldNclm(data, yName, sName, unfairness, covfun = cov, lambda = 0, 
   save.auxiliary = FALSE)
dsldZlm(data, yName, sName, unfairness)
dsldZlrm(data, yName, sName, unfairness)
}

\arguments{
    \item{data}{
        Data frame.
    }
    \item{yName}{
        Name of the response variable column. 
    }
    \item{sName}{
        Name(s) of the sensitive attribute column(s). 
    }
    \item{unfairness}{
        A number in (0, 1]. Degree of unfairness allowed in
        the model.  A value (very near) 0 means the model is completely 
        fair, while a value of 1 means the model is not 
        constrained to be fair at all.
    }
    \item{covfun}{
        A function computing covariance matrices. 
    }
    \item{definition}{
        Character string, the label of the definition of fairness.
        Currently either 'sp-komiyama', 'eo-komiyama' or 'if-berk'.
    }
    \item{family}{
        A character string, either 'gaussian' to fit linear regression, 
        'binomial' for logistic regression, 'poisson' for
        log-linear regression, 'cox' for Cox proportional 
        hazards regression, or 'multinomial' for
        multinomial logistic regression.
    }
    \item{lambda}{
        Non-negative number, a ridge-regression penalty coefficient. 
    }
    \item{save.auxiliary}{
        A logical value, whether to save the fitted values and the residuals 
        of the auxiliary model that constructs the debiased predictors. 
    }
}

\details{

   See documentation for the \pkg{fairml} package.

}

\value{
    An object of class 'dsldFairML', which includes the model 
    information, \code{yName}, and \code{sName}.
}

\author{
    S. Martha, A. Mittal, B. Ouattara, B. Zarate, J. Tran
}

\examples{ 
\donttest{
data(svcensus) 
data(compas1)

yName <- "wageinc"
sName <- "age"
frrmOut <- dsldFrrm(svcensus, yName, sName, 0.2, definition = "sp-komiyama") 
summary(frrmOut)
predict(frrmOut, svcensus[1:10,]) 

yName <- "two_year_recid"
sName <- "age"
fgrrmOut <- dsldFgrrm(compas1, yName, sName, 0.2, definition = "sp-komiyama")  
summary(fgrrmOut)
predict(fgrrmOut, compas1[c(1:10),]) 
}

}
