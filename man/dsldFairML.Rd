\name{dsldFairML Wrappers}
\alias{dsldFrrm}
\alias{dsldFgrrm}
\alias{dsldNclm}
\alias{dsldZlm}
\alias{dsldZlrm}

\title{dsldFairML Wrappers}

\description{
    This is a wrapper for the frrm function from the fairML package
    (link in references).

  This is a wrapper for the Nclm function from the fairml package  
    (link in references). The fairml package bases their fair regression 
    model based on the nonconvex Optimization for Regression with Fairness 
    Constraints from Komiyama et al. (2018) (link in See also).
}

\usage{
    dsldFrrm(data, yName, sName, unfairness,
                     definition = "sp-komiyama", lambda = 0, 
                     save.auxiliary = FALSE)
    dsldFgrrm(data, yName, sName, unfairness,
                      definition = "sp-komiyama", family = "binomial", 
                      lambda = 0, save.auxiliary = FALSE)
    dsldNclm(data, yName, sName, unfairness, covfun, 
             lambda = 0, save.auxiliary = FALSE)
    dsldZlm(data, yName, sName, unfairness)
    dsldZlrm(data, yName, sName, unfairness)
    \method{summary}{dsldFairML}(object,...)
  \method{predict}{dsldFairML}(object,newx,...)
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
        A value of 0 means the model is completely fair, while a value of 1 
        means the model is not constrained to be fair at all.
    }
    \item{covfun}{
        A function computing covariance matrices. It defaults to the cov() function 
        from the stats package.  
    }
    \item{definition}{
        A character string, the label of the definition of fairness used in 
        fitting the model. Currently either "sp-komiyama", "eo-komiyama" or 
        "if-berk". It may also be a custom function: see below for details.
    }
    \item{family}{
        A character string, either "gaussian" to fit a linear regression, 
        "binomial" to fit a logistic regression, "poisson" to fit a log-linear 
        regression, "cox" to fit a Cox proportional hazards regression of 
        "multinomial" to fit a multinomial logistic regression.
    }
    \item{lambda}{
        A non-negative number, a ridge-regression penalty coefficient. 
        It defaults to zero.
    }
    \item{save.auxiliary}{
        A logical value, whether to save the fitted values and the residuals 
        of the auxiliary model that constructs the decorrelated predictors. 
        The default value is FALSE.
    }
    \item{object}{
        Output from dsldLinear function
        [dsldLM]
    }
    \item{xNew}{
        Dataframe to predict new cases for which mean Y | X. Must be in the same format 
        as original data.
        [dataframe]
    }
    \item{...}{Further arguments.}
}

\details{
    dsldFrrm serves as a function wrapper for frrm, which uses the argument 
    definition to accommodate for different definitions of fairness.

    The argument definition is customization to custom definitions of 
    fairness. This function should have signature
    \code{function(model, y, S, U, family)} and return an array with an element
    called \code{"value"} (optionally along with others). The arguments will
    contain the model fitted for the current level of fairness (\code{model}),
    the sanitized response variable (\code{y}), the design matrix for the
    sanitized sensitive attributes (\code{S}), the design matrix for the
    sanitized decorrelated predictors (\code{U}) and the character string
    identifying the family the model belongs to (\code{family}) (Marco Scutari 
    (2023)).

    The arguments yName and sName are used to internally estimate the xName 
    as xName includes everything in the dataset except the yName
    and sName.
}

\value{
    An object of class "dsldFrrm", which has a $data attribute that contains
    an object of class c("frrm", "fair.model").
}

\references{
    https://cran.r-project.org/web/packages/fairml/index.html
}
\author{
    S. Martha, A. Mittal, B. Ouattara, B. Zarate, J. Tran
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
    Komiyama J, Takeda A, Honda J, Shimao H (2018). "Nonconvex Optimization for
    Regression with Fairness Constraints". Proceedints of the 35th International
    Conference on Machine Learning (ICML), PMLR \strong{80}:2737--2746. \cr
    \code{http://proceedings.mlr.press/v80/komiyama18a/komiyama18a.pdf}
}


\examples{    
    data(svcensus) # load dataset
    yName <- "wageinc"
    sName <- "age"

    # sName is age, unfairness parameter set to 0.2
    model <- dsldFrrm(svcensus, yName, sName, 0.2, definition = "sp-komiyama")

    predict(model, svcensus) # generate predictions for new cases
}
