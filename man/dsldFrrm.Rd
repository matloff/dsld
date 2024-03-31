\name{dsldFrrm}
\alias{dsldFrrm}

\title{dsldFrrm}

\description{
    This is a wrapper for the frrm function from the fairML package
    (link in references).
}

\usage{
    dsldFrrm(data, yName, sName, unfairness,
                     definition = "sp-komiyama", lambda = 0, 
                     save.auxiliary = FALSE)
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
    \item{definition}{
        A character string, the label of the definition of fairness used in 
        fitting the model. Currently either "sp-komiyama", "eo-komiyama" or 
        "if-berk". It may also be a custom function: see below for details.
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
    model <- dsldFrrm(data, yName, sName, 0.2, definition = "sp-komiyama")

    predict(model, svcensus) # generate predictions for new cases
}

