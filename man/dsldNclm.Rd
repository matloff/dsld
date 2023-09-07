\name{dsldNclm}
\alias{dsldNclm}

\title{dsldNclm}

\description{
    This is a wrapper for the Nclm function from the fairml package  
    (link in references). The fairml package bases their fair regression 
    model based on the nonconvex Optimization for Regression with Fairness 
    Constraints from Komiyama et al. (2018) (link in See also).
}
\usage{
    dsldNclm <- function (data, yName, sName, unfairness, covfun, 
                     lambda = 0, save.auxiliary = FALSE)
}

\arguments{
    \item{data}{
        The data, in data frame or matrix form.
    }
    \item{yName}{
        Name of the response variable.
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
  \item{covfun}{
        A function computing covariance matrices. It defaults to the cov() function 
        from the stats package.  
    }
    \item{lambda}{
        A non-negative number, a ridge-regression penalty coefficient. It defaults 
        to zero.  
    }
    \item{save.auxiliary}{
        A logical value, whether to save the fitted values and the residuals 
        of the auxiliary model that constructs the decorrelated predictors. 
        The default value is FALSE.
    } 
}

\note{
    The usage of this function requires cccp package.
} 

\details{
  % Probably need to edit this section
    dsldNclm serves as a function wrapper for Nclm, which bounds the proportion 
    of the variance that is explained by the sensitive attributes over 
    the total explained variance.

    The arguments yName and sName are used to internally estimate the xName 
    as xName includes everything in the dataset except the yName
    and sName.
    
    The covfun argument can take in a custom function to compute the covariance
    matrices used in the constrained optimization (Komiyama et al. (2018)).
}

\value{
    dsldNclm returns an object of class c("nclm", "fair.model") from the
    fairml package.
}

\references{
    https://cran.r-project.org/web/packages/fairml/index.html
}

\author{
    S. Martha, A. Mittal, B. Ouattara, B. Zarate
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
    Komiyama J, Takeda A, Honda J, Shimao H (2018). "Nonconvex Optimization for
    Regression with Fairness Constraints". Proceedints of the 35th International
    Conference on Machine Learning (ICML), PMLR \strong{80}:2737--2746. \cr
    \code{http://proceedings.mlr.press/v80/komiyama18a/komiyama18a.pdf}
}

\examples{
  data(svcensus)
  data <- svcensus
  yName <- "wageinc"
  sName <- "gender"
  
  model <- dsldNclm(data, yName, sName, 0)
  predict(model, data)
}

