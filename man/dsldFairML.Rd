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
    functions provide wrappers from the fairML package (link in references). 
}

\usage{
dsldFrrm(data, yName, sName, unfairness, definition = "sp-komiyama", 
    lambda = 0, save.auxiliary = FALSE)
dsldFgrrm(data, yName, sName, unfairness, definition = "sp-komiyama", family = "binomial", 
    lambda = 0, save.auxiliary = FALSE)
dsldNclm(data, yName, sName, unfairness, covfun, lambda = 0, save.auxiliary = FALSE)
dsldZlm(data, yName, sName, unfairness)
dsldZlrm(data, yName, sName, unfairness)
\method{summary}{dsldFairML}(object,...)
\method{predict}{dsldFairML}(object,newx,...)
}

\arguments{
    \item{data}{
        Dataframe, training set. 
    }
    \item{yName}{
        Name of the response variable column. 
    }
    \item{sName}{
        Name(s) of the sensitive attribute column(s). 
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
        "if-berk".
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
        An object returned by the dsld-fairML wrapper.  
    }
    \item{newx}{
        New data to be predicted. Must be in the same format as original data.
    }
    \item{...}{
    Further arguments.
    }
}

\details{
Via FairML: 

Fairml implements key algorithms for learning machine learning models while enforcing fairness with respect 
to a set of observed sensitive (or protected) attributes. The following algorithms are available: \cr

    1. \code{nclm():} the non-convex formulation of fair linear regression model from Komiyama et al. (2018). \cr
    2. \code{frrm():} the fair (linear) ridge regression model from Scutari, Panero and Proissl (2022). \cr
    3. \code{fgrrm():} thefair generalized (linear) ridge regression model from Scutari, Panero and Proissl 
    (2022), supporting the Gaussian, binomial, Poisson, multinomial and Cox (proportional hazards) families. \cr
    4. \code{zlrm():} the fair logistic regression with covariance constraints from Zafar et al. (2019). \cr
    5. \code{zlrm():} a fair linear regression with covariance constraints following Zafar et al. (2019)

Different fairness methods can also be employed in dsldFrrm() and dsldFgrrm(): \cr
    1. \code{"sp-komiyama":} the statistical parity fairness constraint from Komiyama et al. \cr
    2. \code{"eo-komiyama":} the analogous equality of opportunity constraint built on the proportion 
    of variance (or deviance) explained by sensitive attributes. \cr
    3. \code{"if-berk":} the individual fairness constraint from Berk et al. (2017) adapted in Scutari, Panero and Proissl. \cr

More details can be found in the references. 
}

\value{
    An object of class "dsldFairML", which includes the model information, yName, and sName.
}

\author{
    S. Martha, A. Mittal, B. Ouattara, B. Zarate, J. Tran
}

\references{
    https://cran.r-project.org/web/packages/fairml/index.html
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~

Berk R, Heidari H, Jabbari S, Joseph M, Kearns M, Morgenstern J, Neel S, Roth A (2017). "A Convex Framework for Fair Regression". FATML. \cr
\code{https://www.fatml.org/media/documents/convex_framework_for_fair_regression.pdf}

Komiyama J, Takeda A, Honda J, Shimao H (2018). "Nonconvex Optimization for
Regression with Fairness Constraints". Proceedints of the 35th International Conference on Machine Learning (ICML), 
PMLR \strong{80}:2737--2746. \cr 
\code{http://proceedings.mlr.press/v80/komiyama18a/komiyama18a.pdf}

Scutari M, Panero F, Proissl M (2022). "Achieving Fairness with a Simple Ridge Penalty". Statistics and Computing, 32, 77. \cr
\code{https://link.springer.com/content/pdf/10.1007/s11222-022-10143-w.pdf}

Zafar BJ, Valera I, Gomez-Rodriguez M, Gummadi KP (2019). "Fairness Constraints: a Flexible Approach for 
Fair Classification". Journal of Machine Learning Research, 30:1-42. \cr
\code{https://www.jmlr.org/papers/volume20/18-262/18-262.pdf}
}


\examples{ 
    \donttest{
    data(svcensus) 
    data(compas1)
    data(law.school.admissions)

    # unfairness parameter set at 0.2 for all examples

    # dsldFrrm 
    yName <- "wageinc"
    sName <- "age"
    frrmOut <- dsldFrrm(svcensus, yName, sName, 0.2, definition = "sp-komiyama") 
    summary(frrmOut)
    predict(frrmOut, svcensus[1:10,]) 

    # dsldFgrrm
    yName <- "two_year_recid"
    sName <- "age"
    fgrrmOut <- dsldFgrrm(compas1, yName, sName, 0.2, definition = "sp-komiyama")  
    summary(fgrrmOut)
    predict(fgrrmOut, compas1[c(1:10),]) 

    # dsldNclm
    yName <- "lsat"
    sName <- "race1"
    nclmOut <- dsldNclm(law.school.admissions, yName, sName, 0.2)
    summary(nclmOut)
    predict(nclmOut, law.school.admissions[c(1:10),]) 

    # dsldZlm
    yName <- "wageinc"
    sName <- "age"
    zlmOut <- dsldZlm(svcensus, yName, sName, 0.2) 
    summary(zlmOut)
    predict(zlmOut, svcensus[c(1:10),])

    # dsldZlrm
    yName <- "two_year_recid"
    sName <- "age"
    zlrmOut <- dsldZlrm(compas1, yName, sName, 0.2) 
    summary(zlrmOut)
    predict(zlrmOut, compas1[c(1:10),])}
}
