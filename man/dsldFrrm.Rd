\name{dsldFrrm}
\alias{dsldFrrm}

\title{dsldFrrm}

\description{
    This is a wrapper for the Frrm function from the fairML pacakage
    (link in references).
}

\usage{
    dsldFrrm(yName, xName, sName, unfairness, definition = "sp-komiyama",
         lambda = 0, save.auxiliary = FALSE)
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
      the sensitive attributes
  }
  \item{unfairness}{
      A positive number in [0, 1], how unfair is the model allowed to be. 
      A value of 0 means the model is completely fair, while a value of 1 means 
      the model is not constrained to be fair at all.
  }
  \item{definition}{
      A character string, the label of the definition of fairness used in 
      fitting the model. Currently either "sp-komiyama", "eo-komiyama" or 
      "if-berk". It may also be a function: see below for details.
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
  \item{save.auxillary}{
      A logical value, whether to save the fitted values and the residuals 
      of the auxiliary model that constructs the decorrelated predictors. 
      The default value is FALSE.
  }
}

\details{
    % Probably need to edit this section
    dsldFrrm() and dsldFgrrm() can accommodate different definitions of fairness, 
    which can be selected via the definition argument. The labels for the built-in 
    definitions are: "sp-komiyama" for the same definition of fairness as 
    dsldNclm(): the model bounds the proportion of the variance that is explained 
    by the sensitive attributes over the total explained variance. This falls 
    within the definition of statistical parity. "eo-komiyama" enforces equality 
    of opportunity in a similar way: it regresses the fitted values against the 
    sensitive attributes and the response, and it bounds the proportion of the 
    variance explained by the sensitive attributes over the total explained 
    variance in that model. "if-berk" enforces individual fairness by penalizing 
    the model for each pair of observations with different values of the sensitive 
    attributes and different responses. Users may also pass a function via the 
    definition argument to plug custom fairness definitions. This function should 
    have signature function(model, y, S, U, family) and return an array with an element called "value" (optionally along with others). The arguments will contain the model fitted for the current level of fairness (model), the sanitized response variable (y), the design matrix for the sanitized sensitive attributes (S), the design matrix for the sanitized decorrelated predictors (U) and the character string identifying the family the model belongs to (family).

    The algorithm works like this:
        regresses the predictors against the sensitive attributes;
        constructs a new set of predictors that are decorrelated from the 
        sensitive attributes using the residuals of this regression;
        regresses the response against the decorrelated predictors and the 
        sensitive attributes; while using a ridge penalty to control the 
        proportion of variance the sensitive attributes can explain with respect 
        to the overall explained variance of the model.

    Both sensitive and predictors are standardized internally before estimating 
    the regression coefficients, which are then rescaled back to match the 
    original scales of the variables.

    dsldFgrrm() is the extension of dsldFrrm() to generalized linear models, 
    currently implementing linear (family = "gaussian") and 
    logistic (family = "binomial") regressions. fgrrm() is equivalent to frrm() 
    with family = "gaussian". The definition of fairness are identical between 
    dsldFrrm() and dsldFgrrm().
}

\value{
  frrm() returns an object of class c("frrm", "fair.model"). fgrrm() returns an
   object of class c("fgrrm", "fair.model").
}

\references{
    https://cran.r-project.org/web/packages/fairml/index.html
}
\author{
    S. Martha, A. Mittal, B. Ouattara, J. Tran, B. Zarate
}

\examples{
    # Example 1
    library(dsld)
    library(fairml)
    data(communities.and.crime)
    # short-hand variable names.
    cc = communities.and.crime[complete.cases(communities.and.crime), ]
    r = cc[, "ViolentCrimesPerPop"]
    s = cc[, c("racepctblack", "PctForeignBorn")]
    p = cc[, setdiff(names(cc), c("ViolentCrimesPerPop", names(s)))]
    m = dsldFrrm(response = r, sensitive = s, predictors = p, unfairness = 0.05)
    summary(m)

    # Example 2
    library(dsld)
    library(fairml)
    data(law.school.admissions)
    # short-hand variable names.
    ll = law.school.admissions
    r = ll[, "ugpa"]
    s = ll[, c("age", "race1")]
    p = ll[, setdiff(names(ll), c("ugpa", "age", "race1"))]
    m = dsldFrrm(response = r, sensitive = s, predictors = p, unfairness = 0.05)
    summary(m)
}

