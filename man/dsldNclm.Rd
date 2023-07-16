\name{dsldNclm}
\alias{dsldNclm}
\title{nclm in dsld}
\description{Fair regression model based on nonconvex optimization from 
Komiyama et al. (2018).
}
\usage{dsldNclm(response, predictors, sensitive, unfairness, covfun, 
                lambda = 0, save.auxiliary = FALSE) 
}
\arguments{
  \item{response}{
a numeric vector, the response variable.
  }
  \item{predictors}{
a numeric matrix or a data frame containing numeric and factor columns; 
the predictors.
  }
  \item{sensitive}{
a numeric matrix or a data frame containing numeric and factor columns; 
the sensitive attributes.  
  } 
  \item{unfairness}{
a positive number in [0, 1], how unfair is the model allowed to be. A value of 
0 means the model is completely fair, while a value of 1 means the model is not constrained to be fair at all.
  } 
  \item{covfun}{
a function computing covariance matrices. It defaults to the cov() function 
from the stats package.  
  }
  \item{lambda}{
a non-negative number, a ridge-regression penalty coefficient. It defaults 
to zero.  
  }
  \item{save.auxiliary}{
a logical value, whether to save the fitted values and the residuals of the 
auxiliary model that constructs the decorrelated predictors. The default value 
is FALSE.
  } 
  
}
\details{
dsldNclm() defines fairness as statistical parity. The model bounds the 
proportion of the variance that is explained by the sensitive attributes over 
the total explained variance.

The algorithm proposed by Komiyama et al. (2018) works like this:

regresses the predictors against the sensitive attributes;

constructs a new set of predictors that are decorrelated from the sensitive 
attributes using the residuals of this regression;

regresses the response against the decorrelated predictors and the sensitive 
attributes, while

bounding the proportion of variance the sensitive attributes can explain with 
respect to the overall explained variance of the model.

Both sensitive and predictors are standardized internally before estimating the 
regression coefficients, which are then rescaled back to match the original 
scales of the variables. response is only standardized if it has a variance 
smaller than 1, as that seems to improve the stability of the solutions 
provided by the optimizer (as far as the data included in fairml are concerned).

The covfun argument makes it possible to specify a custom function to compute 
the covariance matrices used in the constrained optimization. Some examples 
are the kernel estimators described in Komiyama et al. (2018) and the
shrinkage estimators in the corpcor package.
}
\value{
dsldNclm() returns an object of class c("nclm", "fair.model").
}
\references{
Komiyama J, Takeda A, Honda J, Shimao H (2018). "Nonconvex Optimization for Regression with Fairness Constraints". Proceedints of the 35th International Conference on Machine Learning (ICML), PMLR 80:2737–2746.
http://proceedings.mlr.press/v80/komiyama18a/komiyama18a.pdf~
}
\author{
Marco Scutari
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Example 1
data(communities.and.crime)
# short-hand variable names.
cc = communities.and.crime[complete.cases(communities.and.crime), ]
r = cc[, "ViolentCrimesPerPop"]
s = cc[, c("racepctblack", "PctForeignBorn")]
p = cc[, setdiff(names(cc), c("ViolentCrimesPerPop", names(s)))]

m = dsldNclm(response = r, sensitive = s, predictors = p, unfairness = 0.05)
summary(m)

#Error: ‘there is no package called ‘cccp’
#solution: Install 'cccp' through CRAN

# Example 2

data(law.school.admissions)

# short-hand variable names.
ll = law.school.admissions
r = ll[, "ugpa"]
s = ll[, c("age", "race1")]
p = ll[, setdiff(names(ll), c("ugpa", "age", "race1"))]
m = dsldNclm(response = r, sensitive = s, predictors = p, unfairness = 0.05)
summary(m)
}
