\name{dsldPyFgrrm}
\alias{dsldPyFgrrm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{dsldPyFggrm}
\description{
    The function takes in a list of Python arguments corresponding to the 
    arguments of the R function. These Python arguments are converted to R 
    arguments that are then passed into the R function. This way, the Python 
    user can call the function similar to how an R user would call the function.
    
    The R function, dsldFgrrm(), generates a list in R. In the python 
    environment, this list is printed.

    For more information on the R function itself, please refer to its man page, 
    which you can access through the command ?dsldFgrrm
}
}
\usage{
dsldPyFgrrm(data, yData, xData, sName, unfairness, definition = "sp-komiyama", family = "binomial", lamda = 0, save = False)
}
\arguments{
  \item{data}{
  The data, in data frame or matrix form.
}
  \item{yData}{
  The yData, in data frame or matrix form.
}
  \item{xData}{
    The xData, in data frame or matrix form.
}
  \item{sName}{
  A numeric matrix or a data frame containing numeric and factor columns; the sensitive attributes.  
}
  \item{unfairness}{
  A positive number in [0, 1], how unfair is the model allowed to be. A value of 0 means the model is completely fair, while a value of 1 means the model is not constrained to be fair at all.
}
  \item{definition}{
  a character string, the label of the definition of fairness used in fitting the model. Currently either "sp-komiyama", "eo-komiyama" or "if-berk". It may also be a function: see below for details.
}
  \item{family}{
  a character string, either "gaussian" to fit a linear regression, "binomial" to fit a logistic regression, "poisson" to fit a log-linear regression, "cox" to fit a Cox proportional hazards regression of "multinomial" to fit a multinomial logistic regression.
}
  \item{lambda}{
  A non-negative number, a ridge-regression penalty coefficient. It defaults to zero.  
}
  \item{save}{
  A logical value, whether to save the fitted values and the residuals of the auxiliary model that constructs the decorrelated predictors. The default value is FALSE.
}
}


\author{
    N. Matloff, T. Abdullah, S. Martha, B. Ouattara, B. Zarate
}
\note{
  The usage of this function requires Python 3.10 and rpy2. It might not work on linux machines.
}

\examples{
  # IMPORTANT: Before running any of these examples, go to /dsld/inst/Python 

#Example 1


python
from dsldFgrrm_Py_R import dsldPyFgrrm
import rpy2.robjects as robjects
robjects.r("library(survival)")
robjects.r['data']('flchain')
data = robjects.r('flchain')
prepData = '''flchain = flchain[complete.cases(flchain), ]'''
robjects.r(prepData)

robjects.r('yData = cbind(time = flchain$futime + 1, status = flchain$death)')
yData = robjects.r('yData')

robjects.r('xData = flchain[, c("sample.yr", "kappa", "lambda", "flc.grp", "creatinine", "mgus","chapter")]')
xData = robjects.r('xData')

#Calling the function
m = dsldPyFgrrm(data, yData, xData, ["age", "sex"], 0.05, family="cox")
print(robjects.r['summary'](m))

