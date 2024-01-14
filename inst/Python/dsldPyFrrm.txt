\name{dsldPyFrrm}
\alias{dsldPyFrrm}

\title{dsldPyFrrm}

\description{
    This is a wrapper for the PyFrrm function from the fairml package  
    (link in references). The fairml package bases their fair regression 
    model based on the nonconvex Optimization for Regression with Fairness 
    Constraints from Komiyama et al. (2018) (link in See also).
}
\usage{
    dsldPyFrrm(data, yName, sName, unfairness, definition = "sp-komiyama", 
                lamda = 0, save = False)
}

\arguments{
    \item{data}{
        The data, in data frame or matrix form.
    }
    \item{yName}{
        Name of the response variable.
    }
    \item{sName}{
        Name of the sensitive variable.
    } 
    \item{unfairness}{
        A positive number in [0, 1], how unfair is the model allowed to be. 
        A value of 0 means the model is completely fair, while a value of 1 means 
        the model is not constrained to be fair at all.
    } 
  \item{definition}{
        A character string, the label of the definition of fairness used in 
        fitting the model. Currently either "sp-komiyama", "eo-komiyama" or 
        "if-berk". It may also be a custom function: see below for details.  
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
    dsldPyFrrm serves as a function wrapper for Frrm, which bounds the proportion 
    of the variance that is explained by the sensitive attributes over 
    the total explained variance.

    The arguments yName and sName are used to internally estimate the xName 
    as xName includes everything in the dataset except the yName
    and sName.
    
}

\value{
    dsldPyFrrm returns an object of class c("Frrm", "fair.model") from the
    fairml package.
}

\references{
    https://cran.r-project.org/web/packages/fairml/index.html
}

\author{
    S. Martha, A. Mittal, B. Ouattara, B. Zarate
}

\examples{
    # Example 1
    # Load the required libraries
    from dsldFrrm_Py_R import dsldPyFrrm
    import rpy2.robjects as robjects
    # Define the arguments
    robjects.r['data']('communities.and.crime')
    data = robjects.r('communities.and.crime')
    # Call the function
    PyFrrmR = dsldPyFrrm(data, "ViolentCrimesPerPop", ["racepctblack","PctForeignBorn"], 0.05)
    # Print the result
    print(robjects.r['summary'](PyFrrmR))

    # Example 2
    # Load the required libraries
    from dsldFrrm_Py_R import dsldPyFrrm
    import rpy2.robjects as robjects
    # Define the arguments
    robjects.r['data']('law.school.admissions')
    data = robjects.r('law.school.admissions')
    # Call the function
    PyFrrmR = dsldPyFrrm(data, "ugpa", ["age", "race1"], 0.05)
    # Print the result
    print(robjects.r['summary'](PyFrrmR))
}