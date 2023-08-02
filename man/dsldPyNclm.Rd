\name{dsldPyNclm}
\alias{dsldPyNclm}

\title{dsldPyNclm}

\description{
    This function, meant for Python users, uses the rpy2 library to call the R 
    function dsldNclm() from within a Python environment. 

    %Need to edit this to dsldNclm()
    The function takes in a list of Python arguments corresponding to the 
    arguments to the R function. These Python arguments are converted to R 
    arguments that are then passed into the R function. This way, the Python 
    user can call the function similar to how an R user would call the function.
    
    The R function, dsldNclm(), generates a s3 R object. In the python 
    environment, the s3 R object is printed.

    For more information on the R function itself, please refer to its man page, 
    which you can access through the command ?dsldNclm
}

\usage{
    dsldPyNclm(data, yName, sName, unfairness, covfun = R_NULL, lamda = 0, save = False)
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
    The usage of this function requires Python 3.10 and rpy2. It might not work 
    on linux machines.
}    

\author{
    N. Matloff, T. Abdullah, S. Martha, B. Ouattara, B. Zarate
}

\examples{
    # IMPORTANT: Before running any of these examples, go to /dsld/inst/  

    # ********************************************************
    # The following example 1  show how you can call the function with 
    # the custom function in this case, cov. 
    # ********************************************************

    # Example 1:
    python # Open Python shell prompt
    from dsldNclm_Py_R import dsldPyNclm
    import rpy2.robjects as robjects
    robjects.r['data']('communities.and.crime')
    data = robjects.r('communities.and.crime')
    nclmR = dsldPyNclm(data, "ViolentCrimesPerPop", ["racepctblack","PctForeignBorn"], 0.05, robjects.r('cov'))
    print(robjects.r['summary'](nclmR))

    # ********************************************************
    # The following example 2 shows how you can call the function with 
    # the default cov function in this case, 4. 
    # ********************************************************

    # Example 2:
    python # Open Python shell prompt
    from dsldNclm_Py_R import dsldPyNclm
    import rpy2.robjects as robjects
    robjects.r['data']('law.school.admissions')
    data = robjects.r('law.school.admissions')
    nclmR = dsldPyNclm(data, "ugpa", ["age", "race1"], 0.05)
    print(robjects.r['summary'](nclmR))
}