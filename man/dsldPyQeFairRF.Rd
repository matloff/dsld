\name{dsldPyQeFairRF}
\alias{dsldPyQeFairRF}

\title{dsldPyQeFairRF}

\description{
    This function, meant for Python users, uses the rpy2 library to call the R 
    function dsldQeFairRF() from within a Python environment. 

    The function takes in a list of Python arguments corresponding to the 
    arguments to the R function. These Python arguments are converted to R 
    arguments that are then passed into the R function. This way, the Python 
    user can call the function similar to how an R user would call the function. 
    
    The R function, dsldQeFairRF(), generates a testAcc. In the python 
    environment, this testAcc is printed.

    For more information on the R function itself, please refer to its man page, 
    which you can access through the command ?dsldQeFairRF
}

\usage{
    dsldPyQeFairRF(data, yName, sName = R_NULL, deweightPars = R_NULL, nTree = 500, 
                   minNodeSize = 10, mtry = R_NULL, yesYVal = "", holdout = R_NULL)
}

\arguments{
    \item{data}{
        The dataframe, training set; class labels col is a factor; other
        columns may be factors
    }
    \item{yName}{
        The column name for outcome variable; vector indicates
        regression, factor classification 
    }
    \item{sName}{
        The sensitive variable, sensitive variables to be excluded from 
        the ML analysis.
    }
    \item{deweightPars}{
        to-do
    }
    \item{nTree}{
        Number of trees.
    }
    \item{minNodeSize}{
        Minimum number of data points in a tree node.
    }
    \item{mtry}{
        Number of variables randomly tried at each split.
    }
    \item{yesYVal}{
        Y value to be considered "yes," to be coded 1 rather than 0.
    } 
    \item{holdout}{
        The size of holdout set, if any
    } 
}

\note{
    The usage of this function requires Python 3.10 and rpy2. It might not work on linux
    machines.
}    

\author{
    N. Matloff, S. Martha, A. Mittal, B. Ouattara, B. Zarate
}

\examples{    
    # IMPORTANT: Before running any of these examples, go to /dsld/inst/Python

    # Example 1:
    python # Open Python shell prompt
    from dsldQeFairRF_Py_R import dsldPyQeFairRF
    import rpy2.robjects as robjects
    # Set the arguments
    robjects.r['data']('svcensus')
    data = robjects.r('svcensus')
    # Call the function
    resR = dsldPyQeFairRF(data, "wageinc", sName = "gender", deweightPars = {"occ":0.2})
    robjects.r.assign("resR", resR)
    robjects.r('print(resR$testAcc)') # print the result
}

