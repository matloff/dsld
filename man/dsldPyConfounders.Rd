\name{dsldPyConfounders}
\alias{dsldPyConfounders}

\title{dsldPyConfounders}

\description{
    This function, meant for Python users, uses the rpy2 library to call the R 
    function dsldConfounders() from within a Python environment. 

    The function takes in a list of Python arguments corresponding to the 
    arguments to the R function. These Python arguments are converted to R 
    arguments that are then passed into the R function. This way, the Python 
    user can call the function similar to how an R user would call the function. 

    For more information on the R function itself, please refer to its man page, 
    which you can access through the command ?dsldConfounders
}

\usage{
    dsldPyConfounders(data, sName)
}

\arguments{
    \item{data}{
        If running from the OS shell prompt: A filepath to the csv file which 
        holds your data
        
        If running from the Python shell prompt: A pandas or R dataframe that 
        holds your data
    }
    \item{sName}{
        Column number for the grouping variable, if any (if none, all the data 
        is treated as a single group); vector or factor.
    }
}

\note{
    The usage of this function requires Python 3.10. It might not work on linux
    machines.
}    

\note{ 
    If the user intends to pass in an R dataframe by using the R function 
    write.csv to convert from rdata to csv may create a csv with an extra 
    column. This extra column has an empty name(“”). This change to the data has 
    been known to significantly change the results when fetching with pandas. In 
    order to fix this issue, the entire column with the empty name or “” should 
    be deleted.
}

\note{
    It is better to run through the Python Shell Prompt. If you run through the 
    OS Shell Prompt instead, you have to enter the arguments in their proper 
    order -- in the Python Shell Prompt, this is not required, as you can 
    instead use the convention argName=argValue. In OS Shell Prompt, you cannot 
    specify the argName, only the argValue, so the order must be correct.
}

\author{
    T. Abdullah, A. Ashok, J. Tran
}

\examples{    
    # IMPORTANT: Before running any of these examples, go to /dsld/inst/Python

    # IMPORTANT: It is better to run through the Python Shell Prompt. If you 
    # run through the OS Shell Prompt instead, you have to enter the arguments 
    # in their proper order. See the above note for more information.

    # Example 1: Python Shell Prompt
    python # Open Python shell prompt
    from dsldConfounders_Py_R import dsldPyConfounders
    import rpy2.robjects as robjects
    robjects.r['data']('svcensus')
    data = robjects.r('svcensus')
    dsldPyConfounders(data, "educ")
}

