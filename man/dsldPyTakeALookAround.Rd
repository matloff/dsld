\name{dsldPyTakeALookAround}
\alias{dsldPyTakeALookAround}

\title{DSLDPyTakeALookAround}

\description{
    This function, meant for Python users, uses the rpy2 library to call the R function dsldTakeALookAround() from within a Python environment. 

    The function takes in a list of Python arguments corresponding to the arguments to the R function. These Python arguments are converted to R arguments     
    that are then passed into the R function. This way, the Python user can call the function similar to how an R user would call the function. 
    
    For more information on the R function itself, please refer to its man page, which you can access through the command ?dsldTakeALookAround
}

\usage{
    python takeALook_Py_R <FilePath> yName sName
}

\arguments{
    \item{data}{
        The data, in data frame or matrix form. If there are indicator
    }
    \item{m}{
        Number of lines to plot for each group. A negative value in conjunction with
        method being "maxdens" indicates that the lowest-density lines are to be plotted.
        If method is "locmax", m is forced to 1.
    }
    \item{columns}{
        Numbers of the columns of x to be displayed.
    }
    \item{grpName}{
        Column number for the grouping variable, if any (if none, all the data is treated
        as a single group); vector or factor.
    }
}

\note{
    If the user intends to pass in an R dataframe by using R write.csv to generate a csv file which will be passed into this Python function,
    note that this process may create a csv with an extra column. This can significantly change the result.
}

\author{
    T. Abdullah, S. Martha, B. Ouattara, B. Zarate
}

\examples{
    # This example uses a csv version of the 'pef' dataset.
    python takeALook_Py_R.py "/Path/To/pefcsvTA.csv" wageinc sex
}

