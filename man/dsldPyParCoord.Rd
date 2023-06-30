\name{dsldPyParCoord}
\alias{dsldPyParCoord}

\title{DSLDPyParCoord}

\description{
    This function, meant for Python users, uses the rpy2 library to call the R function dsldParCoord() from within a Python environment. 

    The function takes in a list of Python arguments corresponding to the arguments to the R function. These Python arguments are converted to R arguments     
    that are then passed into the R function. This way, the Python user can call the function similar to how an R user would call the function. 
    
    The R function, dsldParCoord(), generates the graph and saves it as an image. This image is automatically opened from the python environment.
    The image is automatically removed shortly after being saved, so as not to take up space.

    For more information on the R function itself, please refer to its man page, which you can access through the command ?dsldParCoord
}

\usage{
    python dsldParCoord_Py_R.py <FilePath> 10 columns grpName
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
        Numbers of the columns of x to be displayed. This can be entered as either a string or integers of the 
        columns separated by commas but not a mixture of both.
    }
    \item{grpName}{
        Column number for the grouping variable, if any (if none, all the data is treated
        as a single group); vector or factor.
    }
}

\note{
    If the user intends to pass in an R dataframe by using R write.csv to generate a csv file which will be passed into this Python function,
    note that this process may create a csv with an extra column. This has been known to significantly change the resulting graph.
}

\note{
    The usage of this function requires Python 3.10.
}

\author{
    T. Abdullah, S. Martha, B. Ouattara, B. Zarate
}

\examples{    
    # This runnning from the Shell example uses a modified version 'pef' dataset.
    # Due to an error in conversion from Rdata to csv using write.csv, we had to modify the csv file to remove an extra column
    python dsldParCoord_Py_R.py "/Path/To/pefcsvTAFixed.csv" 10 1,5,6 sex

    # This running from Python example uses a csv version of the 'pef' dataset.
    # Make sure you in the correct directory before running these commands.
    import pandas as pd
    from from dsldParCoord_Py_R import dsldPyParCoord
    data = pd.read_csv('csvFileName.csv')
    result = dsldPyParCoord(data, 10, [1,5,6], 'sex')
}

