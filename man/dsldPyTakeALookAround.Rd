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
    python dsldTakeALook_Py_R.py <data> yName sName maxFeatureSetSize = (ncol(data) - 2)
}

\arguments{
    \item{data}{
        If running from the terminal: A filepath to the csv file which holds your data.
        If running from the Python shell/prompt: A pandas or R dataframe that holds your data
    }
    \item{yName}{
        as in qeML functions, the response variable
    }
    \item{sName}{
        name of the sensitive variable, an R factor
    }
    \item{maxFeatureSetSize}{
        maximum number of combinations of features to be included in the dataframe; 
        default argument set as: maxFeatureSetSize = (ncol(data) - 2)
    }
}

\note{
    If the user intends to pass in an R dataframe by using R write.csv to generate a csv file which will be passed into this Python function,
    note that this process may create a csv with an extra column. This can significantly change the result.
}

\note{
    The usage of this function requires Python 3.10.
}

\author{
    T. Abdullah, S. Martha, B. Ouattara, B. Zarate
}

\examples{
    % TODO: These examples use 'sex', but pef should be updated to use 'gender'. We seem to be using an outdated pef dataset.
    # IMPORTANT: Before running any of these examples, go to /dsld/inst/Python

    # These examples use a csv version of the 'pef' dataset.
    # Due to an error in conversion from Rdata to csv using write.csv, we had to modify the csv file to remove an extra column

    # Example 1a: Running from the OS shell prompt
    python dsldTakeALook_Py_R.py ../../data/pefFixed.csv wageinc sex

    # Example 1b: Running from the Python shell prompt
    python # Open Python shell prompt
    import pandas as pd
    from dsldTakeALook_Py_R import dsldPyTakeALookAround
    data = pd.read_csv('../../data/pefFixed.csv')
    result = dsldPyTakeALookAround(data, 'wageinc', 'sex')
    print(result)
}

