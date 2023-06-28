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
    python parCoord_Py_R <FilePath> 10 columns grpName
}

\arguments{
    \item{data}{
        dataset in dataframe form
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
    note that this process may create a csv with an extra column. This has been known to significantly change the resulting graph.
}

\author{
    T. Abdullah, S. Martha, B. Ouattara, B. Zarate
}

\examples{    
    # This example uses a modified version 'pef' dataset.
    # Due to an error in conversion from Rdata to csv using write.csv, we had to modify the csv file to remove an extra column
    python parCoord_Py_R.py "/Path/To/pefcsvTAFixed.csv" 10 1,5,6 sex
}

