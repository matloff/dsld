\name{dsldPyConditDisparity}
\alias{dsldPyConditDisparity}

\title{DSLDPyConditDisparity}

\description{
    This function, meant for Python users, uses the rpy2 library to call the R 
    function dsldConditDisparity() from within a Python environment. 

    The function takes in a list of Python arguments corresponding to the 
    arguments to the R function. These Python arguments are converted to R 
    arguments that are then passed into the R function. This way, the Python 
    user can call the function similar to how an R user would call the function. 
    
    The R function, dsldConditDisparity(), generates a graph. In the python 
    environment, an image of this graph is generated, saved, and automatically 
    opened in an image viewer. The image is automatically removed shortly after 
    being saved (so as not to take up space), but the user can view the graph as 
    long as they keep the image viewer open.

    For more information on the R function itself, please refer to its man page, 
    which you can access through the command ?dsldConditDisparity
}

\usage{
    dsldPyConditDisparity(data, yName, sName, xName, condits, qeFtn, minS, yLim, useLoess)
}

\arguments{
    \item{data}{
        If running from the OS shell prompt: A filepath to the csv file which 
        holds your data
        
        If running from the Python shell prompt: A pandas or R dataframe that 
        holds your data
    }
    \item{yName}{
        as in qeML functions, predictive variable, must be numeric
    }
    \item{sName}{
        name of the sensitive variable, an R factor
    }
    \item{xName}{
        name of a numeric column to take comparisons with
    }
    \item{condits}{
        an R vector; each component is a character string for an R logical 
        expression representing a desired condition; these must NOT involve 
        sName
    }
    \item{qeFtn}{
        reg function (defaults only), defaults to qeKNN (IN PROGRESS)
    }
    \item{minS}{
        minimum size for an S group to be retained in the analysis, defaults to 
        50 (IN PROGRESS)
    }
    \item{yLim}{
        a 2-element vector specifying the lower and upper vertical plot limits, 
        defaults to fit data (IN PROGRESS)
    }
    \item{useLoess}{
        if TRUE, do Loess smoothing on the regression values, defaults to TRUE 
        (IN PROGRESS)
    }
}

\note{
    The usage of this function requires Python 3.10.
    
    
    If the user intends to pass in an R dataframe by using the R function 
    write.csv to convert from rdata to csv may create a csv with an extra 
    column. This extra column has an empty name(“”). This change to the data has 
    been known to significantly change the results when fetching with pandas. In 
    order to fix this issue, the entire column with the empty name or “” should 
    be deleted.
}

\author{
    T. Abdullah, S. Martha, B. Ouattara, B. Zarate
}

\examples{
    # IMPORTANT: Before running any of these examples, go to /dsld/inst/Python

    # Example 1a: Running from the OS shell prompt
    python dsldConditDisparity_Py_R.py ../../data/compasNumericFixed.csv two_year_recid race age 'priors_count<=4','decile_score>=6'

    # Example 1b: Running from the Python shell prompt
    python # Open Python shell prompt
    from dsldConditDisparity_Py_R import dsldPyConditDisparity
    import pandas as pd
    data = pd.read_csv('../../data/compasNumericFixed.csv')
    dsldPyConditDisparity(data, 'two_year_recid', 'race', 'age', ['priors_count <= 4','decile_score>=6'])
}

