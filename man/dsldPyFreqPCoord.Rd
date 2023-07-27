\name{dsldPyFreqPCoord}
\alias{dsldPyFreqPCoord}

\title{dsldPyFreqPCoord}

\description{
    This function, meant for Python users, uses the rpy2 library to call the R 
    function dsldFreqPCoord() from within a Python environment. 

    The function takes in a list of Python arguments corresponding to the 
    arguments to the R function. These Python arguments are converted to R 
    arguments that are then passed into the R function. This way, the Python 
    user can call the function similar to how an R user would call the function. 
    
    The R function, dsldFreqPCoord(), generates a graph. In the python 
    environment, an image of this graph is generated, saved, and automatically 
    opened in an image viewer. The image is automatically removed shortly after 
    being saved (so as not to take up space), but the user can view the graph as 
    long as they keep the image viewer open.

    For more information on the R function itself, please refer to its man page, 
    which you can access through the command ?dsldFreqPCoord
}

\usage{
    dsldPyFreqPCoord(data,m,columns=1:ncol(data),sName=robjects.NULL,
          method="maxdens",faceting="vert",k=50,klm=5*k,
                keepidxs=robjects.NULL,plotidxs=False)
}

\arguments{
    \item{data}{
        If running from the OS shell prompt: A filepath to the csv file which 
        holds your data
        
        If running from the Python shell prompt: A pandas or R dataframe that 
        holds your data
    }
    \item{m}{
        Number of lines to plot for each group. A negative value in conjunction 
        with method being "maxdens" indicates that the lowest-density lines are 
        to be plotted. If method is "locmax", m is forced to 1.
    }
    \item{columns}{
        Numbers of the columns of x to be displayed. 
        These columns can be column numbers or column names.
        
        If running from the OS Shell, enter the columns names separated by 
        commas, without spaces.
        
        If running from the Python Shell prompt, enter the columns as an array 
        of strings (uses '' quotes) or an array of ints.
        
        See the examples section for differences in entering the condits
        for OS Shell vs Python Shell prompt.
    }
    \item{sName}{
        Column number for the grouping variable, if any (if none, all the data 
        is treated as a single group); vector or factor.
    }
    \item{method}{
        What to display: "maxdens" for plotting the most
        (or least) typical lines, "locmax" for cluster hunting, or 
        "randsamp" for plotting a random sample of lines.
    }
    \item{faceting}{
        How to display groups, if present.  Use "vert" for
        vertical stacking of group plots, "horiz" for horizontal ones, or
        "none" to draw all lines in one plot, color-coding by group.
    }
    \item{k}{
        Number of nearest neighbors to use for density estimation.
    }
    \item{klm}{
        If method is "locmax", number of nearest neighbors to 
        use for finding local maxima for cluster hunting. Generally needs
        to be much larger than \code{k}, to avoid "noise fitting."
    }
    \item{keepidxs}{
        If not NULL, the indices of the rows of \code{x} that 
        are plotted will be stored in a component \code{idxs} of the
        return value.  The rows themselves will be in a component
        \code{xdisp}, ordered by \code{x[,dispcols[1]}.]
    }
    \item{plotidxs}{
        If TRUE, lines in the display will be annotated 
        with their case numbers, i.e. their row numbers within \code{x}.  
        Use only with small values of \code{m}, as overplotting may occur.
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
    T. Abdullah, S. Martha, B. Ouattara, B. Zarate
}

\examples{    
    # IMPORTANT: Before running any of these examples, go to /dsld/inst/Python

    # IMPORTANT: It is better to run through the Python Shell Prompt (see 
    # examples 1b and 2b). If you run through the OS Shell Prompt instead, you 
    # have to enter the arguments in their proper order. See the above note for 
    # more information.

    # These examples use a modified version of the 'pef' dataset.   
    % Due to an error in conversion from Rdata to csv using write.csv, we had to 
    % modify the csv file to remove an extra column

    # Example 1: OS Shell Prompt
    python dsldFreqPCoord_Py_R.py ../../data/svcensusFixed.csv 10 age,wageinc,wkswrkd gender maxdens vert 50 250 -1 False

    # Example 2a: Using Column Numbers (Python Shell Prompt)
    python # Open Python shell prompt
    from dsldFreqPCoord_Py_R import dsldPyFreqPCoord
    import pandas as pd
    data = pd.read_csv('../../data/svcensusFixed.csv')
    dsldPyFreqPCoord(data, 10, [1,4,5], 'gender')

    # Example 2b: Using Column Names (Python Shell Prompt)
    python # Open Python shell prompt
    from dsldFreqPCoord_Py_R import dsldPyFreqPCoord
    import pandas as pd
    data = pd.read_csv('../../data/svcensusFixed.csv')
    dsldPyFreqPCoord(data, 10, ['age','wageinc','wkswrkd'], 'gender')   

    # Example 3: Using Rdata input data instead of CSV input data
    python # Open Python shell prompt
    from dsldFreqPCoord_Py_R import dsldPyFreqPCoord
    import rpy2.robjects as robjects
    # Must import dsld library in R env
    robjects.r['data']('svcensus')
    data = robjects.r('svcensus')
    dsldPyFreqPCoord(data, 10, ['age','wageinc','wkswrkd'], 'gender')
}

