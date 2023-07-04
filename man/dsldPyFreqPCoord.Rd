\name{dsldPyFreqPCoord}
\alias{dsldPyFreqPCoord}

\title{dsldPyFreqPCoord}

\description{
    This function, meant for Python users, uses the rpy2 library to call the R function dsldFreqPCoord() from within a Python environment. 

    The function takes in a list of Python arguments corresponding to the arguments to the R function. These Python arguments are converted to R arguments     
    that are then passed into the R function. This way, the Python user can call the function similar to how an R user would call the function. 
    
    The R function, dsldFreqPCoord(), generates the graph and saves it as an image. This image is automatically opened from the python environment.
    The image is automatically removed shortly after being saved, so as not to take up space.

    For more information on the R function itself, please refer to its man page, which you can access through the command ?dsldFreqPCoord
}

\usage{
    python dsldFreqPCoord_Py_R.py <data> m columns grpName
}

\arguments{
    \item{data}{
        If running from the terminal: A filepath to the csv file which holds your data.
        If running from the Python shell/prompt: A pandas or R dataframe that holds your data
    }
    \item{m}{
        Number of lines to plot for each group. A negative value in conjunction with
        method being "maxdens" indicates that the lowest-density lines are to be plotted.
        If method is "locmax", m is forced to 1.
    }
    \item{columns}{
        Numbers of the columns of x to be displayed. 
        These columns can be numbers or names of the columns, but in both cases, this must be in string format.
        If running from the Terminal (not the python prompt), enter the columns normally, separated by columns, without spaces.
        If running from the Python prompt, enter the columns as an array of strings, meaning you surround either the number or name with quotes ''.
        See the examples section below if you're having trouble understanding these directions.
    }
    \item{grpName}{
        Column number for the grouping variable, if any (if none, all the data is treated
        as a single group); vector or factor.
    }
}

\note{
    The usage of this function requires Python 3.10.
    If the user intends to pass in an R dataframe by using R write.csv to generate a csv file which will be passed into this Python function,
    note that this process may create a csv with an extra column. This extra column has an empty name(“”). When fetching the data using pandas
    data frame and calling the head function of pandas object, you would notice a column with the label “unamed: 0”. This has been known to 
    significantly change the resulting data. In order to fix this issue the entire column with the empty name or “” should be deleted.
}

\author{
    T. Abdullah, S. Martha, B. Ouattara, B. Zarate
}

\examples{    
    % TODO: These examples use 'sex', but pef should be updated to use 'gender'. We seem to be using an outdated pef dataset.

    # These examples use a modified version 'pef' dataset.
    # Due to an error in conversion from Rdata to csv using write.csv, we had to modify the csv file to remove an extra column

    # Example 1a: Using Column Numbers (OS Shell Prompt)
    # Running from the OS shell prompt (Before running, go to /dsld/inst/Python)
    python dsldFreqPCoord_Py_R.py ../../data/pefFixed.csv 10 1,5,6 sex


    # Example 1b: Using Column Numbers (Python Shell Prompt)
    # Running from the Python shell prompt (Before running, go to /dsld/inst/Python)
    python # Open Python shell prompt
    import pandas as pd
    from dsldFreqPCoord_Py_R import dsldPyFreqPCoord
    data = pd.read_csv('../../data/pefFixed.csv')
    # Note that you can input column numbers, but they still have to be in string format. You can input column names as well. 
    dsldPyFreqPCoord(data, 10, ['1','5','6'], 'sex')



    # Here is 2 examples (a and b) on how you can call the function with the same columns, just with names instead of column numbers. 
    # These should yield the same output as above examples 1a and 1b.

    # Example 2a: Using Column Names (OS Shell Prompt)
    # Running from the OS shell prompt (Before running, go to /dsld/inst/Python)
    python dsldFreqPCoord_Py_R.py ../../data/pefFixed.csv 10 age,wageinc,wkswrkd sex


    # Example 2b: Using Column Names (Python Shell Prompt)
    # Running from the Python shell prompt (Before running, go to /dsld/inst/Python)
    python # Open Python shell prompt
    import pandas as pd
    from dsldFreqPCoord_Py_R import dsldPyFreqPCoord
    data = pd.read_csv('../../data/pefFixed.csv')
    # Note that you can input column numbers, but they still have to be in string format. You can input column names as well. 
    dsldPyFreqPCoord(data, 10, ['age','wageinc','wkswrkd'], 'sex')   
}

