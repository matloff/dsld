\name{dsldPyConditDisparity}
\alias{dsldPyConditDisparity}

\title{dsldPyConditDisparity}

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

        If running from OS Shell Prompt, conditions cannot have spaces.
        Each condition should be surrounded by quotes '' so that it is 
        considered a string. Conditions are separated by commas.

        If running from the Python Shell Prompt, enter as an array of 
        strings separated by commas. Spaces are ok.

        See the examples section for differences in entering the condits
        for OS Shell vs Python Shell prompt.
    }
    \item{qeFtn}{
        reg function (defaults only), defaults to qeKNN

        Must be entered as a string. In the OS Shell, you can enter the function 
        name without any special characters, for example: qeKNN.

        In the Python Shell Prompt, you must use quotes, for example: 'qeKNN'.
        
        See the examples for clarification.
    }
    \item{minS}{
        minimum size for an S group to be retained in the analysis, defaults to 
        50
    }
    \item{yLim}{
        a 2-element vector specifying the lower and upper vertical plot limits, 
        defaults to fit data
    }
    \item{useLoess}{
        if TRUE, do Loess smoothing on the regression values, defaults to TRUE
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
    order. To omit an optional arguments please enter the value -1. See example 
    below of OS shell for more clarification. 
    
    In the Python Shell Prompt, this is not required, as you can 
    instead use the convention argName=argValue. In OS Shell Prompt, you cannot 
    specify the argName, only the argValue, so the order must be correct.
}

\author{
    N. Matloff, T. Abdullah, S. Martha, B. Ouattara, B. Zarate
}

\examples{
    # IMPORTANT: Before running any of these examples, go to /dsld/inst/Python

    # IMPORTANT: It is better to run through the Python Shell Prompt (see 
    # example 1b). If you run through the OS Shell Prompt instead, you have to 
    # enter the arguments in their proper order. See the above note for more 
    # information.

    # Example 1: Running from the OS shell
    python dsldConditDisparity_Py_R.py ../../data/compasNumericFixed.csv two_year_recid race age 'priors_count<=4','decile_score>=6' qeGBoost -1 -1 -1

    # Example 2a: Running from the Python shell prompt
    python # Open Python shell prompt
    from dsldConditDisparity_Py_R import dsldPyConditDisparity
    import pandas as pd
    # This csv DOES NOT EXIST: insert the path to a csv file from your machine
    data = pd.read_csv('../../data/compasNumericFixed.csv')
    dsldPyConditDisparity(data, 'two_year_recid', 'race', 'age', ['priors_count <= 4','decile_score>=6'], 'qeGBoost')

    # Example 2b: Running from the Python shell prompt
    python # Open Python shell prompt
    from dsldConditDisparity_Py_R import dsldPyConditDisparity
    import rpy2.robjects as robjects
    robjects.r['data']('svcensus')
    data = robjects.r('svcensus')
    dsldPyConditDisparity(data, "age", "gender", "wageinc", ['age<=60', 'wkswrkd>=25'])
}

