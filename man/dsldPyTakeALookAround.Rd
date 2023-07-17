\name{dsldPyTakeALookAround}
\alias{dsldPyTakeALookAround}

\title{DSLDPyTakeALookAround}

\description{
    This function, meant for Python users, uses the rpy2 library to call the R 
    function dsldTakeALookAround() from within a Python environment. 

    The function takes in a list of Python arguments corresponding to the 
    arguments to the R function. These Python arguments are converted to R 
    arguments that are then passed into the R function. This way, the Python 
    user can call the function similar to how an R user would call the function.
    
    The R function, dsldTakeALook(), generates a dataframe. In the python 
    environment, the r dataframe is converted into a pandas dataframe. The user can 
    view the dataframe by printing the result.

    For more information on the R function itself, please refer to its man page, 
    which you can access through the command ?dsldTakeALookAround
}

\usage{
    dsldPyTakeALookAround(data, yName, sName, maxFeatureSetSize = (ncol(data) - 2))
}

\arguments{
    \item{data}{
        If running from the OS shell prompt: A filepath to the csv file which 
        holds your data
        
        If running from the Python shell prompt: A filepath to the csv file which 
        holds your data or R dataframe that holds your data
    }
    \item{yName}{
        the name of the column of the response variable 
    }
    \item{sName}{
        the name of the column of the sensitive variable, an R factor

        This should be numeric -- if it is not numeric, the code will convert 
        the column to numeric values.
    }
    \item{maxFeatureSetSize}{
        maximum number of combinations of features to be included in the 
        dataframe
        
        default argument set as: maxFeatureSetSize = (ncol(data) - 2)
    }
}

\note{
    The usage of this function requires Python 3.10 and rpy2.
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
    # IMPORTANT: Before running any of these examples, go to /dsld/inst/
    
    # IMPORTANT: It is better to run through the Python Shell Prompt (see 
    # example 1b). If you run through the OS Shell Prompt instead, you have to 
    # enter the arguments in their proper order. See the above note for more 
    # information.

    % I think the below statement should be removed
    # Examples 1 and 2 use a csv version of the 'pef' or the 'svcensus' dataset.   

    # ********************************************************
    # The following example 1 (a, b, and c) show how you can call the function with 
    # the default maxFeatureSetSize = (ncol(data) - 2). 
    # ********************************************************

    # Example 1a: Using default maxFeatureSetSize (OS shell prompt)
    # NOTE: The OS shell can only take in a csv dataset
    python dsldTakeALook_Py_R.py ../../data/svcensusFixed.csv wageinc gender

    # Example 1b: Using default maxFeatureSetSize (Python shell prompt)
    python # Open Python shell prompt
    from dsldTakeALook_Py_R import dsldPyTakeALookAround
    import pandas as pd
    data = pd.read_csv('../../data/svcensusFixed.csv')
    result = dsldPyTakeALookAround(data, 'wageinc', 'gender')
    print(result)

    # Example 1c: Using Rdata input data instead of CSV input data (Python shell prompt)
    python # Open Python shell prompt
    from dsldTakeALook_Py_R import dsldPyTakeALookAround
    import rpy2.robjects as robjects
    robjects.r['data']('svcensus')
    data = robjects.r('svcensus')
    result = dsldPyTakeALookAround(data, 'wageinc', 'gender')
    print(result)

    # ********************************************************
    # The following example 2 (a, b, and c) show how you can call the function with 
    # a custom maxFeatureSetSize in this case, 4. 
    # ********************************************************

    # Example 2a: Using maxFeatureSetSize (OS shell prompt)
    # NOTE: The OS shell can only take in a csv dataset
    python dsldTakeALook_Py_R.py ../../data/svcensusFixed.csv wageinc gender 4

    # Example 2b: Using maxFeatureSetSize (Python shell prompt)
    python # Open Python shell prompt
    from dsldTakeALook_Py_R import dsldPyTakeALookAround
    import pandas as pd
    data = pd.read_csv('../../data/svcensusFixed.csv')
    result = dsldPyTakeALookAround(data, 'wageinc', 'gender', '4')
    print(result)

    # Example 2c: Using Rdata input data instead of CSV input data (Python shell prompt)
    python # Open Python shell prompt
    from dsldTakeALook_Py_R import dsldPyTakeALookAround
    import rpy2.robjects as robjects
    robjects.r['data']('svcensus')
    data = robjects.r('svcensus')
    result = dsldPyTakeALookAround(data, 'wageinc', 'gender', '4')
    print(result)
}

