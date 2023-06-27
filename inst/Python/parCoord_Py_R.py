'''
    This file contains the interface code for calling the dsldParCoord from dsld R package.
    The code uses rpy2 to handle dsld functions call from R and pandas library to check if
    users data input is in pandas data frame before doing any computation
'''

import pandas as pd
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from rpy2.robjects import pandas2ri

devtools = importr("devtools")

# This below line may need to be commented
# Would have to use utils package in order to install dsld from CRAN
# We used the commented code below to install dsld from our local computer
#devtools.install_local("/Users/tahaabdullah/Documents/GitHub/dsld")

# dsld package instance. It allows us to call dsld functions inside Python code
dsld = importr("dsld")


# This function converts a pandas data frame into an R data frame
def dsldPandasToRDataframe(pandas_df):
    pandas2ri.activate()
    r_dataframe = pandas2ri.py2rpy(pandas_df)
    return r_dataframe


# This function checks if the data input from the user is in
# R data frame, pandas' data frame or a different type of data frame.
# The function converts the data into r's data frame or
# return -1 which represent an error.
def dsldIsRDataframe(data):
    if isinstance(data, robjects.vectors.DataFrame):
        return data
    elif isinstance(data, pd.DataFrame):
        return dsldPandasToRDataframe(data)
    else:
        # Error case or csv file or other options
        return -1


# dsldParCoord function is called inside this function
# The arguments are passed inside dsldParCoord as r format
# and the result is a graph handled by R.
def dsldPyParCoord(data, m, columns, grpName):
    # Assuming you have the required arguments in Python variables
    r_data = dsldIsRDataframe(data)

    m_r = robjects.IntVector([m])                               # Convert variable name to R character vector
    columns_r = robjects.IntVector([columns])                            # Convert variable name to R character vector
    grpName_r = robjects.StrVector([grpName])   

    dsldParCoord = dsld.dsldParCoord

    dsldParCoord(r_data, m_r, columns_r, grpName_r)

if "__name__" == "__main__":
    args = sys.argv

    file_path = args[1]

    print(file_path)
    data = pd.read_csv(file_path)
    
    dsldPyParCoord(data, int(args[2]), int(args[3]), args[4])
