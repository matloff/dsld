'''
    This file contains the interface code for calling the dsldTakeALook from dsld R package.
    The code uses rpy2 to handle dsld functions call from R and pandas library to check if
    users data input is in pandas data frame before doing any computation
'''

import pandas as pd
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from rpy2.robjects import pandas2ri

# For accessing cmd line args
import sys

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


# dsldTakeALookAround function is called inside this function
# The default value of the maxFeatureSetSize is set to None
# If No input was received from the user for that argument,
# we call dsldTakeAlookAround without that parameter. Otherwise,
# we precise the parameter in the dsldTakeALookAround function.
# The arguments are passed inside dsldTakeALookAround as r format
# and the result is returned as Python's pandas data frame.
def dsldPyTakeALookAround(data, yName, sName, maxFeatureSetSize=None):
    # Assuming you have the required arguments in Python variables
    r_data = dsldIsRDataframe(data)

    yName_r = robjects.StrVector([yName])                               # Convert variable name to R character vector
    sName_r = robjects.StrVector([sName])                               # Convert variable name to R character vector

    dsldTakeALookAround = dsld.dsldTakeALookAround

    if maxFeatureSetSize is None:
        df_r = dsldTakeALookAround(r_data, yName_r, sName_r)
    else:
        maxFeatureSetSize_r = robjects.IntVector([maxFeatureSetSize])    # Convert number to R integer vector
        df_r = dsldTakeALookAround(r_data, yName_r, sName_r, maxFeatureSetSize_r)

    #Result stored in a python pandas' dataframe
    df_py = pandas2ri.rpy2py_dataframe(df_r)

    return df_py

# Code to allow users to run this file from the shell
# Use sys to import and handle command line args
if __name__ == "__main__":
    args = sys.argv

    file_path = args[1]

    data = pd.read_csv(file_path)

    if len(args) != 5:
        dsldPyTakeALookAround(data, args[2], args[3])
    else:
        dsldPyTakeALookAround(data, args[2], args[3], int(args[4]))
