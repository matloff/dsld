'''
    This file contains the interface code for calling the dsldTakeALook from dsld R package.
    The code uses rpy2 to handle dsld functions call from R and pandas library to check if
    users data input is in pandas data frame before doing any computation
'''
import sys
import pandas as pd
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from rpy2.robjects import pandas2ri
from Utils import dsld_Rpy2_IsRDataframe, print_takeALookAround_usage

INPUT_1 = 1
INPUT_2 = 2
INPUT_3 = 3
INPUT_4 = 4
MAX_ARGS = 5

# This below line may need to be commented
# Would have to use utils package in order to install dsld from CRAN
# We used the commented code below to install dsld from our local computer
#devtools.install_local("/Users/tahaabdullah/Documents/GitHub/dsld")

# dsld package instance. It allows us to call dsld functions inside Python code
dsld = importr("dsld")

# dsldTakeALookAround function is called inside this function
# The default value of the maxFeatureSetSize is set to None
# If No input was received from the user for that argument,
# we call dsldTakeAlookAround without that parameter. Otherwise,
# we precise the parameter in the dsldTakeALookAround function.
# The arguments are passed inside dsldTakeALookAround as r format
# and the result is returned as Python's pandas data frame.
def dsldPyTakeALookAround(data, yName, sName, maxFeatureSetSize=None):
    # Assuming you have the required arguments in Python variables
    r_data = dsld_Rpy2_IsRDataframe(data)

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

'''
    For testing: Make sure to print the return value of the function
'''

if __name__ == "__main__":
    args = sys.argv

    try:
        file_path = args[INPUT_1]

        data = pd.read_csv(file_path)

        if len(args) - 1 > MAX_ARGS:
            print("Error: Too many arguments")
            print_takeALookAround_usage()
            exit(1)
        if len(args) - 1 < MAX_ARGS - 2:
            print("ERROR: more arguments are required")
            print_takeALookAround_usage()
            exit(1)

        if len(args) != MAX_ARGS:
            print(dsldPyTakeALookAround(data, args[INPUT_2], args[INPUT_3]))
        else:
            try:
                print(dsldPyTakeALookAround(data, args[INPUT_2], args[INPUT_3], int(args[INPUT_4])))
            except ValueError:
                print("Error: 5th input must be of type int. Entered: ", args[INPUT_4])
    except FileNotFoundError:
        print("Error: File not found")

'''
    # Test Cases: Before running, go to /dsld/inst/Python

    # Running from the OS Shell - CSV input
    python dsldTakeALook_Py_R.py ../../data/pefFixed.csv wageinc gender

    # Running from the Python Shell Prompt - CSV input
    python # Open python prompt
    from dsldTakeALook_Py_R import dsldPyTakeALookAround
    import pandas as pd
    data = pd.read_csv('../../data/pefFixed.csv')
    result = dsldPyTakeALookAround(data, 'wageinc', 'gender')
    print(result)

    # Running from the Python Shell Prompt - Rdata input
    python # Open Python shell prompt
    from dsldFreqPCoord_Py_R import dsldPyFreqPCoord
    import rpy2.robjects as robjects
    robjects.r['data']('pef')
    data = robjects.r('pef')
    result = dsldPyTakeALookAround(data, 'wageinc', 'gender')
    print(result)
'''
