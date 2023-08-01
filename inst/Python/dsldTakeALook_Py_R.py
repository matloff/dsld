'''
    This file contains the interface code for calling the dsldTakeALook from dsld R package.
    The code uses rpy2 to handle dsld functions call from R and pandas library to check if
    users data input is in pandas data frame before doing any computation
'''
from Utils import dsld_Rpy2_IsRDataframe, print_takeALookAround_usage, R_NULL
import pandas as pd
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from rpy2.robjects import pandas2ri
import sys # Cmd line args

# Installing DSLD: must install devtools first since that's how we access dsld during development
# devtools = importr("devtools")

# This below line may need to be commented
# Would have to use utils package in order to install dsld from CRAN
# We used the commented code below to install dsld from our local computer
#devtools.install_local("/Users/tahaabdullah/Documents/GitHub/dsld")

# dsld package instance. It allows us to call dsld functions inside Python code
dsld = importr("dsld")

# This function checks if dsld function arguments are in proper format for computation
# Returns an Error if valid input is not provided
def validate_input(yName, sName, maxFeatureSetSize):
    if type(yName) != list and type(yName) != str:
        print('Error: yName must be a list of string. Entered type:', type(yName))
        exit(1)

    if type(sName) != list and type(sName) != str:
        print('Error: sName must be a list of string. Entered type:', type(sName))
        exit(1)

    if maxFeatureSetSize is not R_NULL and type(maxFeatureSetSize) != int:
        print('Error: maxFeatureSetSize must be an integer. Entered type:', type(maxFeatureSetSize))
        exit(1)

# dsldTakeALookAround function is called inside this function
# The default value of the maxFeatureSetSize is set to None
# If No input was received from the user for that argument,
# we call dsldTakeAlookAround without that parameter. Otherwise,
# we precise the parameter in the dsldTakeALookAround function.
# The arguments are passed inside dsldTakeALookAround as r format
# and the result is returned as Python's pandas data frame.
def dsldPyTakeALookAround(data, yName, sName, maxFeatureSetSize = R_NULL):
    # Assuming you have the required arguments in Python variables
    r_data = dsld_Rpy2_IsRDataframe(data)

    robjects.r.assign("r_data", r_data)                                 # Assign the 'r_data' variable to R

    # Calls the function validate_input() to check if all the inputs are valid
    validate_input(yName, sName, maxFeatureSetSize)

    # sName must be numeric. If column values are not numeric then
    # the if block below takes care of the conversion
    if bool(robjects.r('is.numeric')(f'r_data${sName}')[0]) == False:
        robjects.r(f"r_data${sName} <- as.numeric(as.character(r_data${sName}) == r_data${sName}[1])") 
        r_data = robjects.r("r_data")                                   # Assign the modified R dataframe back to Python  

    yName_r = robjects.StrVector([yName])                               # Convert variable name to R character vector
    sName_r = robjects.StrVector([sName])                               # Convert variable name to R character vector

    dsldTakeALookAround = dsld.dsldTakeALookAround

    if maxFeatureSetSize == R_NULL:
        maxFeatureSetSize_r = robjects.r.ncol(r_data)[0] - 2
    else:
        maxFeatureSetSize_r = robjects.IntVector([maxFeatureSetSize])    # Convert number to R integer vector
       
    # All necessary arguments are in R format at this point
    #************************** END ARGUMENTS *******************************************

    #************************** RETURN VALUE *******************************************
    # Calling the R function which returns a r dataframe
    df_r = dsldTakeALookAround(r_data, yName_r, sName_r, maxFeatureSetSize_r)

    # Result stored in a python pandas' dataframe
    df_py = pandas2ri.rpy2py_dataframe(df_r)

    return df_py
    #************************** END FUNCTION *******************************************

'''
    For testing: Make sure to print the return value of the function
'''

#************************** OS SHELL FUNCTIONALITY *************************************
if __name__ == "__main__":
    args = sys.argv

    try:
        file_path = args[1]

        data = pd.read_csv(file_path)

        MAX_ARGS = 5

        if len(args) - 1 > MAX_ARGS:
            print("Error: Too many arguments")
            print_takeALookAround_usage()
            exit(1)
        if len(args) - 1 < MAX_ARGS - 2:
            print("ERROR: more arguments are required")
            print_takeALookAround_usage()
            exit(1)

        if len(args) != MAX_ARGS:
            print(dsldPyTakeALookAround(data, args[2], args[3]))
        else:
            try:
                print(dsldPyTakeALookAround(data, args[2], args[3], int(args[4])))
            except ValueError:
                print("Error: 5th input must be of type int. Entered: ", args[4])
    except FileNotFoundError:
        print("Error: File not found")
#************************** END OS SHELL *******************************************

'''
    # Test Cases: Before running, go to /dsld/inst/Python

    # Running from the OS Shell (Before running, go to /dsld/inst/Python)
    python dsldTakeALook_Py_R.py ../../data/svcensusFixed.csv wageinc gender

    # Running from the Python Shell Prompt - CSV input
    python # Open python prompt
    from dsldTakeALook_Py_R import dsldPyTakeALookAround
    import pandas as pd
    # This csv DOES NOT EXIST: insert the path to a csv file from your machine
    data = pd.read_csv('../../data/svcensusFixed.csv')
    result = dsldPyTakeALookAround(data, 'wageinc', 'gender')
    print(result)

    # Running from the Python Shell Prompt - Rdata input
    python # Open Python shell prompt
    from dsldTakeALook_Py_R import dsldPyTakeALookAround
    import rpy2.robjects as robjects
    robjects.r['data']('svcensus')
    data = robjects.r('svcensus')
    result = dsldPyTakeALookAround(data, 'wageinc', 'gender')
    print(result)
'''
