'''
    This file contains the interface code for calling the dsldParCoord from dsld R package.
    The code uses rpy2 to handle dsld functions call from R and pandas library to check if
    users data input is in pandas data frame before doing any computation
'''
import pandas as pd
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from rpy2.robjects import pandas2ri

# For displaying the graph
import os
from PIL import Image
ggplot2 = importr('ggplot2')

# Column vector input
from rpy2.robjects import r

# Cmd line args
import sys

# Displaying the graph: User must have the following installed in R env: Ggally, ggplo2, and graphics
#base = importr('base')
#graphics = importr('graphics')

# Installing DSLD: must install devtools first since that's how we access dsld during development
# devtools = importr("devtools")

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
    
    # TODO: Delete these 3 lines -- they disregard the csv and directly pass in an R dataframe
    # robjects.r['data']('pef')
    # pef = robjects.r['pef']
    # r_data = pef
    # end TODO

    # At this point, data is always intended to be in R dataframe format

    m_r = robjects.IntVector([m])                              # Convert variable name to R character vector
    columns_r = robjects.IntVector([int(x) for x in columns])  # Convert 'columns' to an R integer vector
    grpName_r = robjects.StrVector([grpName])

    # All necessary arguments are in R format at this point

    # Graph plot will be saved as a file
    plot_filename = "plot.png"

    # Calling the R function
    dsld.dsldParCoord(r_data, m_r, columns_r, grpName_r, plot_filename)
    
    # Load and display the saved image in Python
    img = Image.open(plot_filename)
    img.show()
    # Close the displayed image
    img.close()
    # Delete the image file
    os.remove(plot_filename)


# Code to allow users to run this file from the shell
# Use sys to import and handle command line args
if __name__ == "__main__":
    args = sys.argv

    file_path = args[1]

    data = pd.read_csv(file_path)
    
    # split() attempts to comvert Cmd Line string list input into array
    # example: "1,3,5" becomes [1,3,5]
    dsldPyParCoord(data, int(args[2]), sys.argv[3].split(','), args[4])

'''
    Test case
    python parCoord_Py_R.py "/Path/To/pefcsvTAFixed.csv" 10 1,5,6 sex
'''
