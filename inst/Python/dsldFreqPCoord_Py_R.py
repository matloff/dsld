'''
    This file contains the interface code for calling the dsldFreqPCoord from dsld R package.
    The code uses rpy2 to handle dsld functions call from R and pandas library to check if
    users data input is in pandas data frame before doing any computation
'''
import pandas as pd
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from rpy2.robjects import pandas2ri
from Utils import dsld_Rpy2_IsRDataframe

# For displaying the graph
import os
import stat
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

# dsldFreqPCoord function is called inside this function
# The arguments are passed inside dsldFreqPCoord as r format
# and the result is a graph handled by R.
def dsldPyFreqPCoord(data, m, columns, grpName):
    # Assuming you have the required arguments in Python variables
    r_data = dsld_Rpy2_IsRDataframe(data) # At this point, data is always intended to be in R dataframe format

    # Columns can be entered in shell as all strings or all ints
    if all(column.isdigit() for column in columns):
        columns_r = robjects.IntVector([int(x) for x in columns])  # Convert 'columns' to an R integer vector
    else:
        columns_r = robjects.StrVector(columns)
    m_r = robjects.IntVector([m])                              # Convert variable name to R character vector
    grpName_r = robjects.StrVector([grpName])

    # All necessary arguments are in R format at this point

    # Graph plot will be saved as a file
    plot_filename = os.getcwd()+"/freqp_coord.png"
    print(plot_filename)
    # Calling the R function
    dsld.dsldFreqPCoord(r_data, m_r, columns_r, grpName_r, plot_filename)

    # Current image file permissions: -rw-r--r-- (Owner can read/write, Group can read, Others can read)
    os.chmod(plot_filename, stat.S_IRUSR | stat.S_IRGRP | stat.S_IROTH | stat.S_IWUSR | stat.S_IWGRP | stat.S_IWOTH)
    
    # Load and display the saved image in Python
    image = Image.open(plot_filename)
    image.show()

    # Close the displayed image
    # image.close()
    # Delete the image file
    # os.remove(plot_filename)


# Code to allow users to run this file from the shell
# Use sys to import and handle command line args
if __name__ == "__main__":
    args = sys.argv

    file_path = args[1]

    data = pd.read_csv(file_path)
    
    # split() attempts to comvert Cmd Line string list input into array
    # example: "1,3,5" becomes ['1','3','5']
    dsldPyFreqPCoord(data, int(args[2]), sys.argv[3].split(','), args[4])

'''
    # Test case
    # Running from the OS Shell (Before running, go to /dsld/inst/Python)
    python dsldFreqPCoord_Py_R.py ../../data/pefFixed.csv 10 1,5,6 sex


    # Running from the Python Shell Prompt (Before running, go to /dsld/inst/Python)
    python # Open Python shell prompt
    from dsldFreqPCoord_Py_R import dsldPyFreqPCoord
    import pandas as pd
    data = pd.read_csv('../../data/pefFixed.csv')
    dsldPyFreqPCoord(data, 10, ['age','wageinc','wkswrkd'], 'sex')   
'''
