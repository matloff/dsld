'''
    This file contains the interface code for calling the dsldFreqPCoord from dsld R package.
    The code uses rpy2 to handle dsld functions call from R and pandas library to check if
    users data input is in pandas data frame before doing any computation
'''
from Utils import dsld_Rpy2_IsRDataframe
import pandas as pd
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from rpy2.robjects import r
from rpy2.robjects import pandas2ri
import os # Image removal
from PIL import Image # For displaying the graph
import sys # Cmd line args

# Installing DSLD: must install devtools first since that's how we access dsld during development
# devtools = importr("devtools")

# This below line may need to be commented
# Would have to use utils package in order to install dsld from CRAN
# We used the commented code below to install dsld from our local computer
#devtools.install_local("/Users/tahaabdullah/Documents/GitHub/dsld")

# dsld package instance. It allows us to call dsld functions inside Python code
dsld = importr("dsld")

# For handling null arguments
R_NULL = robjects.NULL

# dsldFreqPCoord function is called inside this function
# The arguments are passed inside dsldFreqPCoord as r format
# and the result is a graph handled by R.
def dsldPyFreqPCoord(data, m, columns = R_NULL, sName = R_NULL, method = "maxdens", faceting = "vert", k = 50, klm = R_NULL, keepidxs = R_NULL, plotidxs = False, randclrs = False):
    #************************** ARGUMENTS *******************************************
    r_data = dsld_Rpy2_IsRDataframe(data) # At this point, data is always intended to be in R dataframe format
    m_r = robjects.IntVector([m])  

    # Columns can be entered in shell as all strings or all ints
    if columns == R_NULL:
        robjects.r.assign("r_data", r_data)  
        robjects.r('columns_r <- 1:ncols(r_data)')
        columns_r = robjects.r("columns_r")
    else:
        if all(isinstance(column, str) for column in columns):
            columns_r = robjects.StrVector(columns)
        elif all(isinstance(column, int) for column in columns):
            columns_r = robjects.IntVector(columns)
        else:
            print("ERROR: Columns should be either all integers or all strings")

    if sName == R_NULL:
        sName_r = sName
    elif isinstance(sName, str):
        sName_r = robjects.StrVector([sName])
    elif isinstance(sName, int):
        sName_r = robjects.IntVector([sName])

    method_r = robjects.StrVector([method])
    faceting_r = robjects.StrVector([faceting])
    k_r = robjects.IntVector([k])

    if klm == R_NULL:
        klm = 5*k # Couldn't be done in function definition due to k not being recognized
    klm_r = robjects.IntVector([klm])

    if keepidxs == R_NULL:
        keepidxs_r = keepidxs
    else:
        keepidxs_r = robjects.IntVector([keepidxs])
    
    plotidxs_r = robjects.BoolVector([plotidxs])
    randclrs_r = robjects.BoolVector([randclrs])

    # All necessary arguments are in R format at this point
    #************************** END ARGUMENTS *******************************************

    #************************** RETURN VALUE *******************************************
    # Graph plot will be saved as a file
    plot_filename = "freqp_coord.png"

    # Calling the R function
    dsld.dsldFreqPCoord(r_data, m_r, columns_r, sName_r, method_r, faceting_r, k_r, klm_r, keepidxs_r, plotidxs_r, cls_r, plot_filename)

    # Load and display the saved image in Python
    image = Image.open(plot_filename)
    image.show()

    # Close the displayed image
    image.close()
    # Delete the image file
    os.remove(plot_filename)
    #************************** END FUNCTION *******************************************


#************************** OS SHELL FUNCTIONALITY *************************************
# TODO: OS Shell functionality is INCOMPLETE; Need to reimplement columns parsing (see git 7/15/23)
if __name__ == "__main__":
    args = sys.argv

    file_path = args[1]

    data = pd.read_csv(file_path)
    
    # split() attempts to comvert Cmd Line string list input into array
    # example: "1,3,5" becomes ['1','3','5']
    dsldPyFreqPCoord(data, int(args[2]), sys.argv[3].split(','), args[4])
#************************** END OS SHELL *******************************************

'''
    # Test cases: Before running, go to /dsld/inst/Python

    # Running from the OS Shell (Before running, go to /dsld/inst/Python)
    python dsldFreqPCoord_Py_R.py ../../data/pefFixed.csv 10 1,4,5 gender

    # Running from the Python Shell Prompt - CSV input
    python # Open Python shell prompt
    from dsldFreqPCoord_Py_R import dsldPyFreqPCoord
    import pandas as pd
    data = pd.read_csv('../../data/pefFixed.csv')
    dsldPyFreqPCoord(data, 10, ['age','wageinc','wkswrkd'], 'gender')   

    # Running from the Python Shell Prompt - Rdata input
    python # Open Python shell prompt
    from dsldFreqPCoord_Py_R import dsldPyFreqPCoord
    import rpy2.robjects as robjects
    robjects.r['data']('pef')
    data = robjects.r('pef')
    dsldPyFreqPCoord(data, 10, ['age','wageinc','wkswrkd'], 'gender')   
'''
