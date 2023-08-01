'''
    This file contains the interface code for calling the dsldFreqPCoord from dsld R package.
    The code uses rpy2 to handle dsld functions call from R and pandas library to check if
    users data input is in pandas data frame before doing any computation
'''
from Utils import dsld_Rpy2_IsRDataframe, R_NULL
import pandas as pd
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from rpy2.robjects import r
from rpy2.robjects import pandas2ri
import os # Image removal
from PIL import Image # For displaying the graph
import sys # Cmd line args

# dsld package instance. It allows us to call dsld functions inside Python code
dsld = importr("dsld")

# This function checks if dsld function arguments are in proper format for computation
def validate_input(m, columns, sName, method, faceting, k, klm, keepidxs, plotidxs):
    if type(m) != int:
        print('Error: m must be an int type. Entered type:', type(m))
        exit(1)

    if (
            columns is not R_NULL and 
            not all(isinstance(column, str) for column in columns) and
            not all(isinstance(column, int) for column in columns)
        ):
        print('Error: columns must be a list of strings or ints. Entered type:', type(columns))
        exit(1)

    if sName != R_NULL and type(sName) != str and type(sName) != int:
        print('Error: sName must be a string or intg. Entered type:', type(sName))
        exit(1)

    if type(method) != str:
        print('Error: method must be a string type. Entered type:', type(method))
        exit(1)

    if type(faceting) != str:
        print('Error: faceting must be a string type. Entered type:', type(faceting))
        exit(1)

    if type(k) != int:
        print('Error: k must be an int type. Entered type:', type(k))
        exit(1)

    if klm != R_NULL and type(klm) != int:
        print('Error: klm must be an int type. Entered type:', type(klm))
        exit(1)

    if keepidxs != R_NULL and type(keepidxs) != int:
        print('Error: keepidxs must be an int type. Entered type:', type(keepidxs))
        exit(1)

    if type(plotidxs) != bool:
        print('Error: plotidxs must be an bool type. Entered type:', type(plotidxs))
        exit(1) 

# dsldFreqPCoord function is called inside this function
# The arguments are passed inside dsldFreqPCoord as r format
# and the result is a graph handled by R.
def dsldPyFreqPCoord(data, m, columns = R_NULL, sName = R_NULL, method = "maxdens", faceting = "vert", k = 50, klm = R_NULL, keepidxs = R_NULL, plotidxs = False):
    # ************************** ARGUMENTS *******************************************
    
    # Type validation of everything except for data
    validate_input(m,columns,sName,method,faceting,k,klm,keepidxs,plotidxs)

    # Data conversion handled by Utils function
    r_data = dsld_Rpy2_IsRDataframe(data) # At this point, data is always intended to be in R dataframe format
    
    m_r = robjects.IntVector([m])

    if columns == R_NULL:
        robjects.r.assign("r_data", r_data)  
        robjects.r('columns_r <- 1:ncols(r_data)')
        columns_r = robjects.r("columns_r")
    else:
        if all(isinstance(column, str) for column in columns):
            columns_r = robjects.StrVector(columns)
        elif all(isinstance(column, int) for column in columns):
            columns_r = robjects.IntVector(columns)

    # sName can be either an int (col number) or str (col name)
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

    # All necessary arguments are in R format at this point
    # ************************** END ARGUMENTS *******************************************

    # ************************** RETURN VALUE *******************************************
    # Graph plot will be saved as a file
    plot_filename = "freqp_coord.png"

    # Calling the R function
    dsld.dsldFreqPCoord(r_data, m_r, columns_r, sName_r, method_r, faceting_r, k_r, klm_r, keepidxs_r, plotidxs_r, R_NULL, plot_filename)

    # Load and display the saved image in Python
    image = Image.open(plot_filename)
    image.show()

    # Close the displayed image
    image.close()
    # Delete the image file
    os.remove(plot_filename)
    # ************************** END FUNCTION *******************************************


#************************** OS SHELL FUNCTIONALITY *************************************
# TODO: OS Shell functionality is INCOMPLETE; Need to implement NULL values
# OS Shell currently assumes that all args are entered by user and that sName and Columns are strings
if __name__ == "__main__":
    args = sys.argv

    file_path = args[1]
    data = pd.read_csv(file_path)
    
    # split() attempts to comvert Cmd Line string list input into array
    # example: "1,3,5" becomes ['1','3','5']

    # -1 is an int null value
    if int(args[9]) == -1:
        dsldPyFreqPCoord(
            data, int(args[2]), sys.argv[3].split(','), args[4], args[5], args[6], 
            int(args[7]), int(args[8]), R_NULL, bool(args[10])
        )
    else:
        dsldPyFreqPCoord(
            data, int(args[2]), sys.argv[3].split(','), args[4], args[5], args[6], 
            int(args[7]), int(args[8]), int(args[9]), bool(args[10])
        )   
#************************** END OS SHELL *******************************************

'''
    # Test cases: Before running, go to /dsld/inst/Python

    # Running from the OS Shell
    python dsldFreqPCoord_Py_R.py ../../data/svcensusFixed.csv 10 age,wageinc,wkswrkd gender maxdens vert 50 250 -1 False

    # Running from the Python Shell Prompt - CSV input
    python # Open Python shell prompt
    from dsldFreqPCoord_Py_R import dsldPyFreqPCoord
    import pandas as pd
    # This csv DOES NOT EXIST: insert the path to a csv file from your machine
    data = pd.read_csv('../../data/svcensusFixed.csv')
    dsldPyFreqPCoord(data, 10, ['age','wageinc','wkswrkd'], 'gender')   

    # Running from the Python Shell Prompt - Rdata input
    python # Open Python shell prompt
    from dsldFreqPCoord_Py_R import dsldPyFreqPCoord
    import rpy2.robjects as robjects
    robjects.r['data']('svcensus')
    data = robjects.r('svcensus')
    dsldPyFreqPCoord(data, 10, ['age','wageinc','wkswrkd'], 'gender')   
'''
