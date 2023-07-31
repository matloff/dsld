'''
    This file contains the interface code for calling the dsldConditDisparity from dsld R package.
    The code uses rpy2 to handle dsld functions call from R and pandas library to check if
    users data input is in pandas data frame before doing any computation
'''
from Utils import dsld_Rpy2_IsRDataframe, changeBg, R_NULL, ERROR
import sys
import os
import pandas as pd
from PIL import Image
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from rpy2.robjects import r

''' 
    Importing instance of dsld package to allow dsld function calls.
    Importing qeML for qeML function assignments to dsldConditDisparity
    function qeFtn argument.
    Importing grDevices to allow plot saving for display to user.
'''
dsld = importr("dsld")
qeML = importr("qeML")
grdevices = importr('grDevices')

# This function checks if dsld function arguments are in proper format for computation
def validate_input(yName, sName, xName, condits, minS, yLim, useLoess):
    if type(yName) != str and (type(yName) == list and not all(isinstance(y, str) for y in yName)):
        print('Error: yName must be a list of string. Entered type:', type(yName))
        exit(ERROR)

    if type(sName) != str and (type(sName) == list and not all(isinstance(s, str) for s in sName)):
        print('Error: sName must be a list of string. Entered type:', type(sName))
        exit(ERROR)

    if type(xName) != str and (type(xName) == list and not all(isinstance(x, str) for x in xName)):
        print('Error: xName must be a list of string. Entered type:', type(xName))
        exit(ERROR)

    if type(condits) != str and (type(condits) == list and not all(isinstance(c, str) for c in condits)):
        print('Error: condits must be a list of string. Entered type:', type(condits))
        exit(ERROR)

    if type(minS) != int:
        print('Error: minS must be an int type. Entered type:', type(minS))
        exit(ERROR)

    if type(useLoess) != bool:
        print('Error: useLoess must be boolean type. Entered:', type(useLoess))
        exit(ERROR)

    if yLim is not R_NULL and type(yLim) != list:
        print('Error: yLim must be a list of integers. Entered type:', type(yLim))
        exit(ERROR)
    
    if yLim is not R_NULL and len(yLim) != 2:
        print('Error: yLim must be a list of 2 integers(lower and upper bound). Size Entered:', len(yLim))
        exit(ERROR)

    
# This is the interface function for R's dsldConditDisparity function
# The arguments are converted into R data type before calling dsldConditDisparity function
# This function uses qeML's qeKNN function as default argument for qeFtn
def dsldPyConditDisparity(data, yName, sName, xName, condits, qeFtn="qeKNN", minS=50, yLim=R_NULL, useLoess=True):
    #************************** ARGUMENTS *******************************************
    r_data = dsld_Rpy2_IsRDataframe(data)                       # Convert the data to R format if not already

    # Validate the input to make sure that they ready to be converted to R format
    validate_input(yName, sName, xName, condits, minS, yLim, useLoess)

    robjects.r.assign("r_data", r_data)                         # Assign the 'r_data' variable to R
    robjects.r(f"r_data${sName} <- as.factor(r_data${sName})")  # Call as.factor() on the 'sName' column
    r_data = robjects.r("r_data")                               # Assign the modified R dataframe back to Python

    yName_r = robjects.StrVector([yName])                       # Convert variable name to R character vector
    sName_r = robjects.StrVector([sName])                       # Convert variable name to R character vector
    xName_r = robjects.StrVector([xName])                       # Convert variable name to R character vector
    condits_r = robjects.StrVector([cond for cond in condits])  # Convert variable name to R character vector
    minS_r = robjects.IntVector([minS])                         # Convert variable name to R;s number type

    # Checks if the qeFtn function exists in qeML package before calling it
    if hasattr(qeML, qeFtn) and callable(getattr(qeML, qeFtn)):
        # Call the function
        qeFtn_r = getattr(qeML, qeFtn)
    else:
        print(f"ERROR: qeML do not have function name: '{qeFtn}'\n")
        return

    yLim_r = R_NULL

    # If yLim is not of value NULL converts the elements to float
    if yLim is not R_NULL:
        try:
            yLim_r = robjects.FloatVector([float(x) for x in yLim])
        except ValueError:
            print("ERROR: Please enter a list of two integers separated by comma\n")
            exit(ERROR)

    useLoess_r = robjects.BoolVector([useLoess])                # Convert variable name to R boolean type

    # All necessary arguments are in R format at this point
    #************************** END ARGUMENTS *******************************************
    

    #************************** RETURN VALUE *******************************************
    dsld.dsldConditDisparity(r_data, yName_r, sName_r, xName_r, condits_r, qeFtn_r, minS_r, yLim_r, useLoess_r)

    # Copy and saves the image as condits_disparity_plot.png
    plot_filename = "condits_disparity_plot.png"
    grdevices.dev_copy(device=r.png, filename=plot_filename)
    grdevices.dev_off()

    # Set background of image saved to white
    changeBg(plot_filename)

    # Load the image file in Python
    image = Image.open(plot_filename)
    image.show()                                                # Display the plot using the default image viewer

    # Close the displayed image
    image.close()
    # Delete the image file
    os.remove(plot_filename)
    #************************** END OF FUNCTION *******************************************


#************************** OS SHELL FUNCTIONALITY *************************************
if __name__ == "__main__":
    # Checks if user entered correct number of arguments
    if len(sys.argv) != 10:
        print('All 9 arguments are required. Entered: \n', len(sys.argv))
        exit(ERROR)

    # Get all required argumets from the command line
    _,file_path, yName, sName, xName, condits, qeFtn, minS, yLim, useLoess = sys.argv

    # ******************** Convert data to right data type ********************
    if minS == '-1':
        minS = 50
    else:
        minS = int(minS)

    if qeFtn == '-1':
        qeFtn = "qeKNN"

    if yLim != '-1':
        yLim = yLim.split(',')
    else:
        yLim = R_NULL

    if useLoess == '-1' or useLoess == 'True':
        useLoess = True
    else:
        useLoess = False

    try:
        data = pd.read_csv(file_path)
    except FileNotFoundError:
        print(f"ERROR: File not found")
        exit(ERROR)
    # ******************** End of data type conversion ********************

    dsldPyConditDisparity(data, yName, sName, xName, condits.split(','), qeFtn=qeFtn, minS=minS, yLim=yLim, useLoess=useLoess)
#************************** END OS SHELL *******************************************

'''
    # Test cases: Before running, go to /dsld/inst/Python

    # Running from the OS Shell
    python dsldConditDisparity_Py_R.py ../../data/compasNumericFixed.csv
    two_year_recid race age 'priors_count<=4','decile_score>=6' qeGBoost 50 -1 -1

    # Running from the Python Shell Prompt
    python # Open Python shell prompt
    from dsldConditDisparity_Py_R import dsldPyConditDisparity
    import pandas as pd
    # This csv DOES NOT EXIST: insert the path to a csv file from your machine
    data = pd.read_csv('../../data/compasNumericFixed.csv')
    dsldPyConditDisparity(data, 'two_year_recid', 'race', 'age', ['priors_count <= 4','decile_score>=6'], 'qeGBoost')

    # Running from the Python Shell Prompt
    python # Open Python shell prompt
    from dsldConditDisparity_Py_R import dsldPyConditDisparity
    import rpy2.robjects as robjects
    robjects.r['data']('svcensus')
    data = robjects.r('svcensus')
    dsldPyConditDisparity(data, "age", "gender", "wageinc", ['age<=60', 'wkswrkd>=25'])
'''
