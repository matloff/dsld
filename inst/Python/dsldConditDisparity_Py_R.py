'''
    This file contains the interface code for calling the dsldConditDisparity from dsld R package.
    The code uses rpy2 to handle dsld functions call from R and pandas library to check if
    users data input is in pandas data frame before doing any computation
'''
from Utils import dsld_Rpy2_IsRDataframe, changeBg
import sys
import os
import pandas as pd
from PIL import Image
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from rpy2.robjects import r

dsld = importr("dsld")
qeML = importr("qeML")
grdevices = importr('grDevices')

# This function checks if dsld function arguments are in proper format for computation
def validate_input(yName, sName, xName, condits, minS, yLim, useLoess):
    if type(yName) != list and type(yName) != str:
        print('Error: yName must be a list of string. Entered type:', type(yName))
        exit(1)

    if type(sName) != list and type(sName) != str:
        print('Error: sName must be a list of string. Entered type:', type(sName))
        exit(1)

    if type(xName) != list and type(xName) != str:
        print('Error: xName must be a list of string. Entered type:', type(xName))
        exit(1)

    if type(condits) != list and type(condits) != str:
        print('Error: condits must be a list of string. Entered type:', type(condits))
        exit(1)

    if type(minS) != int:
        print('Error: minS must be an int type. Entered type:', type(minS))
        exit(1)

    if type(useLoess) != bool:
        print('Error: useLoess must be boolean type. Entered:', type(useLoess))
        exit(1)


    if yLim is not None and type(yLim) != list:
        print('Error: yLim must be a list of integers. Entered type:', type(yLim))
        exit(1)
    
    if yLim is not None and len(yLim) != 2:
        print('Error: yLim must be a list of 2 integers(lower and upper bound). Size Entered:', len(yLim))
        exit(1)

    
# This is the interface function for R's dsldConditDisparity function
# The arguments are converted into R data type before calling dsldConditDisparity function
# This function uses qeML's qeKNN function as default argument for qeFtn
def dsldPyConditDisparity(data, yName, sName, xName, condits, qeFtn="qeKNN", minS=50, yLim=None, useLoess=True):
    r_data = dsld_Rpy2_IsRDataframe(data)

    validate_input(yName, sName, xName, condits, minS, yLim, useLoess)

    robjects.r.assign("r_data", r_data)                         # Assign the 'r_data' variable to R
    robjects.r(f"r_data${sName} <- as.factor(r_data${sName})")  # Call as.factor() on the 'sName' column
    r_data = robjects.r("r_data")                               # Assign the modified R dataframe back to Python

    yName_r = robjects.StrVector([yName])                       # Convert variable name to R character vector
    sName_r = robjects.StrVector([sName])                       # Convert variable name to R character vector
    xName_r = robjects.StrVector([xName])                       # Convert variable name to R character vector
    condits_r = robjects.StrVector([cond for cond in condits])  # Convert variable name to R character vector
    minS_r = robjects.IntVector([minS])                         # Convert variable name to R;s number type

    # Checks if the qeFtn function exists in qeML library before calling it
    if hasattr(qeML, qeFtn) and callable(getattr(qeML, qeFtn)):
        # Call the function
        qeFtn_r = getattr(qeML, qeFtn)
    else:
        print(f"ERROR: qeML do not have function name: '{qeFtn}'\n")
        return

    yLim_r = robjects.NULL

    if yLim is not None:
        try:
            yLim_r = robjects.FloatVector([float(x) for x in yLim])
        except ValueError:
            print("Please enter a list of two integers separated by comma")
            exit(1)

    useLoess_r = robjects.BoolVector([useLoess])

    dsld.dsldConditDisparity(r_data, yName_r, sName_r, xName_r, condits_r, qeFtn_r, minS_r, yLim_r, useLoess_r)

    # Copy and saves the image as plot.png
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


if __name__ == "__main__":
    if len(sys.argv) != 10:
        print('All 9 arguments are required. Entered: \n', len(sys.argv))
        exit(1)

    _,file_path, yName, sName, xName, condits, qeFtn, minS, yLim, useLoess = sys.argv

    minS = int(minS)

    if qeFtn == "-1":
        qeFtn = "qeKNN"

    if yLim != '-1':
        yLim = yLim.split(',')
    else:
        yLim = None

    if useLoess == "-1" or useLoess == "True":
        useLoess = True
    else:
        useLoess = False

    try:
        data = pd.read_csv(file_path)
    except FileNotFoundError:
        print(f"File not found")
        exit(1)

    dsldPyConditDisparity(data, yName, sName, xName, condits.split(','), qeFtn=qeFtn, minS=minS, yLim=yLim, useLoess=useLoess)

'''
    # Test cases: Before running, go to /dsld/inst/Python

    # Running from the OS Shell
    python dsldConditDisparity_Py_R.py ../../data/compasNumericFixed.csv two_year_recid race age 'priors_count<=4','decile_score>=6' qeGBoost

    # Running from the Python Shell Prompt
    python # Open Python shell prompt
    from dsldConditDisparity_Py_R import dsldPyConditDisparity
    import pandas as pd
    data = pd.read_csv('../../data/compasNumericFixed.csv')
    dsldPyConditDisparity(data, 'two_year_recid', 'race', 'age', ['priors_count <= 4','decile_score>=6'], 'qeGBoost')

    # Running from the Python Shell Prompt
    python # Open Python shell prompt
    from dsldConditDisparity_Py_R import dsldPyConditDisparity
    import rpy2.robjects as robjects
    robjects.r['data']('svcensus')
    data = robjects.r('svcensus')
    dsldPyConditDisparity(data, "age", "gender", "wageinc", ['age<=60', 'wkswrkd>=25'])
    

    PEF Data Example from qeML
    python # Open Python shell prompt
    from dsldConditDisparity_Py_R import dsldPyConditDisparity
    robjects.r['data']('pef')
    pef = robjects.r['pef']
    dsldPyConditDisparity(pef, "age", "sex", "wageinc", ['age<=60', 'wkswrkd>=25'])
'''
