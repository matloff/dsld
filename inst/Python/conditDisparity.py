'''
    This file contains the interface code for calling the dsldConditDisparity from dsld R package.
    The code uses rpy2 to handle dsld functions call from R and pandas library to check if
    users data input is in pandas data frame before doing any computation
'''
import pandas as pd
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from Utils import dsld_Rpy2_IsRDataframe


dsld = importr("dsld")
qeML = importr("qeML")


# This is the interface function for R's dsldConditDisparity function
# The arguments are converted into R data type before calling dsldConditDisparity function
# This function uses qeML's qeKNN function as default argument for qeFtn
def dsldPyConditDisparity(data, yName, sName, xName, condits, qeFtn="qeKNN", minS=50, yLim=None, useLoess=True):
    r_data = dsld_Rpy2_IsRDataframe(data)

    robjects.r.assign("r_data", r_data)                         # Assign the 'r_data' variable to R
    robjects.r(f"r_data${sName} <- as.factor(r_data${sName})")  # Call as.factor() on the 'sName' column
    r_data = robjects.r("r_data")                               # Assign the modified R dataframe back to Python

    yName_r = robjects.StrVector([yName])  # Convert variable name to R character vector
    sName_r = robjects.StrVector([sName])  # Convert variable name to R character vector
    xName_r = robjects.StrVector([xName])  # Convert variable name to R character vector
    condits_r = robjects.StrVector([cond for cond in condits])  # Convert variable name to R character vector
    minS_r = robjects.IntVector([minS])    # Convert variable name to R;s number type

    # Checks if the qeFtn function exists in qeML library before calling it
    if hasattr(qeML, qeFtn) and callable(getattr(qeML, qeFtn)):
        # Call the function
        qeFtn_r = getattr(qeML, qeFtn)
    else:
        print(f"ERROR: qeML do not have function name: '{qeFtn}'\n")
        return

    yLim_r = robjects.NULL

    if yLim is not None:
        yLim_r = robjects.IntVector([int(x) for x in yLim])
        print(yLim_r)

    useLoess_r = robjects.BoolVector([useLoess])

    dsld.dsldConditDisparity(r_data, yName_r, sName_r, xName_r, condits_r, qeFtn_r, minS_r, yLim_r, useLoess_r)

    input('Enter any key to quit...')


data = pd.read_csv('../../data/compasNumericFixed.csv')
dsldPyConditDisparity(data, 'two_year_recid', 'race', 'age', ['priors_count <= 4','decile_score>=6'], 'qeGBoost')
