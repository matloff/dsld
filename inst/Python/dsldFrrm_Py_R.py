'''
    dsldFrrm_Py_R.py is the python interface for dsldFrrm in the dsld R package.
    Utils contains helper functions which are used in multiple files
    rpy2 is used handle dsld function calls from R
    pandas processes dataset into a panda's data frame before performing any computation
    sys is used for OS shell
'''

from Utils import dsld_Rpy2_IsRDataframe, ERROR, R_NULL
from rpy2.robjects.packages import importr
import rpy2.robjects as robjects
import pandas as pd
import sys

'''
    Importing r packages {dsld} into Python through rpy2
    dsld contains this file's main R function, dsldFrrm
'''
dsld    = importr("dsld")


def dsldPyFrrm(data, yName, sName, unfairness, xName = R_NULL, definition = "sp-komiyama", lamda = 0, save = False):
    # ************************** ARGUMENTS *******************************************
    
    # Data conversion handled by Utils function
    r_data = dsld_Rpy2_IsRDataframe(data)

    yName_r = robjects.StrVector([yName])                               # Convert variable name to R character vector

    xName_r = robjects.StrVector([xName])                               # Convert variable name to R character vector

    sName_r = robjects.StrVector([sName])                               # Convert variable name to R character vector

    unfairness_r = robjects.FloatVector([unfairness])                   # Convert variable name to R float vector

    def_r = robjects.StrVector([definition])                            # Convert variable name to R character vector

    dsldlambda_r = robjects.IntVector([lamda])                          # Convert variable name to R int vector

    save_r = robjects.BoolVector([save])                                # Convert variable name to R boolean vector

    # Might need to convert the data back into pandas data frame or proper
    # python data format
    return dsld.dsldFrrm(data = r_data, yName = yName_r, sName = sName_r, unfairness = unfairness_r, definition = def_r, save = save_r) #lamda=dsldlambda)