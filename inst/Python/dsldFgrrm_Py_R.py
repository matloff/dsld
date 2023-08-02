'''
    dsldFgrrm_Py_R.py is the python interface for dsldFgrrm in the dsld R package.
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
    dsld contains this file's main R function, dsldFgrrm
'''
dsld    = importr("dsld")

#dsldFgrrm <- function(data, yData, xData, sName, unfairness, definition = "sp-komiyama", family = "binomial", lambda = 0, save.auxiliary = FALSE)

def dsldPyFrrm(data, yData, xData, sName, unfairness, definition = "sp-komiyama", family = "binomial", lamda = 0, save = False):
    # ************************** ARGUMENTS *******************************************
    # Data conversion handled by Utils function
    r_data = dsld_Rpy2_IsRDataframe(data)

    yData_r = yData                               

    xData_r = xData

    sName_r = robjects.StrVector(sName)                                 # Convert variable name to R character vector

    unfairness_r = robjects.FloatVector([unfairness])                   # Convert variable name to R float vector

    definition_r = robjects.StrVector([definition])                     # Convert variable name to R character vector

    family_r = robjects.StrVector([family])                     # Convert variable name to R character vector

    dsldlambda_r = robjects.FloatVector([lamda])                        # Convert variable name to R int vector

    save_r = robjects.BoolVector([save])                                # Convert variable name to R boolean vector
    
    # All necessary arguments are in R format at this point
    # ************************** END ARGUMENTS *******************************************

    # ************************** RETURN VALUE *******************************************
    return dsld.dsldFgrrm(r_data, yData_r, xData_r, sName_r, unfairness_r, definition_r, family_r, dsldlambda_r, save_r)

'''
    # Examples NOT WORKING
    # TODO: Figure out how to input the yData and xData as r data frames/ matrices
    python
    from dsldFrrm_Py_R import dsldPyFrrm
    import rpy2.robjects as robjects
    robjects.r['data']('communities.and.crime')
    data = robjects.r('communities.and.crime')
    nclmR = dsldPyFrrm(data, "ViolentCrimesPerPop", ["racepctblack","PctForeignBorn"], 0.05)
    print(robjects.r['summary'](nclmR))

    # Other examples
    from dsldFrrm_Py_R import dsldPyFrrm; import rpy2.robjects as robjects; robjects.r['data']('communities.and.crime'); data = robjects.r('communities.and.crime'); nclmR = dsldPyFrrm(data, "ViolentCrimesPerPop", ["racepctblack","PctForeignBorn"], 0.05); print(robjects.r['summary'](nclmR))
'''