'''
    This file contains the interface code for calling the dsldLinear from dsld R package.
    The code uses rpy2 to handle dsld functions call from R and pandas library to check if
    users data input is in pandas data frame before doing any computation
'''

from Utils import dsld_Rpy2_IsRDataframe, R_NULL, ERROR, DsldLinear, DsldDiffModel
import sys
import pandas as pd
import rpy2.robjects as robjects
from rpy2.robjects import pandas2ri
from rpy2.robjects.packages import importr


# Creating instance of dsld package
dsld = importr('dsld')


def dsldPyLinear(data, yName, sName, interactions = False, newData = R_NULL, returnType = "R"):
    # ************************** ARGUMENTS *******************************************
    r_data = dsld_Rpy2_IsRDataframe(data)

    yName = robjects.StrVector([yName])                       # Convert variable name to R character vector
    sName = robjects.StrVector([sName])                       # Convert variable name to R character vector
    interactions = robjects.BoolVector([interactions])        # Convert python bool value to R Boolean value

    if newData != R_NULL:
        newData = dsld_Rpy2_IsRDataframe(newData)
    
    # ************************** RETURN VALUE ******************************************
    dsldLinearObj = dsld.dsldLinear(data, yName, sName, interactions, newData)

    if returnType.lower() == "python" or returnType.lower() == "py":
        return DsldLinear(dsldLinearObj) # Create an instance of the DsldLinear class
    else:
        return dsldLinearObj # Return an R object that we can call summary on


def dsldPyDiffS(dsldLinear, newData = R_NULL, returnType = "R"):
    if newData != R_NULL:
        newData = dsld_Rpy2_IsRDataframe(newData)

    dsldDiffObj = dsld.dsldDiffS(dsldLinear, newData)

    if returnType.lower() == "python" or returnType.lower() == "py":
        return pandas2ri.rpy2py_dataframe(dsldDiffObj) # Return a python dataframe
    else:
        return dsldDiffObj # Return an R dataframe


def dsldPyLinearSummary(dsldLinear): # TODO: function name
    robjects.r.assign("dsldLinear", dsldLinear)
    result = robjects.r('summary(dsldLinear)')

    print(result)
    return result
    

'''
    # Test case: Before running, go to /dsld/inst/Python
    import rpy2.robjects as robjects
    from dsldLinear_Py_R import dsldPyLinear, dsldPyDiffS, dsldPyLinearSummary

    robjects.r['data']('svcensus')
    robjects.r('svcensus$occ <- as.factor(svcensus$occ)')
    robjects.r('svcensus$gender <- as.factor(svcensus$gender)')
    robjects.r('svcensus$educ <- as.factor(svcensus$educ)')

    robjects.r('new_data <- data.frame(age = c(18, 60), educ = c("zzzOther", "zzzOther"), wkswrkd = c(50, 50), occ = c("106", "106"))')

    data = robjects.r['svcensus'] 
    new_data = robjects.r('new_data')

    dsldLinRObject = dsldPyLinear(data, 'wageinc', 'gender', True, new_data)
    dsldPyLinearSummary(dsldLinRObject)

    dsldLinPyObject = dsldPyLinear(data, 'wageinc', 'gender', True, new_data, "Python")


    robjects.r('X_new <- data.frame(age = c(18, 60), educ = c("16", "16"), occ = c("106", "106"), wkswrkd = c(50, 50))')
    X_new = robjects.r('X_new')

    dsldDiffRObject = dsldPyDiffS(dsldLinRObject, X_new)
    print(dsldDiffRObject)
'''


