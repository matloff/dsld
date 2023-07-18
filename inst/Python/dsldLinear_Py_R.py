'''
    This file contains the interface code for calling the dsldLinear from dsld R package.
    The code uses rpy2 to handle dsld functions call from R and pandas library to check if
    users data input is in pandas data frame before doing any computation
'''

from Utils import dsld_Rpy2_IsRDataframe, R_NULL, ERROR
import sys
import pandas as pd
import rpy2.robjects as robjects
from rpy2.robjects import pandas2ri
from rpy2.robjects.packages import importr


# Creating instance of dsld package
dsld = importr('dsld')


def dsldPyLinear(data, yName, sName, interactions = False, newData = R_NULL):
    # ************************** ARGUMENTS *******************************************
    r_data = dsld_Rpy2_IsRDataframe(data)

    yName = robjects.StrVector([yName])                       # Convert variable name to R character vector
    sName = robjects.StrVector([sName])                       # Convert variable name to R character vector
    interactions = robjects.BoolVector([interactions])        # Convert python bool value to R Boolean value

    if newData != R_NULL:
        newData = dsld_Rpy2_IsRDataframe(newData)
    # ************************** END ARGUMENTS *****************************************
    
    # ************************** RETURN VALUE ******************************************
    dsldLinearObj = dsld.dsldLinear(data, yName, sName, interactions, newData)

    return dsldLinearObj
# ************************** END OF FUNCTION *******************************************


def dsldPyDiffS(dsldLinear, newData = R_NULL):
    if newData != R_NULL:
        newData = dsld_Rpy2_IsRDataframe(newData)

    result = dsld.dsldDiffS(dsldLinear, newData)

    return result
    # return pandas2ri.rpy2py_dataframe(result)


def summary(dsldLinear): # TODO: function name
    robjects.r.assign("dsldLinear", dsldLinear)
    result = robjects.r('summary(dsldLinear)')

    print(result)

    return result
    

# Test cases: Before running, go to /dsld/inst/Python
robjects.r['data']('svcensus')
data = robjects.r('svcensus')


robjects.r('svcensus$occ <- as.factor(svcensus$occ)')
robjects.r('svcensus$gender <- as.factor(svcensus$gender)')
robjects.r('svcensus$educ <- as.factor(svcensus$educ)')

robjects.r('new_data <- data.frame(age = c(18, 60), educ = c("zzzOther", "zzzOther"), wkswrkd = c(50, 50), occ = c("106", "106"))')

data = robjects.r['svcensus'] 
new_data = robjects.r('new_data')

dsldLinearObject = dsldPyLinear(data, 'wageinc', 'gender', interactions=True, newData=new_data)

summary(dsldLinearObject)


