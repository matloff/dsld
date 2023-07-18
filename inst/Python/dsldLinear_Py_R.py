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


def dsldLinear_Py_R(data, yName, sName, interactions = False, newData = R_NULL):
    r_data = dsld_Rpy2_IsRDataframe(data)

    yName = robjects.StrVector([yName])                       # Convert variable name to R character vector
    sName = robjects.StrVector([sName])                       # Convert variable name to R character vector
    interactions = robjects.BoolVector([interactions])        # Convert python bool value to R Boolean value

    if newData != R_NULL:
        newData = dsld_Rpy2_IsRDataframe(newData)

    dsldLinearObj = dsld.dsldLinear(data, yName, sName, interactions, newData)

    return dsldLinearObj
    


robjects.r['data']('svcensus')
data = robjects.r('svcensus')

#robjects.r.assign('data', data)

robjects.r('svcensus$occ <- as.factor(svcensus$occ)')
robjects.r('svcensus$gender <- as.factor(svcensus$gender)')
robjects.r('svcensus$educ <- as.factor(svcensus$educ)')

robjects.r('new_data <- data.frame(age = c(18, 60), educ = c("zzzOther", "zzzOther"), wkswrkd = c(50, 50), occ = c("106", "106"))')

data = robjects.r['svcensus'] 
new_data = robjects.r('new_data')

dsldLinearObject = dsldLinear_Py_R(data, 'wageinc', 'gender', interactions=True, newData=new_data)

print(dsldLinearObject)


