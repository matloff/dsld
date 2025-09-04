'''
    This file contains the interface code for calling the dsldLinear from dsld R package.
    The code uses rpy2 to handle dsld functions call from R and pandas library to check if
    users data input is in pandas data frame before doing any computation
'''

from .Utils import get_dsld, dsld_Rpy2_IsRDataframe, dsld_Rpy2_RDataframeToPandas
import sys
import pandas as pd
import rpy2.robjects as robjects
from rpy2.robjects import pandas2ri
from rpy2.robjects import conversion
from rpy2.robjects.packages import importr
import rpy2.robjects as ro
import math


def dsldPyLinear(data, yName, sName, interactions=False,
                 sComparisonPts=None, useSandwich=False):

    """Python wrapper for dsldLinear in the dsld R package"""

    r_data = dsld_Rpy2_IsRDataframe(data)
    yName = robjects.StrVector([yName])
    sName = robjects.StrVector([sName])
    interactions = robjects.BoolVector([interactions])
    useSandwich = robjects.BoolVector([useSandwich])

    if sComparisonPts is not None:
        sComparisonPts = dsld_Rpy2_IsRDataframe(sComparisonPts)
    else:
        sComparisonPts = robjects.NULL

    dsld = get_dsld()
    dsldLinearObj = dsld.dsldLinear(r_data, yName, sName,
                                    interactions, sComparisonPts, useSandwich)
    return dsldLinearObj

def dsldPyLinearSummary(dsldLinear): 
    robjects.r.assign("dsldLinear", dsldLinear)
    result = robjects.r('summary(dsldLinear)')
    print(result)
    return result

def dsldPyLinearCoef(dsldLinear):  
    robjects.r.assign("dsldLinear", dsldLinear)
    result = robjects.r('coef(dsldLinear)')
    print(result)
    return result

def dsldPyLinearVcov(dsldLinear):  
    robjects.r.assign("dsldLinear", dsldLinear)
    result = robjects.r('vcov(dsldLinear)')
    print(result)
    return result

def dsldPyLinearGetData(dsldLinear):  
    robjects.r.assign("dsldLinear", dsldLinear)
    result = robjects.r('dsldGetData(dsldLinear)')
    print(result)
    return result

def dsldPyLinearPredict(dsldLinear, newData):  
    robjects.r.assign("dsldLinear", dsldLinear)
    xNew = dsld_Rpy2_IsRDataframe(newData)
    # xNew = dsld.convert_cols(newData, cat_features, num_features)
    robjects.r.assign("xNew", xNew)
    result = robjects.r('predict(dsldLinear, xNew)')
    with conversion.localconverter(pandas2ri.converter):
        result_py = conversion.rpy2py(result)
    return result_py


    
