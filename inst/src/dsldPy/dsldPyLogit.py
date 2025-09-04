'''
    This file contains the interface code for calling the dsldLogit from dsld R package.
    The code uses rpy2 to handle dsld functions call from R and pandas library to check if
    users data input is in pandas data frame before doing any computation
'''

from .Utils import get_dsld, dsld_Rpy2_IsRDataframe, dsld_Rpy2_RDataframeToPandas
import rpy2.robjects as robjects
from rpy2.robjects import pandas2ri
from rpy2.robjects import conversion
from rpy2.robjects.packages import importr


# Import R packages

def dsldPyLogit(data, yName, sName, sComparisonPts=None, interactions=False, yesYVal=None): 

    r_data = dsld_Rpy2_IsRDataframe(data)
    yName = robjects.StrVector([yName])
    sName = robjects.StrVector([sName])
    interactions = robjects.BoolVector([interactions])
    yesYVal = robjects.StrVector([yesYVal]) if yesYVal is not None else robjects.NULL
    
    if sComparisonPts is not None:
        sComparisonPts = dsld_Rpy2_IsRDataframe(sComparisonPts)
    else:
        sComparisonPts = robjects.NULL

    # call dsldLogit in R
    dsld = get_dsld()
    model = dsld.dsldLogit(r_data, yName, sName, sComparisonPts, interactions, yesYVal)
    return model

def dsldPyLogitSummary(dsldLogit): 
    robjects.r.assign("dsldLogit", dsldLogit)
    result = robjects.r('summary(dsldLogit)')
    print(result)
    return result

def dsldPyLogitCoef(dsldLogit):  
    robjects.r.assign("dsldLogit", dsldLogit)
    result = robjects.r('coef(dsldLogit)')
    print(result)
    return result

def dsldPyLogitVcov(dsldLogit):  
    robjects.r.assign("dsldLogit", dsldLogit)
    result = robjects.r('vcov(dsldLogit)')
    print(result)
    return result

def dsldPyLogitGetData(dsldLogit):  
    robjects.r.assign("dsldLogit", dsldLogit)
    result = robjects.r('dsldGetData(dsldLogit)')
    print(result)
    return result

def dsldPyLogitPredict(dsldLogit, newData):  
    robjects.r.assign("dsldLogit", dsldLogit)
    xNew = dsld_Rpy2_IsRDataframe(newData)
    # xNew = dsld.convert_cols(newData, cat_features, num_features)
    robjects.r.assign("xNew", xNew)
    result = robjects.r('predict(dsldLogit, xNew)')
    with conversion.localconverter(pandas2ri.converter):
        result_py = conversion.rpy2py(result)
    return result_py
