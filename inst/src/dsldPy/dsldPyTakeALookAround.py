'''
    This file contains the interface code for calling the dsldConditDisparity from dsld R package.
    The code uses rpy2 to handle dsld functions call from R and pandas library to check if
    users data input is in pandas data frame before doing any computation
'''
from .Utils import get_dsld, dsld_Rpy2_IsRDataframe, dsld_Rpy2_RDataframeToPandas
import sys
import os
import pandas as pd
from PIL import Image
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from rpy2.robjects import r
import math

def dsldPyTakeALookAround(data, yName, sName, maxFeatureSize = None, holdout = None):
    r_data = dsld_Rpy2_IsRDataframe(data)
    yName_r = robjects.StrVector([yName])
    sName_r = robjects.StrVector([sName])

    if maxFeatureSize is not None:
        maxFeatureSize_r = robjects.IntVector([maxFeatureSize])
    else:
        maxFeatureSize_r = robjects.IntVector([dsld_Rpy2_RDataframeToPandas(data).shape[1] - 2])

    if holdout is not None:
        holdout_r = robjects.IntVector([holdout])
    else:
        holdout_r = robjects.IntVector([math.floor(min(1000, 0.1 * dsld_Rpy2_RDataframeToPandas(data).shape[0]))])

    dsld = get_dsld()
    res = dsld.dsldTakeALookAround(r_data, yName_r, sName_r, maxFeatureSize_r, holdout_r)
    return dsld_Rpy2_RDataframeToPandas(res)
