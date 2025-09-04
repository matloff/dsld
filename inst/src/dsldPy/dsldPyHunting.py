from .Utils import get_dsld, dsld_Rpy2_IsRDataframe, dsld_Rpy2_RDataframeToPandas
import sys
import os
import pandas as pd
from PIL import Image
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from rpy2.robjects import r
import rpy2.robjects as ro

### dsldPyCHunting
def dsldPyCHunting(data,yName,sName,intersectDepth=10):
    r_data = dsld_Rpy2_IsRDataframe(data)
    yName_r = robjects.StrVector([yName])
    sName_r = robjects.StrVector([sName])
    intersectDepth_r = robjects.IntVector([intersectDepth])

    dsld = get_dsld()
    res = dsld.dsldCHunting(r_data, yName_r, sName_r, intersectDepth_r)
    result = {'impForY' : list(zip(list(res[0].names), list(res[0]))), 'impForS' : list(zip(list(res[1].names), list(res[1])))}
    return result

### dsldPyOHunting
def dsldPyOHunting(data,yName,sName):
    r_data = dsld_Rpy2_IsRDataframe(data)
    yName_r = robjects.StrVector([yName])
    sName_r = robjects.StrVector([sName])
    dsld = get_dsld()
    res = dsld.dsldOHunting(r_data, yName_r, sName_r)
    
    # print in R
    ro.r("print")(res)
    return res
