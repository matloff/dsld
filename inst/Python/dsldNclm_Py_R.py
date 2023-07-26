from Utils import dsld_Rpy2_IsRDataframe, R_NULL, ERROR, DsldLinear, DsldDiffModel
import sys
import pandas as pd
import rpy2.robjects as robjects
from rpy2.robjects import pandas2ri
from rpy2.robjects.packages import importr

dsld = importr('dsld')

#changed yName to data
def dsldPyNclm(data, yName, xName, sName, unfairness, covfun, dsldLambda = 0, auxiliary = False):

    yName = robjects.IntVector([yName])                       # Convert variable name to R character vector
    xName = robjects.StrVector([xName])                       # Convert variable name to R character vector
    sName = robjects.StrVector([sName])                       # Convert variable name to R character vector