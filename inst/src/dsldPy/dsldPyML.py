'''
    This file contains the interface code for calling the dsldLogit from dsld R package.
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
from rpy2.robjects.vectors import ListVector, FloatVector
from .Utils import dsld_Rpy2_RDataframeToPandas
import pandas as pd

# Import R packages

def map_last_k(df: pd.DataFrame, values: list):
    k = len(values)
    last_k_cols = df.columns[-k:]
    # add "testAcc: " prefix to each column name
    prefixed_cols = [f"testAcc: {col}" for col in last_k_cols]
    return dict(zip(prefixed_cols, values))

def dsldPyML(data, yName, sName, qeMLftnName, sComparisonPts='rand5', opts=None):

    r_data = dsld_Rpy2_IsRDataframe(data)
    yName = robjects.StrVector([yName])
    sName = robjects.StrVector([sName])
    qeMLftnName = robjects.StrVector([qeMLftnName])

    if sComparisonPts != 'rand5':
        if isinstance(sComparisonPts, pd.DataFrame):
            sComparisonPts = dsld_Rpy2_IsRDataframe(sComparisonPts)
        else:
            sComparisonPts = robjects.StrVector([sComparisonPts])
    else:
        sComparisonPts = robjects.StrVector(['rand5'])

    if opts is not None:
        opts = ListVector({k: FloatVector([v]) for k, v in opts.items()})
    else:
        opts = robjects.NULL
    
    # call dsldML in R
    dsld = get_dsld()
    model = dsld.dsldML(r_data, yName, sName, qeMLftnName, sComparisonPts, opts)

    test_accuracies = model[0]
    comparison_points = dsld_Rpy2_RDataframeToPandas(model[1])
    comparison_points_dict = map_last_k(comparison_points, test_accuracies)

    return comparison_points_dict, comparison_points
