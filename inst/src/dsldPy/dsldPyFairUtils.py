'''
Python interface for dsldFairUtils functions in the dsld R package.
'''

from .Utils import get_dsld, dsld_Rpy2_IsRDataframe, dsld_Rpy2_RDataframeToPandas
from rpy2.robjects.packages import importr
import rpy2.robjects as robjects
import numpy as np
import pandas as pd

def _to_float_vector(x):
    if x is None:
        return robjects.NULL
    if isinstance(x, (list, tuple, np.ndarray, pd.Series)):
        return robjects.FloatVector([float(v) for v in x])
    return robjects.FloatVector([float(x)])

def _to_int_vector_scalar(x):
    if x is None:
        return robjects.NULL
    return robjects.IntVector([int(x)])

def _to_scalar_vector(x):
    if x is None:
        return robjects.NULL
    if isinstance(x, (int, np.integer)):
        return robjects.IntVector([int(x)])
    if isinstance(x, (float, np.floating)):
        return robjects.FloatVector([float(x)])
    return robjects.StrVector([str(x)])

def _to_str_vector(x):
    if x is None:
        return robjects.NULL
    if isinstance(x, (list, tuple, np.ndarray, pd.Series)):
        return robjects.StrVector([str(v) for v in x])
    return robjects.StrVector([str(x)])


def dsldPyFairUtils(data, yName, sName, dsldFTNname,
                    unfairness=None, deweightPars=None,
                    yesYVal=None, k_folds=5):

    r_data = dsld_Rpy2_IsRDataframe(data)

    yName_r = robjects.StrVector([yName])           # keep single y
    sName_r = _to_str_vector(sName)                 # str or list -> R character vector
    dsldFTNname_r = robjects.StrVector([dsldFTNname])

    unfairness_r = _to_float_vector(unfairness)

    if deweightPars is not None:
        deweightPars_r = robjects.ListVector(
            {k: _to_float_vector(v) for k, v in deweightPars.items()}
        )
    else:
        deweightPars_r = robjects.NULL

    yesYVal_r = _to_scalar_vector(yesYVal)
    k_folds_r = _to_int_vector_scalar(k_folds)

    dsld = get_dsld()
    model = dsld.dsldFairUtils(
        r_data, yName_r, sName_r, dsldFTNname_r,
        unfairness_r, deweightPars_r, yesYVal_r, k_folds_r, robjects.NULL
    )
    return dsld_Rpy2_RDataframeToPandas(model)
