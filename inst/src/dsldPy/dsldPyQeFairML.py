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

from rpy2.robjects import conversion, default_converter
from rpy2.robjects import pandas2ri

# add pandas converter to the default rpy2 converter
converter = default_converter + pandas2ri.converter

# Import R packages

### qeFairKNN-------------------------------------------------------------------
def dsldPyQeFairKNN(data, yName, sNames, deweightPars=None, yesYVal=None, k=25, scaleX=True):
    r_data = dsld_Rpy2_IsRDataframe(data)
    yName = robjects.StrVector([yName])
    sNames = robjects.StrVector([sNames])

    if deweightPars is not None:
        deweightPars = ListVector({k: FloatVector([v]) for k, v in deweightPars.items()})
    else:
        deweightPars = robjects.NULL

    if yesYVal is not None:
        yesYVal = robjects.StrVector([yesYVal])
    else:
        yesYVal = robjects.NULL

    k = robjects.IntVector([k])

    scaleX = robjects.BoolVector([scaleX])

    dsld = get_dsld()
    model = dsld.dsldQeFairKNN(r_data, yName, sNames, deweightPars, yesYVal, k, scaleX)

    preds = model.rx2("trainPreds")[0]
    acc   = model.rx2("trainAcc")
    corrs = model.rx2("trainCorrs")

    with conversion.localconverter(converter):
        corrs_df = conversion.rpy2py(corrs)

    result = {
        "model": model,
        "train_predictions": list(preds),
        "train_accuracy": float(acc[0]),
        "train_correlations": list(zip(corrs_df["feature"], corrs_df["correlation"]))
    }
    return result

## dsldQeFairRF-------------------------------------------------------------------
def dsldPyQeFairRF(data, yName, sNames, deweightPars=None, nTree=500, minNodeSize=10, mtry=None, yesYVal=None):    

    temp_data = dsld_Rpy2_RDataframeToPandas(data)
    if mtry is None:
        mtry = math.floor(math.sqrt(temp_data.shape[1]))

    r_data = dsld_Rpy2_IsRDataframe(data)
    yName = robjects.StrVector([yName])
    sNames = robjects.StrVector([sNames])

    if deweightPars is not None:
        deweightPars = ListVector({k: FloatVector([v]) for k, v in deweightPars.items()})
    else:
        deweightPars = robjects.NULL

    if yesYVal is not None:
        yesYVal = robjects.StrVector([yesYVal])
    else:
        yesYVal = robjects.NULL

    nTree = robjects.IntVector([nTree])
    minNodeSize = robjects.IntVector([minNodeSize])
    mtry = robjects.IntVector([mtry])

    dsld = get_dsld()
    model = dsld.dsldQeFairRF(r_data, yName, sNames, deweightPars, nTree, minNodeSize, mtry, yesYVal)

    preds = model.rx2("trainPreds")[0]
    acc   = model.rx2("trainAcc")
    corrs = model.rx2("trainCorrs")

    with conversion.localconverter(converter):
        corrs_df = conversion.rpy2py(corrs)

    result = {
        "model": model,
        "train_predictions": list(preds),
        "train_accuracy": float(acc[0]),
        "train_correlations": list(zip(corrs_df["feature"], corrs_df["correlation"]))
    }
    return result

### dsldQeFairRidgeLin-------------------------------------------------------------------

def dsldPyQeFairRidgeLin(data, yName, sNames, deweightPars=None):
    r_data = dsld_Rpy2_IsRDataframe(data)
    yName = robjects.StrVector([yName])
    sNames = robjects.StrVector([sNames])

    if deweightPars is not None:
        deweightPars = ListVector({k: FloatVector([v]) for k, v in deweightPars.items()})
    else:
        deweightPars = robjects.NULL

    dsld = get_dsld()
    model = dsld.dsldQeFairRidgeLin(r_data, yName, sNames, deweightPars)

    preds = model.rx2("trainPreds")[0]
    acc   = model.rx2("trainAcc")
    corrs = model.rx2("trainCorrs")

    with conversion.localconverter(converter):
        corrs_df = conversion.rpy2py(corrs)

    result = {
        "model": model,
        "train_predictions": list(preds),
        "train_accuracy": float(acc[0]),
        "train_correlations": list(zip(corrs_df["feature"], corrs_df["correlation"]))
    }
    return result

### dsldQeFairRidgeLog-------------------------------------------------------------------

def dsldPyQeFairRidgeLog(data, yName, sNames, deweightPars=None, yesYVal=None):
    r_data = dsld_Rpy2_IsRDataframe(data)
    yName = robjects.StrVector([yName])
    sNames = robjects.StrVector([sNames])

    if deweightPars is not None:
        deweightPars = ListVector({k: FloatVector([v]) for k, v in deweightPars.items()})
    else:
        deweightPars = robjects.NULL

    if yesYVal is not None:
        yesYVal = robjects.StrVector([yesYVal])
    else:
        yesYVal = robjects.NULL

    dsld = get_dsld()
    model = dsld.dsldQeFairRidgeLog(r_data, yName, sNames, deweightPars, yesYVal)

    preds = model.rx2("trainPreds")[0]
    acc   = model.rx2("trainAcc")
    corrs = model.rx2("trainCorrs")

    with conversion.localconverter(converter):
        corrs_df = conversion.rpy2py(corrs)

    result = {
        "model": model,
        "train_predictions": list(preds),
        "train_accuracy": float(acc[0]),
        "train_correlations": list(zip(corrs_df["feature"], corrs_df["correlation"]))
    }
    return result

### predict() method for all the models
def dsldPyQeFairML_Predict(model, newData):
    robjects.r.assign("model", model['model'])
    xNew = dsld_Rpy2_IsRDataframe(newData)
    robjects.r.assign("xNew", xNew)
    result = robjects.r('predict(model, xNew)')
    names = list(result[1][0])
    vals  = [float(v) for v in result[1][1]]
    correlations = list(zip(names, vals))
    output = {'test_predictions': list(result[0]),
              'test_correlations': correlations}
    return output
