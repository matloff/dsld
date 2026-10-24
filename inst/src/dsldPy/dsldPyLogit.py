"""Python interface to DSLD logit regression and its S3 methods."""

import rpy2.robjects as ro
from .Utils import (
    get_dsld,
    dsld_Rpy2_IsRDataframe,
    dsld_Rpy2_RDataframeToPandas,
    _r_value,
)


def dsldPyLogit(
    data, yName, sName, sComparisonPts=None, interactions=False, yesYVal=None
):
    points = (
        ro.NULL if sComparisonPts is None else dsld_Rpy2_IsRDataframe(sComparisonPts)
    )
    return get_dsld().dsldLogit(
        dsld_Rpy2_IsRDataframe(data),
        yName,
        sName,
        points,
        _r_value(interactions),
        _r_value(yesYVal),
    )


def dsldPyLogitSummary(dsldLogit):
    result = ro.r["summary"](dsldLogit)
    print(result)
    return result


def dsldPyLogitCoef(dsldLogit):
    result = ro.r["coef"](dsldLogit)
    print(result)
    return result


def dsldPyLogitVcov(dsldLogit):
    result = ro.r["vcov"](dsldLogit)
    print(result)
    return result


def dsldPyLogitGetData(dsldLogit):
    get_data = ro.r["getFromNamespace"]("dsldGetData", "dsld")
    result = get_data(dsldLogit)
    print(result)
    return result


def dsldPyLogitPredict(dsldLogit, newData):
    result = ro.r["predict"](dsldLogit, dsld_Rpy2_IsRDataframe(newData))
    return dsld_Rpy2_RDataframeToPandas(result)
