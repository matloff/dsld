"""Python interface to DSLD linear regression and its S3 methods."""

import rpy2.robjects as ro
from .Utils import (
    get_dsld,
    dsld_Rpy2_IsRDataframe,
    dsld_Rpy2_RDataframeToPandas,
    _r_value,
)


def dsldPyLinear(
    data, yName, sName, interactions=False, sComparisonPts=None, useSandwich=False
):
    points = (
        ro.NULL if sComparisonPts is None else dsld_Rpy2_IsRDataframe(sComparisonPts)
    )
    return get_dsld().dsldLinear(
        dsld_Rpy2_IsRDataframe(data),
        yName,
        sName,
        _r_value(interactions),
        points,
        _r_value(useSandwich),
    )


def dsldPyLinearSummary(dsldLinear):
    result = ro.r["summary"](dsldLinear)
    print(result)
    return result


def dsldPyLinearCoef(dsldLinear):
    result = ro.r["coef"](dsldLinear)
    print(result)
    return result


def dsldPyLinearVcov(dsldLinear):
    result = ro.r["vcov"](dsldLinear)
    print(result)
    return result


def dsldPyLinearGetData(dsldLinear):
    get_data = ro.r["getFromNamespace"]("dsldGetData", "dsld")
    result = get_data(dsldLinear)
    print(result)
    return result


def dsldPyLinearPredict(dsldLinear, newData):
    result = ro.r["predict"](dsldLinear, dsld_Rpy2_IsRDataframe(newData))
    return dsld_Rpy2_RDataframeToPandas(result)
