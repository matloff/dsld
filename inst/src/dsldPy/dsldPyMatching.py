"""Matching-based treatment effects."""

import rpy2.robjects as ro
from .Utils import get_dsld, dsld_Rpy2_IsRDataframe, _r_value


def dsldPyMatchedATE(
    data, yName, sName, yesSVal, yesYVal=None, propensFtn=None, k=None
):
    result = get_dsld().dsldMatchedATE(
        dsld_Rpy2_IsRDataframe(data),
        yName,
        sName,
        _r_value(yesSVal),
        _r_value(yesYVal),
        _r_value(propensFtn),
        _r_value(k),
    )
    ro.r["summary"](result)
    return result
