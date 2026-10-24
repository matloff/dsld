"""Compare predictive errors across candidate feature sets."""

import math
from .Utils import (
    get_dsld,
    dsld_Rpy2_IsRDataframe,
    dsld_Rpy2_RDataframeToPandas,
    _r_value,
)


def dsldPyTakeALookAround(data, yName, sName, maxFeatureSize=None, holdout=None):
    r_data = dsld_Rpy2_IsRDataframe(data)
    if maxFeatureSize is None:
        maxFeatureSize = r_data.ncol - 2
    if holdout is None:
        holdout = max(1, math.floor(min(1000, 0.1 * r_data.nrow)))
    result = get_dsld().dsldTakeALookAround(
        r_data, yName, sName, _r_value(maxFeatureSize), _r_value(holdout)
    )
    return dsld_Rpy2_RDataframeToPandas(result)
