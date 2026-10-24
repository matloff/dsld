"""Cross-validation for the fairness/utility tradeoff."""

from .Utils import (
    get_dsld,
    dsld_Rpy2_IsRDataframe,
    dsld_Rpy2_RDataframeToPandas,
    _str_vector,
    _r_value,
)


def dsldPyFairUtils(
    data,
    yName,
    sName,
    dsldFTNname,
    unfairness=None,
    deweightPars=None,
    yesYVal=None,
    k_folds=5,
    model_args=None,
):
    """Evaluate a parameter grid; testAcc is an error (lower is better)."""
    result = get_dsld().dsldFairUtils(
        dsld_Rpy2_IsRDataframe(data),
        yName,
        _str_vector(sName),
        dsldFTNname,
        _r_value(unfairness),
        _r_value(deweightPars),
        _r_value(yesYVal),
        _r_value(k_folds),
        _r_value(model_args),
    )
    return dsld_Rpy2_RDataframeToPandas(result)
