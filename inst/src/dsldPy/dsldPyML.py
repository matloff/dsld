"""Nonparametric group comparisons through qeML."""

import pandas as pd
from .Utils import (
    get_dsld,
    dsld_Rpy2_IsRDataframe,
    dsld_Rpy2_RDataframeToPandas,
    _r_value,
)


def map_last_k(df: pd.DataFrame, values: list):
    return dict(
        zip([f"testAcc: {column}" for column in df.columns[-len(values) :]], values)
    )


def dsldPyML(data, yName, sName, qeMLftnName, sComparisonPts="rand5", opts=None):
    if isinstance(sComparisonPts, str):
        if sComparisonPts != "rand5":
            raise ValueError("sComparisonPts must be a data frame or 'rand5'")
        comparison_points = _r_value(sComparisonPts)
    else:
        comparison_points = dsld_Rpy2_IsRDataframe(sComparisonPts)
    model = get_dsld().dsldML(
        dsld_Rpy2_IsRDataframe(data),
        yName,
        sName,
        qeMLftnName,
        comparison_points,
        _r_value(opts),
    )
    comparisons = dsld_Rpy2_RDataframeToPandas(model.rx2("comparisons"))
    accuracies = model.rx2("testAccs")
    return dict(
        zip([f"testAcc: {name}" for name in accuracies.names], list(accuracies))
    ), comparisons
