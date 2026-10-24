"""Conditional disparity plots using a qeML prediction function."""

from rpy2.robjects.packages import importr
from .Utils import (
    get_dsld,
    dsld_Rpy2_IsRDataframe,
    _str_vector,
    _r_value,
    _display_r_plot,
)


def dsldPyConditDisparity(
    data, yName, sName, xName, condits=None, qeFtn="qeKNN", minS=50, useLoess=True
):
    qe_ml = importr("qeML")
    if not hasattr(qe_ml, qeFtn):
        raise ValueError(f"Unknown qeML function: {qeFtn}")
    r_data = dsld_Rpy2_IsRDataframe(data)
    return _display_r_plot(
        lambda: get_dsld().dsldConditDisparity(
            r_data,
            yName,
            sName,
            xName,
            _str_vector(condits),
            getattr(qe_ml, qeFtn),
            _r_value(minS),
            _r_value(useLoess),
        )
    )
