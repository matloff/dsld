"""Frequency-based parallel coordinates."""

import rpy2.robjects as ro
from .Utils import (
    get_dsld,
    dsld_Rpy2_IsRDataframe,
    _str_vector,
    _r_value,
    _display_r_plot,
)


def dsldPyFreqPCoord(
    data,
    m,
    sName,
    method="maxdens",
    faceting="vert",
    k=50,
    klm=None,
    keepidxs=None,
    plotidxs=False,
    cls=None,
    plot_filename=None,
    show=True,
):
    r_data = dsld_Rpy2_IsRDataframe(data)
    if klm is None:
        klm = 5 * k

    def draw():
        return get_dsld().dsldFreqPCoord(
            r_data,
            _r_value(m),
            _str_vector(sName),
            method,
            faceting,
            _r_value(k),
            _r_value(klm),
            _r_value(keepidxs),
            _r_value(plotidxs),
            _r_value(cls),
            _r_value(plot_filename),
        )

    if plot_filename:
        result = draw()
        if show:
            ro.r["print"](result)
        return result
    return _display_r_plot(draw, show=show)
