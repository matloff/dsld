"""Group densities as an interactive Plotly figure or a static R plot."""

from .Utils import (
    get_dsld,
    dsld_Rpy2_IsRDataframe,
    _r_value,
    _display_r_plot,
    _plotly_figure,
)


def dsldPyDensitybyS(data, cName, sName, graphType="plotly", fill=False):
    r_data = dsld_Rpy2_IsRDataframe(data)
    draw = lambda: get_dsld().dsldDensityByS(
        r_data, cName, sName, graphType, _r_value(fill)
    )
    if graphType.lower() == "plotly":
        figure = _plotly_figure(draw())
        figure.show()
        return figure
    return _display_r_plot(draw)
