"""Display frequency tables and densities for candidate confounders."""

import rpy2.robjects as ro
from IPython.display import display
from .Utils import dsld_Rpy2_IsRDataframe
from .dsldPyDensitybyS import dsldPyDensitybyS
from .dsldPyFrequencybyS import dsldPyFrequencybyS


def dsldPyConfounders(data, sName, graphType="plotly", fill=False):
    r_data = dsld_Rpy2_IsRDataframe(data)
    results = {}
    for name in r_data.names:
        if name == sName:
            continue
        column = r_data.rx2(name)
        if bool(ro.r["is.factor"](column)[0]) or bool(ro.r["is.character"](column)[0]):
            results[name] = dsldPyFrequencybyS(r_data, name, sName)
            display(results[name])
        elif bool(ro.r["is.numeric"](column)[0]):
            results[name] = dsldPyDensitybyS(r_data, name, sName, graphType, fill)
        else:
            raise TypeError(f"Unsupported column type: {name}")
    return results
