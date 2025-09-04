#   OVERVIEW:  
#   No need to access dsldScatterPlot3D from dsld-package.
#   The function uses the package plotly in R, which is also available in Python.
#   This file requires packages: pandas, plotly, pyreadr
#           

import pandas as pd 
import plotly.express as px 
from rpy2.robjects import pandas2ri
from rpy2.robjects import vectors as rvectors
from rpy2.robjects import conversion, default_converter

def dsldPyScatterPlot3D(data, yNames, sName, *, bin_numeric_color=True, n_bins=5, renderer=None):
    if not isinstance(yNames, (list, tuple)) or len(yNames) != 3:
        raise ValueError("yNames must be a list of exactly 3 column names [x, y, z].")

    # Accept either pandas DataFrame or R data.frame; convert R -> pandas
    if isinstance(data, pd.DataFrame):
        df = data.copy()
    elif isinstance(data, rvectors.DataFrame):
        with conversion.localconverter(default_converter + pandas2ri.converter):
            df = conversion.rpy2py(data)
    else:
        raise TypeError("data must be a pandas DataFrame or an R data.frame")
    color_col = sName

    # If sName is numeric with many unique values, bin for discrete legend
    if bin_numeric_color and pd.api.types.is_numeric_dtype(df[sName]) and df[sName].nunique() > 20:
        color_col = f"{sName}_bin"
        df[color_col] = pd.qcut(df[sName], q=n_bins, duplicates="drop")

    fig = px.scatter_3d(
        df,
        x=yNames[0],
        y=yNames[1],
        z=yNames[2],
        color=color_col,
        opacity=0.75
    )

    fig.update_traces(marker=dict(size=4))
    fig.update_layout(legend_title_text=sName)

    if renderer:
        fig.show(renderer=renderer)
    else:
        fig.show()

    return fig
