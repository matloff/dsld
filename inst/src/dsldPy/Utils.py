import pandas as pd
import numpy as np
from PIL import Image
import rpy2.robjects as robjects
from rpy2.robjects import StrVector, FloatVector, ListVector
from rpy2.robjects import pandas2ri


import rpy2.robjects as ro
from rpy2.robjects import default_converter
from rpy2.robjects.conversion import localconverter
from rpy2.robjects import pandas2ri

import os
import pyreadr
from rpy2.robjects.packages import importr


def get_dsld():
    """Return the R 'dsld' package handle using the installed version in R."""
    return importr("dsld")

def get_dsld_version():
    """Return the installed dsld version as a string (or None if unavailable)."""
    try:
        importr("utils")  # ensure utils is available
        ver = ro.r('as.character(utils::packageVersion("dsld"))')[0]
        return ver
    except Exception:
        return None

### data-frame conversion functions
# This function converts a pandas data frame into an R data frame
## updated to remove depracated function
def dsld_Rpy2_IsRDataframe(data):
    """
    If data is R data.frame, return it.
    If data is pandas DataFrame, convert to R and return.
    Otherwise return -1.
    """
    if isinstance(data, ro.vectors.DataFrame):
        return data
    elif isinstance(data, pd.DataFrame):
        return dsld_Rpy2_PandasToRDataframe(data)
    else:
        print("Error: not Rdata or Pandas Dataframe")
        return -1
              
### helper functions for dsld_Rpy2_IsRDataframe
def dsld_Rpy2_PandasToRDataframe(pandas_df: pd.DataFrame):
    """convert pandas -> R data.frame."""
    with localconverter(default_converter + pandas2ri.converter):
        return ro.conversion.py2rpy(pandas_df)

def dsld_Rpy2_RDataframeToPandas(r_df):
    """convert R data.frame -> pandas DataFrame."""
    with localconverter(default_converter + pandas2ri.converter):
        return ro.conversion.rpy2py(r_df)

### reading data // data cleaning
def read_data(filepath, **kwargs):

    ext = os.path.splitext(filepath)[1].lower()
    
    if ext == ".csv":
        return pd.read_csv(filepath, **kwargs)
    
    elif ext in [".rdata", ".rda"]:
        result = pyreadr.read_r(filepath)
        key = list(result.keys())[0]
        return result[key]
    
    else:
        raise ValueError(f"Unsupported file extension: {ext}")

def preprocess_data(data, cat_features, num_features):
    r_data = dsld_Rpy2_IsRDataframe(data)
    dsld = get_dsld()
    r_data = dsld.convert_cols(r_data, cat_features, num_features)
    return r_data
