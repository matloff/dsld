import os
import tempfile
from typing import Optional, Union, Sequence
from IPython.display import Image, display

import pandas as pd
import rpy2.robjects as ro
from rpy2.robjects.vectors import IntVector, StrVector, BoolVector
from rpy2.robjects.packages import importr

from .Utils import dsld_Rpy2_IsRDataframe, get_dsld 

def _maybe_intvec(x):
    if x is None:
        return ro.NULL
    if isinstance(x, (list, tuple)):
        return IntVector(list(x))
    return IntVector([int(x)])

def _maybe_strvec(x: Optional[Union[str, Sequence[str]]]):
    if x is None:
        return ro.NULL
    if isinstance(x, (list, tuple)):
        return StrVector(list(x))
    return StrVector([str(x)])

def dsldPyFreqPCoord(data, m, sName, method = "maxdens", faceting = "vert", k = 50, klm = None, keepidxs = None, plotidxs = False, cls = None, plot_filename = None, show = True):
  
    # Prepare inputs (pass scalars where R expects scalars)
    r_data = dsld_Rpy2_IsRDataframe(data)
    r_m = int(m)
    r_sName = _maybe_strvec(sName)
    r_method = str(method)
    r_faceting = str(faceting)
    r_k = int(k)
    if klm is None:
        klm = 5 * k
    r_klm = int(klm)
    r_keepidxs = _maybe_intvec(keepidxs)
    r_plotidxs = bool(plotidxs)
    r_cls = _maybe_strvec(cls)

    # Case A: user provided output filename
    if plot_filename:
        dsld = get_dsld()
        res = dsld.dsldFreqPCoord(
            r_data, r_m, r_sName, r_method, r_faceting, r_k, r_klm,
            r_keepidxs, r_plotidxs, r_cls, plot_filename
        )
    
        try:
            ro.r("print")(res)
        except Exception:
            pass
        return  

    # Case B: capture to a temporary PNG and show 
    fd, tmpfile = tempfile.mkstemp(suffix=".png")
    os.close(fd)
    try:
        grdevices = importr("grDevices")
        grdevices.png(file=tmpfile, width=1200, height=800, res=150)
        dsld = get_dsld()
        res = dsld.dsldFreqPCoord(
            r_data, r_m, r_sName, r_method, r_faceting, r_k, r_klm,
            r_keepidxs, r_plotidxs, r_cls, ro.NULL
        )
        
        try:
            ro.r("print")(res)
        except Exception:
            pass
    finally:
        grdevices.dev_off()

    if os.path.exists(tmpfile):
        display(Image(filename=tmpfile))
