
from .Utils import get_dsld, dsld_Rpy2_IsRDataframe
import sys
import os
import pandas as pd
from PIL import Image
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from rpy2.robjects import r
import os, tempfile
import rpy2.robjects as ro
from rpy2.robjects.vectors import StrVector, IntVector, BoolVector
from IPython.display import Image, display
from .Utils import dsld_Rpy2_IsRDataframe
from rpy2.robjects.packages import importr

def dsldPyConfounders(data,sName, graphType = "plotly",fill=False):
    r_data = dsld_Rpy2_IsRDataframe(data)
    sName_r = robjects.StrVector([sName])
    graphType_r = robjects.StrVector([graphType])
    fill_r = robjects.BoolVector([fill])
    
    fd, tmpfile = tempfile.mkstemp(suffix=".png"); os.close(fd)
    grdevices = importr('grDevices')
    grdevices.png(file=tmpfile, width=1200, height=800, res=150)
    try:
        dsld = get_dsld()
        res = dsld.dsldConfounders(r_data, sName_r, graphType_r, fill_r)
        try: ro.r("print")(res)
        except: pass
    finally:
        grdevices.dev_off()

    if os.path.exists(tmpfile): display(Image(filename=tmpfile))
    return

