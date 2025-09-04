import os, tempfile
import rpy2.robjects as ro
from rpy2.robjects.vectors import StrVector, IntVector, BoolVector
from IPython.display import Image, display
from .Utils import get_dsld, dsld_Rpy2_IsRDataframe
from rpy2.robjects.packages import importr

def dsldPyDensitybyS(data, cName, sName, graphType = "plotly", fill = False):

    r_data = dsld_Rpy2_IsRDataframe(data)
    cName_r = StrVector([cName])
    sName_r = StrVector([sName])
    graphType_r = StrVector([graphType])
    fill_r = BoolVector([fill])

    fd, tmpfile = tempfile.mkstemp(suffix=".png"); os.close(fd)
    grdevices = importr("grDevices")
    grdevices.png(file=tmpfile, width=1200, height=800, res=150)
    try:
        dsld = get_dsld()
        res = dsld.dsldDensityByS(r_data, cName_r, sName_r, graphType_r, fill_r)
        try: ro.r("print")(res)
        except: pass
    finally:
        grdevices.dev_off()

    if os.path.exists(tmpfile): display(Image(filename=tmpfile))
    return

