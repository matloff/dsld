"""Bayesian-network discovery and plotting."""

import rpy2.robjects as ro
from rpy2.robjects.packages import importr
from IPython.display import Image, display
from .Utils import get_dsld, dsld_Rpy2_IsRDataframe


def dsldPyIamb(data, file="iamb.png", width=1200, height=900, res=150):
    network = get_dsld().dsldIamb(dsld_Rpy2_IsRDataframe(data))
    graphics = importr("grDevices")
    graphics.png(file=file, width=width, height=height, res=res)
    try:
        ro.r["plot"](network)
    finally:
        graphics.dev_off()
    display(Image(filename=file))
    return file
