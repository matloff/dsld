import os, tempfile
import rpy2.robjects as ro
from rpy2.robjects.vectors import StrVector, IntVector, BoolVector
from IPython.display import Image, display
from .Utils import get_dsld, dsld_Rpy2_IsRDataframe, dsld_Rpy2_RDataframeToPandas
from rpy2.robjects.packages import importr
import rpy2.robjects as robjects

def dsldPyMatchedATE(data, yName, sName, yesSVal, yesYVal=None, propensFtn=None, k=None):

    r_data = dsld_Rpy2_IsRDataframe(data)
    yName_r = robjects.StrVector([yName])
    sName_r = robjects.StrVector([sName])
    yesSVal_r = robjects.StrVector([yesSVal])

    yesYVal_r = robjects.StrVector([yesYVal]) if yesYVal is not None else robjects.NULL
    propensFtn_r = robjects.StrVector([propensFtn]) if propensFtn is not None else robjects.NULL
    k_r = robjects.IntVector([k]) if k is not None else robjects.NULL

    dsld = get_dsld()
    res = dsld.dsldMatchedATE(r_data, yName_r, sName_r, yesSVal_r, yesYVal_r, propensFtn_r, k_r)

    ro.r("summary")(res)

    return


