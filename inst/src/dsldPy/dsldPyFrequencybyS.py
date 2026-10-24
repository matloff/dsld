from rpy2.robjects.vectors import StrVector
from .Utils import get_dsld, dsld_Rpy2_IsRDataframe, dsld_Rpy2_RDataframeToPandas


def dsldPyFrequencybyS(data, cName, sName):

    r_data = dsld_Rpy2_IsRDataframe(data)
    cName_r = StrVector([cName])
    sName_r = StrVector([sName])

    dsld = get_dsld()
    res = dsld.dsldFrequencyByS(r_data, cName_r, sName_r)
    return dsld_Rpy2_RDataframeToPandas(res)
