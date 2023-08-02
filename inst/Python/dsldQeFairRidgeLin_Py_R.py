'''
    dsldQeFairRidgeLin_Py_R.py is the python interface for dsldQeFairRidgeLin in the dsld R package.
    Utils contains helper functions which are used in multiple files
    rpy2 is used handle dsld function calls from R
    pandas processes dataset into a panda's data frame before performing any computation
    sys is used for OS shell
'''

from Utils import dsld_Rpy2_IsRDataframe, ERROR, R_NULL
from rpy2.robjects.packages import importr
import rpy2.robjects as robjects
import pandas as pd
import math
import sys

'''
    Importing r packages {dsld} into Python through rpy2
    dsld contains this file's main R function, dsldQFairRidgeLin
'''
dsld = importr("dsld")

def getDeweightPars(dp):
    param = ""
    # ["occ=0.2", "edu=1", "age=30"]
    for p in dp:
        param += p + ','
    
    param = param[:-1]
    
    # param = "occ=0.2, edu=1, age=30"
    # list(occ=0.2, edu=1, age=30)

    return robjects.r('list')(param)
    

def dsldPyQeFairRidgeLin(data, yName, sName = R_NULL, deweightPars = R_NULL, holdout = R_NULL):
    # ************************** ARGUMENTS *******************************************

    # Data conversion handled by Utils function
    r_data = dsld_Rpy2_IsRDataframe(data)

    yName_r = robjects.StrVector([yName])                             # Convert variable name to R character vector

    if sName == R_NULL:
        sName_r = sName
    else:
        sName_r = robjects.StrVector(sName)                               # Convert variable name to R character vector

    deweightPars_r = getDeweightPars(deweightPars)
    print(deweightPars_r)

    if holdout == R_NULL:
        # holdout_r = robjects.IntVector([math.floor(min(1000, 0.1 * robjects.r('nrow')(r_data)))])
        robjects.r.assign("rdata", r_data)
        robjects.r('hold <- floor(min(1000, 0.1 * nrow(rdata)))')
        holdout_r = robjects.r('hold')
    else:
        holdout_r = holdout
    
    # All necessary arguments are in R format at this point
    # ************************** END ARGUMENTS *******************************************

    # ************************** RETURN VALUE *******************************************

    # Might need to convert the data back into pandas data frame or proper
    # python data format
    return dsld.dsldQeFairRidgeLin(r_data, yName_r, sName_r, deweightPars_r, holdout)


'''
    # Examples
    python
    from dsldQeFairRidgeLin_Py_R import dsldPyQeFairRidgeLin
    import rpy2.robjects as robjects
    robjects.r['data']('svcensus')
    data = robjects.r('svcensus')
    resR = dsldPyQeFairRidgeLin(data, "wageinc", deweightPars = ["occ=0.2"], holdout = "gender")
    robjects.r.assign("resR", resR)
    robjects.r('print(resR$testAcc)')

    # Other examples
    from dsldQeFairRidgeLin_Py_R import dsldPyQeFairRidgeLin; import rpy2.robjects as robjects; robjects.r['data']('svcensus'); data = robjects.r('svcensus'); resR = dsldPyQeFairRidgeLin(data, "wageinc", deweightPars = ["occ=0.2"], holdout = "gender"); robjects.r.assign("resR", resR); robjects.r('print(resR$testAcc)')
'''