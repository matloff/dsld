'''
    dsldQeFairKNN_Py_R.py is the python interface for dsldQeFairKNN in the dsld R package.
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
    dsld contains this file's main R function, dsldQFairKNN
'''
dsld = importr("dsld")

# # dsldQeFairKNN <- function(data, yName, sName = NULL,deweightPars, scaleX = TRUE, yesYVal= NULL,
#                           k=25, holdout=floor(min(1000,0.1*nrow(data)))) 
def dsldPyQeFairKNN(data, yName, sName = R_NULL, deweightPars = R_NULL, scaleX = True,
                   yesYVal = "", k = 25, holdout = R_NULL):
    # ************************** ARGUMENTS *******************************************

    # Data conversion handled by Utils function
    r_data = dsld_Rpy2_IsRDataframe(data)

    yName_r = robjects.StrVector([yName])                             # Convert variable name to R character vector

    if sName == R_NULL:
        sName_r = sName
    else:
        sName_r = robjects.StrVector([sName])                           # Convert variable name to R character vector

    deweightPars_r = robjects.ListVector(deweightPars)

    scaleX_r = robjects.BoolVector([scaleX])

    yesYVal_r = robjects.StrVector([yesYVal])

    k_r = robjects.IntVector(k)

    if holdout == R_NULL:
        # holdout_r = robjects.IntVector([math.floor(min(1000, 0.1 * robjects.r('nrow')(r_data)))])
        robjects.r.assign("rdata", r_data)
        robjects.r('hold <- floor(min(1000, 0.1 * nrow(rdata)))')
        holdout_r = robjects.r('hold')
    else:
        holdout_r = robjects.IntVector([holdout])
    
    # All necessary arguments are in R format at this point
    # ************************** END ARGUMENTS *******************************************

    # ************************** RETURN VALUE *******************************************

    # Might need to convert the data back into pandas data frame or proper
    # python data format
    return dsld.dsldQeFairKNN(r_data, yName_r, sName_r, deweightPars_r, scaleX_r, yesYVal_r, k_r, holdout_r)

'''
    # NON working Examples
    python
    from dsldQeFairKNN_Py_R import dsldPyQeFairKNN
    import rpy2.robjects as robjects
    robjects.r['data']('svcensus')
    data = robjects.r('svcensus')
    resR = dsldPyQeFairKNN(data, "wageinc", sName = "gender", deweightPars = {"occ":0.2})
    robjects.r.assign("resR", resR)
    #print(robjects.r('resR$testAcc'))
    robjects.r('print(resR$testAcc)')

    # Other examples
    from dsldQeFairKNN_Py_R import dsldPyQeFairKNN; import rpy2.robjects as robjects; robjects.r['data']('svcensus'); data = robjects.r('svcensus'); resR = dsldPyQeFairKNN(data, "wageinc", sName = "gender", deweightPars = {"occ":0.2}); robjects.r.assign("resR", resR); robjects.r('print(resR$testAcc)')
'''