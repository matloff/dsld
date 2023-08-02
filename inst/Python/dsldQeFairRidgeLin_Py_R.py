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


"""

function(data,yName,deweightPars,sensNames=NULL,
                           holdout=floor(min(1000,0.1*nrow(data))))

"""

def getDeweightPars(dp):
    param = ""
    # ["occ=0.2", "edu=1", "age=30"]
    for p in dp:
        param += p + ','
    
    param = param[:-1]
    
    # param = "occ=0.2, edu=1, age=30"
    # list(occ=0.2, edu=1, age=30)

    return robjects.r('list')(param)
    



def dsldPyQeFairRidgeLin(data, yName, sName, deweightPars, holdout = R_NULL):
    # ************************** ARGUMENTS *******************************************

    # Data conversion handled by Utils function
    r_data = dsld_Rpy2_IsRDataframe(data)

    yName_r = robjects.StrVector([yName])                             # Convert variable name to R character vector

    sName_r = robjects.StrVector(sName)                               # Convert variable name to R character vector

    deweightPars_r = getDeweightPars(deweightPars)

    if holdout == R_NULL:
        holdout_r = math.floor(min(1000, 0.1 * robjects.r('nrow')(r_data)))
    else:
        holdout_r = holdout
    
    # All necessary arguments are in R format at this point
    # ************************** END ARGUMENTS *******************************************

    # ************************** RETURN VALUE *******************************************

    # Might need to convert the data back into pandas data frame or proper
    # python data format
    return dsld.dsldQeFairRidgeLin(r_data, yName_r, sName_r, deweightPars_r, holdout)

"""

#library(dsld)
#data("svcensus")
#z <- dsldQeFairRidgeLin(data=svcensus,yName='wageinc',deweightPars=list(occ=0.2),sensNames='gender')
#z$testAcc


"""



'''
    # Examples
    python
    from dsldNclm_Py_R import dsldPyNclm
    import rpy2.robjects as robjects
    robjects.r['data']('communities.and.crime')
    data = robjects.r('communities.and.crime')
    nclmR = dsldPyNclm(data, "ViolentCrimesPerPop", ["racepctblack","PctForeignBorn"], 0.05, robjects.r('cov'))
    print(robjects.r['summary'](nclmR))

    # Other examples
    from dsldNclm_Py_R import dsldPyNclm; import rpy2.robjects as robjects; robjects.r['data']('communities.and.crime'); data = robjects.r('communities.and.crime'); nclmR = dsldPyNclm(data, "ViolentCrimesPerPop", ["racepctblack","PctForeignBorn"], 0.05, robjects.r('cov')); print(robjects.r['summary'](nclmR))
'''