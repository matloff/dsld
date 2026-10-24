from .Utils import get_dsld, dsld_Rpy2_IsRDataframe
import rpy2.robjects as robjects
import rpy2.robjects as ro


### dsldPyCHunting
def dsldPyCHunting(data, yName, sName, intersectDepth=10):
    r_data = dsld_Rpy2_IsRDataframe(data)
    yName_r = robjects.StrVector([yName])
    sName_r = robjects.StrVector([sName])
    intersectDepth_r = robjects.IntVector([intersectDepth])

    dsld = get_dsld()
    res = dsld.dsldCHunting(r_data, yName_r, sName_r, intersectDepth_r)
    result = {
        "impForY": list(zip(list(res[0].names), list(res[0]))),
        "impForS": list(zip(list(res[1].names), list(res[1]))),
    }
    result["inCommon"] = [list(features) for features in res.rx2("inCommon")]
    return result


### dsldPyOHunting
def dsldPyOHunting(data, yName, sName):
    r_data = dsld_Rpy2_IsRDataframe(data)
    yName_r = robjects.StrVector([yName])
    sName_r = robjects.StrVector([sName])
    dsld = get_dsld()
    res = dsld.dsldOHunting(r_data, yName_r, sName_r)

    # print in R
    ro.r("print")(res)
    return res
