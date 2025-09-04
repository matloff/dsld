'''
Python interface for dsldFairML functions in the dsld R package.
'''

from .Utils import get_dsld, dsld_Rpy2_IsRDataframe
from rpy2.robjects.packages import importr
import rpy2.robjects as robjects


# ================== dsldFrrm ==================

def dsldPyFrrm(data, yName, sName, unfairness, definition="sp-komiyama", lamda=0, save=False):
    r_data   = dsld_Rpy2_IsRDataframe(data)
    yName_r  = robjects.StrVector([yName])
    sName_r  = robjects.StrVector([sName])
    unfair_r = robjects.FloatVector([unfairness])
    def_r    = robjects.StrVector([definition])
    lamda_r  = robjects.FloatVector([lamda])
    save_r   = robjects.BoolVector([save])

    dsld = get_dsld()
    model = dsld.dsldFrrm(r_data, yName_r, sName_r, unfair_r, def_r, lamda_r, save_r)

    preds = model.rx2("trainPreds")
    acc   = model.rx2("trainAcc")
    corrs = model.rx2("trainCorrs")

    result = {
        "model": model,
        "train_predictions": list(preds),
        "train_accuracy": float(acc[0]),
        "train_correlations": list(zip(list(corrs.rx2("feature")),
                                       list(corrs.rx2("correlation"))))
    }
    return result

# ================== dsldFgrrm ==================

def dsldPyFgrrm(data, yName, sName, unfairness, definition="sp-komiyama", family="binomial", lamda=0, save=False, yesYVal = None):
    r_data   = dsld_Rpy2_IsRDataframe(data)
    yName_r  = robjects.StrVector([yName])
    sName_r  = robjects.StrVector([sName])

    unfair_r = robjects.FloatVector([unfairness])
    
    def_r    = robjects.StrVector([definition])
    fam_r    = robjects.StrVector([family])
    lamda_r  = robjects.FloatVector([lamda])
    save_r   = robjects.BoolVector([save])
    yesYVal_r = robjects.StrVector([yesYVal])


    dsld = get_dsld()
    model = dsld.dsldFgrrm(r_data, yName_r, sName_r, unfair_r, def_r, fam_r, lamda_r, save_r, yesYVal_r)

    preds = model.rx2("trainPreds")
    acc   = model.rx2("trainAcc")
    corrs = model.rx2("trainCorrs")

    result = {
        "model": model,
        "train_predictions": list(preds),
        "train_accuracy": float(acc[0]),
        "train_correlations": list(zip(list(corrs.rx2("feature")),
                                       list(corrs.rx2("correlation"))))
    }
    return result

# ================== dsldNclm ==================

def dsldPyNclm(data, yName, sName, unfairness, covfun=robjects.r('cov'), lamda=0, save=False):
    r_data   = dsld_Rpy2_IsRDataframe(data)
    yName_r  = robjects.StrVector([yName])
    sName_r  = robjects.StrVector([sName])
    unfair_r = robjects.FloatVector([unfairness])
    lamda_r  = robjects.FloatVector([lamda])
    save_r   = robjects.BoolVector([save])

    dsld = get_dsld()
    model = dsld.dsldNclm(r_data, yName_r, sName_r, unfair_r, covfun, lamda_r, save_r)

    preds = model.rx2("trainPreds")
    acc   = model.rx2("trainAcc")
    corrs = model.rx2("trainCorrs")

    result = {
        "model": model,
        "train_predictions": list(preds),
        "train_accuracy": float(acc[0]),
        "train_correlations": list(zip(list(corrs.rx2("feature")),
                                       list(corrs.rx2("correlation"))))
    }
    return result

# ================== dsldZlm ==================

def dsldPyZlm(data, yName, sName, unfairness):
    r_data   = dsld_Rpy2_IsRDataframe(data)
    yName_r  = robjects.StrVector([yName])
    sName_r  = robjects.StrVector([sName])
    unfair_r = robjects.FloatVector([unfairness])

    dsld = get_dsld()
    model = dsld.dsldZlm(r_data, yName_r, sName_r, unfair_r)

    preds = model.rx2("trainPreds")
    acc   = model.rx2("trainAcc")
    corrs = model.rx2("trainCorrs")

    result = {
        "model": model,
        "train_predictions": list(preds),
        "train_accuracy": float(acc[0]),
        "train_correlations": list(zip(list(corrs.rx2("feature")),
                                       list(corrs.rx2("correlation"))))
    }
    return result

# ================== dsldZlrm ==================

def dsldPyZlrm(data, yName, sName, unfairness, yesYVal):
    r_data   = dsld_Rpy2_IsRDataframe(data)
    yName_r  = robjects.StrVector([yName])
    sName_r  = robjects.StrVector([sName])
    unfair_r = robjects.FloatVector([unfairness])
    yesYVal_r = robjects.StrVector([yesYVal])

    dsld = get_dsld()
    model = dsld.dsldZlrm(r_data, yName_r, sName_r, unfair_r, yesYVal_r)

    preds = model.rx2("trainPreds")
    acc   = model.rx2("trainAcc")
    corrs = model.rx2("trainCorrs")

    result = {
        "model": model,
        "train_predictions": list(preds),
        "train_accuracy": float(acc[0]),
        "train_correlations": list(zip(list(corrs.rx2("feature")),
                                       list(corrs.rx2("correlation"))))
    }
    return result


# predict() and summary() method for all the models

def dsldPyFairML_Summary(model):
    print(robjects.r['summary'](model['model']))
    return robjects.r['summary'](model['model'])

def dsldPyFairML_Predict(model, newData):
    robjects.r.assign("model", model['model'])
    xNew = dsld_Rpy2_IsRDataframe(newData)
    robjects.r.assign("xNew", xNew)
    result = robjects.r('predict(model, xNew)')
    names = list(result[1][0])
    vals  = [float(v) for v in result[1][1]]
    correlations = list(zip(names, vals))
    output = {'test_predictions': list(result[0]),
              'test_correlations': correlations}
    return output
