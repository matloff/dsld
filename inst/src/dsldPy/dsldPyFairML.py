"""Python interfaces to the fairml models; lamda retains its public spelling."""

import rpy2.robjects as robjects
from .Utils import (
    get_dsld,
    dsld_Rpy2_IsRDataframe,
    _r_value,
    _str_vector,
    _model_result,
    _predict_result,
)


def dsldPyFrrm(
    data, yName, sName, unfairness, definition="sp-komiyama", lamda=0, save=False
):
    model = get_dsld().dsldFrrm(
        dsld_Rpy2_IsRDataframe(data),
        yName,
        _str_vector(sName),
        _r_value(float(unfairness)),
        _r_value(definition),
        _r_value(float(lamda)),
        _r_value(save),
    )
    return _model_result(model)


def dsldPyFgrrm(
    data,
    yName,
    sName,
    unfairness,
    definition="sp-komiyama",
    family="binomial",
    lamda=0,
    save=False,
    yesYVal=None,
):
    model = get_dsld().dsldFgrrm(
        dsld_Rpy2_IsRDataframe(data),
        yName,
        _str_vector(sName),
        _r_value(float(unfairness)),
        _r_value(definition),
        _r_value(family),
        _r_value(float(lamda)),
        _r_value(save),
        _r_value(yesYVal),
    )
    return _model_result(model)


def dsldPyNclm(
    data, yName, sName, unfairness, covfun=robjects.r("cov"), lamda=0, save=False
):
    model = get_dsld().dsldNclm(
        dsld_Rpy2_IsRDataframe(data),
        yName,
        _str_vector(sName),
        _r_value(float(unfairness)),
        _r_value(covfun),
        _r_value(float(lamda)),
        _r_value(save),
    )
    return _model_result(model)


def dsldPyZlm(data, yName, sName, unfairness):
    model = get_dsld().dsldZlm(
        dsld_Rpy2_IsRDataframe(data),
        yName,
        _str_vector(sName),
        _r_value(float(unfairness)),
    )
    return _model_result(model)


def dsldPyZlrm(data, yName, sName, unfairness, yesYVal):
    model = get_dsld().dsldZlrm(
        dsld_Rpy2_IsRDataframe(data),
        yName,
        _str_vector(sName),
        _r_value(float(unfairness)),
        _r_value(yesYVal),
    )
    return _model_result(model)


def dsldPyFairML_Summary(model):
    result = robjects.r["summary"](model["model"])
    print(result)
    return result


def dsldPyFairML_Predict(model, newData):
    return _predict_result(model, newData)
