"""Python interfaces to the explicitly deweighted feature models."""

from .Utils import (
    get_dsld,
    dsld_Rpy2_IsRDataframe,
    _str_vector,
    _r_value,
    _model_result,
    _predict_result,
)


def dsldPyQeFairKNN(
    data, yName, sNames, deweightPars=None, yesYVal=None, k=25, scaleX=True
):
    model = get_dsld().dsldQeFairKNN(
        dsld_Rpy2_IsRDataframe(data),
        yName,
        _str_vector(sNames),
        _r_value(deweightPars),
        _r_value(yesYVal),
        _r_value(k),
        _r_value(scaleX),
    )
    return _model_result(model, edf=True)


def dsldPyQeFairRF(
    data,
    yName,
    sNames,
    deweightPars=None,
    nTree=500,
    minNodeSize=10,
    mtry=None,
    yesYVal=None,
):
    r_data = dsld_Rpy2_IsRDataframe(data)
    if mtry is None:
        # Let R determine the default after factor expansion and S exclusion.
        model = get_dsld().dsldQeFairRF(
            r_data,
            yName,
            _str_vector(sNames),
            deweightPars=_r_value(deweightPars),
            nTree=_r_value(nTree),
            minNodeSize=_r_value(minNodeSize),
            yesYVal=_r_value(yesYVal),
        )
    else:
        model = get_dsld().dsldQeFairRF(
            r_data,
            yName,
            _str_vector(sNames),
            _r_value(deweightPars),
            _r_value(nTree),
            _r_value(minNodeSize),
            _r_value(mtry),
            _r_value(yesYVal),
        )
    return _model_result(model, edf=True)


def dsldPyQeFairRidgeLin(data, yName, sNames, deweightPars=None):
    model = get_dsld().dsldQeFairRidgeLin(
        dsld_Rpy2_IsRDataframe(data), yName, _str_vector(sNames), _r_value(deweightPars)
    )
    return _model_result(model, edf=True)


def dsldPyQeFairRidgeLog(data, yName, sNames, deweightPars=None, yesYVal=None):
    model = get_dsld().dsldQeFairRidgeLog(
        dsld_Rpy2_IsRDataframe(data),
        yName,
        _str_vector(sNames),
        _r_value(deweightPars),
        _r_value(yesYVal),
    )
    return _model_result(model, edf=True)


def dsldPyQeFairML_Predict(model, newData):
    return _predict_result(model, newData)
