"""Data conversion and shared R/Python interface helpers."""

import json
import numbers
import os
import tempfile
from pathlib import Path

import numpy as np
import pandas as pd
import pyreadr
import rpy2.robjects as ro
from rpy2.robjects import default_converter, pandas2ri
from rpy2.robjects.conversion import localconverter
from rpy2.robjects.packages import importr
from rpy2.robjects.functions import Function


def get_dsld():
    """Load the installed R package; respect the caller's R library paths."""
    return importr("dsld")


def get_dsld_version():
    """Return the installed R package version, or None if it is unavailable."""
    try:
        return str(ro.r('as.character(utils::packageVersion("dsld"))')[0])
    except Exception:
        return None


def dsld_Rpy2_IsRDataframe(data):
    """Accept a pandas or R data frame and return an R data frame."""
    if isinstance(data, ro.vectors.DataFrame):
        return data
    if isinstance(data, pd.DataFrame):
        return dsld_Rpy2_PandasToRDataframe(data)
    raise TypeError("data must be a pandas.DataFrame or an R data.frame")


def dsld_Rpy2_PandasToRDataframe(pandas_df: pd.DataFrame):
    """Convert pandas data with a local converter; preserve categorical levels."""
    if not isinstance(pandas_df, pd.DataFrame):
        raise TypeError("Expected a pandas.DataFrame")
    with localconverter(default_converter + pandas2ri.converter):
        return ro.conversion.get_conversion().py2rpy(pandas_df)


def dsld_Rpy2_RDataframeToPandas(r_df):
    """Convert an R data frame, or copy an already converted pandas frame."""
    if isinstance(r_df, pd.DataFrame):
        return r_df.copy()
    if not isinstance(r_df, ro.vectors.DataFrame):
        raise TypeError("Expected an R data.frame or pandas.DataFrame")
    with localconverter(default_converter + pandas2ri.converter):
        return ro.conversion.get_conversion().rpy2py(r_df)


def read_data(filepath, **kwargs):
    """Read CSV or the first data object in an RData/RDA file."""
    extension = Path(filepath).suffix.lower()
    if extension == ".csv":
        return pd.read_csv(filepath, **kwargs)
    if extension in {".rdata", ".rda"}:
        objects = pyreadr.read_r(str(filepath))
        if not objects:
            raise ValueError("The R data file contains no readable objects")
        return next(iter(objects.values()))
    raise ValueError(f"Unsupported file extension: {extension}")


def preprocess_data(data, cat_features, num_features):
    """Convert named columns to R factors/numeric values and return an R frame."""
    r_data = dsld_Rpy2_IsRDataframe(data)
    convert_columns = ro.r["getFromNamespace"]("convert_cols", "dsld")
    return convert_columns(r_data, _str_vector(cat_features), _str_vector(num_features))


def _str_vector(value):
    if value is None:
        return ro.NULL
    if isinstance(value, str):
        return ro.StrVector([value])
    return ro.StrVector([str(item) for item in value])


def _r_value(value):
    """Convert typed scalar/list options without coercing booleans or strings to floats."""
    if value is None:
        return ro.NULL
    if isinstance(value, (ro.RObject, Function)):
        return value
    if isinstance(value, (bool, np.bool_)):
        return ro.BoolVector([bool(value)])
    if isinstance(value, numbers.Integral):
        return ro.IntVector([int(value)])
    if isinstance(value, numbers.Real):
        return ro.FloatVector([float(value)])
    if isinstance(value, str):
        return ro.StrVector([value])
    if isinstance(value, dict):
        return ro.ListVector({str(key): _r_value(item) for key, item in value.items()})
    if isinstance(value, (list, tuple, np.ndarray, pd.Series)):
        items = list(value)
        if not items:
            return ro.FloatVector([])
        if all(isinstance(item, str) for item in items):
            return ro.StrVector(items)
        if all(isinstance(item, (bool, np.bool_)) for item in items):
            return ro.BoolVector(items)
        if all(isinstance(item, numbers.Integral) for item in items):
            return ro.IntVector([int(item) for item in items])
        if all(isinstance(item, numbers.Real) for item in items):
            return ro.FloatVector([float(item) for item in items])
    raise TypeError(f"Unsupported R argument type: {type(value).__name__}")


def _python_predictions(predictions, flatten_matrix=True):
    """Preserve the legacy list shape while converting nested R values."""
    if isinstance(predictions, ro.vectors.ListVector):
        return [_python_predictions(item, flatten_matrix=False) for item in predictions]
    if isinstance(predictions, ro.vectors.FactorVector):
        return list(ro.r["as.character"](predictions))
    values = np.asarray(predictions)
    if flatten_matrix and values.ndim == 2 and 1 in values.shape:
        values = values.reshape(-1)
    return values.tolist()


def _correlations(frame):
    return list(
        zip(
            list(frame.rx2("feature")),
            [float(value) for value in frame.rx2("correlation")],
        )
    )


def _model_result(model, edf=False):
    predictions = model.rx2("trainPreds")
    if edf:
        predictions = predictions.rx2("preds")
    result = {
        "model": model,
        "train_predictions": _python_predictions(predictions),
        "train_accuracy": float(model.rx2("trainAcc")[0]),
        "train_correlations": _correlations(model.rx2("trainCorrs")),
    }
    if isinstance(predictions, ro.vectors.ListVector):
        result["train_classes"] = _python_predictions(predictions.rx2("predClasses"))
        result["train_probabilities"] = _python_predictions(
            predictions.rx2("probs"), flatten_matrix=False
        )
    return result


def _predict_result(model, new_data):
    prediction = ro.r["predict"](model["model"], dsld_Rpy2_IsRDataframe(new_data))
    predictions = prediction.rx2("preds")
    result = {
        "test_predictions": _python_predictions(predictions),
        "test_correlations": _correlations(prediction.rx2("correlations")),
    }
    if isinstance(predictions, ro.vectors.ListVector):
        result["test_classes"] = _python_predictions(predictions.rx2("predClasses"))
        result["test_probabilities"] = _python_predictions(
            predictions.rx2("probs"), flatten_matrix=False
        )
    return result


def _display_r_plot(draw, width=1200, height=800, res=150, show=True):
    """Render a base/ggplot R plot; close the device and remove temporary files."""
    from IPython.display import Image, display

    descriptor, filename = tempfile.mkstemp(suffix=".png")
    os.close(descriptor)
    graphics = importr("grDevices")
    try:
        graphics.png(file=filename, width=width, height=height, res=res)
        try:
            result = draw()
            if result is not ro.NULL and bool(ro.r["inherits"](result, "ggplot")[0]):
                ro.r["print"](result)
        finally:
            graphics.dev_off()
        picture = Image(data=Path(filename).read_bytes())
        if show:
            display(picture)
        return picture
    finally:
        Path(filename).unlink(missing_ok=True)


def _plotly_figure(widget):
    """Convert an R plotly widget to a Python figure without opening a browser."""
    import plotly.graph_objects as go

    payload = json.loads(str(importr("plotly").plotly_json(widget, jsonedit=False)[0]))
    # R plotly attaches an internal frame marker to static traces. It is
    # not a Plotly.js trace property and Python correctly rejects it.
    traces = [
        {key: value for key, value in trace.items() if key != "frame"}
        for trace in payload["data"]
    ]
    return go.Figure(data=traces, layout=payload.get("layout", {}))
