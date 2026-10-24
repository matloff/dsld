"""Integration tests against an installed R package and the Python interface."""

import contextlib
import io
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

import numpy as np
import pandas as pd
import plotly.basedatatypes
import rpy2.robjects as ro
from rpy2.robjects.packages import importr
import dsldPy as ds
from dsldPy.Utils import _r_value, _display_r_plot, get_dsld_version


class WrapperTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        rng = np.random.default_rng(910)
        cls.data = pd.DataFrame(
            {
                "x": rng.normal(size=120),
                "z": rng.normal(size=120),
                "group": pd.Categorical(["a", "b"] * 60),
            }
        )
        cls.data["y"] = 2 + 3 * cls.data.x + 0.5 * cls.data.z + rng.normal(size=120)
        cls.binary = cls.data.copy()
        cls.binary["y"] = pd.Categorical(
            np.where(rng.random(120) < 1 / (1 + np.exp(-cls.data.x)), "yes", "no")
        )
        cls.predictors = cls.data[["x", "z", "group"]]
        ro.r["set.seed"](910)
        cls.linear = ds.dsldPyLinear(cls.data, "y", "group")
        cls.logit = ds.dsldPyLogit(cls.binary, "y", "group", yesYVal="yes")

    def test_versions(self):
        self.assertEqual(get_dsld_version(), "2.0.0")
        self.assertEqual(ds.__version__, "2.0.0")

    def test_roundtrip_and_validation(self):
        r_data = ds.dsld_Rpy2_IsRDataframe(self.data)
        actual = ds.dsld_Rpy2_RDataframeToPandas(r_data)
        np.testing.assert_allclose(actual.x, self.data.x)
        self.assertEqual(list(actual.group.cat.categories), ["a", "b"])
        with self.assertRaises(TypeError):
            ds.dsld_Rpy2_IsRDataframe([[1, 2]])

    def test_preprocess_factor_labels_to_numeric(self):
        frame = pd.DataFrame(
            {"label": ["a", "b"], "value": pd.Categorical(["10", "20"])}
        )
        result = ds.preprocess_data(frame, ["label"], ["value"])
        np.testing.assert_allclose(list(result.rx2("value")), [10, 20])

    def test_typed_options(self):
        self.assertEqual(list(_r_value(True)), [True])
        self.assertEqual(list(_r_value("yes")), ["yes"])
        self.assertEqual(list(_r_value([1, 2])), [1, 2])
        self.assertIs(_r_value(None), ro.NULL)

    def test_linear_methods_without_global_assignments(self):
        before = set(ro.globalenv.keys())
        with contextlib.redirect_stdout(io.StringIO()):
            ds.dsldPyLinearSummary(self.linear)
            ds.dsldPyLinearCoef(self.linear)
            ds.dsldPyLinearVcov(self.linear)
            values = ds.dsldPyLinearGetData(self.linear)
        result = ds.dsldPyLinearPredict(self.linear, self.predictors.head())
        self.assertEqual(result.shape, (5, 3))
        self.assertTrue(len(values))
        self.assertEqual(before, set(ro.globalenv.keys()))

    def test_logit_methods_without_global_assignments(self):
        before = set(ro.globalenv.keys())
        with contextlib.redirect_stdout(io.StringIO()):
            ds.dsldPyLogitSummary(self.logit)
            ds.dsldPyLogitCoef(self.logit)
            ds.dsldPyLogitVcov(self.logit)
            ds.dsldPyLogitGetData(self.logit)
        result = ds.dsldPyLogitPredict(self.logit, self.predictors.head())
        self.assertTrue(result.prediction.between(0, 1).all())
        self.assertEqual(before, set(ro.globalenv.keys()))

    def test_custom_ml_comparisons_and_typed_opts(self):
        accuracy, comparisons = ds.dsldPyML(
            self.data,
            "y",
            "group",
            "qeKNN",
            self.predictors.drop(columns="group").head(3),
            {"k": 5, "scaleX": False, "holdout": 10},
        )
        self.assertEqual(comparisons.shape, (3, 4))
        self.assertEqual(set(accuracy), {"testAcc: a", "testAcc: b"})

    def test_edf_regression_models(self):
        for function, options in [
            (ds.dsldPyQeFairKNN, {"k": 5}),
            (ds.dsldPyQeFairRF, {"nTree": 30}),
            (ds.dsldPyQeFairRidgeLin, {}),
        ]:
            with self.subTest(model=function.__name__):
                model = function(self.data, "y", "group", {"x": 0.5}, **options)
                result = ds.dsldPyQeFairML_Predict(model, self.predictors)
                self.assertEqual(len(result["test_predictions"]), len(self.data))
                self.assertTrue(np.isfinite(result["test_predictions"]).all())

    def test_edf_classification_models(self):
        for function, options in [
            (ds.dsldPyQeFairKNN, {"k": 5}),
            (ds.dsldPyQeFairRF, {"nTree": 30}),
            (ds.dsldPyQeFairRidgeLog, {}),
        ]:
            with self.subTest(model=function.__name__):
                model = function(
                    self.binary, "y", "group", {"x": 0.5}, yesYVal="no", **options
                )
                result = ds.dsldPyQeFairML_Predict(model, self.predictors)
                self.assertEqual(len(result["test_classes"]), len(self.data))
                self.assertTrue(set(result["test_classes"]).issubset({"yes", "no"}))
                self.assertIn("test_probabilities", result)

    def test_single_row_classifier_preserves_probability_columns(self):
        model = ds.dsldPyQeFairRF(self.binary, "y", "group", nTree=20, yesYVal="yes")
        result = ds.dsldPyQeFairML_Predict(model, self.predictors.head(1))
        self.assertEqual(np.asarray(result["test_probabilities"]).shape, (1, 2))
        self.assertEqual(len(result["test_classes"]), 1)
        self.assertTrue(
            all(np.isnan(value) for _, value in result["test_correlations"])
        )

    def test_multiple_sensitive_columns(self):
        frame = self.data.copy()
        frame["group2"] = pd.Categorical(["u", "u", "v", "v"] * 30)
        model = ds.dsldPyQeFairKNN(frame, "y", ["group", "group2"], {"x": 0.5}, k=5)
        self.assertEqual(len(model["train_correlations"]), 4)

    def test_fairml_models(self):
        functions = [
            (ds.dsldPyFrrm, False),
            (ds.dsldPyFgrrm, True),
            (ds.dsldPyNclm, False),
            (ds.dsldPyZlm, False),
            (ds.dsldPyZlrm, True),
        ]
        for function, classification in functions:
            with self.subTest(model=function.__name__):
                optional = (
                    "cccp"
                    if function == ds.dsldPyNclm
                    else "CVXR"
                    if function in (ds.dsldPyZlm, ds.dsldPyZlrm)
                    else None
                )
                if optional and not bool(
                    ro.r["requireNamespace"](optional, quietly=True)[0]
                ):
                    continue
                options = {"yesYVal": "yes"} if classification else {}
                model = function(
                    self.binary if classification else self.data,
                    "y",
                    "group",
                    0.8,
                    **options,
                )
                result = ds.dsldPyFairML_Predict(model, self.predictors)
                self.assertEqual(len(result["test_predictions"]), len(self.data))
                with contextlib.redirect_stdout(io.StringIO()):
                    self.assertIsNotNone(ds.dsldPyFairML_Summary(model))

    def test_cross_validation_model_args(self):
        result = ds.dsldPyFairUtils(
            self.data,
            "y",
            "group",
            "dsldQeFairKNN",
            deweightPars={"x": [0.3, 1]},
            k_folds=3,
            model_args={"k": 5, "scaleX": False},
        )
        self.assertEqual(result.shape[0], 2)
        self.assertTrue(np.isfinite(result.testAcc).all())

    def test_frequency_labels(self):
        frame = pd.DataFrame({"group": ["a", "a", "b"], "category": ["z", "a", "a"]})
        result = ds.dsldPyFrequencybyS(frame, "category", "group")
        self.assertEqual(list(result.columns), ["Frequency of a", "Frequency of z"])

    def test_feature_comparisons(self):
        result = ds.dsldPyTakeALookAround(
            self.data, "y", "group", maxFeatureSize=1, holdout=20
        )
        self.assertEqual(list(result.columns), ["Feature Names", "a", "b", "c"])
        self.assertEqual(len(result), 2)

    def test_matching_returns_object(self):
        if not bool(ro.r["requireNamespace"]("Matching", quietly=True)[0]):
            self.skipTest("Matching unavailable")
        result = ds.dsldPyMatchedATE(self.binary, "y", "group", "b", "yes")
        self.assertIn("est", result.names)

    def test_hunting_results(self):
        if not bool(ro.r["requireNamespace"]("randomForest", quietly=True)[0]):
            self.skipTest("randomForest unavailable")
        result = ds.dsldPyCHunting(self.data, "y", "group", intersectDepth=2)
        self.assertEqual(set(result), {"impForY", "impForS", "inCommon"})
        self.assertIsNotNone(ds.dsldPyOHunting(self.data, "y", "group"))

    def test_graphics(self):
        with (
            patch.object(plotly.basedatatypes.BaseFigure, "show"),
            patch("IPython.display.display"),
        ):
            figure = ds.dsldPyDensitybyS(self.data, "x", "group")
            self.assertEqual(len(figure.data), 32)
            self.assertEqual(len(figure.layout.sliders[0].steps), 16)
            image = ds.dsldPyDensitybyS(self.data, "x", "group", graphType="plot")
            self.assertTrue(image.data)
            figure3d = ds.dsldPyScatterPlot3D(self.data, ["x", "z", "y"], "group")
            self.assertTrue(len(figure3d.data))
            results = ds.dsldPyConfounders(self.data, "group", graphType="plot")
            self.assertEqual(set(results), {"x", "z", "y"})
            self.assertTrue(
                ds.dsldPyFreqPCoord(
                    self.data, 10, "group", k=5, klm=10, show=False
                ).data
            )
            self.assertTrue(
                ds.dsldPyConditDisparity(
                    self.data, "y", "group", "x", minS=10, useLoess=False
                ).data
            )

    def test_network_graph_and_device_cleanup(self):
        if not bool(ro.r["requireNamespace"]("bnlearn", quietly=True)[0]):
            self.skipTest("bnlearn unavailable")
        with (
            tempfile.TemporaryDirectory() as directory,
            patch("dsldPy.dsldPyBnLearn.display"),
        ):
            path = Path(directory) / "network.png"
            device = int(importr("grDevices").dev_cur()[0])
            result = ds.dsldPyIamb(self.data.drop(columns="y"), file=str(path))
            self.assertEqual(result, str(path))
            self.assertGreater(path.stat().st_size, 0)
            self.assertEqual(device, int(importr("grDevices").dev_cur()[0]))

    def test_graphics_device_closes_on_failure(self):
        before = int(importr("grDevices").dev_cur()[0])

        def fail():
            raise ValueError("deliberate rendering failure")

        with self.assertRaisesRegex(ValueError, "deliberate"):
            _display_r_plot(fail, show=False)
        self.assertEqual(before, int(importr("grDevices").dev_cur()[0]))

    def test_read_csv(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "data.csv"
            self.data.to_csv(path, index=False)
            self.assertEqual(ds.read_data(path).shape, self.data.shape)


if __name__ == "__main__":
    unittest.main(verbosity=2)
