"""Execute every notebook code cell without opening browsers or saving outputs.

Run from the repository root with an environment containing dsldPy, its example
extras, and the installed R package. Each notebook gets its own namespace.
"""

import contextlib
import json
from pathlib import Path
from unittest.mock import patch

import plotly.basedatatypes


@contextlib.contextmanager
def quiet_figures():
    with (
        patch.object(plotly.basedatatypes.BaseFigure, "show"),
        patch("IPython.display.display"),
    ):
        yield


def main():
    root = Path(__file__).resolve().parents[1]
    for path in sorted((root / "inst/examples").glob("*.ipynb")):
        notebook = json.loads(path.read_text())
        namespace = {"__name__": "__main__"}
        with quiet_figures():
            for index, cell in enumerate(notebook["cells"]):
                if cell["cell_type"] != "code":
                    continue
                source = cell["source"]
                if isinstance(source, list):
                    source = "".join(source)
                print(f"RUN {path.name}, cell {index}", flush=True)
                exec(compile(source, f"{path.name}:cell-{index}", "exec"), namespace)
        print(f"PASS {path.name}", flush=True)


if __name__ == "__main__":
    main()
