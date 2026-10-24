"""Execute notebook examples in separate Jupyter kernels; keep outputs outside source."""

import argparse
import os
from pathlib import Path

import nbformat
from nbclient import NotebookClient


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output_directory", type=Path)
    parser.add_argument("--kernel", default="python3")
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[1]
    output_directory = args.output_directory.resolve()
    if output_directory.is_relative_to(root):
        parser.error("Choose an output directory outside the source checkout.")
    output_directory.mkdir(parents=True, exist_ok=True)
    # Render Plotly as notebook MIME output without launching a web browser.
    os.environ["PLOTLY_RENDERER"] = "plotly_mimetype"
    for path in sorted((root / "inst/examples").glob("*.ipynb")):
        notebook = nbformat.read(path, as_version=4)
        client = NotebookClient(
            notebook,
            timeout=600,
            kernel_name=args.kernel,
            allow_errors=False,
            resources={"metadata": {"path": str(root)}},
        )
        print(f"RUN {path.name} in a fresh {args.kernel} kernel", flush=True)
        client.execute()
        code_cells = [cell for cell in notebook.cells if cell.cell_type == "code"]
        assert all(cell.execution_count is not None for cell in code_cells)
        nbformat.write(notebook, output_directory / path.name)
        print(f"PASS {path.name}: {len(code_cells)} code cells", flush=True)


if __name__ == "__main__":
    main()
