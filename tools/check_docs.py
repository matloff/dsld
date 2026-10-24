"""Validate local Markdown links, notebook structure, and notebook Python syntax."""

import ast
import re
from pathlib import Path
from urllib.parse import unquote, urlparse

import nbformat

ROOT = Path(__file__).resolve().parents[1]


def main():
    checked = 0
    for path in sorted(ROOT.rglob("*")):
        if any(part.startswith(".") for part in path.relative_to(ROOT).parts):
            continue
        if path.suffix not in {".md", ".Rmd"}:
            continue
        source = path.read_text()
        assert sum(line.startswith("```") for line in source.splitlines()) % 2 == 0, (
            path
        )
        prose = re.sub(r"^```[^\n]*\n.*?^```", "", source, flags=re.M | re.S)
        for target in re.findall(r"!?\[[^\]]*\]\(([^\s)]+)(?:\s+[^)]*)?\)", prose):
            parsed = urlparse(target.strip("<>"))
            if parsed.scheme or parsed.netloc or not parsed.path:
                continue
            assert (path.parent / unquote(parsed.path)).exists(), (
                f"Broken link in {path}: {target}"
            )
            checked += 1
    cells = 0
    for path in sorted((ROOT / "inst/examples").glob("*.ipynb")):
        notebook = nbformat.read(path, as_version=4)
        nbformat.validate(notebook)
        for cell in notebook.cells:
            if cell.cell_type == "code":
                ast.parse(cell.source, filename=path.name)
                assert not cell.outputs and cell.execution_count is None, path
                cells += 1
    print(
        f"PASS: {checked} local Markdown links; notebook schemas and {cells} clean code cells."
    )


if __name__ == "__main__":
    main()
