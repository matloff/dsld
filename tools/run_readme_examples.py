"""Run analysis blocks from both README files using the installed packages.

Installation commands and vignette viewer calls are deliberately excluded.
R vignette execution is covered by R CMD build/check.
"""

import re
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def blocks(path, language):
    return re.findall(rf"^```{language}\s*\n(.*?)^```", path.read_text(), re.M | re.S)


def main():
    r_blocks = blocks(ROOT / "README.md", "r")
    analysis = [block for block in r_blocks if "install.packages(" not in block]
    source = "\n".join(analysis)
    source = "\n".join(
        line for line in source.splitlines() if not line.startswith("vignette(")
    )
    with tempfile.TemporaryDirectory() as directory:
        script = Path(directory) / "readme.R"
        script.write_text(source)
        subprocess.run(["Rscript", "--vanilla", str(script)], cwd=directory, check=True)
    print(f"PASS: {len(analysis)} R README analysis blocks.", flush=True)
    namespace = {"__name__": "__main__"}
    python_blocks = blocks(ROOT / "inst/README.md", "python")
    for index, block in enumerate(python_blocks):
        exec(compile(block, f"inst/README.md:block-{index}", "exec"), namespace)
    print(f"PASS: {len(python_blocks)} Python README blocks.", flush=True)


if __name__ == "__main__":
    main()
