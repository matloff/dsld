"""Verify release archives against source, including version, contact, and license."""

import argparse
import email
import hashlib
import json
import re
import tarfile
import zipfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def read_dcf(content):
    fields = {}
    key = None
    for line in content.decode().splitlines():
        if line[:1].isspace():
            fields[key] += " " + line.strip()
        elif line:
            key, value = line.split(":", 1)
            fields[key] = value.strip()
    return {key: " ".join(value.split()) for key, value in fields.items()}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("r_archive", type=Path)
    parser.add_argument("wheel", type=Path)
    parser.add_argument("python_archive", type=Path)
    parser.add_argument("--report", type=Path)
    args = parser.parse_args()
    description = read_dcf((ROOT / "DESCRIPTION").read_bytes())
    project = (ROOT / "inst/pyproject.toml").read_text()
    python_version = re.search(r'^version = "([^"]+)"$', project, re.M).group(1)
    assert python_version == description["Version"], "R/Python versions differ"
    module = (ROOT / "inst/src/dsldPy/__init__.py").read_text()
    assert f'__version__ = "{python_version}"' in module

    with tarfile.open(args.r_archive) as archive:
        names = archive.getnames()
        for folder in (
            "R",
            "man",
            "data",
            "tests",
            "vignettes",
            "inst/src/dsldPy",
            "inst/examples",
        ):
            for path in (ROOT / folder).rglob("*"):
                if (
                    not path.is_file()
                    or "__pycache__" in path.parts
                    or path.suffix == ".pyc"
                ):
                    continue
                relative = path.relative_to(ROOT).as_posix()
                assert (
                    archive.extractfile("dsld/" + relative).read() == path.read_bytes()
                ), relative
        for name in (
            "NAMESPACE",
            "NEWS.md",
            "README.md",
            "inst/README.md",
            "inst/pyproject.toml",
        ):
            assert (
                archive.extractfile("dsld/" + name).read() == (ROOT / name).read_bytes()
            ), name
        packaged = read_dcf(archive.extractfile("dsld/DESCRIPTION").read())
        assert all(packaged[key] == value for key, value in description.items())
        forbidden = (
            "/docs/",
            "/tools/",
            "/.github/",
            "__pycache__",
            ".egg-info",
            "/inst/build/",
            "/cran-comments.md",
        )
        assert not any(marker in name for marker in forbidden for name in names)

    license_text = (ROOT / "inst/src/dsldPy/LICENSE").read_bytes()
    with zipfile.ZipFile(args.wheel) as archive:
        for path in (ROOT / "inst/src/dsldPy").glob("*.py"):
            assert archive.read("dsldPy/" + path.name) == path.read_bytes(), path.name
        metadata_name = next(
            name for name in archive.namelist() if name.endswith(".dist-info/METADATA")
        )
        metadata = email.message_from_bytes(archive.read(metadata_name))
        assert metadata["Version"] == python_version
        assert metadata["Maintainer-email"] == description["Maintainer"]
        assert metadata["License-Expression"] == "MIT"
        license_path = (
            metadata_name.rsplit("/", 1)[0] + "/licenses/" + metadata["License-File"]
        )
        assert archive.read(license_path) == license_text, (
            "Wheel license notice differs"
        )

    with tarfile.open(args.python_archive) as archive:
        prefix = f"dsldpy-{python_version}/"
        assert archive.extractfile(prefix + "src/dsldPy/LICENSE").read() == license_text
        assert (
            archive.extractfile(prefix + "pyproject.toml").read()
            == (ROOT / "inst/pyproject.toml").read_bytes()
        )
        for path in (ROOT / "inst/src/dsldPy").glob("*.py"):
            assert (
                archive.extractfile(prefix + "src/dsldPy/" + path.name).read()
                == path.read_bytes()
            ), path.name

    artifacts = (args.r_archive, args.wheel, args.python_archive)
    manifest = {
        path.name: {
            "sha256": hashlib.sha256(path.read_bytes()).hexdigest(),
            "bytes": path.stat().st_size,
        }
        for path in artifacts
    }
    if args.report:
        args.report.write_text(json.dumps(manifest, indent=2) + "\n")
    print(
        "PASS: R archive and Python wheel/sdist match source; versions, maintainer, and license agree."
    )
    print(json.dumps(manifest, indent=2))


if __name__ == "__main__":
    main()
