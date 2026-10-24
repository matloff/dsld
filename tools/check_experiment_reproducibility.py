"""Compare independently executed experiment CSVs from the same environment."""

import argparse
import csv
import hashlib
import json
import math
from pathlib import Path

FILES = ("heldout-results.csv", "paper-compas-grid.csv", "paper-census-grid.csv")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("first", type=Path)
    parser.add_argument("second", type=Path)
    parser.add_argument("--report", type=Path)
    args = parser.parse_args()
    report = {}
    for name in FILES:
        first_path, second_path = args.first / name, args.second / name
        with (
            first_path.open(newline="") as first,
            second_path.open(newline="") as second,
        ):
            first_rows = list(csv.reader(first))
            second_rows = list(csv.reader(second))
        assert first_rows[0] == second_rows[0], f"{name}: different columns"
        assert len(first_rows) == len(second_rows), f"{name}: different row counts"
        for row_number, (left, right) in enumerate(
            zip(first_rows[1:], second_rows[1:]), 2
        ):
            assert len(left) == len(right), f"{name}:{row_number}: different widths"
            for column, (actual, expected) in enumerate(zip(left, right)):
                if actual == expected:
                    continue
                try:
                    equal = math.isclose(
                        float(actual), float(expected), rel_tol=1e-10, abs_tol=1e-10
                    )
                except ValueError:
                    equal = False
                assert equal, (
                    f"{name}:{row_number}:{column}: {actual!r} != {expected!r}"
                )
        hashes = [
            hashlib.sha256(path.read_bytes()).hexdigest()
            for path in (first_path, second_path)
        ]
        report[name] = {
            "rows": len(first_rows) - 1,
            "numerically_equal": True,
            "byte_identical": hashes[0] == hashes[1],
            "sha256": hashes,
            "relative_tolerance": 1e-10,
            "absolute_tolerance": 1e-10,
        }
        print(
            f"PASS {name}: {len(first_rows) - 1} rows; byte-identical={hashes[0] == hashes[1]}"
        )
    if args.report:
        args.report.write_text(json.dumps(report, indent=2) + "\n")


if __name__ == "__main__":
    main()
