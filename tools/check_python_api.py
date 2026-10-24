"""Check original public parameter names/defaults without importing R or Python deps."""

import ast
import json
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def stable_ast_dump(node):
    """Match the compact baseline across Python versions' ast.dump formatting.

    Python 3.13 added show_empty=False; earlier versions print empty list
    fields. Ignore only empty/optional fields, preserving literal None defaults
    and the distinction between positional, keyword-only, and variadic args.
    """
    if isinstance(node, ast.AST):
        fields = []
        for name, value in ast.iter_fields(node):
            if isinstance(value, list) and not value:
                continue
            if value is None and name != "value":
                continue
            fields.append(f"{name}={stable_ast_dump(value)}")
        return f"{type(node).__name__}({', '.join(fields)})"
    if isinstance(node, list):
        return "[" + ", ".join(stable_ast_dump(value) for value in node) + "]"
    return repr(node)


def signatures(source):
    result = {}
    for node in ast.parse(source).body:
        if isinstance(node, ast.FunctionDef) and not node.name.startswith("_"):
            arguments = node.args
            # Annotations are descriptive and do not change call compatibility.
            for argument in (
                arguments.posonlyargs + arguments.args + arguments.kwonlyargs
            ):
                argument.annotation = None
            result[node.name] = stable_ast_dump(arguments)
    return result


def main():
    expected = json.loads((ROOT / "tools/baselines/python-api-0.0.3.json").read_text())
    count = 0
    for filename, functions in expected.items():
        source = (ROOT / "inst/src/dsldPy" / filename).read_text()
        tree = ast.parse(source)
        for node in tree.body:
            # The only intentional signature addition is an optional final argument.
            if isinstance(node, ast.FunctionDef) and node.name == "dsldPyFairUtils":
                assert node.args.args[-1].arg == "model_args"
                assert ast.literal_eval(node.args.defaults[-1]) is None
                node.args.args.pop()
                node.args.defaults.pop()
        actual = signatures(ast.unparse(tree))
        for name, signature in functions.items():
            assert actual.get(name) == signature, (
                f"Python signature changed: {filename}:{name}"
            )
            count += 1
    print(
        f"PASS: {count} Python signatures/defaults preserved; optional model_args added."
    )


if __name__ == "__main__":
    main()
