"""The declared dependencies are actually installed, and the package imports as a package.

This exists because they were not. `uv sync` read no dependencies from poetry's table, so `rdflib` was
absent and five test modules failed to collect with a message naming a submodule rather than the missing
dependency. A test that asserts the environment is honest is cheaper than re-diagnosing that.
"""

from __future__ import annotations

import importlib

import pytest

#: Every module in the package that has a third-party import. If one cannot be imported, the environment
#: is broken and every other test result in this repo is meaningless.
SUBMODULES = [
    "openapi_to_rdf.property_uri",
    "openapi_to_rdf.property_index",
    "openapi_to_rdf.shacl_converter",
    "openapi_to_rdf.rdf_converter",
    "openapi_to_rdf.main",
]


@pytest.mark.parametrize("name", SUBMODULES)
def test_every_submodule_imports(name: str) -> None:
    importlib.import_module(name)


def test_the_third_party_dependencies_are_installed() -> None:
    """Named explicitly, so the failure says WHICH dependency rather than which submodule."""
    for dependency in ("rdflib", "yaml"):
        importlib.import_module(dependency)


def test_the_package_is_importable_without_relying_on_the_current_directory(tmp_path) -> None:
    """A cwd-dependent import hid the real problem: the package resolved, its submodules did not."""
    import subprocess
    import sys

    result = subprocess.run(
        [sys.executable, "-c", "import openapi_to_rdf.property_uri"],
        cwd=tmp_path, capture_output=True, text=True,
    )
    assert result.returncode == 0, result.stderr
