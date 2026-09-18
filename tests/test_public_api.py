"""The package has a supported import surface, so a consumer never imports internals.

`snm-api-native` is about to depend on this package. With an empty `__init__.py` its only options
were the CLI or reaching into modules with no stability contract; the first is not composable and the
second has no promise attached.
"""

from __future__ import annotations

import importlib

import openapi_to_rdf


def test_the_package_declares_what_is_public() -> None:
    assert hasattr(openapi_to_rdf, "__all__"), "no __all__: nothing is declared public"
    assert openapi_to_rdf.__all__, "__all__ is empty"


def test_every_declared_name_is_importable_from_the_package_root() -> None:
    missing = [name for name in openapi_to_rdf.__all__ if not hasattr(openapi_to_rdf, name)]
    assert not missing, f"declared in __all__ but not importable: {missing}"


def test_the_version_is_exposed_and_matches_the_package_metadata() -> None:
    from importlib.metadata import version

    assert openapi_to_rdf.__version__ == version("openapi-to-rdf")


def test_nothing_public_is_a_private_name() -> None:
    private = [n for n in openapi_to_rdf.__all__ if n.startswith("_")]
    assert not private, f"private names declared public: {private}"
