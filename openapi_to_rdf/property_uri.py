"""Class-scoped property URI minting.

OpenAPI property names are locally scoped to their containing schema: two
schemas that happen to use the same property name (e.g. ``status``) are
declaring two distinct fields, not the same one. The RDF/SHACL output of
this tool therefore mints each property under a per-class namespace, so a
schema ``TimeWindow`` with a ``startTime`` property yields the URI::

    <base>/TimeWindow#startTime

rather than sharing a single ``<base>#startTime`` with every other schema
that uses the same name. Any semantic merging is a modelling decision that
belongs in a separate, opinion-driven step (see the ``property_index``
sidecar manifest and the deferred Stage-2 ``merge`` workflow).

This module contains only pure functions used by both the SHACL and OWL
converters; it has no rdflib Graph dependencies and is cheap to test.
"""

from __future__ import annotations

from rdflib import URIRef


def format_local_name(name: str) -> str:
    """Normalise a class or property local name for RDF.

    Mirrors the pre-existing ``format_name`` rule used in the converters:
    replace dashes with underscores. Anything else is left alone so we
    don't silently mangle user-supplied identifiers.

    Raises:
        ValueError: if ``name`` is empty or ``None``.
    """
    if not name:
        raise ValueError("local name must be a non-empty string")
    return name.replace("-", "_")


def class_namespace(base_namespace: str, class_name: str) -> str:
    """Return the per-class namespace URI for a schema.

    ``class_namespace("http://x/TS28623/ComDefs#", "TimeWindow")`` yields
    ``"http://x/TS28623/ComDefs/TimeWindow#"``. The trailing ``#`` is
    stripped from the file-level base before appending ``/<Class>#`` so
    the resulting URI uses the conventional hash-namespace shape.

    Raises:
        ValueError: if ``base_namespace`` or ``class_name`` is empty/None.
    """
    if not base_namespace:
        raise ValueError("base_namespace must be a non-empty string")
    safe_class = format_local_name(class_name)
    trimmed = base_namespace.rstrip("#").rstrip("/")
    return f"{trimmed}/{safe_class}#"


def property_uri(base_namespace: str, class_name: str, property_name: str) -> URIRef:
    """Return the class-scoped URI for a property as an ``rdflib.URIRef``.

    Combines :func:`class_namespace` with a dash-normalised property local
    name. Two classes declaring the same property name produce two
    distinct URIs, one per class.

    Raises:
        ValueError: if any argument is empty/None.
    """
    safe_prop = format_local_name(property_name)
    return URIRef(class_namespace(base_namespace, class_name) + safe_prop)
