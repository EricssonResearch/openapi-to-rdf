"""Context projection from the derived Mapping.

A JSON-LD 1.1 @context giving each class a type-scoped term, id → @id, @type: @id for IRI-valued
properties, and datatype coercion where the Mapping recorded one.
"""

from __future__ import annotations

import pytest

from openapi_to_rdf import build_mapping

SPEC = {
    "openapi": "3.0.0",
    "info": {"title": "ContextProbe", "version": "1.0"},
    "components": {
        "schemas": {
            "Order": {
                "type": "object",
                "properties": {
                    "id": {"type": "string"},
                    "href": {"type": "string"},
                    "orderDate": {"type": "string", "format": "date-time"},
                },
            }
        }
    },
}
NS = "https://example.org/ontology/"


def test_class_term_is_type_scoped() -> None:
    """Each class gets a type-scoped term."""
    from openapi_to_rdf.projections.context import context_from_mapping

    mapping = build_mapping(SPEC, namespace=NS)
    context = context_from_mapping(mapping, base=NS)

    # The @context should have an entry for Order
    ctx = context["@context"]
    assert "Order" in ctx
    # Type-scoped term points to the class IRI
    assert ctx["Order"]["@id"] == NS + "Order"
    assert ctx["Order"]["@context"] is not None


def test_href_carries_type_id() -> None:
    """IRI-valued properties get @type: @id."""
    from openapi_to_rdf.projections.context import context_from_mapping

    mapping = build_mapping(SPEC, namespace=NS)
    context = context_from_mapping(mapping, base=NS)

    ctx = context["@context"]
    # href is IRI-valued (by convention, see mapping.py)
    order_ctx = ctx["Order"]["@context"]
    assert "href" in order_ctx
    assert order_ctx["href"]["@type"] == "@id"


def test_orderDate_carries_datatype() -> None:
    """Datatype coercion for typed properties."""
    from openapi_to_rdf.projections.context import context_from_mapping

    mapping = build_mapping(SPEC, namespace=NS)
    context = context_from_mapping(mapping, base=NS)

    ctx = context["@context"]
    order_ctx = ctx["Order"]["@context"]
    assert "orderDate" in order_ctx
    assert order_ctx["orderDate"]["@type"] == "http://www.w3.org/2001/XMLSchema#dateTime"


def test_id_maps_to_at_id() -> None:
    """The id property maps to @id."""
    from openapi_to_rdf.projections.context import context_from_mapping

    mapping = build_mapping(SPEC, namespace=NS)
    context = context_from_mapping(mapping, base=NS)

    ctx = context["@context"]
    order_ctx = ctx["Order"]["@context"]
    assert "id" in order_ctx
    assert order_ctx["id"] == "@id"


def test_zero_classes_raises() -> None:
    """Refuse a zero-class Mapping rather than returning an empty context."""
    from openapi_to_rdf.projections.context import context_from_mapping

    empty_spec = {
        "openapi": "3.0.0",
        "info": {"title": "Empty", "version": "1.0"},
        "components": {"schemas": {}},
    }
    mapping = build_mapping(empty_spec, namespace=NS)
    with pytest.raises(ValueError, match="no classes"):
        context_from_mapping(mapping, base=NS)

def test_the_context_declares_json_ld_1_1_and_honours_the_base() -> None:
    """`@version: 1.1` and `@base` — both were missing, and the second was silently ignored.

    **`@version` is load-bearing, not decoration.** A term definition carrying its own `@context` is
    a JSON-LD 1.1 feature, and a processor in `json-ld-1.0` processing mode is required to signal an
    error for it. The practical result of omitting the declaration is that every scoped term is
    dropped — 95 on TMF641, including every `href` coercion, which is exactly what turns a string
    into a followable edge. The context's most important content was conditional on a processor's
    default mode.

    **`base` did nothing.** Until 2026-09-22 two calls differing only in `base` produced
    byte-identical output — the second parameter in this package found to be accepted and ignored,
    after `operations_from_mapping(base=...)`. The inversion below is what makes that impossible to
    reintroduce: a `base` that stopped being read would make the two assertions equal.
    """
    spec = {
        "openapi": "3.0.0",
        "info": {"title": "Ctx", "version": "1.0"},
        "paths": {},
        "components": {
            "schemas": {
                "Thing": {
                    "type": "object",
                    "properties": {"id": {"type": "string"}, "href": {"type": "string"}},
                }
            }
        },
    }
    from openapi_to_rdf.projections.context import context_from_mapping

    mapping = build_mapping(spec, namespace="https://example.org/o/")

    one = context_from_mapping(mapping, base="https://a.example/r/")["@context"]
    two = context_from_mapping(mapping, base="https://b.example/other/")["@context"]

    assert one["@version"] == 1.1, one.get("@version")
    assert one["@base"] == "https://a.example/r/", one.get("@base")
    assert two["@base"] == "https://b.example/other/", two.get("@base")
    assert one["@base"] != two["@base"], "`base` is being ignored again"

    # Not vacuous: the scoped term the @version declaration exists for is actually present.
    assert isinstance(one["Thing"], dict) and "@context" in one["Thing"], one["Thing"]

