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
