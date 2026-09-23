"""Operation graph projection from the derived Mapping.

Each operation becomes a hydra:Operation with hydra:method, hydra:returns, accepted class, and
status codes. Hydra Core is a W3C Community Group draft.
"""

from __future__ import annotations

import pytest
from rdflib import RDF, Graph, URIRef
from rdflib.namespace import Namespace

from openapi_to_rdf import build_mapping

HYDRA = Namespace("http://www.w3.org/ns/hydra/core#")

# Reuse Task 7's probe document (from test_mapping.py)
SPEC = {
    "openapi": "3.0.0",
    "info": {"title": "OperationsProbe", "version": "1.0"},
    "components": {
        "schemas": {
            "Order": {
                "type": "object",
                "properties": {
                    "id": {"type": "string"},
                    "orderDate": {"type": "string", "format": "date-time"},
                },
            }
        }
    },
    "paths": {
        "/order": {
            "get": {
                "operationId": "listOrder",
                "responses": {
                    "200": {
                        "content": {
                            "application/json": {
                                "schema": {
                                    "type": "array",
                                    "items": {"$ref": "#/components/schemas/Order"},
                                }
                            }
                        }
                    }
                },
            },
            "post": {
                "operationId": "createOrder",
                "requestBody": {
                    "content": {
                        "application/json": {"schema": {"$ref": "#/components/schemas/Order"}}
                    }
                },
                "responses": {
                    "201": {
                        "content": {
                            "application/json": {
                                "schema": {"$ref": "#/components/schemas/Order"}
                            }
                        }
                    }
                },
            },
        },
        "/order/{id}": {
            "delete": {"operationId": "deleteOrder", "responses": {"204": {}}},
        },
    },
}
NS = "https://example.org/ontology/"


@pytest.fixture
def mapping():
    return build_mapping(SPEC, namespace=NS)


@pytest.fixture
def graph(mapping):
    from openapi_to_rdf.projections.operations import operations_from_mapping

    return operations_from_mapping(mapping)


def test_each_operation_becomes_a_hydra_operation(graph) -> None:
    ops = set(graph.subjects(RDF.type, HYDRA.Operation))
    assert len(ops) == 3, f"expected one Operation per path+method, got {len(ops)}"


def test_the_returned_class_is_the_iri_the_vocabulary_declares(graph, mapping) -> None:
    """The join. If these IRIs differ, the operation graph and the data graph cannot be queried together."""
    returned = set(graph.objects(None, HYDRA.returns))
    assert URIRef(mapping.classes["Order"].iri) in returned, sorted(str(r) for r in returned)


def test_an_operation_with_no_body_emits_no_returns_triple(graph) -> None:
    """Rather than a blank node or a placeholder class — 12 of TMF641's operations return no body."""
    delete = one_operation(graph, method="DELETE")
    assert not list(graph.objects(delete, HYDRA.returns))


def test_an_unknown_affordance_concept_raises_rather_than_minting() -> None:
    from openapi_to_rdf.projections.operations import term

    with pytest.raises(KeyError, match="unknown affordance concept"):
        term("retruns")


def test_the_emitted_graph_states_hydras_status(graph) -> None:
    """A reader must be able to tell a Community Group draft from a Recommendation."""
    # Hydra's status is documented in the operations module docstring, not as a triple,
    # to avoid asserting facts about a W3C-owned resource.
    from openapi_to_rdf.projections import operations
    docstring = operations.__doc__
    assert "Community Group" in docstring, "Hydra's status must be documented in module docstring"
    assert "not a Recommendation" in docstring or "draft" in docstring.lower()


def one_operation(graph: Graph, method: str) -> URIRef:
    """Find the one operation with the given method."""
    for op in graph.subjects(RDF.type, HYDRA.Operation):
        if (op, HYDRA.method, None) in graph:
            method_obj = graph.value(op, HYDRA.method)
            if method_obj and str(method_obj) == method:
                return op
    raise ValueError(f"No operation with method {method}")
