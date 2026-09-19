"""One derived object; every artifact is a projection of it.

Two independently-computed derivations of the same mapping can agree today and drift tomorrow. This
is the spine that makes drift impossible rather than merely absent.
"""

from __future__ import annotations

SPEC = {
    "openapi": "3.0.0",
    "info": {"title": "MappingProbe", "version": "1.0"},
    "components": {
        "schemas": {
            "Addressable": {
                "type": "object",
                "properties": {"href": {"type": "string"}, "id": {"type": "string"}},
            },
            "Entity": {"allOf": [{"$ref": "#/components/schemas/Addressable"}]},
            "Order": {
                "allOf": [
                    {"$ref": "#/components/schemas/Entity"},
                    {
                        "type": "object",
                        "required": ["orderDate"],
                        "properties": {
                            "orderDate": {"type": "string", "format": "date-time"},
                            "completed": {"type": "boolean"},
                            "note": {"type": "string"},
                        },
                    },
                ]
            },
            "OrderCreateEvent": {"allOf": [{"$ref": "#/components/schemas/Entity"}]},
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
                "responses": {"201": {"content": {"application/json": {
                    "schema": {"$ref": "#/components/schemas/Order"}}}}},
            },
        },
        "/order/{id}": {
            "delete": {"operationId": "deleteOrder", "responses": {"204": {}}},
        },
    },
}
NS = "https://example.org/ontology/"


def _mapping():
    from openapi_to_rdf import build_mapping

    return build_mapping(SPEC, namespace=NS)


def test_every_class_records_its_allof_parents() -> None:
    """L7: subClassOf is owed for EVERY class, including transport ones."""
    mapping = _mapping()
    assert mapping.classes["Entity"].parents == ("Addressable",)
    assert mapping.classes["Order"].parents == ("Entity",)
    # The event envelope is transport, and it still records its parent.
    assert mapping.classes["OrderCreateEvent"].parents == ("Entity",)
    assert mapping.classes["OrderCreateEvent"].is_transport is True
    assert mapping.classes["Order"].is_transport is False


def test_a_datetime_property_records_its_datatype() -> None:
    """L5: 38 of 321 properties on TMF641 have a non-string datatype and none was emitted."""
    mapping = _mapping()
    assert mapping.properties["orderDate"].datatype == "http://www.w3.org/2001/XMLSchema#dateTime"
    assert mapping.properties["completed"].datatype == "http://www.w3.org/2001/XMLSchema#boolean"
    assert mapping.properties["note"].datatype == "http://www.w3.org/2001/XMLSchema#string"


def test_href_is_iri_valued_even_without_format_uri() -> None:
    """S5/F10: TMF declares href as a bare string and never marks it `format: uri`."""
    mapping = _mapping()
    assert mapping.properties["href"].is_iri_valued is True
    assert mapping.properties["note"].is_iri_valued is False


def test_a_property_is_attributed_to_its_declaring_class() -> None:
    mapping = _mapping()
    assert mapping.properties["href"].declaring_class == "Addressable"


def test_required_and_optional_are_recorded_as_cardinality() -> None:
    mapping = _mapping()
    assert mapping.properties["orderDate"].min_count == 1
    assert mapping.properties["note"].min_count == 0


def test_every_operation_is_recorded_with_its_method_and_path() -> None:
    """`paths` come into scope: this is what the current README disclaims."""
    mapping = _mapping()
    assert set(mapping.operations) == {"GET /order", "POST /order", "DELETE /order/{id}"}, (
        sorted(mapping.operations)
    )


def test_an_operation_records_the_class_it_returns_through_an_array() -> None:
    """A list endpoint returns the ITEM class, not an anonymous array."""
    mapping = _mapping()
    assert mapping.operations["GET /order"].returns_class == "Order"


def test_an_operation_records_the_class_it_accepts() -> None:
    mapping = _mapping()
    assert mapping.operations["POST /order"].accepts_class == "Order"
    assert mapping.operations["GET /order"].accepts_class is None


def test_an_operation_with_no_body_records_none_rather_than_guessing() -> None:
    """Measured on the real corpora: 12 of TMF641's operations return no body, 32 of TMF620's."""
    mapping = _mapping()
    delete = mapping.operations["DELETE /order/{id}"]
    assert delete.returns_class is None and delete.accepts_class is None
    assert delete.status_codes == (204,)


def test_an_operation_reuses_the_class_iri_the_vocabulary_declares() -> None:
    """THE claim the whole approach rests on: one contract, one set of class IRIs.

    Not a separate vocabulary that happens to look similar — the identical IRI, so a query can join
    an operations question to a data question without alignment.
    """
    mapping = _mapping()
    assert mapping.operations["GET /order"].returns_class in mapping.classes
    returned = mapping.classes[mapping.operations["GET /order"].returns_class]
    assert returned.iri == mapping.classes["Order"].iri
