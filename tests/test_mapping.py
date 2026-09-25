"""One derived object; every artifact is a projection of it.

Two independently-computed derivations of the same mapping can agree today and drift tomorrow. This
is the spine that makes drift impossible rather than merely absent.
"""

from __future__ import annotations

import yaml

from conftest import SKIP_WITHOUT_CORPUS

#: The corpus is fetched, not committed; skip with the remedy rather than failing on a fresh clone.
pytestmark = SKIP_WITHOUT_CORPUS

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


# --------------------------------------------------------------------------------------------
# One rule, two implementations: the guard that they answer the same.
#
# `mapping._resolve_declaring_class` walks `ClassFact.parents`; `shacl_converter._find_declaring_class`
# probes an in-progress `rdflib.Graph` for an already-emitted `rdfs:domain`. Task 8 collapses them
# into one. Until it does, nothing asserted they agree — and a property attributed to two different
# classes mints two IRIs for one field, which is precisely the drift `Mapping` exists to prevent.
# --------------------------------------------------------------------------------------------

#: An ancestor-first inheritance chain that exercises the parts a single class cannot: a
#: grandparent's property reached transitively (`GeographicLocation` → `Place` → `Addressable`) and a
#: child restating a property its ancestor declares. The in-repo 3GPP corpus has **0 non-trivial
#: attributions in 2,822**, so a guard run only on 3GPP data could not fail however wrong either
#: implementation was; this fixture is what reaches the failure region.
_INHERITANCE_CHAIN = {
    "Addressable": {
        "type": "object",
        "properties": {"href": {"type": "string"}, "id": {"type": "string"}},
    },
    "Place": {
        "allOf": [
            {"$ref": "#/components/schemas/Addressable"},
            {
                "type": "object",
                # Restates `id`, which `Addressable` declares: attribution must not mint a second IRI.
                "properties": {"id": {"type": "string"}, "city": {"type": "string"}},
            },
        ]
    },
    "GeographicLocation": {
        "allOf": [
            {"$ref": "#/components/schemas/Place"},
            {
                "type": "object",
                # Restates a GRANDparent's property: only a transitive walk attributes this right.
                "properties": {"href": {"type": "string"}, "name": {"type": "string"}},
            },
        ]
    },
}


def _declaring_class_both_ways(
    document: dict, spec_path
) -> tuple[int, int, list[str], dict[tuple[str, str], tuple[str, str]]]:
    """Attribute every property twice, by each implementation.

    Returns ``(compared, non_trivial, disagreements, attributions)`` where ``non_trivial`` counts
    the attributions that landed on an ancestor rather than the mentioning class — the only ones
    that can distinguish the two implementations at all — and ``attributions`` maps
    ``(class, property)`` to ``(emitter_answer, mapping_answer)`` so a test can assert the answer
    itself and not merely that two paths agreed on a wrong one.
    """
    from openapi_to_rdf import build_mapping
    from openapi_to_rdf.mapping import _resolve_declaring_class, flattened_properties
    from openapi_to_rdf.shacl_converter import OpenAPIToSHACLConverter

    converter = OpenAPIToSHACLConverter(
        str(spec_path), base_namespace=NS, output_dir=str(spec_path.parent / "out")
    )
    seen: list[tuple[str, str, str]] = []
    emitter_rule = converter._find_declaring_class

    def local(uri) -> str:
        return str(uri).rsplit("#", 1)[-1].rsplit("/", 1)[-1]

    def spy(current_class, prop_name):
        answer = emitter_rule(current_class, prop_name)
        seen.append((local(current_class), prop_name, local(answer)))
        return answer

    converter._find_declaring_class = spy
    converter.convert()

    mapping = build_mapping(document, namespace=NS)
    schemas = (document.get("components") or {}).get("schemas") or {}
    parents = {name: fact.parents for name, fact in mapping.classes.items()}
    declared = {name: flattened_properties(schemas[name]) for name in mapping.classes}

    compared = non_trivial = 0
    disagreements: list[str] = []
    attributions: dict[tuple[str, str], tuple[str, str]] = {}
    for class_name, prop_name, emitter_says in seen:
        if class_name not in mapping.classes:
            continue  # an inline anonymous sub-object has no named class on either side
        compared += 1
        mapping_says = _resolve_declaring_class(class_name, parents, declared, prop_name)
        attributions[(class_name, prop_name)] = (emitter_says, mapping_says)
        if emitter_says != class_name or mapping_says != class_name:
            non_trivial += 1
        if emitter_says != mapping_says:
            disagreements.append(
                f"{class_name}.{prop_name}: emitter={emitter_says} mapping={mapping_says}"
            )
    return compared, non_trivial, disagreements, attributions


def _write(tmp_path, schemas: dict):
    path = tmp_path / "TS99999_Probe.yaml"
    path.write_text(
        yaml.safe_dump(
            {"openapi": "3.0.0", "info": {"title": "P", "version": "1"},
             "components": {"schemas": schemas}},
            sort_keys=False,
        ),
        encoding="utf-8",
    )
    return path


def test_both_declaring_class_implementations_agree(tmp_path) -> None:
    """The guard. Two implementations of the Task 4 rule must answer identically.

    Run over a real 3GPP spec from ``assets/`` for breadth and over an inheritance chain for depth,
    because the real corpus contains no non-trivial attribution at all. The comparison count is
    asserted so a scan that silently compared nothing cannot pass as agreement.
    """
    from pathlib import Path

    real_spec = Path("assets/MnS-Rel-19-OpenAPI/OpenAPI/TS28623_GenericNrm.yaml")
    assert real_spec.is_file(), f"corpus spec missing: {real_spec}"
    real_document = yaml.safe_load(real_spec.read_text(encoding="utf-8"))

    real_compared, _real_non_trivial, real_disagreements, _ = _declaring_class_both_ways(
        real_document, real_spec
    )
    # 31 named-class attributions measured on this spec (92 calls, the rest on inline anonymous
    # sub-objects, which have no named class on either side). A floor, so a scan that goes stale
    # after a rename fails loudly instead of quietly comparing nothing.
    assert real_compared >= 30, f"compared only {real_compared} attributions on {real_spec.name}"
    assert not real_disagreements, real_disagreements

    chain_path = _write(tmp_path, _INHERITANCE_CHAIN)
    chain_compared, chain_non_trivial, chain_disagreements, attributed = (
        _declaring_class_both_ways(
            yaml.safe_load(chain_path.read_text(encoding="utf-8")), chain_path
        )
    )
    assert chain_compared >= 6, f"compared only {chain_compared} attributions on the chain"
    # Measured: 2 of the 6 land on an ancestor. Without this floor the whole guard would pass on a
    # sample that cannot exhibit the defect it exists to catch.
    assert chain_non_trivial >= 2, (
        f"only {chain_non_trivial} of {chain_compared} attributions were non-trivial: the sample "
        "cannot distinguish the two implementations"
    )
    assert not chain_disagreements, chain_disagreements
    # And the answer itself, not merely that two paths agreed on one: a restated grandparent
    # property is attributed to the grandparent, by both.
    assert attributed[("GeographicLocation", "href")] == ("Addressable", "Addressable"), attributed
    assert attributed[("Place", "id")] == ("Addressable", "Addressable"), attributed


def test_the_two_paths_agree_even_when_the_parent_is_declared_after_the_child(tmp_path) -> None:
    """Document order must not change which class declares a property.

    This was a **strict xfail** until Task 8, and its flip is what closed the defect. The emitter
    used to answer this question by probing the ``rdflib.Graph`` it was still building, so where a
    parent appeared after its child in the document the ancestor's property IRI had not been emitted
    yet and attribution fell back to the leaf — minting two IRIs for one field. Measured before the
    collapse: **43 of 138** non-trivial attributions differed across the three TM Forum v5 documents
    (23 of 57 on TMF641, 12 of 32 on TMF622, 8 of 49 on TMF620), and **0 of 2,822** on the 3GPP
    corpus, which contains no non-trivial attribution at all — so no 3GPP-only check could see it.
    ``shacl_converter._find_declaring_class`` now reads ``Mapping.declaring_class``.

    The schemas are written children-first deliberately: that ordering is the failure region, and
    without it this test passes however order-dependent the implementation is.
    """
    reordered = {
        name: _INHERITANCE_CHAIN[name]
        for name in ("GeographicLocation", "Place", "Addressable")  # children first
    }
    assert list(reordered) == ["GeographicLocation", "Place", "Addressable"], (
        "the ordering IS the test: a parent must be declared after its child"
    )
    path = _write(tmp_path, reordered)
    compared, non_trivial, disagreements, attributed = _declaring_class_both_ways(
        yaml.safe_load(path.read_text(encoding="utf-8")), path
    )
    assert non_trivial >= 2, f"only {non_trivial} of {compared} were non-trivial"
    assert not disagreements, disagreements
    # And the answer itself: the grandparent owns a property both descendants restate, whichever
    # order the document declares them in.
    assert attributed[("GeographicLocation", "href")] == ("Addressable", "Addressable"), attributed
    assert attributed[("Place", "id")] == ("Addressable", "Addressable"), attributed


def test_an_operation_reuses_the_class_iri_the_vocabulary_declares() -> None:
    """THE claim the whole approach rests on: one contract, one set of class IRIs.

    Not a separate vocabulary that happens to look similar — the identical IRI, so a query can join
    an operations question to a data question without alignment.
    """
    mapping = _mapping()
    assert mapping.operations["GET /order"].returns_class in mapping.classes
    returned = mapping.classes[mapping.operations["GET /order"].returns_class]
    assert returned.iri == mapping.classes["Order"].iri
