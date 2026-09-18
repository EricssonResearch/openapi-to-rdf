"""`rdfs:range` propagates under RDFS entailment, so it is emitted only where it is true.

A range says every object of this predicate is an instance of that class, and RDFS infers it rather
than checking it. So an invented range is not a loose constraint, it is a false axiom. The previous
fallback returned `xsd:string` for anything it could not analyse, which put `rdfs:range xsd:string`
on object-valued properties — inferring that every Party is a literal.
"""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml
from rdflib import RDFS, XSD, Graph

SPEC = {
    "openapi": "3.0.0",
    "info": {"title": "RangeProbe", "version": "1.0"},
    "components": {
        "schemas": {
            "Party": {"type": "object", "properties": {"name": {"type": "string"}}},
            "PartyRole": {"type": "object", "properties": {"name": {"type": "string"}}},
            "Service": {
                "type": "object",
                "properties": {
                    "single": {"$ref": "#/components/schemas/Party"},
                    "multi": {
                        "anyOf": [
                            {"$ref": "#/components/schemas/Party"},
                            {"$ref": "#/components/schemas/PartyRole"},
                        ]
                    },
                    "opaque": {},
                },
            },
        }
    },
}


@pytest.fixture
def graphs(tmp_path: Path) -> tuple[Graph, Graph]:
    """Convert the probe and return (vocabulary graph, shapes graph)."""
    spec_file = tmp_path / "RangeProbe.yaml"
    spec_file.write_text(yaml.safe_dump(SPEC))
    # Corrected import: use OpenAPIToSHACLConverter per Task 1's actual export
    from openapi_to_rdf import OpenAPIToSHACLConverter

    converter = OpenAPIToSHACLConverter(str(spec_file), output_dir=str(tmp_path / "out"))
    converter.convert()
    # Access graphs directly from converter, no file I/O needed
    return converter.rdf_graph, converter.shacl_graph


def test_no_property_gets_a_string_range_it_did_not_declare(graphs) -> None:
    """The load-bearing assertion: the xsd:string fallback is gone."""
    vocabulary, _ = graphs
    offenders = [
        str(s) for s, o in vocabulary.subject_objects(RDFS.range)
        if o == XSD.string and str(s).endswith(("multi", "opaque", "single"))
    ]
    assert not offenders, f"invented xsd:string range on object/untyped properties: {offenders}"


def test_a_single_target_property_keeps_its_class_range(graphs) -> None:
    """Not vacuous: the fix must not remove the ranges that ARE true."""
    vocabulary, _ = graphs
    ranges = {str(s).rsplit("#", 1)[-1].rsplit("/", 1)[-1]: str(o)
              for s, o in vocabulary.subject_objects(RDFS.range)}
    assert "single" in ranges, f"lost a true range; got {ranges}"
    assert ranges["single"].endswith("Party"), ranges["single"]


def test_a_multi_target_property_has_no_range_but_is_constrained_in_shacl(graphs) -> None:
    """The information is not dropped — it moves to where it binds without entailing."""
    from rdflib.namespace import SH
    from rdflib.collection import Collection

    vocabulary, shapes = graphs
    multi = [s for s, _ in vocabulary.subject_objects(RDFS.range) if str(s).endswith("multi")]
    assert not multi, "multi-target property must carry no rdfs:range"

    # Find the property shape for 'multi' specifically
    multi_prop_uri = [s for s in vocabulary.subjects() if str(s).endswith("Service#multi")][0]
    multi_shape = None
    for shape in shapes.subjects(SH.path, multi_prop_uri):
        multi_shape = shape
        break

    assert multi_shape is not None, "no property shape found for multi"

    # Assert this specific shape has sh:or
    or_node = shapes.value(multi_shape, SH["or"])
    assert or_node is not None, "multi property shape must have sh:or constraint"

    # Verify it contains the two class alternatives
    or_list = list(Collection(shapes, or_node))
    assert len(or_list) == 2, f"sh:or must have 2 alternatives, got {len(or_list)}"
