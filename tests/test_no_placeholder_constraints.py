"""A shape says what the schema said. It does not invent a constraint to satisfy a triple store.

`sh:nodeKind sh:IRI` on an untyped property asserts the value must be an IRI — which the schema did
not say, and which is false of the empty schema that triggered it. If a store requires a constraint,
the honest answer is to emit no PropertyShape for a property with nothing to constrain.
"""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml
from rdflib import Graph
from rdflib.namespace import OWL, SH

SPEC = {
    "openapi": "3.0.0",
    "info": {"title": "PlaceholderProbe", "version": "1.0"},
    "components": {
        "schemas": {
            "A": {"type": "object", "properties": {"x": {"type": "string"}}},
            "B": {"type": "object", "properties": {"y": {"type": "string"}}},
            "Thing": {"type": "object", "properties": {"opaque": {}}},
            "AOrB": {"oneOf": [
                {"$ref": "#/components/schemas/A"}, {"$ref": "#/components/schemas/B"}]},
        }
    },
}


def _convert(tmp_path: Path, fmt: str) -> Graph:
    spec_file = tmp_path / "PlaceholderProbe.yaml"
    spec_file.write_text(yaml.safe_dump(SPEC))
    from openapi_to_rdf import OpenAPIToRDFConverter, OpenAPIToSHACLConverter

    out = tmp_path / fmt
    if fmt == "shacl":
        OpenAPIToSHACLConverter(str(spec_file), output_dir=str(out)).run()
        return Graph().parse(str(next((out / "shacl").glob("*.ttl"))), format="turtle")
    OpenAPIToRDFConverter(str(spec_file), base_namespace="https://example.org/test#", output_dir=str(out)).run()
    return Graph().parse(str(next(out.glob("*.ttl"))), format="turtle")


def test_no_shape_asserts_node_kind_the_schema_never_declared(tmp_path: Path) -> None:
    shapes = _convert(tmp_path, "shacl")
    invented = list(shapes.subject_objects(SH.nodeKind))
    assert not invented, f"invented sh:nodeKind on {len(invented)} shape(s): {invented[:3]}"


def test_a_union_of_classes_is_a_union_not_an_enumeration(tmp_path: Path) -> None:
    owl = _convert(tmp_path, "owl")
    assert not list(owl.subject_objects(OWL.oneOf)), (
        "owl:oneOf enumerates INDIVIDUALS; a union of classes is owl:unionOf"
    )
