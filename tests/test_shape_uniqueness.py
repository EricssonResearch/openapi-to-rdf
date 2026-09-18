"""Each class gets exactly one NodeShape, and each property-shape one minCount.

Two NodeShapes on one target class are not merely redundant: a validator reports violations twice,
and a reader cannot tell which shape is authoritative. Two `sh:minCount` values are a contradiction
a validator cannot satisfy.
"""

from __future__ import annotations

import collections
from pathlib import Path

import pytest
import yaml
from rdflib import Graph
from rdflib.namespace import SH

SPEC = {
    "openapi": "3.0.0",
    "info": {"title": "ShapeProbe", "version": "1.0"},
    "components": {
        "schemas": {
            "Base": {"type": "object", "properties": {"id": {"type": "string"}}},
            "Derived": {
                "allOf": [
                    {"$ref": "#/components/schemas/Base"},
                    {"type": "object", "properties": {"extra": {"type": "string"}}},
                ]
            },
            "Bounded": {
                "type": "object",
                "required": ["tags"],
                "properties": {
                    "tags": {"type": "array", "items": {"type": "string"}, "minItems": 2}
                },
            },
            "Choice": {
                "oneOf": [
                    {"$ref": "#/components/schemas/Base"},
                    {"type": "object", "properties": {"other": {"type": "string"}}},
                ]
            },
            "Alternative": {
                "anyOf": [
                    {"$ref": "#/components/schemas/Base"},
                    {"type": "object", "properties": {"fallback": {"type": "string"}}},
                ]
            },
            "Complex": {
                "oneOf": [
                    {"type": "object", "properties": {"id": {"type": "string"}}, "required": ["id"]},
                    {"anyOf": [{"$ref": "#/components/schemas/Base"}, {"$ref": "#/components/schemas/Derived"}]},
                ]
            },
        }
    },
}


@pytest.fixture
def shapes(tmp_path: Path) -> Graph:
    spec_file = tmp_path / "ShapeProbe.yaml"
    spec_file.write_text(yaml.safe_dump(SPEC))
    from openapi_to_rdf import OpenAPIToSHACLConverter

    converter = OpenAPIToSHACLConverter(str(spec_file), output_dir=str(tmp_path / "out"))
    converter.convert()
    converter.save_rdf()
    return Graph().parse(str(next((tmp_path / "out" / "shacl").glob("*.ttl"))), format="turtle")


def test_each_target_class_has_exactly_one_node_shape(shapes: Graph) -> None:
    targets = collections.Counter(str(o) for _, o in shapes.subject_objects(SH.targetClass))
    duplicated = {k: v for k, v in targets.items() if v > 1}
    assert not duplicated, f"{len(duplicated)} class(es) with more than one NodeShape: {duplicated}"
    # Not vacuous: six schemas with top-level constructs.
    assert len(targets) >= 6, dict(targets)


def test_a_required_array_gets_one_min_count_not_two(shapes: Graph) -> None:
    offenders = {
        str(shape): sorted(int(v) for v in shapes.objects(shape, SH.minCount))
        for shape in set(shapes.subjects(SH.minCount, None))
        if len(list(shapes.objects(shape, SH.minCount))) > 1
    }
    assert not offenders, f"contradictory sh:minCount: {offenders}"


def test_the_required_array_still_has_its_declared_lower_bound(shapes: Graph) -> None:
    """The surviving value must be the STRICTER one — minItems 2, not required's 1."""
    counts = {
        str(p): int(c)
        for shape in shapes.subjects(SH.path, None)
        for p in shapes.objects(shape, SH.path)
        for c in shapes.objects(shape, SH.minCount)
    }
    tags = [v for k, v in counts.items() if k.endswith("tags")]
    assert tags == [2], f"expected minCount 2 for a required array with minItems 2, got {tags}"
