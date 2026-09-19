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
    # Not vacuous, and the EXACT set rather than a floor: the fixture declares six schemas with
    # top-level constructs, and two of them (`Choice`, `Complex`) are `oneOf` unions, which get no
    # class and therefore no NodeShape. A floor would have been satisfied by silently losing one of
    # the four that must be here; naming them means a schema going missing fails loudly.
    assert {str(t).rsplit("#", 1)[-1] for t in targets} == {
        "Base", "Derived", "Bounded", "Alternative"
    }, dict(targets)


@pytest.mark.parametrize(
    ("spec_name", "expected_terms"),
    [
        ("TS29520_Nnwdaf_AnalyticsInfo.yaml", 16),
        ("TS29520_Nnwdaf_EventsSubscription.yaml", 79),
        ("TS29571_CommonData.yaml", 309),
        ("TS28623_GenericNrm.yaml", 36),
    ],
)
def test_every_declared_term_has_exactly_one_node_shape_on_real_specs(
    spec_name: str, expected_terms: int, tmp_path: Path
) -> None:
    """Every ``rdfs:Class`` and ``rdfs:Datatype`` this tool declares gets one NodeShape.

    **Over the real corpus, because a fixture could not see the defect this guards.** These four
    3GPP documents are exactly the ones where 10 of 1,698 declared terms silently lost their
    NodeShape: ``_type_clause`` deletes a shape that gained no value constraint, which is right for an
    anonymous PropertyShape and removes a class's ``sh:targetClass`` when the shape handed to it is a
    NodeShape. It fires on ``allOf: [{oneOf: [{required: [a]}, {required: [b]}]}]`` — a pure
    co-occurrence rule with no value constraint anywhere in it — which both corpora write and no
    hand-written fixture in this suite contained.

    ``expected_terms`` is asserted so a document changing shape, or a path going stale after a
    rename, fails loudly instead of quietly checking fewer terms.
    """
    from rdflib.namespace import RDF, RDFS as RDFS_NS

    from openapi_to_rdf import OpenAPIToSHACLConverter

    spec = Path("assets/MnS-Rel-19-OpenAPI/OpenAPI") / spec_name
    assert spec.is_file(), f"corpus spec missing: {spec}"
    converter = OpenAPIToSHACLConverter(str(spec), output_dir=str(tmp_path / "out"))
    converter.convert()

    terms = set(converter.rdf_graph.subjects(RDF.type, RDFS_NS.Class)) | set(
        converter.rdf_graph.subjects(RDF.type, RDFS_NS.Datatype)
    )
    assert len(terms) == expected_terms, (
        f"{spec_name} declares {len(terms)} terms, expected {expected_terms}"
    )
    counts = collections.Counter(
        str(o) for _, o in converter.shacl_graph.subject_objects(SH.targetClass)
    )
    missing = sorted(str(t) for t in terms if counts[str(t)] == 0)
    duplicated = {k: v for k, v in counts.items() if v > 1}
    assert not missing, f"{len(missing)} declared term(s) with no NodeShape: {missing}"
    assert not duplicated, f"term(s) with more than one NodeShape: {duplicated}"


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


@pytest.mark.xfail(
    strict=True,
    reason="oneOf with inline object + nested anyOf creates empty duplicate NodeShape. "
    "Observed in Resource (TS28532_ProvMnS.yaml). Nested logical operators inside "
    "oneOf may require different handling from top-level cases."
)
def test_nested_logical_operators_oneOf_with_anyOf() -> None:
    """Reproduce the Resource duplicate: oneOf containing inline object + nested anyOf.

    This is the minimal structure from TS28532_ProvMnS.yaml Resource schema that
    produces two NodeShapes (one populated, one empty).
    """
    spec = {
        "openapi": "3.0.0",
        "info": {"title": "Test", "version": "1.0"},
        "components": {
            "schemas": {
                "RefTarget1": {"type": "object", "properties": {"x": {"type": "string"}}},
                "RefTarget2": {"type": "object", "properties": {"y": {"type": "string"}}},
                "ResourceLike": {
                    "oneOf": [
                        {
                            "type": "object",
                            "properties": {
                                "id": {"type": "string"},
                                "objectClass": {"type": "string"},
                                "attributes": {"type": "object"},
                            },
                            "additionalProperties": {
                                "type": "array",
                                "items": {"type": "object"}
                            },
                            "required": ["id"]
                        },
                        {
                            "anyOf": [
                                {"$ref": "#/components/schemas/RefTarget1"},
                                {"$ref": "#/components/schemas/RefTarget2"},
                            ]
                        },
                    ]
                },
            }
        },
    }

    import tempfile
    with tempfile.TemporaryDirectory() as tmp:
        spec_file = Path(tmp) / "test.yaml"
        spec_file.write_text(yaml.safe_dump(spec))
        from openapi_to_rdf import OpenAPIToSHACLConverter

        converter = OpenAPIToSHACLConverter(str(spec_file), output_dir=str(Path(tmp) / "out"))
        converter.convert()
        converter.save_rdf()

        g = Graph()
        g.parse(str(Path(tmp) / "out" / "shacl" / "test_shacl.ttl"), format="turtle")

        targets = collections.Counter(str(o) for _, o in g.subject_objects(SH.targetClass))
        resource_like = [k for k in targets.keys() if k.endswith("ResourceLike")]
        assert len(resource_like) == 1, "Expected exactly one ResourceLike class"

        count = targets[resource_like[0]]
        assert count == 1, f"ResourceLike has {count} NodeShapes (expected 1); this is the Resource duplicate"
