"""A property name maps to one IRI, and an inherited property does not get a second one.

Both halves were measured. The dash collision falsified this tool's own documented one-domain-one-range
invariant. The attribution rule is `snm-api-native`'s finding: attributing a property to the class that
DECLARES it scored 93.9% against a TBox where attributing to the leaf scored 68.5%.
"""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml
from rdflib import RDFS, Graph

SPEC = {
    "openapi": "3.0.0",
    "info": {"title": "IdentityProbe", "version": "1.0"},
    "components": {
        "schemas": {
            "Dashes": {
                "type": "object",
                "properties": {"my-prop": {"type": "string"}, "my_prop": {"type": "integer"}},
            },
            "Base": {"type": "object", "properties": {"href": {"type": "string"}}},
            "Child": {
                "allOf": [
                    {"$ref": "#/components/schemas/Base"},
                    {"type": "object", "properties": {"href": {"type": "string"}}},
                ]
            },
        }
    },
}


@pytest.fixture
def vocabulary(tmp_path: Path) -> Graph:
    spec_file = tmp_path / "IdentityProbe.yaml"
    spec_file.write_text(yaml.safe_dump(SPEC))
    from openapi_to_rdf import OpenAPIToSHACLConverter

    OpenAPIToSHACLConverter(str(spec_file), base_namespace=None, external_refs=[]).run()
    # Converter writes to cwd-relative "output/rdf/<stem>_rdf.ttl"
    target = Path("output/rdf/IdentityProbe_rdf.ttl")
    return Graph().parse(target, format="turtle")


def test_two_differently_spelled_properties_get_two_iris(vocabulary: Graph) -> None:
    locals_ = {str(s).replace("#", "/").rsplit("/", 1)[-1]
               for s in set(vocabulary.subjects(RDFS.domain, None))}
    assert {"my-prop", "my_prop"} <= locals_ or len({n for n in locals_ if "prop" in n}) == 2, (
        f"dash and underscore collapsed onto one IRI: {sorted(locals_)}"
    )


def test_no_property_carries_two_ranges(vocabulary: Graph) -> None:
    """The consequence the collision produced, asserted directly."""
    offenders = {
        str(s): sorted(str(o) for o in vocabulary.objects(s, RDFS.range))
        for s in set(vocabulary.subjects(RDFS.range, None))
        if len(list(vocabulary.objects(s, RDFS.range))) > 1
    }
    assert not offenders, f"one property, several ranges: {offenders}"


def test_an_inherited_property_is_attributed_to_the_declaring_class(vocabulary: Graph) -> None:
    href = [str(s) for s in set(vocabulary.subjects(RDFS.domain, None)) if s.endswith("href")]
    assert len(href) == 1, f"inherited property minted more than one IRI: {href}"
    assert "Base" in href[0], f"attributed to the mentioning class, not the declaring one: {href[0]}"
