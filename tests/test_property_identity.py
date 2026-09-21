"""A name maps to one IRI, and an inherited property does not get a second one.

Both halves were measured. The dash collision falsified this tool's own documented one-domain-one-range
invariant. The attribution rule is `snm-api-native`'s finding: attributing a property to the class that
DECLARES it scored 93.9% against a TBox where attributing to the leaf scored 68.5%.

**Classes are covered here too, and they were not until Task 8.** Task 4 made property local names
injective and left `format_name`'s `-` -> `_` folding in place for CLASS names, which is the more
important identifier: a class IRI is what data gets typed with. Measured on the 3GPP corpus, 629 of
1,801 schema names contain a `-` and 0 collide under the folding today, so the fix was free at the time
it was made and would have been a data migration later. 0 of 888 TM Forum v5 names are affected.

The class half of the probe declares BOTH spellings on purpose. Zero real collisions means a probe
built from the corpus cannot exhibit the defect — only a document that declares `Files-Single` and
`Files_Single` together can, and that is exactly what a future 3GPP release is free to do.
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
            # Two class names that differ ONLY in dash-vs-underscore: the failure region for class
            # IRI injectivity. `Files-Single` is a real 3GPP name (TS28623_PmControlNrm).
            "Files-Single": {"type": "object", "properties": {"a": {"type": "string"}}},
            "Files_Single": {"type": "object", "properties": {"b": {"type": "string"}}},
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

    # `output_dir` is REQUIRED here, not optional tidiness: it defaults to a cwd-relative
    # "output/", which is the published deliverable tree. Omitting it wrote this probe's three
    # artifacts into output/ and they were then committed, taking the tree to 120 files against
    # the README's 114. tests/conftest.py now fails any test that does this.
    OpenAPIToSHACLConverter(
        str(spec_file), base_namespace=None, output_dir=str(tmp_path / "out"), external_refs=[]
    ).run()
    target = tmp_path / "out" / "rdf" / "IdentityProbe_rdf.ttl"
    return Graph().parse(target, format="turtle")


def test_two_differently_spelled_properties_get_two_iris(vocabulary: Graph) -> None:
    locals_ = {str(s).replace("#", "/").rsplit("/", 1)[-1]
               for s in set(vocabulary.subjects(RDFS.domain, None))}
    assert {"my-prop", "my_prop"} <= locals_ or len({n for n in locals_ if "prop" in n}) == 2, (
        f"dash and underscore collapsed onto one IRI: {sorted(locals_)}"
    )


def test_two_differently_spelled_classes_get_two_iris(vocabulary: Graph) -> None:
    """A class IRI is an identity, so the dash survives into it.

    Asserted as an exact pair rather than "no duplicates": a projection that emitted neither class
    would satisfy a duplicate check and fail this one.
    """
    from rdflib.namespace import RDF

    declared = {str(c).rsplit("#", 1)[-1] for c in vocabulary.subjects(RDF.type, RDFS.Class)}
    assert {"Files-Single", "Files_Single"} <= declared, (
        f"dash folded into underscore, collapsing two classes onto one IRI: {sorted(declared)}"
    )


def test_the_properties_of_two_differently_spelled_classes_do_not_collide(vocabulary: Graph) -> None:
    """The consequence, one level down: a class-scoped property namespace inherits the collision."""
    namespaces = {
        str(s).rsplit("#", 1)[0]
        for s in vocabulary.subjects(RDFS.domain, None)
        if "Files" in str(s)
    }
    assert len(namespaces) == 2, f"two classes, {len(namespaces)} property namespace(s): {namespaces}"


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
