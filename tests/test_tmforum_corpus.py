"""TM Forum coverage: the corpus that catches what 3GPP cannot.

**Why this file exists.** Acceptance criterion AC-8 of
`snm-api-native/docs/specs/2026-09-18-consolidate-openapi-to-rdf.md` requires both the 3GPP and the
TM Forum corpus to pass, and it is the criterion no single repo could meet before consolidation.
Until 2026-09-21 the suite had **0 of 27 modules** referencing TM Forum, and the cost of that showed
up immediately: determination S1 (`rdfs:range` only where provably true) was implemented, validated
against 3GPP, and left **167 of 2,895 TM Forum properties (5.8%)** carrying two or more ranges,
against **6 of 3,622 (0.17%)** on 3GPP — a 34× difference that no 3GPP test could report.

The mechanism behind that gap is the thing to remember: TM Forum leans on polymorphic `oneOf`
references (`party` resolves to `Party` *or* `PartyRole` — defect O1's own example), while 3GPP
rarely does. A guard written against one corpus was sound, in scope, and given a sample that could
not reach the failure region.

**These specs are not vendored.** They live outside the repo, so every test here skips with a stated
reason when they are absent rather than passing quietly — a green run on a machine without the
corpus would be the same false assurance this file was written to end.
"""

from __future__ import annotations

import collections
import sys
from pathlib import Path

import pytest
from rdflib import RDF, RDFS, Graph, URIRef

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "scripts"))

import _corpora  # noqa: E402

from openapi_to_rdf import OpenAPIToSHACLConverter  # noqa: E402

SH_TARGET_CLASS = URIRef("http://www.w3.org/ns/shacl#targetClass")


def _tmforum_paths() -> list[Path]:
    corpus = _corpora.tmforum()
    if not corpus:
        pytest.skip(corpus.skip_reason or "TM Forum corpus unavailable")
    return corpus.paths


@pytest.fixture(scope="module")
def converted() -> dict[str, tuple[Graph, Graph]]:
    """Convert every TM Forum document once; return {stem: (vocabulary, shapes)}."""
    result: dict[str, tuple[Graph, Graph]] = {}
    for path in _tmforum_paths():
        converter = OpenAPIToSHACLConverter(
            str(path), base_namespace=f"https://example.org/{path.stem}/", external_refs=[]
        )
        converter.convert()
        result[path.stem] = (converter.rdf_graph, converter.shacl_graph)
    return result


def test_every_tmforum_document_converts(converted: dict[str, tuple[Graph, Graph]]) -> None:
    """Three documents, each producing a non-trivial vocabulary.

    The count is asserted so that a corpus list which silently shrinks — a renamed file, a changed
    default directory — fails here instead of making every other test in this module vacuous.
    """
    assert len(converted) == 3, sorted(converted)
    for stem, (vocabulary, shapes) in converted.items():
        assert len(vocabulary) > 100, f"{stem}: only {len(vocabulary)} vocabulary triples"
        assert len(shapes) > 100, f"{stem}: only {len(shapes)} shape triples"


def test_no_property_carries_two_ranges(converted: dict[str, tuple[Graph, Graph]]) -> None:
    """Determination S1: a property gets `rdfs:range` only where it is provably true.

    `rdfs:range` propagates under RDFS entailment (W3C Recommendation), so two ranges on one
    property assert that its value instantiates BOTH classes — an intersection the document almost
    never means. Where a property has several possible targets the constraint belongs in SHACL,
    which binds without entailing.

    This is the regression test for the 167 TM Forum properties that carried two ranges while the
    3GPP suite was green. It asserts zero AND prints the worst offenders, because "0" alone would
    not distinguish a fix from a graph that failed to load.
    """
    offenders: dict[str, dict[str, list[str]]] = {}
    total_properties = 0
    for stem, (vocabulary, _shapes) in converted.items():
        per_property: dict[URIRef, set] = collections.defaultdict(set)
        for subject, target in vocabulary.subject_objects(RDFS.range):
            per_property[subject].add(target)
        total_properties += len(per_property)
        multi = {
            str(prop): sorted(str(t) for t in targets)
            for prop, targets in per_property.items()
            if len(targets) > 1
        }
        if multi:
            offenders[stem] = multi

    # Confirm the check could have produced a positive result before trusting a negative one.
    assert total_properties > 500, (
        f"only {total_properties} properties had any rdfs:range at all — the graphs probably did "
        "not load, so a clean result here means nothing"
    )
    assert offenders == {}, (
        f"{sum(len(v) for v in offenders.values())} properties carry multiple rdfs:range targets "
        f"across {total_properties} ranged properties: "
        + str({k: dict(list(v.items())[:3]) for k, v in offenders.items()})
    )


def test_one_nodeshape_per_class(converted: dict[str, tuple[Graph, Graph]]) -> None:
    """Defect O2: `allOf` was processed twice, giving 630 excess NodeShapes on 3GPP (+36%).

    TM Forum uses `allOf` for inheritance as heavily as 3GPP does, so this is the second place the
    fix has to hold. Asserted as an exact equality rather than "no more than" — a converter that
    emitted no shapes at all would satisfy an inequality.
    """
    for stem, (_vocabulary, shapes) in converted.items():
        targets = [o for _s, _p, o in shapes.triples((None, SH_TARGET_CLASS, None))]
        duplicates = {t: n for t, n in collections.Counter(targets).items() if n > 1}
        assert targets, f"{stem}: no sh:targetClass at all"
        assert duplicates == {}, (
            f"{stem}: {len(targets) - len(set(targets))} excess NodeShapes — "
            f"{dict(list(duplicates.items())[:5])}"
        )


def test_declared_classes_are_shaped(converted: dict[str, tuple[Graph, Graph]]) -> None:
    """Every class the vocabulary declares should have a shape targeting it.

    A class with no shape is unvalidated, and on the 3GPP test corpus that condition made 260 of
    1,075 declared instance types inert — every constraint on them, of every kind, silently unenforced.
    Reported as a ratio rather than asserted at zero: `rdfs:Datatype` targets and `oneOf` wrappers
    are known, deliberate exceptions (see HYPOTHESES.md H3), so a hard zero would be a false claim.
    """
    for stem, (vocabulary, shapes) in converted.items():
        classes = set(vocabulary.subjects(RDF.type, RDFS.Class))
        targeted = {o for _s, _p, o in shapes.triples((None, SH_TARGET_CLASS, None))}
        unshaped = classes - targeted
        assert classes, f"{stem}: no classes declared"
        share = len(unshaped) / len(classes)
        assert share < 0.10, (
            f"{stem}: {len(unshaped)} of {len(classes)} declared classes have no NodeShape "
            f"({share:.1%}) — every constraint on them is inert. Examples: "
            + str(sorted(str(c) for c in unshaped)[:5])
        )
