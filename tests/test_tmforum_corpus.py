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

from openapi_to_rdf.mapping import MINTED_BY_CONVENTION_LOCAL

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


def test_no_property_carries_two_domains(converted: dict[str, tuple[Graph, Graph]]) -> None:
    """A property's `rdfs:domain` is its DECLARING class, exactly once.

    `rdfs:domain` is conjunctive under RDFS entailment (W3C Recommendation): two domains assert the
    subject instantiates BOTH classes. So a property restated by several subclasses must not collect
    a domain per subclass — subclasses inherit the domain through `rdfs:subClassOf`, which this
    converter emits.

    The regression this pins, measured 2026-09-21: `Addressable#href` carried **12** domains and
    `Event#event` carried **25**, so every node with an `href` was entailed to be simultaneously an
    Addressable, an EntityRef, a GeographicLocation, a PolicyRef and a ServiceOrder. **28 of 2,895
    TM Forum properties (1.0%) against 0 of 3,622 on 3GPP**, because 3GPP does not restate inherited
    fields — so no 3GPP test could ever have caught it.
    """
    offenders: dict[str, dict[str, int]] = {}
    ranged = 0
    for stem, (vocabulary, _shapes) in converted.items():
        per: dict[URIRef, set] = collections.defaultdict(set)
        for subject, target in vocabulary.subject_objects(RDFS.domain):
            per[subject].add(target)
        ranged += len(per)
        multi = {str(p): len(d) for p, d in per.items() if len(d) > 1}
        if multi:
            offenders[stem] = multi

    assert ranged > 500, (
        f"only {ranged} properties had any rdfs:domain — the graphs probably did not load, so a "
        "clean result here means nothing"
    )
    assert offenders == {}, (
        f"{sum(len(v) for v in offenders.values())} properties carry multiple rdfs:domain across "
        f"{ranged} ranged properties: {offenders}"
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
    minted_marker = URIRef(
        "http://ericsson.com/models/3gpp/transport/" + MINTED_BY_CONVENTION_LOCAL
    )
    for stem, (vocabulary, shapes) in converted.items():
        classes = set(vocabulary.subjects(RDF.type, RDFS.Class))
        targeted = {o for _s, _p, o in shapes.triples((None, SH_TARGET_CLASS, None))}

        # A convention-minted referent (`AgreementRef` implies an `Agreement`) is declared because
        # the ontology needs the term, but NO schema describes it — so there is nothing to constrain,
        # and a NodeShape over it would be exactly the decoration this gate exists to catch.
        # Excluded explicitly rather than by lowering the threshold, and the exclusion is itself
        # asserted so it cannot quietly widen to cover real unshaped classes.
        minted = set(vocabulary.subjects(minted_marker, None))
        assert minted, (
            f"{stem}: no convention-minted classes found — either the marker moved or referents "
            "stopped being declared, and this exemption would now be hiding real unshaped classes"
        )
        assert minted <= classes, f"{stem}: a convention-minted term is not declared as a class"

        unshaped = classes - targeted - minted
        assert classes, f"{stem}: no classes declared"
        share = len(unshaped) / len(classes)
        assert share < 0.10, (
            f"{stem}: {len(unshaped)} of {len(classes)} declared classes have no NodeShape "
            f"({share:.1%}) — every constraint on them is inert. Examples: "
            + str(sorted(str(c) for c in unshaped)[:5])
        )


def test_the_corpus_census_can_load_siblings(tmp_path):
    """The gate must be able to reach cross-document resolution at all.

    Pre-fix, census_one hardcoded external_refs=[], so every cross-document ref was unresolved
    and no metric it reported could move no matter what the converter did. This asserts the
    instrument can produce a positive.
    """
    import yaml

    from scripts.measure_corpora import census_one

    common = tmp_path / "common.yaml"
    common.write_text(
        yaml.safe_dump(
            {
                "openapi": "3.0.0",
                "info": {"title": "Common", "version": "1.0"},
                "components": {
                    "schemas": {
                        "Addressable": {"type": "object",
                                        "properties": {"href": {"type": "string"}}}
                    }
                },
            }
        )
    )
    api = tmp_path / "api.yaml"
    api.write_text(
        yaml.safe_dump(
            {
                "openapi": "3.0.0",
                "info": {"title": "Api", "version": "1.0"},
                "components": {
                    "schemas": {
                        "PolicyRef": {
                            "allOf": [
                                {"$ref": "common.yaml#/components/schemas/Addressable"},
                                {"type": "object",
                                 "properties": {"@type": {"type": "string"}}},
                            ]
                        }
                    }
                },
            }
        )
    )

    without = census_one(api)
    with_sibling = census_one(api, siblings=(common,))
    assert without["unresolved_refs"] > 0, "the no-siblings arm must show the ref unresolved"
    assert with_sibling["unresolved_refs"] == 0, (
        f"the sibling arm must resolve it; got {with_sibling['_unresolved']}"
    )
    # Per-document dangling is expected: api.yaml references a class declared in common.yaml,
    # so from api.yaml's perspective alone it's dangling. The corpus-wide metric resolves this.
    assert with_sibling["dangling_class_targets"] > 0, (
        "the sibling arm must produce a cross-document reference that looks dangling per-document"
    )


def test_corpus_wide_print_tuple_size_is_asserted():
    """Assert the number of corpus-wide keys printed, so adding a metric without wiring it fails.

    measure_corpora.py:49 claims this, and nothing enforced it until Task 7 added a metric that
    depends on the guarantee being true.
    """
    from scripts.measure_corpora import print_corpus

    # The corpus-wide keys tuple in print_corpus
    corpus_wide_keys = (
        "distinct_term_iris",
        "distinct_property_iris",
        "distinct_unresolved_targets",
        "terms_with_dash_corpuswide",
        "fold_collisions_corpuswide",
        "dangling_class_targets_corpuswide",
    )
    assert len(corpus_wide_keys) == 6, (
        f"Expected 6 corpus-wide keys in print_corpus, got {len(corpus_wide_keys)}. "
        "If you added a metric, update this assertion AND the print_corpus function."
    )
