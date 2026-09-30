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

from openapi_to_rdf.mapping import minted_by_convention

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
    """Every resolvable TM Forum document converts to a non-trivial vocabulary.

    Two assertions, because they guard different things and one cannot do both:

    * **agreement with the resolver** — the fixture and `_corpora.tmforum()` must see the same
      documents, so a rename shows up here rather than silently shrinking the sample;
    * **a floor** — at least the two documents vendored in `assets/tmforum/` must be present, which
      is what catches an entry being dropped from `TMF_FILENAMES`. The agreement check alone cannot:
      both sides read the same list, so a deletion would keep them agreeing.

    This previously read `== 3`, a hardcoded count, and on 2026-09-30 it did its job and then had to
    be rewritten: the corpus moved from three documents at an absolute path outside any working tree
    to the two vendored in this repository, and a retyped literal cannot follow that. The count now
    lives in `_corpora.TMF_FILENAMES` with everything else reading it.
    """
    resolved = _corpora.tmforum()
    assert len(converted) == len(resolved.paths), (
        f"fixture and resolver disagree: {sorted(converted)} vs "
        f"{sorted(p.name for p in resolved.paths)}"
    )
    assert len(converted) >= len(_corpora.TMF_FILENAMES) >= 2, (
        f"only {len(converted)} documents — the corpus list has shrunk below the vendored set"
    )
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
    for stem, (vocabulary, shapes) in converted.items():
        classes = set(vocabulary.subjects(RDF.type, RDFS.Class))
        targeted = {o for _s, _p, o in shapes.triples((None, SH_TARGET_CLASS, None))}

        # A convention-minted referent (`AgreementRef` implies an `Agreement`) is declared because
        # the ontology needs the term, but NO schema describes it — so there is nothing to constrain,
        # and a NodeShape over it would be exactly the decoration this gate exists to catch.
        # Excluded explicitly rather than by lowering the threshold, and the exclusion is itself
        # asserted so it cannot quietly widen to cover real unshaped classes.
        # The exemption is the library's, not this test's: `measure_corpora.py` needs the same rule
        # for its declared-terms equality, and when only one of the two had it that script reported
        # MISMATCH (+109) on TM Forum for an intended reason. One implementation, both callers.
        minted = minted_by_convention(vocabulary)
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


def test_every_corpus_wide_key_is_both_computed_and_printed(converted):
    """A metric added to `census` but not printed must fail here.

    **This test was vacuous until 2026-09-30.** It defined its own six-element tuple and asserted
    that tuple's length was six -- a literal checking a literal, unable to fail unless someone edited
    the test. It was introduced precisely so a new metric could not go unsummarised, and on the day a
    new metric (`minted_by_convention`) was added without being wired into the corpus-wide report, it
    passed.

    It now reads the real tuple from `measure_corpora` and reconciles it against a real `census`
    result, so both directions fail: a key printed but never computed raises `KeyError` in
    `print_corpus`, and a key computed corpus-wide but absent from `CORPUS_WIDE_KEYS` is caught below.
    """
    from scripts.measure_corpora import CORPUS_WIDE_KEYS, census

    result = census(_corpora.tmforum())

    missing = [k for k in CORPUS_WIDE_KEYS if k not in result]
    assert not missing, f"printed but not computed: {missing}"

    # Corpus-wide scalars are those `census` adds outside the per-document SUMMARY_KEYS sum. Any new
    # one has to be listed for printing; this is the direction the old test claimed to guard.
    computed_corpus_wide = {
        k for k, v in result.items()
        if isinstance(v, int) and (k.endswith("_corpuswide") or k.startswith("distinct_"))
    }
    unprinted = computed_corpus_wide - set(CORPUS_WIDE_KEYS)
    assert not unprinted, (
        f"computed corpus-wide but never printed: {sorted(unprinted)} — add them to "
        "measure_corpora.CORPUS_WIDE_KEYS"
    )
    assert len(CORPUS_WIDE_KEYS) >= 6, "the corpus-wide report has shrunk"
