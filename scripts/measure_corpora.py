#!/usr/bin/env python3
"""What does this converter produce over a whole corpus, and does it reconcile with itself?

Answers, per corpus and per document: does every document convert, how many triples, how many
classes and datatypes, how many ``sh:targetClass``, how many distinct property IRIs, how
``rdfs:range`` splits between datatype-valued and class-valued, and how many ``$ref`` targets went
unresolved.

It also checks three invariants that only real data can falsify, and that are the reason this script
exists rather than a paragraph in a commit message:

1. **declared terms == ``sh:targetClass``** — every class and datatype gets exactly one NodeShape.
   A regression that stripped 10 of 1,698 3GPP terms of their shape was found by this equality and
   by nothing else; the suite was green throughout.
2. **``sh:targetClass`` pointing at an ``rdfs:Datatype``** is counted separately. Such a shape can
   never match a node, because a datatype has no instances carrying ``rdf:type``. 352 of 1,698 on
   3GPP, 17 of 839 on TM Forum. Reported, not fixed.
3. **class IRI collisions under ``-`` → ``_`` folding** — 0 today on both corpora, which is exactly
   why the folding looked harmless for four tasks.

Runs over BOTH corpora by default; see ``scripts/_corpora.py`` for why one is not enough. Nothing is
written to the repository: graphs are counted in memory.

    uv run python scripts/measure_corpora.py
    uv run python scripts/measure_corpora.py --json /tmp/census.json
    uv run python scripts/measure_corpora.py --corpus probe=path/to/spec.yaml
"""

from __future__ import annotations

import argparse
import json
import sys
import traceback
from pathlib import Path

from rdflib import RDF, RDFS, Graph, URIRef
from rdflib.namespace import XSD

sys.path.insert(0, str(Path(__file__).resolve().parent))
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

import _corpora  # noqa: E402

from openapi_to_rdf.shacl_converter import OpenAPIToSHACLConverter  # noqa: E402

SH_TARGET_CLASS = URIRef("http://www.w3.org/ns/shacl#targetClass")

#: Every scalar the summary prints, so adding a measurement without summarising it fails loudly.
SUMMARY_KEYS = (
    "triples",
    "rdf_triples",
    "shacl_triples",
    "classes",
    "datatypes",
    "declared_terms",
    "properties",
    "target_class_triples",
    "target_class_on_datatype",
    "range_datatype",
    "range_class",
    "properties_multi_range",
    "properties_multi_domain",
    "unresolved_refs",
    "unresolved_refs_distinct",
)


def local_name(iri: str) -> str:
    return iri.rsplit("#", 1)[-1].rsplit("/", 1)[-1]


def census_one(path: Path, siblings: tuple[Path, ...] = ()) -> dict:
    """Convert one document in memory and count everything the summary reports.

    ``siblings`` are the other documents of the OAD, loaded so cross-document ``$ref`` resolves.
    It defaults to empty, and for six months that default was the only behaviour — which meant no
    metric in this file could move in response to a cross-document defect, in either direction.
    `dangling_class_targets` in particular is identically 0 without siblings, because an
    unresolved ref emits no triple at all and a triple that is never emitted cannot dangle.
    """
    converter = OpenAPIToSHACLConverter(
        str(path), external_refs=[str(sibling) for sibling in siblings]
    )
    converter.convert()
    rdf: Graph = converter.rdf_graph
    shacl: Graph = converter.shacl_graph

    classes = set(rdf.subjects(RDF.type, RDFS.Class))
    datatypes = set(rdf.subjects(RDF.type, RDFS.Datatype))
    properties = set(rdf.subjects(RDF.type, RDF.Property))

    target_triples = list(shacl.triples((None, SH_TARGET_CLASS, None)))
    on_datatype = sum(1 for _s, _p, o in target_triples if o in datatypes)

    range_datatype = range_class = 0
    ranges_per_property: dict = {}
    for s, _p, o in rdf.triples((None, RDFS.range, None)):
        ranges_per_property.setdefault(s, set()).add(o)
        if str(o).startswith(str(XSD)) or o == RDFS.Literal:
            range_datatype += 1
        else:
            range_class += 1

    # Determination S1: a range is emitted only where provably true. Two ranges on one property
    # assert, under RDFS entailment, that the value instantiates BOTH classes — an intersection the
    # document almost never means. This counter exists because the S1 guard was written and
    # validated against 3GPP, where it left 6 of 3,622 properties multi-ranged (0.17%), and TM
    # Forum — which uses polymorphic `oneOf` references heavily — had 167 of 2,895 (5.8%). A metric
    # that only ever ran on the corpus a fix was written for cannot report that fix's blind spot.
    properties_multi_range = sum(1 for targets in ranges_per_property.values() if len(targets) > 1)

    # Same defect shape on the other side of the triple: `rdfs:domain` is conjunctive, so two
    # domains assert the subject instantiates both classes. Measured before the fix: 28 of 2,895
    # TM Forum properties, worst `Event#event` with 25 domains, against 0 of 3,622 on 3GPP.
    domains_per_property: dict = {}
    for s_, _p, o in rdf.triples((None, RDFS.domain, None)):
        domains_per_property.setdefault(s_, set()).add(o)
    properties_multi_domain = sum(1 for d in domains_per_property.values() if len(d) > 1)

    terms = classes | datatypes
    folded: dict[str, set[str]] = {}
    for term in terms:
        folded.setdefault(str(term).replace("-", "_"), set()).add(str(term))

    # A class target no document DECLARES. This is what D1 produced: the declaring document minted
    # a class under one namespace while every referring document re-derived another from the
    # filename, so the arrow pointed at nothing. Counted per document against that document's own
    # declarations; `dangling_class_targets_corpuswide` in `census` is the number that matters,
    # because a target declared by a SIBLING is correct and only the union can see that.
    declared_here = set(rdf.subjects())
    local_dangling = {
        o
        for predicate in (RDFS.subClassOf, RDFS.range)
        for o in rdf.objects(None, predicate)
        if isinstance(o, URIRef)
        and not str(o).startswith(str(XSD))
        and not str(o).startswith("http://www.w3.org/")
        and o not in declared_here
    }

    return {
        "spec": path.name,
        "rdf_triples": len(rdf),
        "shacl_triples": len(shacl),
        "triples": len(rdf) + len(shacl),
        "classes": len(classes),
        "datatypes": len(datatypes),
        "declared_terms": len(terms),
        "properties": len(properties),
        "target_class_triples": len(target_triples),
        "distinct_target_classes": len({o for _s, _p, o in target_triples}),
        "target_class_on_datatype": on_datatype,
        "range_datatype": range_datatype,
        "range_class": range_class,
        "properties_multi_range": properties_multi_range,
        "properties_multi_domain": properties_multi_domain,
        "fold_collisions": sum(1 for v in folded.values() if len(v) > 1),
        "terms_with_dash": sum(1 for t in terms if "-" in local_name(str(t))),
        "unresolved_refs": len(converter.unresolved_references),
        "unresolved_refs_distinct": len(set(converter.unresolved_references)),
        "dangling_class_targets": len(local_dangling),
        "_term_iris": sorted(str(t) for t in terms),
        "_property_iris": sorted(str(p) for p in properties),
        "_unresolved": sorted(set(converter.unresolved_references)),
        "_dangling": sorted(str(o) for o in local_dangling),
        "_declared": sorted(str(s) for s in declared_here),
    }


def census(corpus: _corpora.Corpus, load_siblings: bool = True) -> dict:
    rows: list[dict] = []
    failures: list[dict] = []
    for path in corpus.paths:
        try:
            siblings = tuple(p for p in corpus.paths if p != path) if load_siblings else ()
            row = census_one(path, siblings=siblings)
            rows.append(row)
        except Exception as exc:  # a conversion failure is a finding, not a crash
            failures.append({"spec": path.name, "error": f"{type(exc).__name__}: {exc}"})
            traceback.print_exc(limit=3)

    all_terms: set[str] = set()
    all_properties: set[str] = set()
    all_unresolved: set[str] = set()
    for row in rows:
        all_terms |= set(row["_term_iris"])
        all_properties |= set(row["_property_iris"])
        all_unresolved |= set(row["_unresolved"])

    folded: dict[str, set[str]] = {}
    for term in all_terms:
        folded.setdefault(term.replace("-", "_"), set()).add(term)

    # Corpus-wide, because a target declared by a SIBLING is correct: only the union of the
    # corpus's graphs can tell a cross-document reference from a dangling one. Pre-fix this read
    # 175 on 3GPP (34 subClassOf + 141 range) against 0 rdfs:domain, which was the control.
    all_declared: set[str] = set()
    for row in rows:
        all_declared |= set(row["_declared"])
    dangling_corpuswide = {
        target for row in rows for target in row["_dangling"] if target not in all_declared
    }

    result = {
        "corpus": corpus.label,
        "documents": len(corpus.paths),
        "converted": len(rows),
        "failed": len(failures),
        "failures": failures,
        "distinct_term_iris": len(all_terms),
        "distinct_property_iris": len(all_properties),
        "distinct_unresolved_targets": len(all_unresolved),
        "terms_with_dash_corpuswide": sum(1 for t in all_terms if "-" in local_name(t)),
        "fold_collisions_corpuswide": sum(1 for v in folded.values() if len(v) > 1),
        "dangling_class_targets_corpuswide": len(dangling_corpuswide),
        "dangling_class_targets_examples": sorted(dangling_corpuswide)[:20],
        "per_spec": [{k: v for k, v in r.items() if not k.startswith("_")} for r in rows],
    }
    for key in SUMMARY_KEYS:
        result[key] = sum(r[key] for r in rows)
    return result


def print_corpus(result: dict) -> None:
    print(
        f"\n=== {result['corpus']}: {result['converted']}/{result['documents']} converted, "
        f"{result['failed']} failed ==="
    )
    for key in SUMMARY_KEYS:
        print(f"  {key:34s} {result[key]:>8}")
    for key in (
        "distinct_term_iris",
        "distinct_property_iris",
        "distinct_unresolved_targets",
        "terms_with_dash_corpuswide",
        "fold_collisions_corpuswide",
        "dangling_class_targets_corpuswide",
    ):
        print(f"  {key:34s} {result[key]:>8}")
    for failure in result["failures"]:
        print(f"  FAILED {failure['spec']}: {failure['error']}")

    # Invariant 1: one NodeShape per declared term. Stated as an expectation, with the count it
    # checked, so a zero cannot be mistaken for a vacuous pass.
    terms, targets = result["declared_terms"], result["target_class_triples"]
    verdict = "OK" if terms == targets else f"MISMATCH ({terms - targets:+d})"
    print(f"  -> declared terms {terms} vs sh:targetClass {targets}: {verdict}")
    if result["target_class_on_datatype"]:
        print(
            f"  -> {result['target_class_on_datatype']} of {targets} sh:targetClass point at an "
            "rdfs:Datatype and can never match a node (reported, not fixed)"
        )
    dangling = result["dangling_class_targets_corpuswide"]
    print(
        f"  -> dangling class targets (no document in the corpus declares them): {dangling} "
        + ("OK" if dangling == 0 else "FAIL")
    )
    for example in result["dangling_class_targets_examples"]:
        print(f"       {example}")


def main() -> int:
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    _corpora.add_arguments(parser)
    parser.add_argument(
        "--no-load-siblings",
        action="store_true",
        help=(
            "convert each document alone, as this script did before 2026-09-23. Every "
            "cross-document $ref is then unresolved and dangling_class_targets is identically 0 "
            "— useful only for reproducing the old figures."
        ),
    )
    args = parser.parse_args()

    corpora = _corpora.resolve(args)
    results = {c.label: census(c, load_siblings=not args.no_load_siblings) for c in corpora if c}
    for result in results.values():
        print_corpus(result)
    skipped = _corpora.report_skips(corpora)

    if args.json:
        args.json.write_text(json.dumps(results, indent=2), encoding="utf-8")
        print(f"\nwrote {args.json}")

    failed = sum(r["failed"] for r in results.values())
    mismatched = sum(
        1 for r in results.values() if r["declared_terms"] != r["target_class_triples"]
    )
    dangling = sum(r["dangling_class_targets_corpuswide"] for r in results.values())
    if not results:
        print("no corpus was measured")
        return 2
    return 1 if (failed or mismatched or skipped or dangling) else 0


if __name__ == "__main__":
    sys.exit(main())
