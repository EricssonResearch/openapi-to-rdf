"""Does converting one document whole agree with converting it split across two?

The acceptance gate for external schema ingestion. A description split into (common + api) must
yield the same vocabulary as the same description in one file — that is the premise of TM Forum's
split model ("one class, one IRI, referenced by N APIs") and it is what
`docs/superpowers/specs/2026-09-23-external-schema-ingestion-design.md` exists to restore.

**Why a committed script rather than a probe.** The comparison was written ad hoc twice during
diagnosis, and the first version did not rewrite `$ref` strings when it split — so it measured an
absent ancestor rather than an external one, and produced a number that looked like evidence. See
`split_document`: the rewrite is the load-bearing part.

Two measurements, because they fail independently:

* **Attribution** (`attribution_pairs`): the `(declaring_class, property)` pairs. A split that
  invents a pair the whole document does not have has attributed an inherited property to the
  leaf, minting a second IRI for one field.
* **Graph isomorphism** (`compare`): the RDF vocabulary of the whole document against the UNION of
  the split conversions. Isomorphism rather than byte or triple-count equality, for the reason
  `tests/test_output_freshness.py` records — rdflib's blank-node ordering is not stable across
  runs, so bytes are the wrong equivalence relation for a graph.

  **PROVENANCE IS EXCLUDED FROM IT, and that is a correction rather than a convenience** (2026-09-28).
  Provenance is a statement about a FILE: a document converted whole carries one `dcterms:source`, the
  same document converted as two and merged carries two. That is correct — the vocabulary really did
  come from two files — and it means the merged graph always has one more triple, so
  `graphs_isomorphic` was **unsatisfiable by construction** and every document failed for a reason
  that is not the defect this gate exists to find. Measured: 0 of 46 documents isomorphic with
  provenance included, **4 of 46** with it excluded, and those four were being reported as broken.

  The partition comes from `openapi_to_rdf.provenance.split_provenance`, which reads the same term set
  the converter emits — not a list retyped here, because a retyped list is how the constant it
  replaced came to name four of the six terms.

  `provenance_divergence` is reported as its own field rather than dropped, so the +1 stays visible.

Usage:
    uv run python scripts/measure_split_isomorphism.py --tmforum-dir /path/to/tmf
    uv run python scripts/measure_split_isomorphism.py --corpus mine=path/to/one.yaml --json out.json
"""

from __future__ import annotations

import argparse
import copy
import json
import sys
from pathlib import Path
from typing import Any

import yaml
from rdflib import Graph

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
sys.path.insert(0, str(Path(__file__).resolve().parent))

from openapi_to_rdf import OpenAPIToSHACLConverter, build_mapping  # noqa: E402
from openapi_to_rdf.mapping import Mapping  # noqa: E402
from openapi_to_rdf.provenance import split_provenance  # noqa: E402

import _corpora  # noqa: E402

#: The filename the moved half is written to, and the document qualifier refs are rewritten to.
COMMON_NAME = "common.yaml"


def attribution_pairs(mapping: Mapping) -> set[tuple[str, str]]:
    """``(declaring_class, property)`` for every LOCAL class in the mapping.

    External classes are excluded because their declaring document owns them: they appear in the
    split's mapping and not in the whole document's, and counting them would report a difference
    where the two agree.
    """
    return {
        (declaring, prop)
        for (declaring, prop) in mapping.properties_by_class
        if declaring in mapping.classes and not mapping.classes[declaring].is_external
    }


def _rewrite_refs(node: Any, moved: set[str], document_name: str) -> Any:
    """Rewrite every internal ``$ref`` naming a moved schema into its external form.

    **This is the step the diagnosis probe skipped.** Without it, a popped schema leaves
    `#/components/schemas/X` behind — an internal ref to something absent — so `external_schemas`
    is never consulted and the measurement cannot distinguish a cross-document reference from a
    missing ancestor. The request's own reproduction has this defect.
    """
    if isinstance(node, dict):
        rewritten = {}
        for key, value in node.items():
            if (
                key == "$ref"
                and isinstance(value, str)
                and value.startswith("#/components/schemas/")
                and value.rsplit("/", 1)[-1] in moved
            ):
                rewritten[key] = f"{document_name}#/components/schemas/{value.rsplit('/', 1)[-1]}"
            else:
                rewritten[key] = _rewrite_refs(value, moved, document_name)
        return rewritten
    if isinstance(node, list):
        return [_rewrite_refs(item, moved, document_name) for item in node]
    return node


def split_document(
    document: dict, *, moved: set[str] | None = None, fraction: float = 0.5
) -> tuple[dict, dict]:
    """Split one document into (api, common), rewriting refs across the new boundary.

    ``moved`` names the schemas that go to ``common``; by default the first ``fraction`` of the
    names in sorted order. Refs are rewritten in BOTH halves: a schema left in ``api`` may refer
    to a moved one, and a moved one may refer back to a schema that stayed.
    """
    schemas = (document.get("components") or {}).get("schemas") or {}
    if moved is None:
        names = sorted(schemas)
        moved = set(names[: int(len(names) * fraction)])

    api = copy.deepcopy(document)
    api_schemas = {n: d for n, d in schemas.items() if n not in moved}
    common_schemas = {n: copy.deepcopy(d) for n, d in schemas.items() if n in moved}

    # Schemas that stayed in api might reference moved ones → rewrite to common.yaml
    if "components" not in api:
        api["components"] = {}
    api["components"]["schemas"] = _rewrite_refs(api_schemas, moved, COMMON_NAME)

    # Schemas moved to common might reference ones that stayed → rewrite to api.yaml
    stayed = set(schemas.keys()) - moved
    common_schemas_rewritten = _rewrite_refs(common_schemas, stayed, "api.yaml")

    common = {
        "openapi": document.get("openapi", "3.0.0"),
        "info": {"title": "Common", "version": (document.get("info") or {}).get("version", "1.0")},
        "components": {"schemas": common_schemas_rewritten},
    }
    return api, common


#: `len(split provenance) - len(whole provenance)`, expected on EVERY document. One: the split's second
#: `dcterms:source`, because the vocabulary came from two files instead of one. Every other provenance
#: term is identical between the two arms -- same tool, same version, same creator, same disclaimer --
#: so the whole provenance difference is that one statement. Measured uniform across all 47 documents
#: on 2026-09-28; if it is not, the partition no longer accounts for the difference and the vocabulary
#: comparison is no longer clean.
EXPECTED_PROVENANCE_DIVERGENCE = 1

#: Below this many vocabulary triples, an isomorphic verdict is reported VACUOUS rather than OK. Not a
#: pass/fail threshold -- such a document still passes -- it is an honesty threshold on the headline,
#: because two empty graphs are isomorphic and a two-triple document cannot exercise the ancestry walk
#: this gate exists to test. 10 is a judgement, and a loose one; the point is that the number of
#: SUBSTANTIVE passes is reported separately from the number of passes.
VACUOUS_BELOW = 10


def compare(path: Path, namespace: str, work_dir: Path) -> dict:
    """Convert ``path`` whole and split, and report whether the two agree."""
    with open(path, encoding="utf-8") as handle:
        document = yaml.safe_load(handle)

    whole_mapping = build_mapping(document, namespace=namespace)
    api, common = split_document(document)

    api_path = work_dir / "api.yaml"
    common_path = work_dir / COMMON_NAME
    api_path.write_text(yaml.safe_dump(api), encoding="utf-8")
    common_path.write_text(yaml.safe_dump(common), encoding="utf-8")

    split_api_mapping = build_mapping(
        api,
        namespace=namespace,
        external_schemas={COMMON_NAME: (common["components"]["schemas"])},
    )
    split_common_mapping = build_mapping(
        common,
        namespace=namespace,
        external_schemas={"api.yaml": (api["components"]["schemas"])},
    )

    def convert(spec: Path, siblings: list[Path]) -> Graph:
        converter = OpenAPIToSHACLConverter(
            str(spec),
            base_namespace=namespace,
            output_dir=str(work_dir / "out"),
            external_refs=[str(s) for s in siblings],
        )
        converter.convert()
        return converter.rdf_graph

    whole_graph = convert(path, [])
    split_graph = convert(api_path, [common_path]) + convert(common_path, [api_path])

    # Provenance is about the FILE, and a split vocabulary legitimately comes from two of them, so it
    # is partitioned out before the graphs are compared. Without this every document is non-isomorphic
    # on the extra `dcterms:source` alone -- see the module docstring for the measurement.
    whole_vocab, whole_prov = split_provenance(whole_graph, namespace)
    split_vocab, split_prov = split_provenance(split_graph, namespace)

    whole_pairs = attribution_pairs(whole_mapping)
    # Union both halves' attribution pairs
    split_pairs = attribution_pairs(split_api_mapping) | attribution_pairs(split_common_mapping)
    return {
        "spec": path.name,
        "moved_schemas": len(common["components"]["schemas"]),
        "whole_triples": len(whole_graph),
        "split_triples": len(split_graph),
        "whole_vocabulary_triples": len(whole_vocab),
        "split_vocabulary_triples": len(split_vocab),
        "pairs_only_in_split": sorted(split_pairs - whole_pairs),
        "pairs_only_in_whole": sorted(whole_pairs - split_pairs),
        # THE verdict, and it speaks about the vocabulary only.
        "graphs_isomorphic": whole_vocab.isomorphic(split_vocab),
        # Reported, not dropped: the expected value is +1 (one extra `dcterms:source`), and anything
        # else means the provenance stamp changed shape.
        "provenance_divergence": len(split_prov) - len(whole_prov),
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    _corpora.add_arguments(parser)
    parser.add_argument(
        "--namespace",
        default="https://tmforum.org/ontology/",
        help="the shared namespace both halves are converted under",
    )
    parser.add_argument(
        "--work-dir",
        type=Path,
        default=None,
        help="where the split halves are written (default: a temporary directory)",
    )
    args = parser.parse_args()

    import tempfile

    results: list[dict] = []
    failed = False
    with tempfile.TemporaryDirectory() as temporary:
        work_dir = args.work_dir or Path(temporary)
        work_dir.mkdir(parents=True, exist_ok=True)
        for corpus in _corpora.resolve(args):
            if not corpus:
                # Skip WITH the reason. A summary that quietly covers one corpus instead of two
                # is worse than one that fails, because its number still looks like a number.
                print(f"\n=== {corpus.label}: SKIPPED — {corpus.skip_reason} ===")
                continue
            print(f"\n=== {corpus.label}: {len(corpus.paths)} documents ===")
            for path in corpus.paths:
                result = compare(path, args.namespace, work_dir)
                result["corpus"] = corpus.label
                results.append(result)
                invented = len(result["pairs_only_in_split"])
                # `provenance_divergence` carries a stated expectation, not just a report. It is +1 on
                # every document -- the split's second `dcterms:source` -- and that uniformity is what
                # licenses partitioning provenance out of the isomorphism check at all. If the stamp
                # grows a second per-file term the value becomes +2, the partition stops accounting for
                # the whole difference, and this gate would otherwise go quietly back to comparing
                # provenance with the vocabulary. The defect this guards against has already happened
                # once, which is why it is asserted rather than printed.
                prov_ok = result["provenance_divergence"] == EXPECTED_PROVENANCE_DIVERGENCE
                passing = invented == 0 and result["graphs_isomorphic"] and prov_ok
                # VACUOUS is not OK, and separating them is the difference between an honest headline
                # and a misleading one. Two empty graphs are isomorphic, so a document whose vocabulary
                # is a handful of triples passes this gate without exercising the ancestry walk, the
                # suppression rule or the external registration that the whole comparison is about.
                # Measured: of the 5 documents that pass, `TS28111_FaultNotifications` has ZERO
                # vocabulary triples and three others have two, so exactly ONE -- `TS28623_ComDefs`,
                # 271 triples across 35 moved schemas -- is a substantive pass. Reporting "5 of 47" as
                # though it were five would overstate the evidence by five times.
                vacuous = passing and result["whole_vocabulary_triples"] < VACUOUS_BELOW
                verdict = "OK" if passing and not vacuous else ("VACUOUS" if vacuous else "DIVERGENT")
                failed = failed or not passing
                print(
                    f"  {path.name:55s} moved={result['moved_schemas']:4d} "
                    f"whole={result['whole_vocabulary_triples']:6d} "
                    f"split={result['split_vocabulary_triples']:6d} "
                    f"invented_pairs={invented:4d} isomorphic={result['graphs_isomorphic']!s:5s} "
                    f"{verdict}"
                )
                if not prov_ok:
                    print(
                        f"      PROVENANCE divergence {result['provenance_divergence']}, expected "
                        f"{EXPECTED_PROVENANCE_DIVERGENCE}: the stamp changed shape, so excluding it "
                        f"no longer accounts for the whole whole-vs-split difference"
                    )
                for pair in result["pairs_only_in_split"][:10]:
                    print(f"      invented: {pair[0]}.{pair[1]}")

    if not results:
        print("\nNo corpus was measured. Nothing is asserted by this run.", file=sys.stderr)
        return 2
    if args.json:
        args.json.write_text(json.dumps(results, indent=2), encoding="utf-8")
        print(f"\nWrote {args.json}")
    substantive = sum(
        1 for r in results
        if r["graphs_isomorphic"] and not r["pairs_only_in_split"]
        and r["whole_vocabulary_triples"] >= VACUOUS_BELOW
    )
    passing = sum(1 for r in results if r["graphs_isomorphic"] and not r["pairs_only_in_split"])
    print(
        f"\n{'FAIL' if failed else 'PASS'}: {len(results)} documents compared; "
        f"{passing} isomorphic on vocabulary, of which {substantive} are substantive "
        f"(>= {VACUOUS_BELOW} vocabulary triples)"
    )
    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
