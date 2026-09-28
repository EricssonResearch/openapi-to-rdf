#!/usr/bin/env python3
"""WHY do whole and split conversions disagree? Classify every differing triple, corpus-wide.

`scripts/measure_split_isomorphism.py` answers *whether* they agree. It reported 41 of 47 documents
divergent and a net loss of 701 vocabulary triples, which is a number with no mechanism attached — and
a number with no mechanism invites the wrong repair. This answers *why*, by partitioning the difference
into causes that need different decisions.

**Written because extrapolating from one document was wrong twice.** Diagnosing `TS28104_MdaReport`
(the smallest divergence) suggested the dominant cause was a namespace relabelling; declaring one
namespace for the two halves fixed that document and changed only **6 of 47**, leaving the net loss
slightly worse. The lesson is the file's own rule 12: a sample of one cannot reach the failure region.
So this classifies all of them.

The four buckets, which need four different responses:

* **vanished_unscoped** — a subject present in the whole conversion and absent from both halves, whose
  IRI has no `/Class/` segment. These come from `shacl_converter`'s inline-sub-object fallback: an
  inline object has no schema name, so there is nothing to scope the property under. Blocked on a
  VOCABULARY DECISION that has not been taken (what an inline object's scope should be) — see
  HYPOTHESES.md, "128 of 965 property IRIs are unscoped". Not an ingestion defect.
* **vanished_scoped** — the same, but class-scoped. This IS the ingestion defect the AC-7 orphaning
  hypothesis is about: a property whose declaring class is external in the half that would publish it.
* **range_corrected** — the whole conversion says `rdfs:range xsd:string` and the split names a class.
  The SPLIT IS RIGHT and the whole document is wrong: the target is a top-level `oneOf` union, which
  determination S2 gives no class, so the whole conversion falls back to a string while the split
  registers it as an external class (D2) and resolves it properly. A defect in the WHOLE path.
* **range_degraded** — the reverse of the above: the whole conversion names a class and the split falls
  back to a datatype. The ONE bucket where the split is genuinely worse, and it is small.
* **other** — anything the four above do not explain. Kept so the classification cannot quietly
  claim completeness it has not got; the accounting assertion below fails if this is non-empty and
  unreported.

Usage:
    uv run python scripts/diagnose_split_divergence.py
    uv run python scripts/diagnose_split_divergence.py --json artifacts/split-divergence-causes.json
    uv run python scripts/diagnose_split_divergence.py --spec TS28541_5GcNrm.yaml   # one document

Exit codes:
    0 - every differing triple was classified
    1 - some triples fell into `other`, so the classification is incomplete
    2 - no corpus was measured, so nothing is asserted
"""

from __future__ import annotations

import argparse
import collections
import json
import sys
import tempfile
from pathlib import Path
from typing import Any

import yaml
from rdflib import Graph, URIRef
from rdflib.namespace import RDFS, XSD

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
sys.path.insert(0, str(Path(__file__).resolve().parent))

from measure_split_isomorphism import COMMON_NAME, split_document  # noqa: E402
from openapi_to_rdf import OpenAPIToSHACLConverter  # noqa: E402
from openapi_to_rdf.provenance import split_provenance  # noqa: E402

#: The namespace both arms are converted under. Fixed rather than derived: the comparison is about
#: WHICH TRIPLES differ, and a namespace that varied per document would make the `/Class/` test below
#: depend on the document.
NAMESPACE = "https://example.org/v/"


def _is_unscoped(subject: URIRef) -> bool:
    """True when the IRI carries no `<Class>/` segment under the namespace.

    `<ns>AnLFFunction` is unscoped; `<ns>ChfInfo/plmnRangeList` is scoped. The distinction is the whole
    point of the classification: an unscoped IRI came from the inline-sub-object fallback, which has no
    declaring class to scope under, and is therefore a vocabulary question rather than an ingestion bug.
    """
    tail = str(subject)[len(NAMESPACE):] if str(subject).startswith(NAMESPACE) else str(subject)
    return "/" not in tail


def classify(path: Path, work_dir: Path) -> dict[str, Any]:
    """Convert `path` whole and split, and bucket every differing triple by cause."""
    document = yaml.safe_load(path.read_text(encoding="utf-8"))
    api, common = split_document(document)
    api_path, common_path = work_dir / "api.yaml", work_dir / COMMON_NAME
    api_path.write_text(yaml.safe_dump(api), encoding="utf-8")
    common_path.write_text(yaml.safe_dump(common), encoding="utf-8")

    # The two halves are ONE vocabulary and the converter has to be told so; otherwise each half mints
    # the other's classes under a filename-derived namespace and every cross-half range reads as a
    # loss plus a gain. Same reason, same comment, as in `measure_split_isomorphism.compare`.
    one_vocabulary = {api_path.name: NAMESPACE, common_path.name: NAMESPACE}

    def convert(spec: Path, siblings: list[Path]) -> Graph:
        converter = OpenAPIToSHACLConverter(
            str(spec),
            base_namespace=NAMESPACE,
            output_dir=str(work_dir / "out"),
            external_refs=[str(s) for s in siblings],
            document_namespaces=one_vocabulary,
        )
        converter.convert()
        return split_provenance(converter.rdf_graph, NAMESPACE)[0]

    whole = convert(path, [])
    split = convert(api_path, [common_path]) + convert(common_path, [api_path])

    lost = set(whole) - set(split)
    gained = set(split) - set(whole)

    buckets: dict[str, list[str]] = collections.defaultdict(list)
    counts: collections.Counter = collections.Counter()

    for subject in {s for s, _, _ in lost}:
        present_in_split = bool(list(split.triples((subject, None, None))))
        triples_here = sum(1 for t in lost if t[0] == subject)
        if not present_in_split:
            key = "vanished_unscoped" if _is_unscoped(subject) else "vanished_scoped"
            counts[key] += triples_here
            buckets[key].append(str(subject)[len(NAMESPACE):])
            continue
        whole_range = set(whole.objects(subject, RDFS.range))
        split_range = set(split.objects(subject, RDFS.range))
        whole_is_datatype = any(str(r).startswith(str(XSD)) for r in whole_range)
        split_is_datatype = any(str(r).startswith(str(XSD)) for r in split_range)
        whole_names_class = bool(whole_range) and not whole_is_datatype
        split_names_class = bool(split_range) and not split_is_datatype
        if whole_is_datatype and split_names_class:
            # The split resolved a class where the whole document fell back to a datatype. Keyed on the
            # XSD namespace rather than on `xsd:string` alone: the first version of this test hardcoded
            # the string case and left 14 triples "unexplained", 12 of which were the same phenomenon
            # with `xsd:integer`. A filter narrower than the class it describes is this file's own
            # recurring error.
            counts["range_corrected"] += triples_here
            buckets["range_corrected"].append(str(subject)[len(NAMESPACE):])
        elif whole_names_class and split_is_datatype:
            # The reverse, and the ONE bucket where the split is genuinely worse: it lost a class range
            # the whole document resolved. This is the real split defect, and it is small.
            counts["range_degraded"] += triples_here
            buckets["range_degraded"].append(
                f"{str(subject)[len(NAMESPACE):]} {sorted(map(str, whole_range))} -> "
                f"{sorted(map(str, split_range))}"
            )
        else:
            counts["other"] += triples_here
            buckets["other"].append(
                f"{str(subject)[len(NAMESPACE):]} whole_range={sorted(map(str, whole_range))} "
                f"split_range={sorted(map(str, split_range))}"
            )

    return {
        "spec": path.name,
        "whole_triples": len(whole),
        "split_triples": len(split),
        "lost_triples": len(lost),
        "gained_triples": len(gained),
        "by_cause": dict(counts),
        # Capped: the point is the count and a recognisable sample, not a dump of 89 names per document.
        "examples": {k: sorted(v)[:8] for k, v in buckets.items()},
        "subjects_by_cause": {k: len(v) for k, v in buckets.items()},
        # The accounting check: every lost triple landed in exactly one bucket.
        "accounted": sum(counts.values()) == len(lost),
    }


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        "--gpp-dir", type=Path, default=Path("assets/MnS-Rel-19-OpenAPI/OpenAPI"),
        help="directory of 3GPP OpenAPI documents",
    )
    parser.add_argument("--spec", help="one document by file name, for a single-case trace")
    parser.add_argument("--json", type=Path, help="write the full classification here")
    args = parser.parse_args(argv)

    if not args.gpp_dir.is_dir():
        print(f"no corpus at {args.gpp_dir}; run scripts/fetch_corpus.py", file=sys.stderr)
        return 2
    specs = sorted(args.gpp_dir.glob("*.yaml"))
    if args.spec:
        specs = [p for p in specs if p.name == args.spec]
        if not specs:
            print(f"{args.spec} not found in {args.gpp_dir}", file=sys.stderr)
            return 2

    results = []
    for path in specs:
        with tempfile.TemporaryDirectory() as tmp:
            try:
                results.append(classify(path, Path(tmp)))
            except Exception as error:  # a document this cannot convert is data, not a crash
                results.append({"spec": path.name, "error": f"{type(error).__name__}: {error}"})

    ok = [r for r in results if "error" not in r]
    if not ok:
        print("\nNo document was classified. Nothing is asserted by this run.", file=sys.stderr)
        return 2

    totals: collections.Counter = collections.Counter()
    for r in ok:
        totals.update(r["by_cause"])
    lost_total = sum(r["lost_triples"] for r in ok)

    print(f"{len(ok)} documents classified, {len(results) - len(ok)} failed to convert")
    print(f"{lost_total} triples present whole and absent split, by cause:\n")
    labels = {
        "vanished_unscoped": "unscoped IRI, dropped by the split  -> VOCABULARY DECISION, not a bug",
        "vanished_scoped":   "class-scoped IRI, dropped           -> the AC-7 orphaning defect",
        "range_corrected":   "datatype range -> a class           -> the SPLIT is right, whole is wrong",
        "range_degraded":    "class range -> a datatype           -> the SPLIT is wrong: the real defect",
        "other":             "unexplained                         -> the classification is incomplete",
    }
    for key in ("vanished_unscoped", "vanished_scoped", "range_corrected", "range_degraded", "other"):
        n = totals.get(key, 0)
        share = f"{100 * n / lost_total:5.1f}%" if lost_total else "    -"
        print(f"  {n:6}  {share}  {labels[key]}")

    unaccounted = [r["spec"] for r in ok if not r["accounted"]]
    if unaccounted:
        print(f"\n{len(unaccounted)} documents had triples that fell in no bucket: {unaccounted[:5]}")

    if totals.get("range_degraded"):
        print("\nTHE SPLIT DEFECT -- every instance, because it is small enough to list:")
        for r in ok:
            for example in r["examples"].get("range_degraded", []):
                print(f"  {r['spec']}: {example}")

    if totals.get("other"):
        print("\nUNEXPLAINED examples:")
        for r in ok:
            for example in r["examples"].get("other", [])[:2]:
                print(f"  {r['spec']}: {example}")

    if args.json:
        args.json.parent.mkdir(parents=True, exist_ok=True)
        args.json.write_text(json.dumps(results, indent=2), encoding="utf-8")
        print(f"\nWrote {args.json}")

    # A classification that cannot explain everything it found must say so in its exit status, or the
    # shares above read as a complete account when they are a partial one.
    return 1 if totals.get("other") or unaccounted else 0


if __name__ == "__main__":
    raise SystemExit(main())
