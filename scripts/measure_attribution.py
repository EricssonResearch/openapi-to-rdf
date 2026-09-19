#!/usr/bin/env python3
"""Do the emitter and the Mapping attribute every property to the same declaring class?

An ontology declares an inherited property once, on the class that introduces it. The emitter used to
answer "which class declares this?" by probing the ``rdflib.Graph`` it was still building, so its
answer depended on the order schemas appear in the document; ``Mapping`` answers from the document's
``allOf`` structure and is order-independent. Where the two disagreed, one OpenAPI field became two
RDF properties.

This script is the instrument that measured that. Task 8 collapsed the two implementations, so it now
answers 0 everywhere — and the point of keeping it is that **it reports the non-trivial count
alongside the disagreement count**. A zero disagreement over zero non-trivial attributions is not
evidence of anything, and that is exactly the trap the 3GPP corpus sets:

    corpus        compared   non-trivial   disagreed (before Task 8)   disagreed (after)
    3GPP             3,622             0                           0                  0
    TM Forum         3,033           138                          43                  0

**Read the non-trivial column first.** 3GPP cannot exhibit this defect at all, so a 3GPP-only check
was a null from an instrument that could only produce nulls.

"Non-trivial" means the attribution landed on an ancestor rather than on the class that mentions the
property — the only cases where the two implementations can differ.

    uv run python scripts/measure_attribution.py
    uv run python scripts/measure_attribution.py --json /tmp/attribution.json
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

import yaml

sys.path.insert(0, str(Path(__file__).resolve().parent))
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

import _corpora  # noqa: E402

from openapi_to_rdf import build_mapping  # noqa: E402
from openapi_to_rdf.mapping import _resolve_declaring_class, flattened_properties  # noqa: E402
from openapi_to_rdf.shacl_converter import OpenAPIToSHACLConverter  # noqa: E402


def attribute_both_ways(path: Path) -> dict:
    """Attribute every property twice — once by each implementation — and compare.

    The emitter is observed rather than re-implemented: its ``_find_declaring_class`` is wrapped so
    every question it actually asks is recorded, which is what makes this a comparison of the two live
    code paths rather than of this script's idea of them.
    """
    document = yaml.safe_load(path.read_text(encoding="utf-8"))
    converter = OpenAPIToSHACLConverter(str(path), external_refs=[])

    asked: list[tuple[str, str, str]] = []
    emitter_rule = converter._find_declaring_class

    def local(uri) -> str:
        return str(uri).rsplit("#", 1)[-1].rsplit("/", 1)[-1]

    def spy(current_class, prop_name):
        answer = emitter_rule(current_class, prop_name)
        asked.append((local(current_class), prop_name, local(answer)))
        return answer

    converter._find_declaring_class = spy
    converter.convert()

    mapping = build_mapping(document, namespace=converter.base_namespace)
    schemas = (document.get("components") or {}).get("schemas") or {}
    parents = {name: fact.parents for name, fact in mapping.classes.items()}
    declared = {name: flattened_properties(schemas[name]) for name in mapping.classes}

    compared = non_trivial = 0
    disagreements: list[str] = []
    for class_name, prop_name, emitter_says in asked:
        if class_name not in mapping.classes:
            continue  # an inline anonymous sub-object has no named class on either side
        compared += 1
        mapping_says = _resolve_declaring_class(class_name, parents, declared, prop_name)
        if emitter_says != class_name or mapping_says != class_name:
            non_trivial += 1
        if emitter_says != mapping_says:
            disagreements.append(
                f"{class_name}.{prop_name}: emitter={emitter_says} mapping={mapping_says}"
            )
    return {
        "spec": path.name,
        "compared": compared,
        "non_trivial": non_trivial,
        "disagreements": len(disagreements),
        "detail": disagreements[:20],
    }


def main() -> int:
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    _corpora.add_arguments(parser)
    args = parser.parse_args()

    corpora = _corpora.resolve(args)
    results: dict[str, dict] = {}
    for corpus in corpora:
        if not corpus:
            continue
        rows = [attribute_both_ways(p) for p in corpus.paths]
        totals = {
            key: sum(r[key] for r in rows) for key in ("compared", "non_trivial", "disagreements")
        }
        results[corpus.label] = {"per_spec": rows, **totals}
        print(f"\n=== {corpus.label} ===")
        for row in rows:
            if len(rows) <= 5 or row["disagreements"]:
                print(
                    f"  {row['spec']:52s} compared={row['compared']:5d} "
                    f"non_trivial={row['non_trivial']:4d} disagree={row['disagreements']:4d}"
                )
        print(
            f"  TOTAL compared={totals['compared']} non_trivial={totals['non_trivial']} "
            f"disagreements={totals['disagreements']}"
        )
        if totals["non_trivial"] == 0:
            print(
                "  -> 0 non-trivial attributions: this corpus CANNOT exhibit the defect, so its "
                "0 disagreements are not evidence the rule is right."
            )
    skipped = _corpora.report_skips(corpora)

    if args.json:
        args.json.write_text(json.dumps(results, indent=2), encoding="utf-8")
        print(f"\nwrote {args.json}")

    if not results:
        print("no corpus was measured")
        return 2
    return 1 if (sum(r["disagreements"] for r in results.values()) or skipped) else 0


if __name__ == "__main__":
    sys.exit(main())
