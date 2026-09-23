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

    whole_pairs = attribution_pairs(whole_mapping)
    # Union both halves' attribution pairs
    split_pairs = attribution_pairs(split_api_mapping) | attribution_pairs(split_common_mapping)
    return {
        "spec": path.name,
        "moved_schemas": len(common["components"]["schemas"]),
        "whole_triples": len(whole_graph),
        "split_triples": len(split_graph),
        "pairs_only_in_split": sorted(split_pairs - whole_pairs),
        "pairs_only_in_whole": sorted(whole_pairs - split_pairs),
        "graphs_isomorphic": whole_graph.isomorphic(split_graph),
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
                verdict = "OK" if invented == 0 and result["graphs_isomorphic"] else "DIVERGENT"
                failed = failed or verdict == "DIVERGENT"
                print(
                    f"  {path.name:55s} moved={result['moved_schemas']:4d} "
                    f"whole={result['whole_triples']:6d} split={result['split_triples']:6d} "
                    f"invented_pairs={invented:4d} isomorphic={result['graphs_isomorphic']!s:5s} "
                    f"{verdict}"
                )
                for pair in result["pairs_only_in_split"][:10]:
                    print(f"      invented: {pair[0]}.{pair[1]}")

    if not results:
        print("\nNo corpus was measured. Nothing is asserted by this run.", file=sys.stderr)
        return 2
    if args.json:
        args.json.write_text(json.dumps(results, indent=2), encoding="utf-8")
        print(f"\nWrote {args.json}")
    print(f"\n{'FAIL' if failed else 'PASS'}: {len(results)} documents compared")
    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
