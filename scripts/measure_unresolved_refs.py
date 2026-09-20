#!/usr/bin/env python3
"""Measure unresolved $ref counts across corpora.

Counts how many external and internal $ref targets go unresolved during conversion.
Task 8b: cross-document resolution should drive the unresolved count toward zero.

    uv run python scripts/measure_unresolved_refs.py
"""

from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from openapi_to_rdf.shacl_converter import OpenAPIToSHACLConverter  # noqa: E402


def count_unresolved_refs(corpus_name: str, specs: list[Path]) -> dict:
    """Count unresolved refs for a corpus."""
    total_unresolved = 0
    specs_with_unresolved = 0
    details = []

    for spec_path in specs:
        try:
            # Get external refs for this spec (all siblings in same directory)
            # Pass just basenames since they're all in the same directory
            spec_dir = spec_path.parent
            external_refs = [
                p.name for p in spec_dir.glob("*.yaml")
                if p != spec_path
            ]

            converter = OpenAPIToSHACLConverter(
                str(spec_path),
                base_namespace=f"https://example.org/{spec_path.stem}/",
                external_refs=external_refs,
            )
            converter.convert()

            unresolved_count = len(converter.unresolved_references)
            if unresolved_count > 0:
                specs_with_unresolved += 1
                total_unresolved += unresolved_count
                details.append({
                    "spec": spec_path.name,
                    "count": unresolved_count,
                    "refs": converter.unresolved_references[:5],  # First 5 for inspection
                })
        except Exception as e:
            print(f"  FAILED {spec_path.name}: {e}", file=sys.stderr)
            continue

    return {
        "total_specs": len(specs),
        "specs_with_unresolved": specs_with_unresolved,
        "total_unresolved": total_unresolved,
        "details": details,
    }


def main():
    print("Measuring unresolved $ref counts...\n")

    # 3GPP corpus
    gpp_dir = Path("assets/MnS-Rel-19-OpenAPI/OpenAPI")
    print("3GPP (38 specs):")
    if not gpp_dir.exists():
        print(f"  SKIPPED: {gpp_dir} not found")
    else:
        gpp_specs = sorted(gpp_dir.glob("*.yaml"))
        gpp_results = count_unresolved_refs("3GPP", gpp_specs)
        print(f"  Total unresolved: {gpp_results['total_unresolved']}")
        print(f"  Specs with unresolved: {gpp_results['specs_with_unresolved']}/{gpp_results['total_specs']}")
        if gpp_results['details']:
            print("  Top specs by unresolved count:")
            for detail in sorted(gpp_results['details'], key=lambda x: x['count'], reverse=True)[:5]:
                print(f"    {detail['spec']}: {detail['count']} unresolved")
                for ref in detail['refs'][:2]:
                    print(f"      - {ref}")
        print()

    # TM Forum corpus
    print("TM Forum (4 specs including common):")
    tmf_base = Path("/home/earejma/snm-api-native-src")
    tmf_specs = [
        tmf_base / "tmf620-productCatalogManagement-v5.0.0.oas.yaml",
        tmf_base / "tmf622-productOrdering-v5.0.0.oas.yaml",
        tmf_base / "tmf641-serviceOrdering-v5.0.0.oas.yaml",
        tmf_base / "openapi/common-tmf-v5.yaml",
    ]
    tmf_specs = [p for p in tmf_specs if p.exists()]
    if not tmf_specs:
        print(f"  SKIPPED: no specs found under {tmf_base}")
    else:
        tmf_results = count_unresolved_refs("TMF", tmf_specs)
        print(f"  Total unresolved: {tmf_results['total_unresolved']}")
        print(f"  Specs with unresolved: {tmf_results['specs_with_unresolved']}/{tmf_results['total_specs']}")
        if tmf_results['details']:
            print("  Specs with unresolved:")
            for detail in tmf_results['details']:
                print(f"    {detail['spec']}: {detail['count']} unresolved")
                for ref in detail['refs'][:2]:
                    print(f"      - {ref}")
        print()


if __name__ == "__main__":
    main()
