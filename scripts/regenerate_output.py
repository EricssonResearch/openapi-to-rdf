#!/usr/bin/env python3
"""Regenerate the output/ deliverable tree from HEAD.

The output/ directory contains generated artifacts (RDF, SHACL, property indices) that serve
as both examples and a deliverable the README advertises. This script regenerates the entire
tree from the current code, ensuring committed artifacts match HEAD.

Usage:
    uv run python scripts/regenerate_output.py

The script processes all specs in the bundled 3GPP corpus (assets/MnS-Rel-19-OpenAPI/OpenAPI/)
and overwrites output/rdf/, output/shacl/, and output/index/ with fresh conversions.
"""

from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from openapi_to_rdf.shacl_converter import OpenAPIToSHACLConverter


def main():
    """Regenerate all output/ artifacts."""
    repo_root = Path(__file__).resolve().parent.parent
    gpp_dir = repo_root / "assets" / "MnS-Rel-19-OpenAPI" / "OpenAPI"
    output_dir = repo_root / "output"

    if not gpp_dir.exists():
        print(f"Error: corpus directory not found: {gpp_dir}", file=sys.stderr)
        sys.exit(1)

    specs = sorted(gpp_dir.glob("*.yaml"))
    print(f"Regenerating output/ from {len(specs)} specs...")

    for spec_path in specs:
        print(f"  {spec_path.name}")
        # Get sibling refs for cross-document resolution
        external_refs = [p.name for p in gpp_dir.glob("*.yaml") if p != spec_path]

        converter = OpenAPIToSHACLConverter(
            str(spec_path),
            base_namespace=f"https://example.org/{spec_path.stem}/",
            output_dir=str(output_dir),
            external_refs=external_refs,
        )
        converter.run()

    print(f"\n✓ Regenerated {len(specs)} specs into {output_dir}")


if __name__ == "__main__":
    main()
