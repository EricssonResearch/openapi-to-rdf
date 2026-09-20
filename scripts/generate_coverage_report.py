#!/usr/bin/env python3
"""
SHACL coverage report for OpenAPI-to-RDF conversion.

Measures what percentage of generated RDF classes, properties, and SHACL shapes
are actually exercised by validation test cases (good + bad instances).

Inspired by tio-shacl's coverage methodology:
- Extract all classes/properties from generated RDF
- Extract all NodeShapes/PropertyShapes from generated SHACL
- Check which are instantiated/validated in test cases
- Report gaps

Usage:
    python scripts/generate_coverage_report.py
"""
import sys
from datetime import datetime
from pathlib import Path

from rdflib import Graph, Namespace
from rdflib.namespace import RDF, RDFS

SH = Namespace("http://www.w3.org/ns/shacl#")


def extract_rdf_elements(rdf_dir: Path) -> tuple[set[str], set[str]]:
    """Extract all classes and properties from generated RDF files."""
    classes, properties = set(), set()
    g = Graph()
    for f in rdf_dir.glob("*.ttl"):
        g.parse(f, format="turtle")
    for cls in g.subjects(RDF.type, RDFS.Class):
        classes.add(str(cls))
    for cls in g.subjects(RDF.type, RDFS.Datatype):
        classes.add(str(cls))
    for prop in g.subjects(RDF.type, RDF.Property):
        properties.add(str(prop))
    return classes, properties


def extract_shacl_elements(shacl_dir: Path) -> tuple[set[str], set[str]]:
    """Extract targetClasses and property paths from SHACL shapes."""
    target_classes, property_paths = set(), set()
    g = Graph()
    for f in shacl_dir.glob("*.ttl"):
        g.parse(f, format="turtle")
    for tc in g.objects(None, SH.targetClass):
        target_classes.add(str(tc))
    for path in g.objects(None, SH.path):
        if hasattr(path, 'toPython'):
            property_paths.add(str(path))
    return target_classes, property_paths


def extract_test_usage(test_dir: Path, test_type: str = None) -> tuple[set[str], set[str]]:
    """Extract classes instantiated and properties used in test case RDF files."""
    used_classes, used_properties = set(), set()
    g = Graph()
    for f in test_dir.rglob("*.ttl"):
        if test_type and f"/{test_type}/" not in str(f):
            continue
        try:
            g.parse(f, format="turtle")
        except Exception:
            pass
    for obj in g.objects(None, RDF.type):
        used_classes.add(str(obj))
    for pred in g.predicates():
        used_properties.add(str(pred))
    return used_classes, used_properties


def local_name(uri: str) -> str:
    return uri.split("#")[-1] if "#" in uri else uri.split("/")[-1]


def pct(n: int, total: int) -> float:
    return n / total * 100 if total else 0


def generate_report(
    rdf_classes, rdf_properties,
    shacl_targets, shacl_paths,
    good_classes, good_properties,
    bad_classes, bad_properties,
) -> str:
    all_tested_classes = good_classes | bad_classes
    all_tested_properties = good_properties | bad_properties

    covered_classes = rdf_classes & all_tested_classes
    covered_properties = rdf_properties & all_tested_properties
    covered_shapes = shacl_targets & all_tested_classes

    missing_classes = rdf_classes - all_tested_classes
    missing_properties = rdf_properties - all_tested_properties
    missing_shapes = shacl_targets - all_tested_classes

    good_cls = rdf_classes & good_classes
    bad_cls = rdf_classes & bad_classes
    good_prop = rdf_properties & good_properties
    bad_prop = rdf_properties & bad_properties

    lines = [
        "# OpenAPI-to-RDF SHACL Coverage Report",
        "",
        f"> Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}",
        "",
        "## Summary",
        "",
        f"- **Classes Covered**: {len(covered_classes)}/{len(rdf_classes)} ({pct(len(covered_classes), len(rdf_classes)):.1f}%)"
        f" — Good: {len(good_cls)}/{len(rdf_classes)} ({pct(len(good_cls), len(rdf_classes)):.1f}%),"
        f" Bad: {len(bad_cls)}/{len(rdf_classes)} ({pct(len(bad_cls), len(rdf_classes)):.1f}%)",
        f"- **Properties Covered**: {len(covered_properties)}/{len(rdf_properties)} ({pct(len(covered_properties), len(rdf_properties)):.1f}%)"
        f" — Good: {len(good_prop)}/{len(rdf_properties)} ({pct(len(good_prop), len(rdf_properties)):.1f}%),"
        f" Bad: {len(bad_prop)}/{len(rdf_properties)} ({pct(len(bad_prop), len(rdf_properties)):.1f}%)",
        f"- **SHACL Shapes Covered**: {len(covered_shapes)}/{len(shacl_targets)} ({pct(len(covered_shapes), len(shacl_targets)):.1f}%)",
        "",
        "## Methodology",
        "",
        "Coverage is measured by checking which RDF classes and properties from the",
        "generated output are actually instantiated in test-case RDF instances.",
        "Test cases are split into `good/` (must pass SHACL) and `bad/` (must fail SHACL).",
        "",
    ]

    if missing_classes:
        lines.extend([
            f"## Classes NOT Covered ({len(missing_classes)})",
            "",
        ])
        for c in sorted(missing_classes, key=local_name):
            lines.append(f"- `{local_name(c)}`")
        lines.append("")
    else:
        lines.extend(["## Classes NOT Covered", "", "All classes covered.", ""])

    if missing_properties:
        lines.extend([
            f"## Properties NOT Covered ({len(missing_properties)})",
            "",
        ])
        for p in sorted(missing_properties, key=local_name):
            lines.append(f"- `{local_name(p)}`")
        lines.append("")
    else:
        lines.extend(["## Properties NOT Covered", "", "All properties covered.", ""])

    if missing_shapes:
        lines.extend([
            f"## SHACL Shapes NOT Covered ({len(missing_shapes)})",
            "",
        ])
        for s in sorted(missing_shapes, key=local_name):
            lines.append(f"- `{local_name(s)}`")
        lines.append("")

    lines.extend([
        "---",
        "",
        "*Auto-generated by `scripts/generate_coverage_report.py`*",
    ])
    return "\n".join(lines)


def main():
    root = Path(__file__).parent.parent
    rdf_dir = root / "output" / "rdf"
    shacl_dir = root / "output" / "shacl"
    test_dir = root / "test-cases"
    output_path = root / "docs" / "coverage-report.md"

    if not rdf_dir.exists():
        print(f"Error: {rdf_dir} not found. Run conversion first.", file=sys.stderr)
        sys.exit(1)

    if not test_dir.exists():
        print("No test-cases/ directory found. Creating empty report.", file=sys.stderr)
        test_dir.mkdir(parents=True, exist_ok=True)

    rdf_classes, rdf_properties = extract_rdf_elements(rdf_dir)
    shacl_targets, shacl_paths = extract_shacl_elements(shacl_dir)
    good_cls, good_prop = extract_test_usage(test_dir, "good")
    bad_cls, bad_prop = extract_test_usage(test_dir, "bad")

    report = generate_report(
        rdf_classes, rdf_properties,
        shacl_targets, shacl_paths,
        good_cls, good_prop,
        bad_cls, bad_prop,
    )

    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(report)
    print(report)
    print(f"\nWritten to: {output_path}")


if __name__ == "__main__":
    main()
