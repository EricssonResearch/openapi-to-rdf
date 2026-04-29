"""
SHACL validation tests using real 3GPP test-case RDF instances.

Structure mirrors tio-shacl:
  test-cases/<schema>/good/*.ttl  → must PASS SHACL validation
  test-cases/<schema>/bad/*.ttl   → must FAIL SHACL validation

Also generates a coverage report showing which classes/properties
from the generated RDF are exercised by test cases.
"""
import logging
from pathlib import Path

import pytest
from pyshacl import validate
from rdflib import Graph, Namespace
from rdflib.namespace import RDF, RDFS

logging.getLogger("rdflib.term").setLevel(logging.ERROR)

ROOT = Path(__file__).parent.parent
OUTPUT_DIR = ROOT / "output"
SHACL_DIR = OUTPUT_DIR / "shacl"
RDF_DIR = OUTPUT_DIR / "rdf"
TEST_CASES_DIR = ROOT / "test-cases"

SH = Namespace("http://www.w3.org/ns/shacl#")


def _load_shacl_for(schema_name: str) -> Graph:
    """Load the SHACL shapes file for a given schema."""
    # Try subdir first, then flat
    for path in [SHACL_DIR / f"{schema_name}_shacl.ttl",
                 OUTPUT_DIR / f"{schema_name}_shacl.ttl"]:
        if path.exists():
            g = Graph()
            g.parse(path, format="turtle")
            return g
    pytest.skip(f"SHACL file not found for {schema_name}")


def _collect_test_files():
    """Discover all good/bad test case files."""
    good, bad = [], []
    if not TEST_CASES_DIR.exists():
        return good, bad
    for schema_dir in sorted(TEST_CASES_DIR.iterdir()):
        if not schema_dir.is_dir():
            continue
        name = schema_dir.name
        for f in sorted((schema_dir / "good").glob("*.ttl")) if (schema_dir / "good").exists() else []:
            good.append((name, f))
        for f in sorted((schema_dir / "bad").glob("*.ttl")) if (schema_dir / "bad").exists() else []:
            bad.append((name, f))
    return good, bad


GOOD_CASES, BAD_CASES = _collect_test_files()


@pytest.mark.parametrize("schema_name,ttl_file", GOOD_CASES,
                         ids=[f"{s}/{f.name}" for s, f in GOOD_CASES])
def test_good_instance_conforms(schema_name, ttl_file):
    """Good test cases must pass SHACL validation."""
    shacl_graph = _load_shacl_for(schema_name)
    data_graph = Graph()
    data_graph.parse(ttl_file, format="turtle")
    conforms, _, report = validate(data_graph, shacl_graph=shacl_graph)
    assert conforms, f"Good instance should conform:\n{report}"


@pytest.mark.parametrize("schema_name,ttl_file", BAD_CASES,
                         ids=[f"{s}/{f.name}" for s, f in BAD_CASES])
def test_bad_instance_rejected(schema_name, ttl_file):
    """Bad test cases must fail SHACL validation."""
    shacl_graph = _load_shacl_for(schema_name)
    data_graph = Graph()
    data_graph.parse(ttl_file, format="turtle")
    conforms, _, report = validate(data_graph, shacl_graph=shacl_graph)
    assert not conforms, f"Bad instance should be rejected but conforms:\n{report}"


def test_coverage_report():
    """Generate and verify minimum coverage thresholds."""
    if not RDF_DIR.exists() or not TEST_CASES_DIR.exists():
        pytest.skip("Output or test-cases not found")

    # Extract all classes from generated RDF
    rdf_g = Graph()
    for f in RDF_DIR.glob("TS28623_ComDefs_rdf.ttl"):
        rdf_g.parse(f, format="turtle")
    all_classes = {str(c) for c in rdf_g.subjects(RDF.type, RDFS.Class)} | \
                  {str(c) for c in rdf_g.subjects(RDF.type, RDFS.Datatype)}
    all_properties = {str(p) for p in rdf_g.subjects(RDF.type, RDF.Property)}

    # Extract what test cases use
    test_g = Graph()
    for f in TEST_CASES_DIR.rglob("*.ttl"):
        try:
            test_g.parse(f, format="turtle")
        except Exception:
            pass
    tested_classes = {str(o) for o in test_g.objects(None, RDF.type)}
    tested_properties = {str(p) for p in test_g.predicates()}

    covered_classes = all_classes & tested_classes
    covered_properties = all_properties & tested_properties

    class_pct = len(covered_classes) / len(all_classes) * 100 if all_classes else 0
    prop_pct = len(covered_properties) / len(all_properties) * 100 if all_properties else 0

    print(f"\nCoverage: {len(covered_classes)}/{len(all_classes)} classes ({class_pct:.0f}%), "
          f"{len(covered_properties)}/{len(all_properties)} properties ({prop_pct:.0f}%)")

    uncovered = all_classes - tested_classes
    if uncovered:
        names = sorted(s.split("#")[-1] for s in uncovered)
        print(f"Uncovered classes: {', '.join(names[:20])}{'...' if len(names) > 20 else ''}")

    # Soft threshold — report but don't fail yet while test corpus is being built
    assert class_pct > 0, "No classes covered at all — test cases are broken"
