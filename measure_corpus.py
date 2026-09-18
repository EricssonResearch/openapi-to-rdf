#!/usr/bin/env python3
"""Measure sh:targetClass count vs distinct class count across the corpus."""

import sys
from pathlib import Path
from rdflib import Graph
from rdflib.namespace import Namespace

SH = Namespace("http://www.w3.org/ns/shacl#")

corpus_dir = Path("assets/MnS-Rel-19-OpenAPI/OpenAPI")
spec_files = sorted(corpus_dir.glob("*.yaml"))

if not spec_files:
    print(f"No YAML files found in {corpus_dir}", file=sys.stderr)
    sys.exit(1)

print(f"Converting {len(spec_files)} specs from {corpus_dir}...")

from openapi_to_rdf import OpenAPIToSHACLConverter
import tempfile

total_target_class_triples = 0
all_classes = set()

with tempfile.TemporaryDirectory() as tmpdir:
    for spec_file in spec_files:
        try:
            converter = OpenAPIToSHACLConverter(str(spec_file), output_dir=tmpdir)
            converter.convert()
            converter.save_rdf()

            # Load the SHACL graph
            shacl_file = Path(tmpdir) / "shacl" / f"{spec_file.stem}_shacl.ttl"
            if not shacl_file.exists():
                print(f"  ⚠️  No SHACL output for {spec_file.name}", file=sys.stderr)
                continue

            g = Graph()
            g.parse(str(shacl_file), format="turtle")

            # Count sh:targetClass triples and collect distinct classes
            for s, p, o in g.triples((None, SH.targetClass, None)):
                total_target_class_triples += 1
                all_classes.add(str(o))
        except Exception as e:
            print(f"  ❌ Failed to process {spec_file.name}: {e}", file=sys.stderr)

print(f"\n✅ Converted {len(spec_files)} specs")
print(f"   sh:targetClass triples: {total_target_class_triples}")
print(f"   Distinct classes: {len(all_classes)}")
excess = total_target_class_triples - len(all_classes)
if excess > 0:
    pct = (excess * 100.0) / len(all_classes)
    print(f"   Excess shapes: {excess} (+{pct:.1f}%)")
else:
    print(f"   ✅ No excess shapes (one NodeShape per class)")
