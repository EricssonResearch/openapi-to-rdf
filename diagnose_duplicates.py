#!/usr/bin/env python3
"""Diagnose which classes still have duplicate NodeShapes."""

import sys
from pathlib import Path
from collections import Counter
from rdflib import Graph
from rdflib.namespace import Namespace

SH = Namespace("http://www.w3.org/ns/shacl#")

corpus_dir = Path("assets/MnS-Rel-19-OpenAPI/OpenAPI")
spec_files = sorted(corpus_dir.glob("*.yaml"))

from openapi_to_rdf import OpenAPIToSHACLConverter
import tempfile

all_targets = []

with tempfile.TemporaryDirectory() as tmpdir:
    for spec_file in spec_files:
        try:
            converter = OpenAPIToSHACLConverter(str(spec_file), output_dir=tmpdir)
            converter.convert()
            converter.save_rdf()

            shacl_file = Path(tmpdir) / "shacl" / f"{spec_file.stem}_shacl.ttl"
            if not shacl_file.exists():
                continue

            g = Graph()
            g.parse(str(shacl_file), format="turtle")

            # Collect all sh:targetClass objects
            for s, p, o in g.triples((None, SH.targetClass, None)):
                # Store the class and which spec it came from
                all_targets.append((str(o), spec_file.name))
        except Exception as e:
            print(f"Failed to process {spec_file.name}: {e}", file=sys.stderr)

# Count occurrences
target_counts = Counter(cls for cls, _ in all_targets)
duplicates = {cls: count for cls, count in target_counts.items() if count > 1}

if duplicates:
    print(f"Found {len(duplicates)} classes with duplicate NodeShapes:\n")
    for cls, count in sorted(duplicates.items(), key=lambda x: -x[1])[:10]:
        # Show which spec(s) this class appears in
        specs = [spec for c, spec in all_targets if c == cls]
        cls_name = cls.split('#')[-1].split('/')[-1]
        print(f"  {cls_name}: {count} NodeShapes (from {specs[0]})")
else:
    print("✅ No duplicate NodeShapes found!")
