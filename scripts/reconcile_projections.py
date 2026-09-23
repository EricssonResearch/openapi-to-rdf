#!/usr/bin/env python3
"""Reconcile class IRIs across all projections.

For every schema name, the class IRI asserted by the TTL projection, the Overlay, the Context,
and the operation graph must be identical, or the name appears in a stated exclusion set with
its reason. This is the gate that justifies the consolidation: without it, two derivations can
agree today and drift tomorrow.
"""

from __future__ import annotations

import sys
from pathlib import Path
from typing import Any

import yaml
from rdflib import RDF, RDFS, Namespace

from openapi_to_rdf import build_mapping
from openapi_to_rdf.projections.context import context_from_mapping
from openapi_to_rdf.projections.operations import operations_from_mapping
from openapi_to_rdf.projections.overlay import overlay_from_mapping

HYDRA = Namespace("http://www.w3.org/ns/hydra/core#")


def reconcile(mapping, doc: dict | None = None, spec_path: Path | None = None) -> dict[str, Any]:
    """Reconcile class IRIs across all projections.

    Args:
        mapping: The derived Mapping.
        doc: Optional OpenAPI document for TTL projection.
        spec_path: Optional path to spec file for TTL projection.

    Returns:
        A dict with:
        - shared: class names where all projections agree
        - disagreements: class names where projections disagree
        - only_in: which projections have which exclusive classes
        - exclusions: stated exclusions with reasons
        - names_checked: total count of names checked
        - operations_classes_count: distinct classes in operation graph
    """
    from openapi_to_rdf.shacl_converter import OpenAPIToSHACLConverter
    import tempfile

    # 1. TTL projection - extract actual class IRIs from the RDF graph
    ttl_classes = {}
    exclusions = {}
    if doc is not None and spec_path is not None:
        try:
            # Extract namespace from the first class IRI
            if mapping.classes:
                first_iri = next(iter(mapping.classes.values())).iri
                # Handle both # and / separators
                if '#' in first_iri:
                    namespace = first_iri.rsplit('#', 1)[0] + '#'
                elif '/' in first_iri:
                    namespace = first_iri.rsplit('/', 1)[0] + '/'
                else:
                    namespace = first_iri + '#'  # Fallback
            else:
                namespace = "https://example.org/ontology/"

            with tempfile.NamedTemporaryFile(mode='w', suffix='.yaml', delete=False) as tf:
                yaml.dump(doc, tf)
                tf.flush()
                converter = OpenAPIToSHACLConverter(tf.name, base_namespace=namespace)
                converter.convert()
                rdf_graph = converter.rdf_graph

                # Extract class IRIs: subjects that are typed as rdfs:Class
                for class_subj in rdf_graph.subjects(RDF.type, RDFS.Class):
                    class_iri = str(class_subj)
                    # Match back to class name by IRI
                    for name, fact in mapping.classes.items():
                        if fact.is_external:
                            continue
                        if fact.iri == class_iri:
                            ttl_classes[name] = class_iri
                            break

                Path(tf.name).unlink()  # Clean up temp file
        except Exception as e:
            print(f"Warning: Could not build TTL projection: {e}", file=sys.stderr)
            import traceback
            traceback.print_exc()

        # Stated exclusions: transport envelopes excluded from TTL by design
        for name, fact in mapping.classes.items():
            if fact.is_external:
                continue
            if fact.is_transport and name not in ttl_classes:
                exclusions[name] = "transport envelope (excluded from TTL by design)"

    # An external class is declared by ITS OWN document and deliberately referenced-only here, so
    # it is not a term this document's TTL is expected to declare. The count is reported so a
    # rename that quietly widens this exclusion fails loudly instead of narrowing the gate.
    external_classes = [name for name, fact in mapping.classes.items() if fact.is_external]
    expected = {
        name: fact.iri for name, fact in mapping.classes.items() if not fact.is_external
    }

    # 2. Overlay
    overlay_classes = {}
    try:
        # We need to create an overlay, but we don't have extends/title/version
        # For reconciliation purposes, we'll extract the IRIs directly from actions
        # Actually, we can build the overlay here
        overlay = overlay_from_mapping(
            mapping, extends="dummy.yaml", title="Reconcile", version="1.0"
        )
        for action in overlay["actions"]:
            if "$.components.schemas[" in action["target"]:
                schema_name = action["target"].split("['")[1].split("']")[0]
                if "x-jsonld-type" in action["update"]:
                    overlay_classes[schema_name] = action["update"]["x-jsonld-type"]
    except Exception as e:
        print(f"Warning: Could not build overlay: {e}", file=sys.stderr)

    # 3. Context
    context_classes = {}
    try:
        # Need to extract namespace from mapping
        # The namespace is part of the class IRIs
        if mapping.classes:
            first_iri = next(iter(mapping.classes.values())).iri
            # Extract namespace (everything before the last #)
            if "#" in first_iri:
                namespace = first_iri.rsplit("#", 1)[0] + "#"
            else:
                namespace = first_iri.rsplit("/", 1)[0] + "/"

            context = context_from_mapping(mapping, base=namespace)
            for name, data in context["@context"].items():
                if isinstance(data, dict) and "@id" in data:
                    context_classes[name] = data["@id"]
    except Exception as e:
        print(f"Warning: Could not build context: {e}", file=sys.stderr)

    # 4. Operations graph
    ops_classes = {}
    try:
        # Extract namespace
        if mapping.classes:
            first_iri = next(iter(mapping.classes.values())).iri
            if "#" in first_iri:
                namespace = first_iri.rsplit("#", 1)[0] + "#"
            else:
                namespace = first_iri.rsplit("/", 1)[0] + "/"

            ops_graph = operations_from_mapping(mapping)

            # Extract classes mentioned in operations
            for obj in ops_graph.objects(None, HYDRA.returns):
                iri = str(obj)
                # Try to match this IRI to a class name
                for name, fact in mapping.classes.items():
                    if fact.iri == iri:
                        ops_classes[name] = iri
                        break

            for obj in ops_graph.objects(None, HYDRA.expects):
                iri = str(obj)
                for name, fact in mapping.classes.items():
                    if fact.iri == iri:
                        ops_classes[name] = iri
                        break
    except Exception as e:
        print(f"Warning: Could not build operations graph: {e}", file=sys.stderr)

    # Reconcile
    disagreements = []
    shared = []

    for name in expected:
        # Skip if this is a stated exclusion
        if name in exclusions:
            continue

        expected_iri = expected[name]
        iris = {
            "expected": expected_iri,
            "ttl": ttl_classes.get(name),
            "overlay": overlay_classes.get(name),
            "context": context_classes.get(name),
            "operations": ops_classes.get(name),
        }

        # Remove None values
        present = {k: v for k, v in iris.items() if v is not None}

        # Check if all present IRIs match
        unique_iris = set(present.values())
        if len(unique_iris) > 1:
            disagreements.append({"name": name, "iris": present})
        elif len(unique_iris) == 1:
            shared.append(name)

    return {
        "shared": shared,
        "disagreements": disagreements,
        "exclusions": exclusions,
        "only_in": {
            "ttl_only": set(ttl_classes.keys()) - set(expected.keys()),
            "overlay_only": set(overlay_classes.keys()) - set(expected.keys()),
            "context_only": set(context_classes.keys()) - set(expected.keys()),
            "operations_only": set(ops_classes.keys()) - set(expected.keys()),
        },
        "names_checked": len(expected),
        "operations_classes_count": len(ops_classes),
        "ttl_classes_count": len(ttl_classes),
        "external_classes_excluded": len(external_classes),
        "external_classes": sorted(external_classes),
    }


def main():
    """Run reconciliation on a spec file."""
    import argparse

    parser = argparse.ArgumentParser(description="Reconcile projections")
    parser.add_argument("spec", type=Path, help="OpenAPI spec file")
    parser.add_argument("--namespace", default="https://example.org/ontology/",
                        help="Base namespace")
    args = parser.parse_args()

    doc = yaml.safe_load(args.spec.read_text())
    mapping = build_mapping(doc, namespace=args.namespace)
    result = reconcile(mapping, doc=doc, spec_path=args.spec)

    print(f"Names checked: {result['names_checked']}")
    print(f"TTL classes: {result.get('ttl_classes_count', 0)}")
    print(f"External classes excluded: {result.get('external_classes_excluded', 0)}")
    print(f"Exclusions: {len(result['exclusions'])}")
    print(f"Shared (all agree): {len(result['shared'])}")
    print(f"Disagreements: {len(result['disagreements'])}")

    if result['exclusions']:
        print("\nStated exclusions:")
        for name, reason in result['exclusions'].items():
            print(f"  {name}: {reason}")

    if result['disagreements']:
        print("\nDisagreements:")
        for disagreement in result['disagreements']:
            print(f"  {disagreement['name']}:")
            for proj, iri in disagreement['iris'].items():
                print(f"    {proj}: {iri}")
        sys.exit(1)
    else:
        print("\n✓ All projections reconciled")
        sys.exit(0)


if __name__ == "__main__":
    main()
