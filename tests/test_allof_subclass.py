"""Tests for `allOf` → `rdfs:subClassOf` emission.

TMF SID and Ericsson CTS OpenAPI specs use `allOf` extensively to express
class inheritance (``Af allOf: [$ref: NgCoreNetFunction, ...inline...]``).
The RDF graph should reflect this as ``rdfs:subClassOf`` edges so the
class hierarchy is navigable — not collapsed into a flat list of classes
with a comment.
"""
from __future__ import annotations

import tempfile
from pathlib import Path

import pytest
import yaml
from rdflib import Graph, Namespace
from rdflib.namespace import RDF, RDFS

from openapi_to_rdf.shacl_converter import OpenAPIToSHACLConverter


def _run(spec: dict, prefix: str = "https://example.com/test/"):
    """Run the converter on a spec dict and return (graph, class_namespace)."""
    with tempfile.TemporaryDirectory() as tmp:
        src = Path(tmp) / "spec.yaml"
        src.write_text(yaml.safe_dump(spec))
        converter = OpenAPIToSHACLConverter(
            str(src),
            base_namespace=None,
            external_refs=[],
            base_namespace_prefix=prefix,
        )
        converter.run()
        # The converter writes to cwd-relative "output/rdf/<stem>_rdf.ttl".
        target = Path("output/rdf/spec_rdf.ttl")
        graph = Graph()
        graph.parse(target, format="turtle")
        # The main class namespace ends with `/<stem>#` — `spec:Parent`
        # serializes as `.../rdf/spec#Parent`.
        class_ns = None
        for p, u in graph.namespaces():
            u = str(u)
            if u.endswith("/spec#"):
                class_ns = Namespace(u)
                break
        assert class_ns is not None, (
            f"couldn't find class namespace; prefixes={list(graph.namespaces())}"
        )
        return graph, class_ns


def test_allof_ref_emits_subclass_edge():
    """`allOf: [$ref: Parent]` should produce `rdfs:subClassOf Parent` in RDF."""
    spec = {
        "openapi": "3.0.0",
        "info": {"title": "Test", "version": "1.0"},
        "paths": {},
        "components": {
            "schemas": {
                "Parent": {
                    "type": "object",
                    "properties": {"id": {"type": "string"}},
                },
                "Child": {
                    "allOf": [
                        {"$ref": "#/components/schemas/Parent"},
                        {
                            "type": "object",
                            "properties": {"extra": {"type": "string"}},
                        },
                    ]
                },
            }
        },
    }
    g, ns = _run(spec)

    # Both classes emitted.
    assert (ns.Parent, RDF.type, RDFS.Class) in g
    assert (ns.Child, RDF.type, RDFS.Class) in g
    # Inheritance edge.
    assert (ns.Child, RDFS.subClassOf, ns.Parent) in g, \
        "Child should declare rdfs:subClassOf Parent"


def test_allof_inline_object_merges_properties_onto_child():
    """Inline object items inside `allOf` should contribute properties to the child class,
    not spawn a separate synthetic class."""
    spec = {
        "openapi": "3.0.0",
        "info": {"title": "Test", "version": "1.0"},
        "paths": {},
        "components": {
            "schemas": {
                "Parent": {
                    "type": "object",
                    "properties": {"id": {"type": "string"}},
                },
                "Child": {
                    "allOf": [
                        {"$ref": "#/components/schemas/Parent"},
                        {
                            "type": "object",
                            "properties": {
                                "childOnly": {"type": "string"},
                            },
                        },
                    ]
                },
            }
        },
    }
    g, ns = _run(spec)

    # A property with rdfs:domain = Child and local name `childOnly` should exist.
    properties_of_child = list(g.subjects(RDFS.domain, ns.Child))
    names = [str(p).rsplit("/", 1)[-1].split("#")[-1] for p in properties_of_child]
    assert "childOnly" in names, f"childOnly should be a property of Child; got {names}"


def test_allof_multiple_refs_emits_all_parents():
    """Multi-inheritance via multiple `$ref` in `allOf` emits one edge per parent."""
    spec = {
        "openapi": "3.0.0",
        "info": {"title": "Test", "version": "1.0"},
        "paths": {},
        "components": {
            "schemas": {
                "A": {"type": "object", "properties": {"a": {"type": "string"}}},
                "B": {"type": "object", "properties": {"b": {"type": "string"}}},
                "AB": {
                    "allOf": [
                        {"$ref": "#/components/schemas/A"},
                        {"$ref": "#/components/schemas/B"},
                    ]
                },
            }
        },
    }
    g, ns = _run(spec)

    assert (ns.AB, RDFS.subClassOf, ns.A) in g
    assert (ns.AB, RDFS.subClassOf, ns.B) in g


def test_allof_without_ref_does_not_emit_subclass():
    """Pure-inline `allOf` (no $ref) should not emit any subClassOf edges."""
    spec = {
        "openapi": "3.0.0",
        "info": {"title": "Test", "version": "1.0"},
        "paths": {},
        "components": {
            "schemas": {
                "InlineOnly": {
                    "allOf": [
                        {"type": "object", "properties": {"a": {"type": "string"}}},
                        {"type": "object", "properties": {"b": {"type": "string"}}},
                    ]
                }
            }
        },
    }
    g, _ns = _run(spec)

    # No rdfs:subClassOf triples should exist at all in this test.
    assert list(g.triples((None, RDFS.subClassOf, None))) == [], \
        "Inline-only allOf must not produce subclass edges"


def test_allof_still_emits_shacl_constraints():
    """SHACL constraints from allOf parents should still be emitted.

    Current behaviour for `allOf: [$ref: Parent]` produces `sh:class Parent`
    on a NodeShape. The new rdfs:subClassOf edge is additive — we don't
    lose the SHACL validation constraint.
    """
    spec = {
        "openapi": "3.0.0",
        "info": {"title": "Test", "version": "1.0"},
        "paths": {},
        "components": {
            "schemas": {
                "Parent": {
                    "type": "object",
                    "properties": {"id": {"type": "string"}},
                },
                "Child": {
                    "allOf": [{"$ref": "#/components/schemas/Parent"}]
                },
            }
        },
    }
    import tempfile
    with tempfile.TemporaryDirectory() as tmp:
        src = Path(tmp) / "spec.yaml"
        src.write_text(yaml.safe_dump(spec))
        OpenAPIToSHACLConverter(
            str(src),
            base_namespace=None,
            external_refs=[],
            base_namespace_prefix="https://example.com/test/",
        ).run()
        shacl_ttl = Path("output/shacl/spec_shacl.ttl").read_text()
        # Some form of class-relation constraint should still be present.
        assert ("sh:class" in shacl_ttl or "sh:and" in shacl_ttl), \
            "SHACL should still emit class-relation constraints from allOf"
