"""Tests for the ``schema_namespaces`` override feature.

A merged multi-domain OpenAPI spec needs to emit different schemas under
different RDF namespaces in a single conversion pass. Feeding the spec
piecewise (one domain per run) duplicates shared classes across
namespaces, so the converter accepts a ``{ClassName: namespace_uri}``
map that overrides the file-level ``base_namespace`` for named schemas.

This covers the CTS use case where ``Resource`` lives under
``.../cts/ctc/`` but ``WirelessNetFunction`` (which ``allOf``-refs
``Resource``) lives under ``.../cts/ctw/``. The emitted
``rdfs:subClassOf`` edge must cross the namespace boundary correctly.
"""
from __future__ import annotations

import tempfile
from pathlib import Path

import yaml
from rdflib import Graph, URIRef
from rdflib.namespace import RDF, RDFS

from openapi_to_rdf.shacl_converter import OpenAPIToSHACLConverter


BASE = "https://example.com/ontology"


def _run_with_overrides(spec: dict, schema_namespaces: dict[str, str]):
    """Run the converter with a ``schema_namespaces`` map; return the parsed graph."""
    with tempfile.TemporaryDirectory() as tmp:
        src = Path(tmp) / "spec.yaml"
        src.write_text(yaml.safe_dump(spec))
        converter = OpenAPIToSHACLConverter(
            str(src),
            base_namespace=f"{BASE}/shared/",
            external_refs=[],
            base_namespace_prefix=f"{BASE}/shared/",
            schema_namespaces=schema_namespaces,
            # Without this the converter writes into the repo's published output/ tree, because
            # its default output_dir is cwd-relative. tests/conftest.py now fails that.
            output_dir=tmp,
        )
        converter.run()
        g = Graph()
        g.parse(Path(tmp) / "rdf" / "spec_rdf.ttl", format="turtle")
        return g


def test_schema_under_override_namespace_emits_class_there():
    """A schema listed in schema_namespaces appears under the override URI."""
    spec = {
        "openapi": "3.0.0",
        "info": {"title": "Test", "version": "1.0"},
        "paths": {},
        "components": {
            "schemas": {
                "Resource": {"type": "object", "properties": {"id": {"type": "string"}}},
            }
        },
    }
    g = _run_with_overrides(spec, {"Resource": f"{BASE}/ctc/"})

    resource_uri = URIRef(f"{BASE}/ctc/Resource")
    assert (resource_uri, RDF.type, RDFS.Class) in g


def test_unlisted_schema_falls_back_to_base_namespace():
    """Schemas not in schema_namespaces keep using the file-level base."""
    spec = {
        "openapi": "3.0.0",
        "info": {"title": "Test", "version": "1.0"},
        "paths": {},
        "components": {
            "schemas": {
                "Mapped": {"type": "object", "properties": {"x": {"type": "string"}}},
                "Unmapped": {"type": "object", "properties": {"y": {"type": "string"}}},
            }
        },
    }
    g = _run_with_overrides(spec, {"Mapped": f"{BASE}/ctc/"})

    assert (URIRef(f"{BASE}/ctc/Mapped"), RDF.type, RDFS.Class) in g
    assert (URIRef(f"{BASE}/shared/Unmapped"), RDF.type, RDFS.Class) in g


def test_allof_ref_crosses_namespace_boundary():
    """$ref inheritance resolves via the override map, producing cross-namespace edges."""
    spec = {
        "openapi": "3.0.0",
        "info": {"title": "Test", "version": "1.0"},
        "paths": {},
        "components": {
            "schemas": {
                "Resource": {"type": "object", "properties": {"id": {"type": "string"}}},
                "WirelessNetFunction": {
                    "allOf": [
                        {"$ref": "#/components/schemas/Resource"},
                        {"type": "object", "properties": {"plmn": {"type": "string"}}},
                    ]
                },
            }
        },
    }
    g = _run_with_overrides(
        spec,
        {
            "Resource": f"{BASE}/ctc/",
            "WirelessNetFunction": f"{BASE}/ctw/",
        },
    )

    wnf = URIRef(f"{BASE}/ctw/WirelessNetFunction")
    resource = URIRef(f"{BASE}/ctc/Resource")
    assert (wnf, RDFS.subClassOf, resource) in g, (
        "Cross-namespace subClassOf edge must point at the parent's override namespace"
    )
    # And crucially, no duplicate Resource under the child's namespace.
    stray = URIRef(f"{BASE}/ctw/Resource")
    assert (stray, RDF.type, RDFS.Class) not in g, (
        "Resource must not be duplicated under the child namespace"
    )


def test_property_uris_follow_owning_class_namespace():
    """Properties are minted under their owning class's (overridden) namespace."""
    spec = {
        "openapi": "3.0.0",
        "info": {"title": "Test", "version": "1.0"},
        "paths": {},
        "components": {
            "schemas": {
                "Resource": {
                    "type": "object",
                    "properties": {
                        "id": {"type": "string"},
                        "name": {"type": "string"},
                    },
                },
            }
        },
    }
    g = _run_with_overrides(spec, {"Resource": f"{BASE}/ctc/"})

    resource_uri = URIRef(f"{BASE}/ctc/Resource")
    # Every property of Resource must sit under ctc/Resource/, not shared/Resource/.
    props = list(g.subjects(RDFS.domain, resource_uri))
    assert props, "Resource should have at least one property"
    for p in props:
        assert str(p).startswith(f"{BASE}/ctc/Resource/"), (
            f"Property {p} should be under the overridden class namespace"
        )


def test_same_namespace_inheritance_still_works():
    """Two classes in the same overridden namespace still link cleanly."""
    spec = {
        "openapi": "3.0.0",
        "info": {"title": "Test", "version": "1.0"},
        "paths": {},
        "components": {
            "schemas": {
                "WirelessNetFunction": {
                    "type": "object",
                    "properties": {"plmn": {"type": "string"}},
                },
                "EpcNetFunction": {
                    "allOf": [
                        {"$ref": "#/components/schemas/WirelessNetFunction"},
                        {"type": "object", "properties": {"role": {"type": "string"}}},
                    ]
                },
            }
        },
    }
    g = _run_with_overrides(
        spec,
        {
            "WirelessNetFunction": f"{BASE}/ctw/",
            "EpcNetFunction": f"{BASE}/ctw/",
        },
    )
    parent = URIRef(f"{BASE}/ctw/WirelessNetFunction")
    child = URIRef(f"{BASE}/ctw/EpcNetFunction")
    assert (child, RDFS.subClassOf, parent) in g


def test_empty_or_missing_overrides_preserve_default_behaviour():
    """Omitting schema_namespaces entirely behaves exactly like the old API."""
    spec = {
        "openapi": "3.0.0",
        "info": {"title": "Test", "version": "1.0"},
        "paths": {},
        "components": {
            "schemas": {
                "A": {"type": "object", "properties": {"id": {"type": "string"}}},
            }
        },
    }
    with tempfile.TemporaryDirectory() as tmp:
        src = Path(tmp) / "spec.yaml"
        src.write_text(yaml.safe_dump(spec))
        # No schema_namespaces arg at all.
        OpenAPIToSHACLConverter(
            str(src),
            base_namespace=None,
            external_refs=[],
            base_namespace_prefix=f"{BASE}/x/",
            output_dir=tmp,
        ).run()
        g = Graph()
        g.parse(Path(tmp) / "rdf" / "spec_rdf.ttl", format="turtle")
        # Class emitted under the file-derived namespace (no override).
        classes = {str(s) for s, _, _ in g.triples((None, RDF.type, RDFS.Class))}
        assert any(c.endswith("/spec#A") for c in classes), f"got {classes}"
