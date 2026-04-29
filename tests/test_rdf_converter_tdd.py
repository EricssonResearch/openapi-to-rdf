"""TDD suite for the OWL/RDF converter: class-scoped property URIs.

Mirrors the property-collision test in tests/test_conversion_tdd.py but
for the OWL path in openapi_to_rdf.rdf_converter.
"""
import os
import tempfile

import pytest
import yaml
from rdflib import Literal, Namespace
from rdflib.namespace import OWL, RDF, RDFS, XSD

from openapi_to_rdf.property_uri import property_uri
from openapi_to_rdf.rdf_converter import OpenAPIToRDFConverter


def _convert(spec, base_ns="http://ericsson.com/models/3gpp/rdf"):
    """Convert an inline OpenAPI spec with the OWL converter."""
    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".yaml", delete=False, prefix="TS00000_Test"
    ) as f:
        yaml.dump(spec, f)
        path = f.name
    try:
        c = OpenAPIToRDFConverter(path, base_ns, output_dir=tempfile.mkdtemp())
        c.convert()
        return c
    finally:
        os.unlink(path)


def _prop(converter, class_name, property_name):
    return property_uri(str(converter.main_prefix), class_name, property_name)


COLLISION_SPEC = {
    "components": {
        "schemas": {
            "A": {
                "type": "object",
                "properties": {"name": {"type": "string"}},
            },
            "B": {
                "type": "object",
                "properties": {"name": {"type": "integer"}},
            },
        }
    }
}


class TestOwlPropertyCollision:
    """Same property name on two classes → two distinct URIs, each with
    a single rdfs:domain and a single rdfs:range."""

    @pytest.fixture(autouse=True)
    def setup(self):
        self.c = _convert(COLLISION_SPEC)
        self.ns = self.c.main_prefix

    def test_both_properties_scoped(self):
        a_name = _prop(self.c, "A", "name")
        b_name = _prop(self.c, "B", "name")
        assert a_name != b_name
        assert (a_name, RDFS.domain, self.ns.A) in self.c.graph
        assert (a_name, RDFS.range, XSD.string) in self.c.graph
        assert (b_name, RDFS.domain, self.ns.B) in self.c.graph
        assert (b_name, RDFS.range, XSD.integer) in self.c.graph

    def test_flat_uri_unused(self):
        """No triples on the old flat <base>#name URI."""
        flat = self.ns.name
        assert not list(self.c.graph.predicates(flat))

    def test_single_domain_per_property(self):
        for p in {_prop(self.c, "A", "name"), _prop(self.c, "B", "name")}:
            domains = list(self.c.graph.objects(p, RDFS.domain))
            assert len(domains) == 1, f"{p} has domains {domains}"

    def test_single_range_per_property(self):
        for p in {_prop(self.c, "A", "name"), _prop(self.c, "B", "name")}:
            ranges = list(self.c.graph.objects(p, RDFS.range))
            assert len(ranges) == 1, f"{p} has ranges {ranges}"


NON_COLLISION_SPEC = {
    "components": {
        "schemas": {
            "Person": {
                "type": "object",
                "description": "A person",
                "properties": {
                    "name": {"type": "string"},
                    "age": {"type": "integer"},
                },
                "required": ["name"],
            }
        }
    }
}


class TestOwlBasicShape:
    """Sanity checks on the OWL output for a single object schema."""

    @pytest.fixture(autouse=True)
    def setup(self):
        self.c = _convert(NON_COLLISION_SPEC)
        self.ns = self.c.main_prefix

    def test_class_declared(self):
        assert (self.ns.Person, RDF.type, OWL.Class) in self.c.graph

    def test_property_under_class_namespace(self):
        uri = _prop(self.c, "Person", "name")
        assert str(uri).endswith("/Person#name")
        assert (uri, RDF.type, OWL.FunctionalProperty) in self.c.graph
        assert (uri, RDFS.domain, self.ns.Person) in self.c.graph
        assert (uri, RDFS.range, XSD.string) in self.c.graph

    def test_non_required_not_functional(self):
        uri = _prop(self.c, "Person", "age")
        assert (uri, RDF.type, OWL.FunctionalProperty) not in self.c.graph
        assert (uri, RDF.type, OWL.DatatypeProperty) in self.c.graph
