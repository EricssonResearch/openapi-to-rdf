"""
TDD test suite for OpenAPI to RDF/SHACL conversion correctness.
Each test uses a minimal inline OpenAPI snippet and asserts specific RDF/SHACL triples.
"""
import os
import tempfile
import pytest
import yaml
from rdflib import Literal, Namespace
from rdflib.namespace import RDF, RDFS, XSD

from openapi_to_rdf.shacl_converter import OpenAPIToSHACLConverter

SH = Namespace("http://www.w3.org/ns/shacl#")
DASH = Namespace("http://datashapes.org/dash#")


def _convert(spec):
    with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False, prefix="TS00000_Test") as f:
        yaml.dump(spec, f)
        path = f.name
    try:
        c = OpenAPIToSHACLConverter(path, output_dir=tempfile.mkdtemp())
        c.convert()
        return c
    finally:
        os.unlink(path)


# ── 1. Object schema ────────────────────────────────────────────────────

SIMPLE_OBJECT = {"components": {"schemas": {
    "Person": {
        "type": "object", "description": "A person",
        "properties": {"name": {"type": "string"}, "age": {"type": "integer"}},
        "required": ["name"],
    }
}}}


class TestObjectSchema:
    @pytest.fixture(autouse=True)
    def setup(self):
        self.c = _convert(SIMPLE_OBJECT)
        self.ns = self.c.main_prefix

    def test_class_created(self):
        assert (self.ns.Person, RDF.type, RDFS.Class) in self.c.rdf_graph

    def test_description_preserved(self):
        assert (self.ns.Person, RDFS.comment, Literal("A person")) in self.c.rdf_graph

    def test_property_declared(self):
        assert (self.ns.name, RDF.type, RDF.Property) in self.c.rdf_graph

    def test_property_domain(self):
        assert (self.ns.name, RDFS.domain, self.ns.Person) in self.c.rdf_graph

    def test_string_range(self):
        assert (self.ns.name, RDFS.range, XSD.string) in self.c.rdf_graph

    def test_integer_range(self):
        assert (self.ns.age, RDFS.range, XSD.integer) in self.c.rdf_graph

    def test_nodeshape_exists(self):
        assert len(list(self.c.shacl_graph.subjects(SH.targetClass, self.ns.Person))) == 1

    def test_required_has_mincount(self):
        for shape in self.c.shacl_graph.subjects(SH.path, self.ns.name):
            if (shape, SH.minCount, Literal(1)) in self.c.shacl_graph:
                return
        pytest.fail("Required property 'name' missing sh:minCount 1")

    def test_non_required_no_mincount(self):
        for shape in self.c.shacl_graph.subjects(SH.path, self.ns.age):
            assert (shape, SH.minCount, Literal(1)) not in self.c.shacl_graph

    def test_non_array_has_maxcount_1(self):
        for shape in self.c.shacl_graph.subjects(SH.path, self.ns.name):
            if (shape, SH.maxCount, Literal(1)) in self.c.shacl_graph:
                return
        pytest.fail("Non-array property missing sh:maxCount 1")


# ── 2. String constraints ───────────────────────────────────────────────

STRING_SPEC = {"components": {"schemas": {"W": {"type": "object", "properties": {
    "code": {"type": "string", "pattern": "^[A-Z]{3}$", "minLength": 3, "maxLength": 3}
}}}}}


class TestStringConstraints:
    @pytest.fixture(autouse=True)
    def setup(self):
        self.c = _convert(STRING_SPEC)
        self.ns = self.c.main_prefix

    def test_datatype(self):
        for s in self.c.shacl_graph.subjects(SH.path, self.ns.code):
            assert (s, SH.datatype, XSD.string) in self.c.shacl_graph

    def test_pattern(self):
        for s in self.c.shacl_graph.subjects(SH.path, self.ns.code):
            assert (s, SH.pattern, Literal("^[A-Z]{3}$")) in self.c.shacl_graph

    def test_minlength(self):
        for s in self.c.shacl_graph.subjects(SH.path, self.ns.code):
            assert (s, SH.minLength, Literal(3)) in self.c.shacl_graph

    def test_maxlength(self):
        for s in self.c.shacl_graph.subjects(SH.path, self.ns.code):
            assert (s, SH.maxLength, Literal(3)) in self.c.shacl_graph


# ── 3. Numeric constraints ──────────────────────────────────────────────

NUM_SPEC = {"components": {"schemas": {"W": {"type": "object", "properties": {
    "lat": {"type": "number", "format": "float", "minimum": -90, "maximum": 90},
    "count": {"type": "integer"},
}}}}}


class TestNumericConstraints:
    @pytest.fixture(autouse=True)
    def setup(self):
        self.c = _convert(NUM_SPEC)
        self.ns = self.c.main_prefix

    def test_float_datatype(self):
        for s in self.c.shacl_graph.subjects(SH.path, self.ns.lat):
            assert (s, SH.datatype, XSD.float) in self.c.shacl_graph

    def test_min(self):
        for s in self.c.shacl_graph.subjects(SH.path, self.ns.lat):
            assert (s, SH.minInclusive, Literal(-90)) in self.c.shacl_graph

    def test_max(self):
        for s in self.c.shacl_graph.subjects(SH.path, self.ns.lat):
            assert (s, SH.maxInclusive, Literal(90)) in self.c.shacl_graph

    def test_integer_datatype(self):
        for s in self.c.shacl_graph.subjects(SH.path, self.ns.count):
            assert (s, SH.datatype, XSD.integer) in self.c.shacl_graph


# ── 4. Enum ─────────────────────────────────────────────────────────────

ENUM_SPEC = {"components": {"schemas": {"W": {"type": "object", "properties": {
    "status": {"type": "string", "enum": ["ACTIVE", "INACTIVE"]}
}}}}}


class TestEnum:
    @pytest.fixture(autouse=True)
    def setup(self):
        self.c = _convert(ENUM_SPEC)
        self.ns = self.c.main_prefix

    def test_has_sh_in(self):
        for s in self.c.shacl_graph.subjects(SH.path, self.ns.status):
            assert len(list(self.c.shacl_graph.objects(s, SH["in"]))) > 0


# ── 5. Array property ───────────────────────────────────────────────────

ARRAY_SPEC = {"components": {"schemas": {
    "Item": {"type": "object", "properties": {"id": {"type": "string"}}},
    "W": {"type": "object", "properties": {
        "items": {"type": "array", "items": {"$ref": "#/components/schemas/Item"}, "minItems": 1}
    }},
}}}


class TestArray:
    @pytest.fixture(autouse=True)
    def setup(self):
        self.c = _convert(ARRAY_SPEC)
        self.ns = self.c.main_prefix

    def test_array_no_maxcount_1(self):
        """Array properties should NOT have sh:maxCount 1."""
        for s in self.c.shacl_graph.subjects(SH.path, self.ns.items):
            assert (s, SH.maxCount, Literal(1)) not in self.c.shacl_graph

    def test_array_item_type(self):
        """Array items $ref should produce sh:class on the property shape."""
        for s in self.c.shacl_graph.subjects(SH.path, self.ns.items):
            assert (s, SH["class"], self.ns.Item) in self.c.shacl_graph


# ── 6. Internal $ref to object type ─────────────────────────────────────

REF_SPEC = {"components": {"schemas": {
    "Address": {"type": "object", "properties": {"city": {"type": "string"}}},
    "Person": {"type": "object", "properties": {
        "address": {"$ref": "#/components/schemas/Address"}
    }},
}}}


class TestInternalRefObject:
    @pytest.fixture(autouse=True)
    def setup(self):
        self.c = _convert(REF_SPEC)
        self.ns = self.c.main_prefix

    def test_range(self):
        assert (self.ns.address, RDFS.range, self.ns.Address) in self.c.rdf_graph

    def test_sh_class(self):
        for s in self.c.shacl_graph.subjects(SH.path, self.ns.address):
            assert (s, SH["class"], self.ns.Address) in self.c.shacl_graph


# ── 7. Internal $ref to primitive type (was broken: used sh:class instead of sh:datatype) ──

REF_PRIMITIVE_SPEC = {"components": {"schemas": {
    "DateTime": {"type": "string", "format": "date-time", "description": "A datetime"},
    "Mcc": {"type": "string", "pattern": "^[0-9]{3}$"},
    "Event": {"type": "object", "properties": {
        "when": {"$ref": "#/components/schemas/DateTime"},
        "mcc": {"$ref": "#/components/schemas/Mcc"},
    }},
}}}


class TestInternalRefPrimitive:
    """$ref to a primitive schema should produce sh:datatype, not sh:class."""
    @pytest.fixture(autouse=True)
    def setup(self):
        self.c = _convert(REF_PRIMITIVE_SPEC)
        self.ns = self.c.main_prefix

    def test_datetime_ref_range_is_xsd(self):
        assert (self.ns.when, RDFS.range, XSD.dateTime) in self.c.rdf_graph

    def test_datetime_ref_shacl_datatype(self):
        for s in self.c.shacl_graph.subjects(SH.path, self.ns.when):
            assert (s, SH.datatype, XSD.dateTime) in self.c.shacl_graph
            assert (s, SH["class"], self.ns.DateTime) not in self.c.shacl_graph

    def test_string_ref_range_is_xsd(self):
        assert (self.ns.mcc, RDFS.range, XSD.string) in self.c.rdf_graph

    def test_string_ref_shacl_datatype(self):
        for s in self.c.shacl_graph.subjects(SH.path, self.ns.mcc):
            assert (s, SH.datatype, XSD.string) in self.c.shacl_graph


# ── 8. oneOf → sh:xone ──────────────────────────────────────────────────

ONEOF_SPEC = {"components": {"schemas": {
    "A": {"type": "object", "properties": {"a": {"type": "string"}}},
    "B": {"type": "object", "properties": {"b": {"type": "integer"}}},
    "Union": {"oneOf": [
        {"$ref": "#/components/schemas/A"},
        {"$ref": "#/components/schemas/B"},
    ]},
}}}


class TestOneOf:
    @pytest.fixture(autouse=True)
    def setup(self):
        self.c = _convert(ONEOF_SPEC)
        self.ns = self.c.main_prefix

    def test_class(self):
        assert (self.ns.Union, RDF.type, RDFS.Class) in self.c.rdf_graph

    def test_xone(self):
        for t in self.c.shacl_graph.subjects(SH.targetClass, self.ns.Union):
            if list(self.c.shacl_graph.objects(t, SH.xone)):
                return
        pytest.fail("Missing sh:xone for oneOf")

    def test_comment_says_oneOf_not_xone(self):
        """The RDF comment should reference the OpenAPI operator name, not the SHACL one."""
        comments = [str(o) for o in self.c.rdf_graph.objects(self.ns.Union, RDFS.comment)]
        assert any("OpenAPI oneOf" in c for c in comments), f"Comments: {comments}"
        assert not any("OpenAPI xone" in c for c in comments), f"Comments: {comments}"


# ── 9. Format → XSD mapping ─────────────────────────────────────────────

FMT_SPEC = {"components": {"schemas": {"W": {"type": "object", "properties": {
    "created": {"type": "string", "format": "date-time"},
    "time": {"type": "string", "format": "full-time"},
    "plain": {"type": "string"},
}}}}}


class TestFormatMapping:
    @pytest.fixture(autouse=True)
    def setup(self):
        self.c = _convert(FMT_SPEC)
        self.ns = self.c.main_prefix

    def test_datetime_range(self):
        assert (self.ns.created, RDFS.range, XSD.dateTime) in self.c.rdf_graph

    def test_time_range(self):
        assert (self.ns.time, RDFS.range, XSD.time) in self.c.rdf_graph

    def test_string_range(self):
        assert (self.ns.plain, RDFS.range, XSD.string) in self.c.rdf_graph

    def test_shacl_datetime(self):
        for s in self.c.shacl_graph.subjects(SH.path, self.ns.created):
            assert (s, SH.datatype, XSD.dateTime) in self.c.shacl_graph


# ── 10. Primitive top-level schemas → rdfs:Datatype (was rdfs:Class) ────

PRIM_SPEC = {"components": {"schemas": {
    "MyFloat": {"type": "number", "format": "float", "description": "A float"},
    "MyString": {"type": "string", "description": "A string"},
}}}


class TestPrimitiveSchemas:
    @pytest.fixture(autouse=True)
    def setup(self):
        self.c = _convert(PRIM_SPEC)
        self.ns = self.c.main_prefix

    def test_float_is_datatype(self):
        assert (self.ns.MyFloat, RDF.type, RDFS.Datatype) in self.c.rdf_graph

    def test_string_is_datatype(self):
        assert (self.ns.MyString, RDF.type, RDFS.Datatype) in self.c.rdf_graph

    def test_float_has_shacl_datatype(self):
        for t in self.c.shacl_graph.subjects(SH.targetClass, self.ns.MyFloat):
            assert (t, SH.datatype, XSD.float) in self.c.shacl_graph

    def test_string_has_shacl_datatype(self):
        for t in self.c.shacl_graph.subjects(SH.targetClass, self.ns.MyString):
            assert (t, SH.datatype, XSD.string) in self.c.shacl_graph

    def test_description_preserved(self):
        assert (self.ns.MyFloat, RDFS.comment, Literal("A float")) in self.c.rdf_graph


# ── 11. Top-level string with constraints gets SHACL ────────────────────

TOPLEVEL_STRING_SPEC = {"components": {"schemas": {
    "Fqdn": {
        "type": "string",
        "pattern": "^[a-z]+$",
        "minLength": 4,
        "maxLength": 253,
    },
}}}


class TestTopLevelStringConstraints:
    @pytest.fixture(autouse=True)
    def setup(self):
        self.c = _convert(TOPLEVEL_STRING_SPEC)
        self.ns = self.c.main_prefix

    def test_shacl_datatype(self):
        for t in self.c.shacl_graph.subjects(SH.targetClass, self.ns.Fqdn):
            assert (t, SH.datatype, XSD.string) in self.c.shacl_graph

    def test_shacl_pattern(self):
        for t in self.c.shacl_graph.subjects(SH.targetClass, self.ns.Fqdn):
            assert (t, SH.pattern, Literal("^[a-z]+$")) in self.c.shacl_graph

    def test_shacl_minlength(self):
        for t in self.c.shacl_graph.subjects(SH.targetClass, self.ns.Fqdn):
            assert (t, SH.minLength, Literal(4)) in self.c.shacl_graph


# ── 12. Top-level numeric with constraints gets SHACL ───────────────────

TOPLEVEL_NUM_SPEC = {"components": {"schemas": {
    "Latitude": {"type": "number", "format": "float", "minimum": -90, "maximum": 90},
}}}


class TestTopLevelNumericConstraints:
    @pytest.fixture(autouse=True)
    def setup(self):
        self.c = _convert(TOPLEVEL_NUM_SPEC)
        self.ns = self.c.main_prefix

    def test_shacl_datatype(self):
        for t in self.c.shacl_graph.subjects(SH.targetClass, self.ns.Latitude):
            assert (t, SH.datatype, XSD.float) in self.c.shacl_graph

    def test_shacl_min(self):
        for t in self.c.shacl_graph.subjects(SH.targetClass, self.ns.Latitude):
            assert (t, SH.minInclusive, Literal(-90)) in self.c.shacl_graph

    def test_shacl_max(self):
        for t in self.c.shacl_graph.subjects(SH.targetClass, self.ns.Latitude):
            assert (t, SH.maxInclusive, Literal(90)) in self.c.shacl_graph


# ── 13. Property name collision → scoped URIs ───────────────────────────

COLLISION_SPEC = {"components": {"schemas": {
    "A": {"type": "object", "properties": {"name": {"type": "string"}}},
    "B": {"type": "object", "properties": {"name": {"type": "integer"}}},
}}}


class TestPropertyCollision:
    @pytest.fixture(autouse=True)
    def setup(self):
        self.c = _convert(COLLISION_SPEC)
        self.ns = self.c.main_prefix

    def test_first_property_unscoped(self):
        """First occurrence keeps the base URI."""
        assert (self.ns.name, RDFS.domain, self.ns.A) in self.c.rdf_graph
        assert (self.ns.name, RDFS.range, XSD.string) in self.c.rdf_graph

    def test_second_property_scoped(self):
        """Second occurrence with different range gets per-class namespace URI."""
        # Per-class namespace: base_ns + '/B#name'
        base = self.c.base_namespace.rstrip('#')
        scoped_ns = Namespace(base + '/B#')
        assert (scoped_ns.name, RDFS.domain, self.ns.B) in self.c.rdf_graph
        assert (scoped_ns.name, RDFS.range, XSD.integer) in self.c.rdf_graph

    def test_no_conflicting_ranges_on_base(self):
        """Base URI should NOT have xsd:integer range."""
        ranges = set(self.c.rdf_graph.objects(self.ns.name, RDFS.range))
        assert XSD.integer not in ranges


# ── 14. allOf with pattern-only items ────────────────────────────────────

ALLOF_SPEC = {"components": {"schemas": {"W": {"type": "object", "properties": {
    "ip": {"type": "string", "allOf": [{"pattern": "^[0-9]+$"}, {"pattern": "^[0-9]{1,3}$"}]}
}}}}}


class TestAllOfPatterns:
    @pytest.fixture(autouse=True)
    def setup(self):
        self.c = _convert(ALLOF_SPEC)
        self.ns = self.c.main_prefix

    def test_both_patterns_preserved(self):
        """Both patterns from allOf should appear as sh:pattern constraints."""
        patterns = set()
        for s in self.c.shacl_graph.subjects(SH.path, self.ns.ip):
            for o in self.c.shacl_graph.objects(s, SH.pattern):
                patterns.add(str(o))
        assert "^[0-9]+$" in patterns
        assert "^[0-9]{1,3}$" in patterns


# ── 15. Top-level enum string gets sh:in ─────────────────────────────────

TOPLEVEL_ENUM_SPEC = {"components": {"schemas": {
    "Status": {"type": "string", "enum": ["UP", "DOWN"]},
}}}


class TestTopLevelEnum:
    @pytest.fixture(autouse=True)
    def setup(self):
        self.c = _convert(TOPLEVEL_ENUM_SPEC)
        self.ns = self.c.main_prefix

    def test_is_datatype(self):
        assert (self.ns.Status, RDF.type, RDFS.Datatype) in self.c.rdf_graph

    def test_shacl_has_in(self):
        for t in self.c.shacl_graph.subjects(SH.targetClass, self.ns.Status):
            assert len(list(self.c.shacl_graph.objects(t, SH["in"]))) > 0
