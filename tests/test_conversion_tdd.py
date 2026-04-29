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

from openapi_to_rdf.property_uri import property_uri
from openapi_to_rdf.shacl_converter import OpenAPIToSHACLConverter

SH = Namespace("http://www.w3.org/ns/shacl#")
DASH = Namespace("http://datashapes.org/dash#")


def _prop(converter, class_name, property_name):
    """Build the class-scoped URI for a property in a converter's namespace."""
    return property_uri(converter.base_namespace, class_name, property_name)


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
        assert (_prop(self.c, "Person", "name"), RDF.type, RDF.Property) in self.c.rdf_graph

    def test_property_domain(self):
        assert (_prop(self.c, "Person", "name"), RDFS.domain, self.ns.Person) in self.c.rdf_graph

    def test_string_range(self):
        assert (_prop(self.c, "Person", "name"), RDFS.range, XSD.string) in self.c.rdf_graph

    def test_integer_range(self):
        assert (_prop(self.c, "Person", "age"), RDFS.range, XSD.integer) in self.c.rdf_graph

    def test_nodeshape_exists(self):
        assert len(list(self.c.shacl_graph.subjects(SH.targetClass, self.ns.Person))) == 1

    def test_required_has_mincount(self):
        name_uri = _prop(self.c, "Person", "name")
        for shape in self.c.shacl_graph.subjects(SH.path, name_uri):
            if (shape, SH.minCount, Literal(1)) in self.c.shacl_graph:
                return
        pytest.fail("Required property 'name' missing sh:minCount 1")

    def test_non_required_no_mincount(self):
        age_uri = _prop(self.c, "Person", "age")
        for shape in self.c.shacl_graph.subjects(SH.path, age_uri):
            assert (shape, SH.minCount, Literal(1)) not in self.c.shacl_graph

    def test_non_array_has_maxcount_1(self):
        name_uri = _prop(self.c, "Person", "name")
        for shape in self.c.shacl_graph.subjects(SH.path, name_uri):
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
        code_uri = _prop(self.c, "W", "code")
        for s in self.c.shacl_graph.subjects(SH.path, code_uri):
            assert (s, SH.datatype, XSD.string) in self.c.shacl_graph

    def test_pattern(self):
        code_uri = _prop(self.c, "W", "code")
        for s in self.c.shacl_graph.subjects(SH.path, code_uri):
            assert (s, SH.pattern, Literal("^[A-Z]{3}$")) in self.c.shacl_graph

    def test_minlength(self):
        code_uri = _prop(self.c, "W", "code")
        for s in self.c.shacl_graph.subjects(SH.path, code_uri):
            assert (s, SH.minLength, Literal(3)) in self.c.shacl_graph

    def test_maxlength(self):
        code_uri = _prop(self.c, "W", "code")
        for s in self.c.shacl_graph.subjects(SH.path, code_uri):
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
        lat_uri = _prop(self.c, "W", "lat")
        for s in self.c.shacl_graph.subjects(SH.path, lat_uri):
            assert (s, SH.datatype, XSD.float) in self.c.shacl_graph

    def test_min(self):
        lat_uri = _prop(self.c, "W", "lat")
        for s in self.c.shacl_graph.subjects(SH.path, lat_uri):
            assert (s, SH.minInclusive, Literal(-90)) in self.c.shacl_graph

    def test_max(self):
        lat_uri = _prop(self.c, "W", "lat")
        for s in self.c.shacl_graph.subjects(SH.path, lat_uri):
            assert (s, SH.maxInclusive, Literal(90)) in self.c.shacl_graph

    def test_integer_datatype(self):
        count_uri = _prop(self.c, "W", "count")
        for s in self.c.shacl_graph.subjects(SH.path, count_uri):
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
        status_uri = _prop(self.c, "W", "status")
        for s in self.c.shacl_graph.subjects(SH.path, status_uri):
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
        items_uri = _prop(self.c, "W", "items")
        for s in self.c.shacl_graph.subjects(SH.path, items_uri):
            assert (s, SH.maxCount, Literal(1)) not in self.c.shacl_graph

    def test_array_item_type(self):
        """Array items $ref should produce sh:class on the property shape."""
        items_uri = _prop(self.c, "W", "items")
        for s in self.c.shacl_graph.subjects(SH.path, items_uri):
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
        assert (_prop(self.c, "Person", "address"), RDFS.range, self.ns.Address) in self.c.rdf_graph

    def test_sh_class(self):
        address_uri = _prop(self.c, "Person", "address")
        for s in self.c.shacl_graph.subjects(SH.path, address_uri):
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
        assert (_prop(self.c, "Event", "when"), RDFS.range, XSD.dateTime) in self.c.rdf_graph

    def test_datetime_ref_shacl_datatype(self):
        when_uri = _prop(self.c, "Event", "when")
        for s in self.c.shacl_graph.subjects(SH.path, when_uri):
            assert (s, SH.datatype, XSD.dateTime) in self.c.shacl_graph
            assert (s, SH["class"], self.ns.DateTime) not in self.c.shacl_graph

    def test_string_ref_range_is_xsd(self):
        assert (_prop(self.c, "Event", "mcc"), RDFS.range, XSD.string) in self.c.rdf_graph

    def test_string_ref_shacl_datatype(self):
        mcc_uri = _prop(self.c, "Event", "mcc")
        for s in self.c.shacl_graph.subjects(SH.path, mcc_uri):
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
        assert (_prop(self.c, "W", "created"), RDFS.range, XSD.dateTime) in self.c.rdf_graph

    def test_time_range(self):
        assert (_prop(self.c, "W", "time"), RDFS.range, XSD.time) in self.c.rdf_graph

    def test_string_range(self):
        assert (_prop(self.c, "W", "plain"), RDFS.range, XSD.string) in self.c.rdf_graph

    def test_shacl_datetime(self):
        created_uri = _prop(self.c, "W", "created")
        for s in self.c.shacl_graph.subjects(SH.path, created_uri):
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

    def test_both_properties_scoped(self):
        """Both occurrences get distinct, class-scoped URIs."""
        a_name = _prop(self.c, "A", "name")
        b_name = _prop(self.c, "B", "name")
        assert a_name != b_name
        assert (a_name, RDFS.domain, self.ns.A) in self.c.rdf_graph
        assert (a_name, RDFS.range, XSD.string) in self.c.rdf_graph
        assert (b_name, RDFS.domain, self.ns.B) in self.c.rdf_graph
        assert (b_name, RDFS.range, XSD.integer) in self.c.rdf_graph

    def test_flat_uri_unused(self):
        """No triples are produced on the old flat <base>#name URI."""
        flat = self.ns.name
        assert not list(self.c.rdf_graph.predicates(flat))

    def test_single_domain_per_property(self):
        """Every property has exactly one rdfs:domain."""
        for p in {_prop(self.c, "A", "name"), _prop(self.c, "B", "name")}:
            domains = list(self.c.rdf_graph.objects(p, RDFS.domain))
            assert len(domains) == 1, f"{p} has domains {domains}"

    def test_single_range_per_property(self):
        """Every property has exactly one rdfs:range."""
        for p in {_prop(self.c, "A", "name"), _prop(self.c, "B", "name")}:
            ranges = list(self.c.rdf_graph.objects(p, RDFS.range))
            assert len(ranges) == 1, f"{p} has ranges {ranges}"


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
        ip_uri = _prop(self.c, "W", "ip")
        for s in self.c.shacl_graph.subjects(SH.path, ip_uri):
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


# ── 16. Enum on same property name across classes keeps per-class sh:in ──

ENUM_COLLISION_SPEC = {"components": {"schemas": {
    "DelayTolerance": {
        "type": "object",
        "properties": {"support": {"type": "string", "enum": ["SUPPORTED", "NOT_SUPPORTED"]}},
    },
    "UserMgmtOpen": {
        "type": "object",
        "properties": {"support": {"type": "string", "enum": ["YES", "NO"]}},
    },
}}}


class TestEnumCollisionAcrossClasses:
    """Regression test: when two classes declare a same-named string
    property with different enum value sets, each class's property shape
    must carry its own sh:in with its own enum list. Under the old flat
    URI scheme only one class kept the constraint; the other silently
    accepted any string.
    """

    @pytest.fixture(autouse=True)
    def setup(self):
        self.c = _convert(ENUM_COLLISION_SPEC)

    def _enum_values_for(self, cls, prop):
        """Return the set of stringified sh:in members for (cls, prop)."""
        path = _prop(self.c, cls, prop)
        values = set()
        for shape in self.c.shacl_graph.subjects(SH.path, path):
            for head in self.c.shacl_graph.objects(shape, SH["in"]):
                # sh:in is an RDF list — walk it.
                node = head
                while node and node != RDF.nil:
                    first = next(self.c.shacl_graph.objects(node, RDF.first), None)
                    if first is not None:
                        values.add(str(first))
                    node = next(self.c.shacl_graph.objects(node, RDF.rest), None)
        return values

    def test_delay_tolerance_keeps_its_enum(self):
        vals = self._enum_values_for("DelayTolerance", "support")
        assert vals == {"SUPPORTED", "NOT_SUPPORTED"}

    def test_user_mgmt_open_keeps_its_enum(self):
        vals = self._enum_values_for("UserMgmtOpen", "support")
        assert vals == {"YES", "NO"}

    def test_enums_are_not_merged(self):
        """The two shapes have disjoint enum sets — no intersection."""
        a = self._enum_values_for("DelayTolerance", "support")
        b = self._enum_values_for("UserMgmtOpen", "support")
        assert a & b == set(), f"Enums unexpectedly overlap: {a & b}"
