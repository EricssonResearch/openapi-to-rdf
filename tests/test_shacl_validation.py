"""
SHACL validation equivalence tests.

For each test case:
1. Define a JSON instance (valid or invalid per OpenAPI schema)
2. Validate with jsonschema (ground truth)
3. Convert to RDF instance data
4. Validate with pyshacl against generated SHACL
5. Assert: jsonschema agrees with pyshacl

If they disagree, the converter has a bug.
"""
import os
import tempfile
import pytest
import yaml
from jsonschema import validate as js_validate, ValidationError as JSError
from pyshacl import validate as shacl_validate
from rdflib import Graph, Literal, Namespace, BNode
from rdflib.namespace import RDF, RDFS, XSD

from openapi_to_rdf.property_uri import property_uri as _class_scoped_uri
from openapi_to_rdf.shacl_converter import OpenAPIToSHACLConverter

SH = Namespace("http://www.w3.org/ns/shacl#")


# ── Helpers ──────────────────────────────────────────────────────────────

def _build_converter(spec):
    """Convert an OpenAPI spec dict, return the converter."""
    with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False, prefix="TS00000_Test") as f:
        yaml.dump(spec, f)
        path = f.name
    try:
        c = OpenAPIToSHACLConverter(path, output_dir=tempfile.mkdtemp())
        c.convert()
        return c
    finally:
        os.unlink(path)


def _jsonschema_valid(schema_def, instance, full_schemas=None):
    """Check if instance is valid per jsonschema. Returns True/False."""
    # Build a JSON Schema with local $ref resolution
    if full_schemas:
        schema = {**schema_def, "components": {"schemas": full_schemas}}
        # jsonschema needs a resolver for $ref
        import jsonschema
        # Inline-resolve refs for simplicity
        resolved = _resolve_refs(schema_def, full_schemas)
    else:
        resolved = schema_def

    try:
        js_validate(instance=instance, schema=resolved)
        return True
    except JSError:
        return False


def _resolve_refs(schema, all_schemas):
    """Recursively resolve $ref in a schema dict (internal refs only)."""
    if isinstance(schema, dict):
        if "$ref" in schema:
            ref = schema["$ref"]
            if ref.startswith("#/components/schemas/"):
                name = ref.split("/")[-1]
                if name in all_schemas:
                    return _resolve_refs(all_schemas[name], all_schemas)
            return schema
        return {k: _resolve_refs(v, all_schemas) for k, v in schema.items()}
    elif isinstance(schema, list):
        return [_resolve_refs(item, all_schemas) for item in schema]
    return schema


XSD_MAP = {
    str: XSD.string,
    int: XSD.integer,
    float: XSD.double,
    bool: XSD.boolean,
}


def _instance_to_rdf(instance, class_uri, ns, converter, owner_class_name):
    """Convert a JSON object instance to an RDF data graph.

    ``owner_class_name`` is the OpenAPI schema name of the outer class, so
    we can mint class-scoped property URIs that match the SHACL
    ``sh:path`` values produced by the converter.

    Uses the SHACL shapes to determine correct XSD datatypes for literals,
    so that pyshacl datatype checks work correctly.
    """
    g = Graph()
    for prefix, namespace in converter.shacl_graph.namespaces():
        g.bind(prefix, namespace)

    subject = ns["test_instance_1"]
    g.add((subject, RDF.type, class_uri))

    if isinstance(instance, dict):
        for key, value in instance.items():
            prop_uri = _class_scoped_uri(converter.base_namespace, owner_class_name, key)
            if isinstance(value, dict):
                # Nested object: we don't know its OpenAPI class here, so
                # the converter's matching shape would be on the nested
                # type. For now, keep the nested properties under the
                # outer owner's namespace — deep-nested inline validation
                # is out of scope for these test cases.
                child = BNode()
                g.add((subject, prop_uri, child))
                for k2, v2 in value.items():
                    nested_prop = _class_scoped_uri(converter.base_namespace, owner_class_name, k2)
                    g.add((child, nested_prop, Literal(v2)))
            elif isinstance(value, list):
                for item in value:
                    g.add((subject, prop_uri, _typed_literal(item, prop_uri, converter)))
            else:
                g.add((subject, prop_uri, _typed_literal(value, prop_uri, converter)))
    return g


def _typed_literal(value, prop_uri, converter):
    """Create a Literal with the XSD datatype expected by the SHACL shape."""
    SH_DT = Namespace("http://www.w3.org/ns/shacl#")
    # Find the PropertyShape for this path and read its sh:datatype
    for shape in converter.shacl_graph.subjects(SH_DT.path, prop_uri):
        for dt in converter.shacl_graph.objects(shape, SH_DT.datatype):
            return Literal(value, datatype=dt)
    # Fallback: let rdflib infer
    return Literal(value)


def _shacl_conforms(data_graph, shacl_graph):
    """Run pyshacl validation, return True if conforms."""
    conforms, _, _ = shacl_validate(data_graph, shacl_graph=shacl_graph)
    return conforms


# ── Test Specs ───────────────────────────────────────────────────────────

SCHEMAS = {
    "Mcc": {"type": "string", "pattern": "^[0-9]{3}$"},
    "Mnc": {"type": "string", "pattern": "^[0-9]{2,3}$"},
    "Latitude": {"type": "number", "format": "float", "minimum": -90, "maximum": 90},
    "Longitude": {"type": "number", "format": "float", "minimum": -180, "maximum": 180},
    "PlmnId": {
        "type": "object",
        "properties": {
            "mcc": {"$ref": "#/components/schemas/Mcc"},
            "mnc": {"$ref": "#/components/schemas/Mnc"},
        },
    },
    "GeoCoordinate": {
        "type": "object",
        "properties": {
            "latitude": {"$ref": "#/components/schemas/Latitude"},
            "longitude": {"$ref": "#/components/schemas/Longitude"},
            "altitude": {"type": "number", "format": "float"},
        },
    },
    "AdministrativeState": {"type": "string", "enum": ["LOCKED", "UNLOCKED"]},
    "Fqdn": {
        "type": "string",
        "pattern": r"^([0-9A-Za-z]([-0-9A-Za-z]{0,61}[0-9A-Za-z])?\.)+[A-Za-z]{2,63}\.?$",
        "minLength": 4,
        "maxLength": 253,
    },
    "NotificationHeader": {
        "type": "object",
        "properties": {
            "href": {"type": "string"},
            "notificationId": {"type": "integer"},
            "eventTime": {"type": "string", "format": "date-time"},
            "systemDN": {"type": "string"},
        },
        "required": ["href", "notificationId", "eventTime", "systemDN"],
    },
}

OPENAPI_SPEC = {"components": {"schemas": SCHEMAS}}


@pytest.fixture(scope="module")
def converter():
    return _build_converter(OPENAPI_SPEC)


# ── Test Cases: (schema_name, instance, expected_valid) ──────────────────

OBJECT_CASES = [
    # PlmnId
    ("PlmnId", {"mcc": "310", "mnc": "14"}, True),
    ("PlmnId", {"mcc": "abc", "mnc": "14"}, False),       # mcc fails pattern
    ("PlmnId", {"mcc": "1234", "mnc": "14"}, False),      # mcc too long
    ("PlmnId", {"mcc": "310"}, True),                      # mnc optional
    # GeoCoordinate
    ("GeoCoordinate", {"latitude": 45.0, "longitude": -73.5, "altitude": 100.0}, True),
    ("GeoCoordinate", {"latitude": 100.0, "longitude": 0.0}, False),   # lat > 90
    ("GeoCoordinate", {"latitude": 0.0, "longitude": -200.0}, False),  # lon < -180
    # NotificationHeader — required fields
    ("NotificationHeader", {
        "href": "https://example.com",
        "notificationId": 42,
        "eventTime": "2024-01-01T00:00:00Z",
        "systemDN": "SubNetwork=1",
    }, True),
    ("NotificationHeader", {
        "href": "https://example.com",
        # missing notificationId — required
        "eventTime": "2024-01-01T00:00:00Z",
        "systemDN": "SubNetwork=1",
    }, False),
]


class TestObjectInstanceValidation:
    """Test that SHACL validation of RDF instances matches jsonschema for object types."""

    @pytest.fixture(autouse=True)
    def setup(self, converter):
        self.c = converter
        self.ns = converter.main_prefix

    @pytest.mark.parametrize("schema_name,instance,expected_valid", OBJECT_CASES,
                             ids=[f"{c[0]}-{'valid' if c[2] else 'invalid'}-{i}"
                                  for i, c in enumerate(OBJECT_CASES)])
    def test_object_validation_equivalence(self, schema_name, instance, expected_valid):
        # 1. jsonschema ground truth
        js_valid = _jsonschema_valid(SCHEMAS[schema_name], instance, SCHEMAS)
        assert js_valid == expected_valid, (
            f"jsonschema disagrees with expected: got {js_valid} for {schema_name}"
        )

        # 2. Convert instance to RDF
        class_uri = self.ns[schema_name]
        data_graph = _instance_to_rdf(instance, class_uri, self.ns, self.c, schema_name)

        # 3. SHACL validation
        shacl_valid = _shacl_conforms(data_graph, self.c.shacl_graph)

        # 4. They must agree
        assert shacl_valid == expected_valid, (
            f"SHACL {'conforms' if shacl_valid else 'rejects'} but jsonschema "
            f"{'accepts' if js_valid else 'rejects'} for {schema_name} with {instance}"
        )


# ── Primitive / enum cases validated at the property level ───────────────

PRIMITIVE_PROPERTY_CASES = [
    # (schema_name, wrapper_prop, value, expected_valid)
    # Enum
    ("AdministrativeState", "state", "LOCKED", True),
    ("AdministrativeState", "state", "INVALID_VALUE", False),
    # String pattern
    ("Mcc", "mcc", "310", True),
    ("Mcc", "mcc", "abc", False),
    ("Mcc", "mcc", "1234", False),
    # Numeric range
    ("Latitude", "lat", 45.0, True),
    ("Latitude", "lat", -90.0, True),
    ("Latitude", "lat", 91.0, False),
]

# Build wrapper schemas that use these as properties
WRAPPER_SCHEMAS = {
    **SCHEMAS,
    "StateWrapper": {
        "type": "object",
        "properties": {"state": {"$ref": "#/components/schemas/AdministrativeState"}},
    },
    "MccWrapper": {
        "type": "object",
        "properties": {"mcc": {"$ref": "#/components/schemas/Mcc"}},
    },
    "LatWrapper": {
        "type": "object",
        "properties": {"lat": {"$ref": "#/components/schemas/Latitude"}},
    },
}
WRAPPER_SPEC = {"components": {"schemas": WRAPPER_SCHEMAS}}


@pytest.fixture(scope="module")
def wrapper_converter():
    return _build_converter(WRAPPER_SPEC)


class TestPrimitivePropertyValidation:
    """Test SHACL validates primitive property values the same as jsonschema."""

    @pytest.fixture(autouse=True)
    def setup(self, wrapper_converter):
        self.c = wrapper_converter
        self.ns = wrapper_converter.main_prefix

    @pytest.mark.parametrize("schema_name,prop,value,expected_valid", PRIMITIVE_PROPERTY_CASES,
                             ids=[f"{c[0]}-{c[2]}-{'valid' if c[3] else 'invalid'}"
                                  for c in PRIMITIVE_PROPERTY_CASES])
    def test_primitive_validation_equivalence(self, schema_name, prop, value, expected_valid):
        # Map to wrapper schema
        wrapper_map = {
            "AdministrativeState": "StateWrapper",
            "Mcc": "MccWrapper",
            "Latitude": "LatWrapper",
        }
        wrapper_name = wrapper_map[schema_name]
        instance = {prop: value}

        # 1. jsonschema
        js_valid = _jsonschema_valid(WRAPPER_SCHEMAS[wrapper_name], instance, WRAPPER_SCHEMAS)
        assert js_valid == expected_valid

        # 2. RDF instance
        class_uri = self.ns[wrapper_name]
        data_graph = _instance_to_rdf(instance, class_uri, self.ns, self.c, wrapper_name)

        # 3. SHACL
        shacl_valid = _shacl_conforms(data_graph, self.c.shacl_graph)

        # 4. Agreement
        assert shacl_valid == expected_valid, (
            f"SHACL {'conforms' if shacl_valid else 'rejects'} but jsonschema "
            f"{'accepts' if js_valid else 'rejects'} for {schema_name}={value}"
        )
