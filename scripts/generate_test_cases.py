#!/usr/bin/env python3
"""
Generate SHACL test cases from OpenAPI YAML schemas (source of truth).

1. Read OpenAPI schemas
2. Generate valid/invalid JSON instances, verified by jsonschema (oracle)
3. Run the converter to get the RDF vocabulary (property URIs may be scoped)
4. Convert instances to RDF using the converter's vocabulary
5. Write TTL test files

The converter is used only to learn the RDF vocabulary (property URIs).
The jsonschema oracle determines valid/invalid — never the SHACL output.
"""
import re
import tempfile
from pathlib import Path

from scripts._namespace import document_namespace
import yaml
from jsonschema import validate as js_validate, ValidationError
from rdflib import Graph, Literal
from rdflib.namespace import RDF, RDFS, XSD

from openapi_to_rdf.provenance import CONTACT, PROJECT_URL


# ── Schema helpers (source of truth: OpenAPI YAML) ──────────────────────

def resolve_refs(schema, all_schemas, depth=0):
    if depth > 10:
        return schema
    if isinstance(schema, dict):
        if "$ref" in schema:
            ref = schema["$ref"]
            if ref.startswith("#/components/schemas/"):
                name = ref.split("/")[-1]
                if name in all_schemas:
                    return resolve_refs(all_schemas[name], all_schemas, depth + 1)
            return schema
        return {k: resolve_refs(v, all_schemas, depth) for k, v in schema.items()}
    if isinstance(schema, list):
        return [resolve_refs(i, all_schemas, depth) for i in schema]
    return schema


def has_external_refs(schema):
    if isinstance(schema, dict):
        if "$ref" in schema and not schema["$ref"].startswith("#/"):
            return True
        return any(has_external_refs(v) for v in schema.values())
    if isinstance(schema, list):
        return any(has_external_refs(i) for i in schema)
    return False


def js_valid(schema, instance, all_schemas):
    resolved = resolve_refs(schema, all_schemas)
    if has_external_refs(resolved):
        return True
    try:
        js_validate(instance=instance, schema=resolved)
        return True
    except ValidationError:
        return False


def gen_valid(schema, all_schemas, depth=0):
    if depth > 5:
        return None
    schema = resolve_refs(schema, all_schemas, depth)
    t = schema.get("type")
    if t == "string":
        if "enum" in schema: return schema["enum"][0]
        if "pattern" in schema: return _gen_pattern(schema)
        fmt = schema.get("format")
        if fmt == "date-time": return "2024-01-15T10:30:00Z"
        if fmt == "full-time": return "10:30:00"
        if fmt == "date-month": return "--01"
        if fmt == "date-mday": return "--01-15"
        return "test-value"
    if t == "integer":
        lo, hi = schema.get("minimum", 0), schema.get("maximum", 100)
        return int((lo + hi) // 2)
    if t == "number":
        lo, hi = schema.get("minimum", 0.0), schema.get("maximum", 100.0)
        return round((lo + hi) / 2, 2)
    if t == "boolean": return True
    if t == "object":
        obj = {}
        for pname, pdef in schema.get("properties", {}).items():
            val = gen_valid(pdef, all_schemas, depth + 1)
            if val is not None: obj[pname] = val
        return obj or None
    if t == "array":
        items = schema.get("items", {})
        val = gen_valid(items, all_schemas, depth + 1)
        return [val] * max(schema.get("minItems", 1), 1) if val is not None else None
    for key in ("oneOf", "anyOf"):
        if key in schema:
            for opt in schema[key]:
                val = gen_valid(opt, all_schemas, depth + 1)
                if val is not None: return val
    if "allOf" in schema:
        merged = {}
        for sub in schema["allOf"]:
            val = gen_valid(resolve_refs(sub, all_schemas, depth), all_schemas, depth + 1)
            if isinstance(val, dict): merged.update(val)
        return merged or None
    return None


def _gen_pattern(schema):
    pattern = schema["pattern"]
    m = re.match(r'^\^?\[([A-Za-z0-9\-]+)\]\{(\d+)\}\$?$', pattern)
    if m:
        pool = m.group(1).replace('-', '')
        return ''.join(pool[i % len(pool)] for i in range(int(m.group(2))))
    m = re.match(r'^\^?\[([A-Za-z0-9\-]+)\]\{(\d+),\d+\}\$?$', pattern)
    if m:
        pool = m.group(1).replace('-', '')
        return ''.join(pool[i % len(pool)] for i in range(int(m.group(2))))
    return "a" * schema.get("minLength", 4)


def gen_invalids(pname, schema, all_schemas):
    schema = resolve_refs(schema, all_schemas)
    t = schema.get("type")
    out = []
    if t == "string":
        if "pattern" in schema: out.append(("bad_pattern", "!!!INVALID!!!"))
        if "enum" in schema: out.append(("bad_enum", "NOT_A_VALID_ENUM_VALUE"))
    if t in ("integer", "number"):
        if "minimum" in schema: out.append(("below_min", schema["minimum"] - 1))
        if "maximum" in schema: out.append(("above_max", schema["maximum"] + 1))
    return out


def xsd_type(schema, all_schemas):
    schema = resolve_refs(schema, all_schemas)
    t = schema.get("type")
    if t == "string":
        return {"date-time": XSD.dateTime, "full-time": XSD.time,
                "date-month": XSD.gMonth, "date-mday": XSD.gMonthDay}.get(schema.get("format"), XSD.string)
    if t == "integer": return XSD.integer
    if t == "number": return XSD.float if schema.get("format") == "float" else XSD.double
    if t == "boolean": return XSD.boolean
    return XSD.string


# ── RDF instance generation (uses converter's vocabulary) ────────────────

def build_prop_uri_map(rdf_graph, class_uri, ns):
    """Build map from OpenAPI property name → RDF property URI.
    
    Reads the converter's RDF graph to find properties with rdfs:domain
    pointing to this class. Handles both scoped and unscoped URIs.
    """
    uri_map = {}
    if rdf_graph is None or class_uri is None:
        return uri_map

    def local_name(iri) -> str:
        """Last segment, splitting on `#` AND `/`.

        The property separator changed from `#` to `/` on 2026-09-22 (see
        `property_uri.class_namespace`). `str(iri).split("#")[-1]` then returns the WHOLE IRI, so
        this map ends up keyed by full IRIs, `prop_uri_map.get(name)` never hits, and every instance
        property silently falls back to the file-level namespace — `.../TS28623_ComDefs/month`
        instead of `.../TS28623_ComDefs/DayInYear/month`. No shape matches those, so bad instances
        stop being rejected and good ones stop conforming: 289 test failures, none of which named a
        separator.
        """
        return str(iri).replace("#", "/").rstrip("/").rsplit("/", 1)[-1]

    # Find all properties with this class as domain
    for prop_uri in rdf_graph.subjects(RDFS.domain, class_uri):
        local = local_name(prop_uri)
        uri_map[local] = prop_uri
        # If scoped (ClassName_propName), also map the unscoped name
        class_local = local_name(class_uri)
        if local.startswith(class_local + "_"):
            unscoped = local[len(class_local) + 1:]
            uri_map[unscoped] = prop_uri
    return uri_map


def to_rdf(instance, class_uri, ns, prop_schemas, all_schemas, prop_uri_map, depth=0, rdf_graph=None):
    """Convert JSON instance to RDF. Nested objects become blank nodes."""
    from rdflib import BNode
    g = Graph()
    subj = ns["test_instance"] if depth == 0 else BNode()
    g.add((subj, RDF.type, class_uri))
    if not isinstance(instance, dict):
        return g, subj
    for key, value in instance.items():
        safe_key = key.replace("-", "_")
        prop_uri = prop_uri_map.get(safe_key, ns[safe_key])
        pschema = prop_schemas.get(key, {})
        resolved = resolve_refs(pschema, all_schemas)

        if isinstance(value, dict) and depth < 3:
            child_class = None
            raw_child_schema = pschema
            if "$ref" in pschema:
                ref_name = pschema["$ref"].split("/")[-1]
                child_class = ns[ref_name.replace("-", "_")]
                raw_child_schema = all_schemas.get(ref_name, {})
            child_props = raw_child_schema.get("properties", {}) if isinstance(raw_child_schema, dict) else {}
            child_map = build_prop_uri_map(rdf_graph, child_class, ns)
            child_g, child_node = to_rdf(value, child_class or ns[safe_key], ns, child_props, all_schemas, child_map, depth + 1, rdf_graph)
            g += child_g
            g.add((subj, prop_uri, child_node))
        elif isinstance(value, list):
            for item in value:
                if isinstance(item, dict) and depth < 3:
                    # Resolve $ref to get the actual schema (may be an array type)
                    actual_schema = pschema
                    if "$ref" in pschema:
                        ref_name = pschema["$ref"].split("/")[-1]
                        actual_schema = all_schemas.get(ref_name, pschema)
                    raw_items = actual_schema.get("items", {}) if isinstance(actual_schema, dict) else {}
                    child_class = None
                    if "$ref" in raw_items:
                        ref_name = raw_items["$ref"].split("/")[-1]
                        child_class = ns[ref_name.replace("-", "_")]
                    raw_child_schema = all_schemas.get(ref_name, raw_items) if "$ref" in raw_items else raw_items
                    child_props = raw_child_schema.get("properties", {}) if isinstance(raw_child_schema, dict) else {}
                    child_map = build_prop_uri_map(rdf_graph, child_class, ns)
                    child_g, child_node = to_rdf(item, child_class or ns[safe_key], ns, child_props, all_schemas, child_map, depth + 1, rdf_graph)
                    g += child_g
                    g.add((subj, prop_uri, child_node))
                elif not isinstance(item, (dict, list)):
                    dt = xsd_type(resolved.get("items", {}), all_schemas)
                    g.add((subj, prop_uri, Literal(item, datatype=dt)))
        else:
            dt = xsd_type(pschema, all_schemas)
            g.add((subj, prop_uri, Literal(value, datatype=dt)))
    return g, subj


def write_ttl(graph, ns_prefix, ns, path):
    graph.bind(ns_prefix, ns)
    graph.bind("xsd", XSD)
    path.parent.mkdir(parents=True, exist_ok=True)
    graph.serialize(destination=str(path), format="turtle")

    # Provenance header, added 2026-09-24. These 628 files carried none, and they are the LARGER of
    # the two generated trees -- `output/` got headers first and I wrongly reported that as covering
    # every generated file. It was 76 of 704.
    #
    # The source document comes from the PATH (`test-cases/<document>/<good|bad>/<case>.ttl`) because
    # `write_ttl` is not told it. That is a real coupling, so it is asserted in
    # `tests/test_generated_file_provenance.py`, which checks each header names its own document.
    #
    # The wording differs from the vocabulary header on the point that matters here: a `good/` and
    # `bad/` tree named after 3GPP documents reads like a conformance suite, and it is not one. The
    # values are synthetic and the cases were chosen to exercise OUR shapes.
    document = path.parent.parent.name
    header = (
        "# GENERATED TEST FIXTURE -- do not edit; regenerate instead.\n"
        "#\n"
        "# SHACL validation instance produced by openapi-to-rdf\n"
        f"#   {PROJECT_URL}\n"
        f"# Contact: {CONTACT}\n"
        f"# Source document: {document}.yaml\n"
        "#\n"
        "# The class and property names are the source document's. The VALUES are synthetic, chosen\n"
        "# to satisfy or violate one constraint of the shapes THIS project derives. The publisher of\n"
        "# that document did not produce, review or endorse this file, and it is NOT conformance test\n"
        "# data for their specification. See NOTICE for the corpora this repository bundles.\n"
        "#\n"
        "# Regenerate: uv run python scripts/generate_test_cases.py\n"
        "\n"
    )
    body = path.read_text(encoding="utf-8")
    path.write_text(header + body, encoding="utf-8")


# ── Main processing ─────────────────────────────────────────────────────

def process_file(yaml_path, output_dir):
    with open(yaml_path) as f:
        data = yaml.safe_load(f)
    schemas = data.get("components", {}).get("schemas", {})
    if not schemas:
        return 0, 0

    stem = yaml_path.stem

    # Run converter — single source for both RDF vocabulary and SHACL output
    # Use same base namespace as regenerate_output.py to ensure test corpus matches SHACL
    from openapi_to_rdf.shacl_converter import OpenAPIToSHACLConverter
    # NOT the repo's output/ tree. This script converts WITHOUT `external_refs`, so writing here
    # overwrote the published deliverable with a cross-document-unresolved conversion and broke
    # tests/test_output_freshness.py. The vocabulary is only needed in memory to mint instance IRIs.
    converter = OpenAPIToSHACLConverter(
        str(yaml_path),
        base_namespace=document_namespace(stem),
        output_dir=tempfile.mkdtemp(prefix="gen-test-cases-"),
    )
    converter.convert()
    converter.save_rdf()

    ns = converter.main_prefix
    ns_prefix = stem.replace("-", "_")

    good_dir = output_dir / stem / "good"
    bad_dir = output_dir / stem / "bad"
    good_count = bad_count = 0

    for schema_name, schema_def in schemas.items():
        if not isinstance(schema_def, dict) or schema_def.get("type") != "object":
            continue
        props = schema_def.get("properties")
        if not props:
            continue
        resolved = resolve_refs(schema_def, schemas)
        if has_external_refs(resolved):
            continue
        has_constraint = any(
            resolve_refs(p, schemas).get("type") in ("string", "integer", "number", "boolean")
            or "enum" in resolve_refs(p, schemas)
            or "pattern" in resolve_refs(p, schemas)
            or "minimum" in resolve_refs(p, schemas)
            for p in props.values() if isinstance(p, dict)
        )
        if not has_constraint:
            continue

        safe = schema_name.replace("-", "_")
        class_uri = ns[safe]
        prop_uri_map = build_prop_uri_map(converter.rdf_graph, class_uri, ns)

        # Good instance
        valid = gen_valid(schema_def, schemas)
        if isinstance(valid, dict) and js_valid(schema_def, valid, schemas):
            # Skip if required properties couldn't be generated
            required = set(schema_def.get("required", []))
            generated_keys = set(valid.keys()) if valid else set()
            if not required - generated_keys:
                g, _ = to_rdf(valid, class_uri, ns, props, schemas, prop_uri_map, rdf_graph=converter.rdf_graph)
                if len(g) > 1:
                    write_ttl(g, ns_prefix, ns, good_dir / f"{safe}.ttl")
                    good_count += 1

        # Bad: constraint violations
        for pname, pdef in props.items():
            for label, bad_val in gen_invalids(pname, pdef, schemas):
                base = gen_valid(schema_def, schemas)
                if not isinstance(base, dict): base = {}
                base[pname] = bad_val
                if not js_valid(schema_def, base, schemas):
                    g, _ = to_rdf(base, class_uri, ns, props, schemas, prop_uri_map, rdf_graph=converter.rdf_graph)
                    if len(g) > 1:
                        write_ttl(g, ns_prefix, ns, bad_dir / f"{safe}_{pname}_{label}.ttl")
                        bad_count += 1

        # Bad: missing required fields
        for req in schema_def.get("required", []):
            base = gen_valid(schema_def, schemas)
            if isinstance(base, dict) and req in base:
                missing = {k: v for k, v in base.items() if k != req}
                if not js_valid(schema_def, missing, schemas):
                    g, _ = to_rdf(missing, class_uri, ns, props, schemas, prop_uri_map, rdf_graph=converter.rdf_graph)
                    write_ttl(g, ns_prefix, ns, bad_dir / f"{safe}_missing_{req}.ttl")
                    bad_count += 1

    return good_count, bad_count


def main():
    root = Path(__file__).resolve().parent.parent
    yaml_dir = root / "assets" / "MnS-Rel-19-OpenAPI" / "OpenAPI"
    output_dir = root / "test-cases"

    # IMPORTANT: This script is the SINGLE source for both SHACL output and test data.
    # Never regenerate SHACL separately — it causes URI mismatches due to property scoping.

    total_good = total_bad = 0
    for f in sorted(yaml_dir.glob("TS*.yaml")):
        good, bad = process_file(f, output_dir)
        if good or bad:
            print(f"  {f.stem:45s}  good={good:3d}  bad={bad:3d}")
        total_good += good
        total_bad += bad
    print(f"\nTotal: {total_good} good, {total_bad} bad test cases")

    # Also regenerate SHACL for files that had no test cases
    # (so output/ is complete), using the same converter logic
    from openapi_to_rdf.shacl_converter import OpenAPIToSHACLConverter
    for f in sorted(yaml_dir.glob("TS*.yaml")):
        shacl_path = root / "output" / "shacl" / f"{f.stem}_shacl.ttl"
        if not shacl_path.exists():
            c = OpenAPIToSHACLConverter(str(f), output_dir=str(root / "output"))
            c.run()


if __name__ == "__main__":
    main()
