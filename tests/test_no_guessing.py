"""No guessing, no placeholders, no silent fallback.

Three sites where the tool invents information rather than admitting it could not derive any:
1. Datatype guessed from substrings of a schema *name* rather than declared type/format
2. Placeholder IRI for unresolvable $ref (leaks build directory, breaks reproducibility)
3. Fragment-less $ref silently mis-resolved instead of raising

Closes O5, O6; absorbs S3, S4, L6.
"""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml
from rdflib import XSD, Graph, RDFS

# Test case 1: Name-based datatype guessing
FLOAT_NAME_SPEC = {
    "openapi": "3.0.0",
    "info": {"title": "NoGuessing", "version": "1.0"},
    "components": {
        "schemas": {
            "FloatThing": {
                "type": "string",
                "description": "Named FloatThing but declared as string"
            },
            "IntegerValue": {
                "type": "string",
                "description": "Named IntegerValue but declared as string"
            },
            "BooleanFlag": {
                "type": "number",
                "format": "double",
                "description": "Named BooleanFlag but declared as double"
            },
            "Container": {
                "type": "object",
                "properties": {
                    "floatThing": {"$ref": "#/components/schemas/FloatThing"},
                    "intValue": {"$ref": "#/components/schemas/IntegerValue"},
                    "boolFlag": {"$ref": "#/components/schemas/BooleanFlag"},
                }
            }
        }
    }
}

# Test case 2: Unresolvable $ref
UNRESOLVED_REF_SPEC = {
    "openapi": "3.0.0",
    "info": {"title": "UnresolvedRef", "version": "1.0"},
    "components": {
        "schemas": {
            "Service": {
                "type": "object",
                "properties": {
                    "party": {"$ref": "#/components/schemas/NonExistent"},
                }
            }
        }
    }
}

# Test case 3: Fragment-less $ref (zero occurrences in corpus, but legal OAS and will appear)
FRAGMENTLESS_REF_SPEC = {
    "openapi": "3.1.0",
    "info": {"title": "FragmentlessRef", "version": "1.0"},
    "components": {
        "schemas": {
            "Service": {
                "type": "object",
                "properties": {
                    "money": {"$ref": "Money.yaml"},
                }
            }
        }
    }
}


@pytest.fixture
def float_name_graphs(tmp_path: Path) -> tuple[Graph, Graph]:
    """Convert spec with misleading schema names."""
    spec_file = tmp_path / "NoGuessing.yaml"
    spec_file.write_text(yaml.safe_dump(FLOAT_NAME_SPEC))
    from openapi_to_rdf import OpenAPIToSHACLConverter

    converter = OpenAPIToSHACLConverter(str(spec_file), output_dir=str(tmp_path / "out"))
    converter.convert()
    return converter.rdf_graph, converter.shacl_graph


def test_datatype_derived_from_declaration_not_name_in_shacl(float_name_graphs) -> None:
    """A schema named FloatThing whose declared type is string must yield xsd:string."""
    vocabulary, _ = float_name_graphs
    # FloatThing is declared as string, so any property referencing it should not get xsd:float
    ranges = {
        (str(s), str(o))
        for s, o in vocabulary.subject_objects(RDFS.range)
        if "float" in str(s).lower() or "int" in str(s).lower() or "bool" in str(s).lower()
    }

    # Check no property got xsd:float just because "float" appears in its target schema name
    float_ranges = [(s, o) for s, o in ranges if str(o) == str(XSD.float)]
    assert not float_ranges, f"FloatThing is declared as string but got xsd:float: {float_ranges}"

    # Check no property got xsd:integer just because "integer" appears in its target schema name
    int_ranges = [(s, o) for s, o in ranges if str(o) == str(XSD.integer)]
    assert not int_ranges, f"IntegerValue is declared as string but got xsd:integer: {int_ranges}"

    # BooleanFlag is declared as double, so it should get xsd:double not xsd:boolean
    bool_ranges = [(s, o) for s, o in ranges if str(o) == str(XSD.boolean)]
    assert not bool_ranges, f"BooleanFlag is declared as double but got xsd:boolean: {bool_ranges}"


def test_datatype_derived_from_declaration_not_name_in_owl(tmp_path: Path) -> None:
    """OWL converter must also derive from type/format, not name substring."""
    spec_file = tmp_path / "NoGuessing.yaml"
    spec_file.write_text(yaml.safe_dump(FLOAT_NAME_SPEC))
    from openapi_to_rdf import OpenAPIToRDFConverter
    from rdflib.namespace import OWL, RDF

    # OpenAPIToRDFConverter requires base_namespace
    converter = OpenAPIToRDFConverter(
        str(spec_file),
        output_dir=str(tmp_path / "out"),
        base_namespace="http://example.com/test#"
    )
    converter.convert()
    vocabulary = converter.graph

    # All three schemas (FloatThing, IntegerValue, BooleanFlag) are primitives (string or number),
    # so properties referencing them should be DatatypeProperty, not ObjectProperty.
    # The old code guessed ObjectProperty if name didn't contain "float"/"int"/"string".
    for prop_name in ["floatThing", "intValue", "boolFlag"]:
        prop_uris = [s for s in vocabulary.subjects(RDF.type, OWL.ObjectProperty)
                     if prop_name in str(s)]
        assert not prop_uris, \
            f"{prop_name} references a primitive but was marked ObjectProperty by name guessing: {prop_uris}"


def test_unresolved_ref_counted_not_placeholder(tmp_path: Path) -> None:
    """Unresolvable $ref must be reported, never minted as placeholder IRI."""
    spec_file = tmp_path / "TestSpec.yaml"  # Changed name to avoid matching in URIs
    spec_file.write_text(yaml.safe_dump(UNRESOLVED_REF_SPEC))
    from openapi_to_rdf import OpenAPIToSHACLConverter

    converter = OpenAPIToSHACLConverter(str(spec_file), output_dir=str(tmp_path / "out"))
    converter.convert()

    # Check that no "UnresolvedRef_" placeholder prefix was created
    # (the old code created URIs like "UnresolvedRef_components_schemas_NonExistent")
    vocabulary = converter.rdf_graph
    all_subjects = [str(s) for s in vocabulary.subjects()]
    placeholder_pattern_uris = [s for s in all_subjects if "UnresolvedRef_" in s]
    assert not placeholder_pattern_uris, \
        f"Found placeholder URIs with UnresolvedRef_ prefix: {placeholder_pattern_uris}"

    # Check that NonExistent does not appear as a class
    nonexistent_classes = [str(s) for s in all_subjects if "NonExistent" in str(s)]
    assert not nonexistent_classes, \
        f"NonExistent schema created as class despite being unresolved: {nonexistent_classes}"

    # The unresolved reference should be tracked
    assert hasattr(converter, 'unresolved_references'), "Converter should track unresolved references"
    assert len(converter.unresolved_references) > 0, \
        f"Unresolved reference should be counted, got: {converter.unresolved_references}"
    assert any("NonExistent" in ref for ref in converter.unresolved_references), \
        f"NonExistent ref should be in unresolved list: {converter.unresolved_references}"


def test_fragmentless_ref_raises_with_oas_citation(tmp_path: Path) -> None:
    """Fragment-less $ref like 'Money.yaml' must raise naming OAS 3.1 §4.8.24."""
    spec_file = tmp_path / "FragmentlessRef.yaml"
    spec_file.write_text(yaml.safe_dump(FRAGMENTLESS_REF_SPEC))
    from openapi_to_rdf import OpenAPIToSHACLConverter

    with pytest.raises(ValueError) as exc_info:
        converter = OpenAPIToSHACLConverter(str(spec_file), output_dir=str(tmp_path / "out"))
        converter.convert()

    error_msg = str(exc_info.value)
    assert "OAS 3.1 §4.8.24" in error_msg or "OAS 3.1" in error_msg, \
        f"Error should cite OAS 3.1 §4.8.24: {error_msg}"
    assert "Money.yaml" in error_msg or "fragment" in error_msg.lower(), \
        f"Error should mention the fragmentless ref: {error_msg}"
