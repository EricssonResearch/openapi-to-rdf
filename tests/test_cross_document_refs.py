"""Cross-document $ref resolution: external refs must resolve as written.

An external ref like ``other.yaml#/components/schemas/Thing`` must be resolved against the
*named document*, not stripped to ``#/components/schemas/Thing`` and looked up in the referring
document. OAS 3.x: *"each document in an OAD MUST be fully parsed in order to locate possible
reference targets"* — the document qualifier is load-bearing.
"""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml


@pytest.fixture
def multidoc_workspace(tmp_path: Path) -> dict[str, Path]:
    """Create a workspace with two documents where the second refs the first."""
    base_doc = {
        "openapi": "3.0.0",
        "info": {"title": "Base", "version": "1.0"},
        "components": {
            "schemas": {
                "Thing": {
                    "type": "object",
                    "properties": {"id": {"type": "string"}},
                },
                "CommonType": {
                    "type": "object",
                    "properties": {"name": {"type": "string"}},
                },
            }
        },
    }

    referring_doc = {
        "openapi": "3.0.0",
        "info": {"title": "Referring", "version": "1.0"},
        "components": {
            "schemas": {
                "Container": {
                    "allOf": [
                        {"$ref": "base.yaml#/components/schemas/Thing"},
                        {
                            "type": "object",
                            "properties": {
                                "payload": {"$ref": "base.yaml#/components/schemas/CommonType"}
                            },
                        },
                    ]
                }
            }
        },
    }

    base_path = tmp_path / "base.yaml"
    referring_path = tmp_path / "referring.yaml"

    with open(base_path, "w") as f:
        yaml.dump(base_doc, f)
    with open(referring_path, "w") as f:
        yaml.dump(referring_doc, f)

    return {"base": base_path, "referring": referring_path}


def test_external_ref_in_allof_resolves_to_correct_class(multidoc_workspace):
    """An external $ref in allOf must resolve against the named document."""
    from openapi_to_rdf import build_mapping
    import os

    referring_path = multidoc_workspace["referring"]
    base_path = multidoc_workspace["base"]

    # Load the referring document
    with open(referring_path) as f:
        referring_doc = yaml.safe_load(f)

    # Load the base document's schemas
    with open(base_path) as f:
        base_doc = yaml.safe_load(f)
        base_schemas = base_doc.get("components", {}).get("schemas", {})

    # Build mapping with external refs
    external_schemas = {os.path.basename(str(base_path)): base_schemas}
    mapping = build_mapping(
        referring_doc,
        namespace="https://example.org/",
        external_schemas=external_schemas,
    )

    # The Container class should have Thing as a parent
    assert "Container" in mapping.classes
    assert mapping.classes["Container"].parents == ("Thing",), (
        f"External $ref to Thing must resolve; got parents={mapping.classes['Container'].parents}"
    )


def test_external_ref_as_property_target_resolves(multidoc_workspace):
    """An external $ref as a property's target must resolve to the correct class IRI."""
    from openapi_to_rdf import OpenAPIToSHACLConverter
    from rdflib import RDFS, URIRef

    referring_path = str(multidoc_workspace["referring"])
    base_path = str(multidoc_workspace["base"])

    converter = OpenAPIToSHACLConverter(
        referring_path,
        base_namespace="https://example.org/",
        external_refs=[base_path],
    )
    converter.convert()

    # The payload property should point to CommonType from base.yaml.
    # With no document_namespaces, base.yaml gets the filename-derived namespace.
    expected_common_type = URIRef("http://ericsson.com/models/3gpp/rdf/base#CommonType")

    # Find all rdfs:range objects
    payload_ranges = set(converter.rdf_graph.objects(None, RDFS.range))

    assert expected_common_type in payload_ranges, (
        f"Property must have rdfs:range pointing to {expected_common_type}, "
        f"got ranges: {[str(r) for r in payload_ranges]}"
    )


def test_unresolved_external_refs_are_counted(multidoc_workspace):
    """Unresolved external refs are tracked, not silently placeholdered."""
    from openapi_to_rdf import OpenAPIToSHACLConverter

    referring_path = str(multidoc_workspace["referring"])

    # Convert WITHOUT passing external_refs — base.yaml won't be loaded
    converter = OpenAPIToSHACLConverter(
        referring_path,
        base_namespace="https://example.org/",
        external_refs=[],  # Explicitly empty
    )
    converter.convert()

    # The external refs should be tracked as unresolved
    assert len(converter.unresolved_references) > 0, (
        "External refs without loaded documents must be tracked as unresolved"
    )
    # At least the Thing ref and CommonType ref
    assert any("base.yaml" in ref for ref in converter.unresolved_references), (
        f"Unresolved refs should include base.yaml references; got {converter.unresolved_references}"
    )


def test_split_model_common_class_iri_is_stable(tmp_path: Path):
    """A class in Common gets the same IRI when using schema_namespaces.

    The TM Forum split model has Common (78 schemas) + specific documents. With schema_namespaces,
    classes from Common get consistent IRIs across conversions. Without it, each document generates
    its own namespace for external refs.
    """
    from openapi_to_rdf import OpenAPIToSHACLConverter
    from rdflib import RDFS

    common_doc = {
        "openapi": "3.0.0",
        "info": {"title": "Common", "version": "1.0"},
        "components": {
            "schemas": {
                "TimePeriod": {
                    "type": "object",
                    "properties": {
                        "startDateTime": {"type": "string", "format": "date-time"},
                        "endDateTime": {"type": "string", "format": "date-time"},
                    },
                },
            }
        },
    }

    specific_doc = {
        "openapi": "3.0.0",
        "info": {"title": "Specific", "version": "1.0"},
        "components": {
            "schemas": {
                "Order": {
                    "allOf": [
                        {"$ref": "common.yaml#/components/schemas/TimePeriod"},
                        {
                            "type": "object",
                            "properties": {"orderId": {"type": "string"}},
                        },
                    ]
                }
            }
        },
    }

    common_path = tmp_path / "common.yaml"
    specific_path = tmp_path / "specific.yaml"

    with open(common_path, "w") as f:
        yaml.dump(common_doc, f)
    with open(specific_path, "w") as f:
        yaml.dump(specific_doc, f)

    # Convert Specific with Common as external ref.
    # This is the split model, so pass document_namespaces for the shared vocabulary.
    shared_namespace = "https://example.org/api/"
    converter = OpenAPIToSHACLConverter(
        str(specific_path),
        base_namespace=shared_namespace,
        external_refs=[str(common_path)],
        document_namespaces={"common.yaml": shared_namespace},
    )
    converter.convert()

    # The key test: cross-document parent resolution works
    assert converter.mapping.classes["Order"].parents == ("TimePeriod",), (
        "Order must have TimePeriod as parent via cross-document $ref"
    )

    # And the inheritance edge is emitted in RDF with both classes under the shared namespace
    from rdflib import URIRef
    order_iri = URIRef(f"{shared_namespace}Order")
    time_period_iri = URIRef(f"{shared_namespace}TimePeriod")
    assert (order_iri, RDFS.subClassOf, time_period_iri) in converter.rdf_graph, (
        "Order must have rdfs:subClassOf TimePeriod under shared namespace"
    )
