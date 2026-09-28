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

from openapi_to_rdf.mapping import DEFAULT_BASE_NAMESPACE_PREFIX


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
    # Derived from the constant, not retyped: this exercises the ZERO-CONFIG path, so it asserts
    # whatever the library's default prefix is. That default changed on 2026-09-24 from
    # `http://ericsson.com/models/3gpp/` -- which claimed an Ericsson authority over a caller's
    # document and said "3gpp" whatever the input was -- to an RFC 2606 placeholder.
    expected_common_type = URIRef(f"{DEFAULT_BASE_NAMESPACE_PREFIX}rdf/base#CommonType")

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


def test_no_property_iri_is_minted_outside_the_mapping(tmp_path) -> None:
    """Every property the graph declares must be one the Mapping attributed. No second IRI.

    THE DEFECT THIS GUARDS, measured 2026-09-28. `_type_clause` passed `subject=None` for an inline
    `allOf` member to suppress a duplicate NodeShape, and the same argument also carried "which class
    owns these properties" -- so `_process_property` fell to `self.main_prefix[safe_prop]` and minted an
    UNSCOPED IRI. The result was two IRIs for one property: `<ns>AnLFFunction` beside the correct
    `<ns>NwdafFunction-Single/AnLFFunction`, which the Mapping had attributed properly all along.

    Corpus effect: 89 duplicate subjects in `TS28541_5GcNrm` alone, 702 triples across the 44 3GPP
    documents, and every one of the 89 already had its scoped twin in the same graph -- pure duplicates,
    nothing lost when they went. It was also the dominant reason whole-vs-split conversions disagreed
    (`scripts/diagnose_split_divergence.py`: 90.2% of the divergence), because a split document never
    minted the duplicate.

    It also COLLIDED: `EP_AIOT3` is an inline member property of both `AmfFunction-Single` and
    `AiotfFunction-Single`, so unscoped they became one IRI carrying two classes' domains.

    Asserted against the MAPPING rather than against a list of expected IRIs, because the Mapping is
    the independent path -- it is built by different code and it had the right answer while the emitter
    did not.
    """
    import yaml

    from rdflib.namespace import RDF

    from openapi_to_rdf import build_mapping
    from openapi_to_rdf.provenance import split_provenance
    from openapi_to_rdf.shacl_converter import OpenAPIToSHACLConverter

    ns = "https://example.org/v/"
    document = {
        "openapi": "3.0.0",
        "info": {"title": "Inline", "version": "1"},
        "components": {"schemas": {
            # The real shape, reduced: a named schema whose `allOf` carries an INLINE member with
            # properties. That member has no name, so its properties belong to `Holder`.
            "Holder": {"allOf": [
                {"$ref": "#/components/schemas/Base"},
                {"type": "object", "properties": {"inlineProp": {"type": "string"}}},
            ]},
            # A second holder declaring the SAME inline property name, which is what made the old
            # unscoped mint collide rather than merely be redundant.
            "OtherHolder": {"allOf": [
                {"$ref": "#/components/schemas/Base"},
                {"type": "object", "properties": {"inlineProp": {"type": "string"}}},
            ]},
            "Base": {"type": "object", "properties": {"id": {"type": "string"}}},
        }},
    }
    spec = tmp_path / "inline.yaml"
    spec.write_text(yaml.safe_dump(document))

    converter = OpenAPIToSHACLConverter(str(spec), base_namespace=ns, output_dir=str(tmp_path / "out"))
    converter.convert()
    graph, _ = split_provenance(converter.rdf_graph, ns)
    mapping = build_mapping(document, namespace=ns)

    mapping_iris = {fact.iri for fact in mapping.properties_by_class.values()}
    # A property subject is one carrying a `/` after the namespace; a class IRI has none.
    graph_properties = {
        str(s) for s in set(graph.subjects())
        if str(s).startswith(ns) and "/" in str(s)[len(ns):]
    }
    unscoped = {
        str(s) for s in set(graph.subjects())
        if str(s).startswith(ns) and "/" not in str(s)[len(ns):]
        and (s, RDF.type, RDF.Property) in graph
    }

    assert not unscoped, f"unscoped property IRIs minted: {sorted(unscoped)}"
    assert graph_properties <= mapping_iris, {
        "in_graph_not_in_mapping": sorted(graph_properties - mapping_iris)
    }
    # Non-vacuous, and the candidate set is stated: the two holders must each have their OWN
    # `inlineProp`, so the collision is what is being ruled out rather than merely the redundancy.
    assert f"{ns}Holder/inlineProp" in graph_properties, sorted(graph_properties)
    assert f"{ns}OtherHolder/inlineProp" in graph_properties, sorted(graph_properties)
    assert len(graph_properties) >= 2, sorted(graph_properties)


def _split_pair():
    """Two documents where the PRIMITIVE alias lives in the other file.

    `Tac`-shaped: a string with a pattern, referenced across a document boundary as `items.$ref`.
    """
    api = {
        "openapi": "3.0.0",
        "info": {"title": "Api", "version": "1"},
        "components": {"schemas": {
            "Holder": {"type": "object", "properties": {
                "codes": {"type": "array", "items": {"$ref": "common.yaml#/components/schemas/Code"}},
                "link": {"$ref": "common.yaml#/components/schemas/Link"},
            }},
        }},
    }
    common_schemas = {
        "Code": {"type": "string", "pattern": "^[0-9a-f]+$", "description": "a hex code"},
        # `format: uri` on an alias in ANOTHER document -- determination S5/F10.
        "Link": {"type": "string", "format": "uri", "description": "a link"},
    }
    return api, {"common.yaml": common_schemas}


def test_an_external_primitive_alias_yields_a_datatype_not_a_class() -> None:
    """Defect D3 at the Mapping level: an external `$ref` to a primitive is a LITERAL, not a class.

    `_resolve_ref` refused to follow an external `$ref` on the stated principle that this module reads
    no filesystem. But `build_mapping` is HANDED `external_schemas` in memory, so refusing was refusing
    to read a fact it already had -- and the consequence was `datatype=None`, which a consumer reads as
    "not a literal" and turns into an `rdfs:range` naming the alias, an IRI nothing declares.

    Measured across the 44 3GPP documents, splitting each in two and comparing every half's facts
    against the whole document's: **216 datatype mismatches before, 0 after.**
    """
    from openapi_to_rdf import build_mapping

    api, external = _split_pair()
    mapping = build_mapping(api, namespace="https://example.org/v/", external_schemas=external)
    fact = mapping.properties_by_class[("Holder", "codes")]

    assert fact.datatype == "http://www.w3.org/2001/XMLSchema#string", fact.datatype
    # And NO class target: naming one would be the dangling `rdfs:range` this defect produced.
    assert fact.target_classes == (), fact.target_classes
    # The alias must not have become a class either.
    assert "Code" not in mapping.classes, sorted(mapping.classes)


def test_format_uri_on_an_external_alias_is_still_iri_valued() -> None:
    """S5/F10 across a document boundary — CONSTRUCTED, because the corpus cannot reach it.

    Stated plainly rather than implied: the same `_resolve_ref` blindness applied to `is_iri_valued`,
    so `format: uri` on an alias in another document was invisible and the property would lift as a
    string instead of an edge. Measured on the 3GPP corpus: **0 instances before the fix and 0 after**
    — no document there puts `format: uri` on a cross-referenced alias. So this is a latent defect
    fixed defensively, and this test is the only thing that exercises it. Without the constructed case
    the fix would be untested code.
    """
    from openapi_to_rdf import build_mapping

    api, external = _split_pair()
    mapping = build_mapping(api, namespace="https://example.org/v/", external_schemas=external)
    fact = mapping.properties_by_class[("Holder", "link")]
    assert fact.is_iri_valued is True, "format: uri across a document boundary was not seen"


def test_an_unresolvable_external_ref_still_yields_no_guess() -> None:
    """The principle the old behaviour was defending, kept: absent stays absent.

    Following an external `$ref` is now possible when the pool is supplied. When it is NOT supplied, or
    names a document the caller did not hand over, the fact must stay missing rather than become a
    default -- `datatype=None` and no target class, not `xsd:string`.
    """
    from openapi_to_rdf import build_mapping

    api, _ = _split_pair()
    mapping = build_mapping(api, namespace="https://example.org/v/")  # no external_schemas at all
    fact = mapping.properties_by_class[("Holder", "codes")]
    assert fact.datatype is None, fact.datatype
    assert fact.target_classes == (), fact.target_classes
