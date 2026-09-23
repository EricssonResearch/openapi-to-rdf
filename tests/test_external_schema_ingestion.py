"""External schemas participate in attribution exactly as local ones do.

Spec: docs/superpowers/specs/2026-09-23-external-schema-ingestion-design.md (D1, D2).

**Read this before adding a fixture here.** The request that prompted this work shipped a
reproduction that popped schemas out of `components/schemas` without rewriting the `$ref`
strings, so the refs stayed *internal* and dangling and `external_schemas` was never consulted.
It fails today for the wrong reason. Every fixture below writes the ref in its external form,
and `split_with_restatement` additionally **restates** an inherited property, which is the only
shape that can reach the attribution half of the defect.
"""

from __future__ import annotations

import pytest

from openapi_to_rdf import build_mapping

#: The document name external refs in these fixtures point at.
COMMON = "common.yaml"


@pytest.fixture
def split_with_restatement() -> dict:
    """An API document whose class inherits across a document boundary AND restates two fields.

    `Addressable` declares `href` and `id`. `PolicyRef` composes it by external `$ref` and
    restates both, exactly as TM Forum does. Whole-document conversion attributes `href`/`id` to
    `Addressable`; without this fix the split attributes them to `PolicyRef`.
    """
    common_schemas = {
        "Extensible": {
            "type": "object",
            "properties": {"@baseType": {"type": "string"}},
        },
        "Addressable": {
            "allOf": [
                {"$ref": "#/components/schemas/Extensible"},
                {
                    "type": "object",
                    "properties": {
                        "href": {"type": "string"},
                        "id": {"type": "string"},
                    },
                },
            ]
        },
    }
    api_document = {
        "openapi": "3.0.0",
        "info": {"title": "Api", "version": "1.0"},
        "components": {
            "schemas": {
                "PolicyRef": {
                    "allOf": [
                        {"$ref": f"{COMMON}#/components/schemas/Addressable"},
                        {
                            "type": "object",
                            "properties": {
                                # Restated from Addressable. This is the load-bearing part.
                                "href": {"type": "string"},
                                "id": {"type": "string"},
                                "@type": {"type": "string"},
                            },
                        },
                    ]
                }
            }
        },
    }
    return {"api": api_document, "external": {COMMON: common_schemas}}


def test_an_external_ancestor_is_registered_as_a_class(split_with_restatement):
    mapping = build_mapping(
        split_with_restatement["api"],
        namespace="https://tmforum.org/ontology/",
        external_schemas=split_with_restatement["external"],
    )
    assert "Addressable" in mapping.classes
    assert mapping.classes["Addressable"].declaring_document == COMMON
    assert mapping.classes["Addressable"].is_external is True
    assert mapping.classes["PolicyRef"].is_external is False


def test_a_restated_inherited_property_is_attributed_across_the_boundary(
    split_with_restatement,
):
    """The 34-misattribution defect, in one assertion."""
    mapping = build_mapping(
        split_with_restatement["api"],
        namespace="https://tmforum.org/ontology/",
        external_schemas=split_with_restatement["external"],
    )
    assert mapping.declaring_class("PolicyRef", "href") == "Addressable"
    assert mapping.declaring_class("PolicyRef", "id") == "Addressable"
    # Genuinely the leaf's own field.
    assert mapping.declaring_class("PolicyRef", "@type") == "PolicyRef"
    # And it is NOT also attributed to the leaf, which is what minted the second IRI.
    assert ("PolicyRef", "href") not in mapping.properties_by_class


def test_ancestry_crosses_the_boundary_transitively(split_with_restatement):
    """A in doc 1 -> B in doc 2 -> C in doc 2. `Extensible` is reached only through `Addressable`."""
    mapping = build_mapping(
        split_with_restatement["api"],
        namespace="https://tmforum.org/ontology/",
        external_schemas=split_with_restatement["external"],
    )
    assert mapping.classes["PolicyRef"].parents == ("Addressable",)
    assert mapping.classes["Addressable"].parents == ("Extensible",)
    assert "Extensible" in mapping.classes
    assert mapping.declaring_class("PolicyRef", "@baseType") == "Extensible"


def test_an_external_class_is_minted_under_its_own_documents_namespace(
    split_with_restatement,
):
    """A class's namespace is a property of the document that declares it (D1)."""
    mapping = build_mapping(
        split_with_restatement["api"],
        namespace="https://tmforum.org/ontology/",
        external_schemas=split_with_restatement["external"],
        external_namespaces={COMMON: "https://common.example/v5/"},
    )
    assert mapping.classes["Addressable"].iri == "https://common.example/v5/Addressable"
    assert mapping.classes["PolicyRef"].iri == "https://tmforum.org/ontology/PolicyRef"


def test_without_an_external_namespace_the_mappings_own_namespace_is_used(
    split_with_restatement,
):
    """The split model's default: one vocabulary spread over documents, zero configuration."""
    mapping = build_mapping(
        split_with_restatement["api"],
        namespace="https://tmforum.org/ontology/",
        external_schemas=split_with_restatement["external"],
    )
    assert mapping.classes["Addressable"].iri == "https://tmforum.org/ontology/Addressable"


def test_a_name_declared_in_two_documents_registers_once_and_reports_the_skip():
    """49 of 1,741 3GPP schema names are declared in more than one document; 44 bodies differ."""
    api_document = {
        "openapi": "3.0.0",
        "info": {"title": "Api", "version": "1.0"},
        "components": {
            "schemas": {
                # Local TimeWindow, with its own body.
                "TimeWindow": {"type": "object", "properties": {"start": {"type": "string"}}},
                "Job": {
                    "allOf": [
                        {"$ref": f"{COMMON}#/components/schemas/TimeWindow"},
                        {"type": "object", "properties": {"name": {"type": "string"}}},
                    ]
                },
            }
        },
    }
    external = {
        COMMON: {
            # Same name, DIFFERENT body — genuinely a different class.
            "TimeWindow": {"type": "object", "properties": {"duration": {"type": "integer"}}}
        }
    }
    mapping = build_mapping(
        api_document, namespace="https://example.org/", external_schemas=external
    )
    # The local declaration wins, and keeps the local namespace.
    assert mapping.classes["TimeWindow"].is_external is False
    assert "start" in {p for _c, p in mapping.properties_by_class}
    assert "duration" not in {p for _c, p in mapping.properties_by_class}
    # And the skip is reported rather than silent.
    assert ("TimeWindow", COMMON) in mapping.external_name_collisions


def test_a_cyclic_allof_across_documents_terminates():
    """Two documents whose classes compose each other must not hang the registration walk."""
    api_document = {
        "openapi": "3.0.0",
        "info": {"title": "Api", "version": "1.0"},
        "components": {
            "schemas": {
                "Leaf": {"allOf": [{"$ref": f"{COMMON}#/components/schemas/Loop"}]},
            }
        },
    }
    external = {
        COMMON: {
            "Loop": {"allOf": [{"$ref": "#/components/schemas/Other"}]},
            "Other": {
                "allOf": [
                    {"$ref": "#/components/schemas/Loop"},
                    {"type": "object", "properties": {"x": {"type": "string"}}},
                ]
            },
        }
    }
    mapping = build_mapping(
        api_document, namespace="https://example.org/", external_schemas=external
    )
    assert {"Leaf", "Loop", "Other"} <= set(mapping.classes)


def test_an_external_ref_with_a_directory_component_resolves_for_inheritance():
    """D3: `_parents_of` keyed on the raw prefix while `_resolve_reference` basenamed it.

    Latent, not live — 0 of 2,313 external refs in the 3GPP corpus carry a directory component —
    so this is the only input that reaches it.
    """
    api_document = {
        "openapi": "3.0.0",
        "info": {"title": "Api", "version": "1.0"},
        "components": {
            "schemas": {
                "Child": {"allOf": [{"$ref": "sub/dir/common.yaml#/components/schemas/Parent"}]}
            }
        },
    }
    external = {COMMON: {"Parent": {"type": "object",
                                    "properties": {"id": {"type": "string"}}}}}
    mapping = build_mapping(
        api_document, namespace="https://example.org/", external_schemas=external
    )
    assert mapping.classes["Child"].parents == ("Parent",)
    assert mapping.classes["Parent"].is_external is True


def test_an_external_property_resolves_its_target_against_its_own_document():
    """Critical 1a: internal `#/...` refs in external schemas resolve against the correct document.

    `common.yaml` declares `Addressable.validFor: {$ref: "#/components/schemas/TimePeriod"}` with
    `TimePeriod` also in `common.yaml`. The local document has neither. Without the fix, the ref
    resolves against the local schemas and `target_classes == ()` where the whole-document
    conversion gives `("TimePeriod",)`. The range is silently lost.
    """
    api_document = {
        "openapi": "3.0.0",
        "info": {"title": "Api", "version": "1.0"},
        "components": {
            "schemas": {
                "Child": {"allOf": [{"$ref": f"{COMMON}#/components/schemas/Addressable"}]}
            }
        },
    }
    external = {
        COMMON: {
            "TimePeriod": {
                "type": "object",
                "properties": {"start": {"type": "string"}},
            },
            "Addressable": {
                "type": "object",
                "properties": {
                    "href": {"type": "string"},
                    "validFor": {"$ref": "#/components/schemas/TimePeriod"},
                },
            },
        }
    }
    mapping = build_mapping(
        api_document, namespace="https://example.org/", external_schemas=external
    )
    # The external property's target resolves against common.yaml, not the local schemas.
    validFor_fact = mapping.properties_by_class[("Addressable", "validFor")]
    assert validFor_fact.target_classes == ("TimePeriod",)


def test_an_external_property_does_not_read_datatypes_from_a_local_alias():
    """Critical 1b: property resolution does not fabricate facts from a same-named local schema.

    Same fixture as 1a, but the local document also declares `TimePeriod: {type: string, format:
    date-time}`. Without the fix, `target_classes == ()` (because the local TimePeriod is a
    primitive, not a class) AND `datatype == xsd:dateTime` (read from the local schema). A wrong
    fact, not a missing one — the property is actually object-valued, not a dateTime literal.
    """
    api_document = {
        "openapi": "3.0.0",
        "info": {"title": "Api", "version": "1.0"},
        "components": {
            "schemas": {
                # Local TimePeriod is a primitive alias, not the external object class.
                "TimePeriod": {"type": "string", "format": "date-time"},
                "Child": {"allOf": [{"$ref": f"{COMMON}#/components/schemas/Addressable"}]},
            }
        },
    }
    external = {
        COMMON: {
            "TimePeriod": {
                "type": "object",
                "properties": {"start": {"type": "string"}},
            },
            "Addressable": {
                "type": "object",
                "properties": {
                    "href": {"type": "string"},
                    "validFor": {"$ref": "#/components/schemas/TimePeriod"},
                },
            },
        }
    }
    mapping = build_mapping(
        api_document, namespace="https://example.org/", external_schemas=external
    )
    # The external property resolves against common.yaml, not the local TimePeriod alias.
    validFor_fact = mapping.properties_by_class[("Addressable", "validFor")]
    assert validFor_fact.target_classes == ("TimePeriod",)
    assert validFor_fact.datatype is None  # Object-valued, not a dateTime literal.


def test_a_shadowed_parent_name_in_an_external_document_does_not_win():
    """Important 2: external ref parent wins over same-named schema in that external document.

    `b.yaml#Middle` has `allOf: [{$ref: "c.yaml#/components/schemas/Base"}]` and `b.yaml` also
    declares a different `Base`. Without the fix, the walk enqueues `(b.yaml, Base)` before
    `(c.yaml, Base)` and FIFO makes it win — the wrong properties registered, and the correct
    target reported as a skipped collision.
    """
    api_document = {
        "openapi": "3.0.0",
        "info": {"title": "Api", "version": "1.0"},
        "components": {
            "schemas": {"Leaf": {"allOf": [{"$ref": "b.yaml#/components/schemas/Middle"}]}}
        },
    }
    external = {
        "b.yaml": {
            # Middle's parent is c.yaml#Base, not b.yaml#Base.
            "Middle": {"allOf": [{"$ref": "c.yaml#/components/schemas/Base"}]},
            # But b.yaml also declares a different Base.
            "Base": {"type": "object", "properties": {"wrong": {"type": "string"}}},
        },
        "c.yaml": {
            "Base": {"type": "object", "properties": {"correct": {"type": "string"}}},
        },
    }
    mapping = build_mapping(
        api_document, namespace="https://example.org/", external_schemas=external
    )
    # Base is registered from c.yaml (the correct ref), not b.yaml.
    assert mapping.classes["Base"].declaring_document == "c.yaml"
    assert ("Base", "correct") in mapping.properties_by_class
    assert ("Base", "wrong") not in mapping.properties_by_class


def test_a_local_primitive_blocks_an_external_class_of_the_same_name():
    """Important 3: local always wins even when the local schema is not class-shaped.

    Local `Money: {type: string}` plus external `common.yaml#Money` as an object. Without the fix,
    `classes["Money"]` exists as an external object class under the local namespace,
    `external_name_collisions` is empty, and the local document uses Money as both a datatype and
    a class. The skip must be reported.
    """
    api_document = {
        "openapi": "3.0.0",
        "info": {"title": "Api", "version": "1.0"},
        "components": {
            "schemas": {
                # Local Money is a primitive alias, not a class.
                "Money": {"type": "string"},
                "Product": {
                    "type": "object",
                    "properties": {"price": {"$ref": "#/components/schemas/Money"}},
                },
                "Child": {"allOf": [{"$ref": f"{COMMON}#/components/schemas/Wealthy"}]},
            }
        },
    }
    external = {
        COMMON: {
            # External Money is an object class.
            "Money": {"type": "object", "properties": {"amount": {"type": "number"}}},
            # Wealthy inherits from Money, so Money will be registered transitively.
            "Wealthy": {
                "allOf": [
                    {"$ref": "#/components/schemas/Money"},
                    {"type": "object", "properties": {"bonus": {"type": "number"}}},
                ]
            },
        }
    }
    mapping = build_mapping(
        api_document, namespace="https://example.org/", external_schemas=external
    )
    # Money is NOT registered as a class — the local primitive wins.
    assert "Money" not in mapping.classes
    # And the skip is reported.
    assert ("Money", COMMON) in mapping.external_name_collisions


def test_identical_bodies_across_external_documents_are_also_reported():
    """Important 4: report EVERY skipped external registration, not only differing bodies.

    5 of 49 multi-document 3GPP names are declared identically. Without the fix, a name declared
    identically in `a.yaml` and `b.yaml` is minted only under `a.yaml`'s namespace and every
    reference from `b.yaml` silently resolves to `nsA:Foo`. The skip must be reported.
    """
    api_document = {
        "openapi": "3.0.0",
        "info": {"title": "Api", "version": "1.0"},
        "components": {
            "schemas": {
                "Leaf": {
                    "allOf": [
                        {"$ref": "a.yaml#/components/schemas/SharedName"},
                        {"$ref": "b.yaml#/components/schemas/SharedName"},
                    ]
                }
            }
        },
    }
    external = {
        "a.yaml": {
            # IDENTICAL body to b.yaml's SharedName.
            "SharedName": {"type": "object", "properties": {"x": {"type": "string"}}}
        },
        "b.yaml": {
            "SharedName": {"type": "object", "properties": {"x": {"type": "string"}}}
        },
    }
    mapping = build_mapping(
        api_document, namespace="https://example.org/", external_schemas=external
    )
    # Only one registration wins (a.yaml, FIFO).
    assert mapping.classes["SharedName"].declaring_document == "a.yaml"
    # And the skip is reported, even though the bodies are identical.
    assert ("SharedName", "b.yaml") in mapping.external_name_collisions


def test_a_third_document_is_reached_through_an_external_ref():
    """Important 5: the third-document enqueue walks external refs transitively.

    `Leaf -> b.yaml#Middle -> c.yaml#Base`. Middle's parent is an external ref to c.yaml, which
    is neither the local document nor the document Middle came from. Without the third-document
    enqueue, Base is not registered.
    """
    api_document = {
        "openapi": "3.0.0",
        "info": {"title": "Api", "version": "1.0"},
        "components": {
            "schemas": {"Leaf": {"allOf": [{"$ref": "b.yaml#/components/schemas/Middle"}]}}
        },
    }
    external = {
        "b.yaml": {
            # Middle's parent is in c.yaml, a THIRD document.
            "Middle": {"allOf": [{"$ref": "c.yaml#/components/schemas/Base"}]}
        },
        "c.yaml": {"Base": {"type": "object", "properties": {"id": {"type": "string"}}}},
    }
    mapping = build_mapping(
        api_document, namespace="https://example.org/", external_schemas=external
    )
    # Base from c.yaml is registered transitively.
    assert "Base" in mapping.classes
    assert mapping.classes["Base"].declaring_document == "c.yaml"
    assert mapping.classes["Middle"].parents == ("Base",)


def _write(directory, name, document):
    import yaml

    path = directory / name
    path.write_text(yaml.safe_dump(document))
    return path


@pytest.fixture
def split_files(tmp_path, split_with_restatement):
    """`split_with_restatement`, on disk, as common.yaml + api.yaml."""
    common = {
        "openapi": "3.0.0",
        "info": {"title": "Common", "version": "1.0"},
        "components": {"schemas": split_with_restatement["external"][COMMON]},
    }
    return {
        "common": _write(tmp_path, COMMON, common),
        "api": _write(tmp_path, "api.yaml", split_with_restatement["api"]),
        "dir": tmp_path,
    }


def test_a_referenced_external_class_gets_its_declaring_documents_iri(split_files, tmp_path):
    """D1: the referring document used to mint this from the FILENAME and a hardcoded prefix.

    Pre-fix this emits <http://ericsson.com/models/3gpp/rdf/common#Addressable> — a 3GPP IRI
    inside a TM Forum vocabulary, and an IRI no document declares.
    """
    from rdflib import RDFS, URIRef

    from openapi_to_rdf import OpenAPIToSHACLConverter

    shared = "https://tmforum.org/ontology/"
    converter = OpenAPIToSHACLConverter(
        str(split_files["api"]),
        base_namespace=shared,
        output_dir=str(tmp_path / "out"),
        external_refs=[str(split_files["common"])],
    )
    converter.convert()
    assert (
        URIRef(f"{shared}PolicyRef"),
        RDFS.subClassOf,
        URIRef(f"{shared}Addressable"),
    ) in converter.rdf_graph


def test_document_namespaces_overrides_per_document(split_files, tmp_path):
    """3GPP's convention: one vocabulary per document, stated by the caller."""
    from rdflib import RDFS, URIRef

    from openapi_to_rdf import OpenAPIToSHACLConverter

    converter = OpenAPIToSHACLConverter(
        str(split_files["api"]),
        base_namespace="https://api.example/",
        output_dir=str(tmp_path / "out"),
        external_refs=[str(split_files["common"])],
        document_namespaces={COMMON: "https://common.example/v5/"},
    )
    converter.convert()
    assert (
        URIRef("https://api.example/PolicyRef"),
        RDFS.subClassOf,
        URIRef("https://common.example/v5/Addressable"),
    ) in converter.rdf_graph


def test_an_external_class_is_referenced_but_never_declared(split_files, tmp_path):
    """AC-7. The declaring document emits the declaration; re-declaring would duplicate it."""
    from rdflib import RDF, RDFS, URIRef

    from openapi_to_rdf import OpenAPIToSHACLConverter

    shared = "https://tmforum.org/ontology/"
    converter = OpenAPIToSHACLConverter(
        str(split_files["api"]),
        base_namespace=shared,
        output_dir=str(tmp_path / "out"),
        external_refs=[str(split_files["common"])],
    )
    converter.convert()
    external = URIRef(f"{shared}Addressable")
    assert (external, RDF.type, RDFS.Class) not in converter.rdf_graph
    assert not list(converter.rdf_graph.triples((None, RDFS.domain, external)))
    assert not list(converter.shacl_graph.triples((None, None, external)))
