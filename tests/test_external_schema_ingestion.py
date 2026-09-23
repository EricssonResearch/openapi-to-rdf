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
