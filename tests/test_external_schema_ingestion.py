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

    TM Forum split model: one shared vocabulary, so document_namespaces is required.
    """
    from rdflib import RDFS, URIRef

    from openapi_to_rdf import OpenAPIToSHACLConverter

    shared = "https://tmforum.org/ontology/"
    converter = OpenAPIToSHACLConverter(
        str(split_files["api"]),
        base_namespace=shared,
        output_dir=str(tmp_path / "out"),
        external_refs=[str(split_files["common"])],
        document_namespaces={COMMON: shared},
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
        document_namespaces={COMMON: shared},
    )
    converter.convert()
    external = URIRef(f"{shared}Addressable")
    # No triple whose SUBJECT is the external class IRI
    assert (external, RDF.type, RDFS.Class) not in converter.rdf_graph
    # No triple whose subject is a property IRI minted under the external class
    external_prop_prefix = f"{shared}Addressable/"
    assert not any(
        str(s).startswith(external_prop_prefix)
        for s, _, _ in converter.rdf_graph.triples((None, None, None))
    )
    # But object-position references are kept: PolicyRef subClassOf Addressable
    assert (
        URIRef(f"{shared}PolicyRef"),
        RDFS.subClassOf,
        external,
    ) in converter.rdf_graph


def test_zero_config_external_class_uses_filename_derived_namespace(tmp_path):
    """3GPP default: no base_namespace, no document_namespaces, external class gets filename-derived IRI.

    A class's namespace is a property of the document that declares it, so absent instruction
    the best available answer is what that document's own conversion would produce.
    """
    from rdflib import RDFS, URIRef

    from openapi_to_rdf import OpenAPIToSHACLConverter

    # Create TS28623_ComDefs.yaml with Thing
    comdefs_doc = {
        "openapi": "3.0.0",
        "info": {"title": "ComDefs", "version": "1.0"},
        "components": {
            "schemas": {
                "Thing": {
                    "type": "object",
                    "properties": {"id": {"type": "string"}},
                }
            }
        },
    }
    comdefs_path = _write(tmp_path, "TS28623_ComDefs.yaml", comdefs_doc)

    # Create local document inheriting from Thing
    local_doc = {
        "openapi": "3.0.0",
        "info": {"title": "Local", "version": "1.0"},
        "components": {
            "schemas": {
                "Leaf": {
                    "allOf": [
                        {"$ref": "TS28623_ComDefs.yaml#/components/schemas/Thing"},
                        {"type": "object", "properties": {"name": {"type": "string"}}},
                    ]
                }
            }
        },
    }
    local_path = _write(tmp_path, "local.yaml", local_doc)

    # Convert with NO base_namespace and NO document_namespaces
    converter = OpenAPIToSHACLConverter(
        str(local_path),
        output_dir=str(tmp_path / "out"),
        external_refs=[str(comdefs_path)],
    )
    converter.convert()

    # Thing's IRI must be the filename-derived one: http://ericsson.com/models/3gpp/TS28623/ComDefs#Thing
    # (not the local document's derived namespace)
    thing_iri = URIRef("http://ericsson.com/models/3gpp/TS28623/ComDefs#Thing")
    local_leaf = URIRef("http://ericsson.com/models/3gpp/rdf/local#Leaf")
    assert (local_leaf, RDFS.subClassOf, thing_iri) in converter.rdf_graph


def test_oneof_with_external_class_preserves_both_members(tmp_path):
    """C2: oneOf over external and local class must preserve both members in sh:xone.

    The suppression must be subject-scoped only. Before the fix, external classes were
    dropped from object position, collapsing a two-member oneOf to a bare sh:class
    (false rejection for legal instances of the external class).
    """
    from rdflib import RDF, RDFS, Namespace, URIRef

    from openapi_to_rdf import OpenAPIToSHACLConverter

    # External document with Addressable (which must be a registered ancestor)
    ext_doc = {
        "openapi": "3.0.0",
        "info": {"title": "External", "version": "1.0"},
        "components": {
            "schemas": {
                "Addressable": {
                    "type": "object",
                    "properties": {"href": {"type": "string"}},
                }
            }
        },
    }
    ext_path = _write(tmp_path, "external.yaml", ext_doc)

    # Local document with:
    # - LocalThing (local class)
    # - Inheritor (inherits from Addressable so it's a registered ancestor)
    # - Container with property whose oneOf includes both
    local_doc = {
        "openapi": "3.0.0",
        "info": {"title": "Local", "version": "1.0"},
        "components": {
            "schemas": {
                "LocalThing": {
                    "type": "object",
                    "properties": {"id": {"type": "string"}},
                },
                "Inheritor": {
                    "allOf": [
                        {"$ref": "external.yaml#/components/schemas/Addressable"},
                        {"type": "object", "properties": {"name": {"type": "string"}}},
                    ]
                },
                "Container": {
                    "type": "object",
                    "properties": {
                        "target": {
                            "oneOf": [
                                {"$ref": "#/components/schemas/LocalThing"},
                                {"$ref": "external.yaml#/components/schemas/Addressable"},
                            ]
                        }
                    },
                },
            }
        },
    }
    local_path = _write(tmp_path, "local.yaml", local_doc)

    # Convert with shared namespace (split model)
    shared = "https://api.example/"
    converter = OpenAPIToSHACLConverter(
        str(local_path),
        base_namespace=shared,
        output_dir=str(tmp_path / "out"),
        external_refs=[str(ext_path)],
        document_namespaces={"external.yaml": shared},
    )
    converter.convert()

    # Find the Container NodeShape and its target property shape
    SH = Namespace("http://www.w3.org/ns/shacl#")
    container_iri = URIRef(f"{shared}Container")

    # Get the NodeShape for Container
    container_shapes = list(converter.shacl_graph.subjects(SH.targetClass, container_iri))
    assert len(container_shapes) == 1, "Container must have exactly one NodeShape"
    container_shape = container_shapes[0]

    # Get the property shape for 'target'
    prop_shapes = list(converter.shacl_graph.objects(container_shape, SH.property))
    target_shape = None
    for ps in prop_shapes:
        path = list(converter.shacl_graph.objects(ps, SH.path))
        if path and "target" in str(path[0]):
            target_shape = ps
            break

    assert target_shape is not None, "Container must have a property shape for 'target'"

    # The property shape must have sh:xone (not a bare sh:class)
    xone_lists = list(converter.shacl_graph.objects(target_shape, SH.xone))
    assert len(xone_lists) == 1, "target must have sh:xone constraint"

    # The xone must have TWO members (LocalThing and Addressable)
    from rdflib.collection import Collection
    members = list(Collection(converter.shacl_graph, xone_lists[0]))
    assert len(members) == 2, f"sh:xone must have 2 members, got {len(members)}"

    # Both members should have sh:class constraints
    member_classes = set()
    for member in members:
        classes = list(converter.shacl_graph.objects(member, getattr(SH, 'class')))
        assert len(classes) == 1, f"Each xone member must have exactly one sh:class"
        member_classes.add(str(classes[0]))

    # Verify both LocalThing and Addressable are present
    assert f"{shared}LocalThing" in member_classes, "sh:xone must include LocalThing"
    assert f"{shared}Addressable" in member_classes, "sh:xone must include Addressable"


def test_restated_property_path_matches_declaring_document(tmp_path):
    """sh:path for restated inherited property must match declaring document's emission.

    Zero-config mode: no document_namespaces. Local class inherits external class and
    restates one of its properties. The sh:path IRI must sit under the declaring
    document's namespace (filename-derived) and match what that document's own
    conversion declares.
    """
    from rdflib import RDF, RDFS, Namespace

    from openapi_to_rdf import OpenAPIToSHACLConverter

    # common.yaml with Addressable that has href and id
    common_doc = {
        "openapi": "3.0.0",
        "info": {"title": "Common", "version": "1.0"},
        "components": {
            "schemas": {
                "Addressable": {
                    "type": "object",
                    "properties": {
                        "href": {"type": "string"},
                        "id": {"type": "string"},
                    },
                }
            }
        },
    }
    common_path = _write(tmp_path, "common.yaml", common_doc)

    # api.yaml with PolicyRef that inherits Addressable and restates href
    api_doc = {
        "openapi": "3.0.0",
        "info": {"title": "Api", "version": "1.0"},
        "components": {
            "schemas": {
                "PolicyRef": {
                    "allOf": [
                        {"$ref": "common.yaml#/components/schemas/Addressable"},
                        {
                            "type": "object",
                            "properties": {
                                "href": {"type": "string"},  # Restated from Addressable
                                "@type": {"type": "string"},
                            },
                        },
                    ]
                }
            }
        },
    }
    api_path = _write(tmp_path, "api.yaml", api_doc)

    # Convert api.yaml with NO document_namespaces (zero-config)
    api_converter = OpenAPIToSHACLConverter(
        str(api_path),
        output_dir=str(tmp_path / "out_api"),
        external_refs=[str(common_path)],
    )
    api_converter.convert()

    # Convert common.yaml standalone
    common_converter = OpenAPIToSHACLConverter(
        str(common_path),
        output_dir=str(tmp_path / "out_common"),
    )
    common_converter.convert()

    # Find the href property IRI in common.yaml's conversion
    SH = Namespace("http://www.w3.org/ns/shacl#")
    common_addressable = None
    for s in common_converter.rdf_graph.subjects(RDF.type, RDFS.Class):
        if "Addressable" in str(s):
            common_addressable = s
            break
    assert common_addressable is not None, "common.yaml must declare Addressable"

    # Get common.yaml's NodeShape for Addressable
    common_shapes = list(common_converter.shacl_graph.subjects(SH.targetClass, common_addressable))
    assert len(common_shapes) == 1
    common_shape = common_shapes[0]

    # Find href property shape in common.yaml
    common_href_path = None
    for ps in common_converter.shacl_graph.objects(common_shape, SH.property):
        paths = list(common_converter.shacl_graph.objects(ps, SH.path))
        if paths and "href" in str(paths[0]):
            common_href_path = paths[0]
            break
    assert common_href_path is not None, "common.yaml must declare href property"

    # Find PolicyRef's NodeShape in api.yaml
    api_policy_ref = None
    for s in api_converter.rdf_graph.subjects(RDF.type, RDFS.Class):
        if "PolicyRef" in str(s):
            api_policy_ref = s
            break
    assert api_policy_ref is not None, "api.yaml must declare PolicyRef"

    api_shapes = list(api_converter.shacl_graph.subjects(SH.targetClass, api_policy_ref))
    assert len(api_shapes) == 1
    api_shape = api_shapes[0]

    # Find href property shape in api.yaml (inherited from Addressable)
    api_href_path = None
    for ps in api_converter.shacl_graph.objects(api_shape, SH.property):
        paths = list(api_converter.shacl_graph.objects(ps, SH.path))
        if paths and "href" in str(paths[0]):
            api_href_path = paths[0]
            break
    assert api_href_path is not None, "api.yaml must have href property shape (inherited)"

    # The sh:path IRIs must be identical
    assert api_href_path == common_href_path, (
        f"sh:path for restated inherited property must match declaring document:\n"
        f"  api.yaml:    {api_href_path}\n"
        f"  common.yaml: {common_href_path}"
    )


def test_splitting_a_document_rewrites_its_refs():
    """The request's repro did not, which is why it proved nothing.

    Popping schemas out of components/schemas leaves `#/components/schemas/X` behind — an
    internal ref to an absent schema. external_schemas is never consulted, so the result is
    indistinguishable from an absent ancestor. The splitter must rewrite.
    """
    from scripts.measure_split_isomorphism import split_document

    document = {
        "openapi": "3.0.0",
        "info": {"title": "Api", "version": "1.0"},
        "components": {
            "schemas": {
                "Addressable": {"type": "object", "properties": {"id": {"type": "string"}}},
                "PolicyRef": {"allOf": [{"$ref": "#/components/schemas/Addressable"}]},
            }
        },
    }
    api, common = split_document(document, moved={"Addressable"})
    assert "Addressable" not in api["components"]["schemas"]
    assert api["components"]["schemas"]["PolicyRef"]["allOf"][0]["$ref"] == (
        "common.yaml#/components/schemas/Addressable"
    )
    assert "Addressable" in common["components"]["schemas"]


def test_a_split_document_attributes_exactly_as_the_whole_one_does(split_with_restatement):
    """AC-1, on the fixture that restates. Pre-fix this reports two extra pairs."""
    from scripts.measure_split_isomorphism import attribution_pairs

    whole = {
        "openapi": "3.0.0",
        "info": {"title": "Api", "version": "1.0"},
        "components": {
            "schemas": {
                **split_with_restatement["external"][COMMON],
                "PolicyRef": {
                    "allOf": [
                        {"$ref": "#/components/schemas/Addressable"},
                        split_with_restatement["api"]["components"]["schemas"]["PolicyRef"][
                            "allOf"
                        ][1],
                    ]
                },
            }
        },
    }
    namespace = "https://tmforum.org/ontology/"
    whole_pairs = attribution_pairs(
        build_mapping(whole, namespace=namespace)
    )
    split_pairs = attribution_pairs(
        build_mapping(
            split_with_restatement["api"],
            namespace=namespace,
            external_schemas=split_with_restatement["external"],
        )
    )
    assert not (split_pairs - whole_pairs), (
        f"split invented attributions absent from the whole document: {sorted(split_pairs - whole_pairs)}"
    )
