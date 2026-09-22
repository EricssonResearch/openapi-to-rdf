"""The TTL projection is a projection: it serialises ``Mapping``'s decisions, it does not take them.

Three determinations are asserted here, each brought in from the consuming project
(``snm-api-native``) with the measurement that settled it. Each is asserted by **count**, so an
empty or collapsed projection cannot pass by producing nothing:

* **A ``oneOf`` union gets no class.** Its members either co-denote ("embed the entity or point at
  it") or enumerate the subclasses of a common ancestor, and the vendor's own ``discriminator`` maps
  ``@type`` to a member and *never* to the wrapper, so no conforming payload can select it. Emitting
  one produced **14 unreachable classes and 14 unreachable context terms**. The union still
  constrains the property that accepts it, via ``sh:xone`` over the members.
* **A transport envelope gets a distinct namespace and a marker, never omission.** Omitting them
  broke nesting: TMF641's ``ServiceOrderCreateEvent`` lifted **1** triple through the envelope
  against **76** for the inner ``ServiceOrder`` standalone, because the path to the domain object was
  gone. So they are emitted, under the transport namespace, marked, **and with their
  ``rdfs:subClassOf``** — a projection that emitted subclass edges for domain classes only left 0 of
  14 owed transport edges in the consuming project.
* **A reference contributes an edge to its referent, never a type.** ``sh:class X`` requires the
  value node to have ``rdf:type X``, so aiming it at a ``*Ref`` asserts that the referenced entity
  *is* a reference while the system that owns it says it is a ``Service``. The ``*Ref`` class is
  still declared — property domains and the inheritance chain depend on it — and flagged, and nothing
  is ever typed as one.

The probe puts ``type: object`` **and** ``allOf`` on two schemas on purpose. That combination is the
failure region for the subclass loop: ``_type_clause`` dispatched on ``type`` first, so every such
schema silently lost its inheritance edges and its ``allOf``-declared properties — **179 of the 816**
parent edges the three TM Forum v5 documents owe, and 0 of 56 on the 3GPP corpus, which has nine such
schemas but none with a ``$ref`` parent. A probe without it cannot exhibit the defect.
"""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml
from rdflib import Graph, Literal, Namespace, URIRef
from rdflib.namespace import RDF, RDFS

SH = Namespace("http://www.w3.org/ns/shacl#")

BASE_PREFIX = "http://example.org/probe/"
NS = Namespace(f"{BASE_PREFIX}rdf/TtlProbe#")
TRANSPORT = Namespace(f"{BASE_PREFIX}transport/")

PROBE = {
    "openapi": "3.0.0",
    "info": {"title": "TtlProbe", "version": "1.0"},
    "components": {
        "schemas": {
            "Addressable": {
                "type": "object",
                "properties": {
                    "href": {"type": "string", "format": "uri"},
                    "id": {"type": "string"},
                },
            },
            "Service": {
                "allOf": [
                    {"$ref": "#/components/schemas/Addressable"},
                    {"type": "object", "properties": {"name": {"type": "string"}}},
                ]
            },
            # `type: object` AND `allOf`: the subclass-loop failure region.
            "ServiceRef": {
                "type": "object",
                "allOf": [
                    {"$ref": "#/components/schemas/Addressable"},
                    {"type": "object", "properties": {"referredType": {"type": "string"}}},
                ],
            },
            # The union. No class; its members constrain whatever property accepts it.
            "ServiceRefOrValue": {
                "oneOf": [
                    {"$ref": "#/components/schemas/Service"},
                    {"$ref": "#/components/schemas/ServiceRef"},
                ]
            },
            "ServiceOrder": {
                "type": "object",
                "properties": {
                    "service": {"$ref": "#/components/schemas/ServiceRefOrValue"},
                    "place": {"$ref": "#/components/schemas/ServiceRef"},
                },
            },
            # A transport envelope, also in the `type: object` + `allOf` failure region.
            "ServiceOrderCreateEvent": {
                "type": "object",
                "allOf": [
                    {"$ref": "#/components/schemas/Addressable"},
                    {
                        "type": "object",
                        "properties": {"event": {"$ref": "#/components/schemas/ServiceOrder"}},
                    },
                ],
            },
        }
    },
}


@pytest.fixture(scope="module")
def graphs(tmp_path_factory) -> tuple[Graph, Graph]:
    """The two serialised graphs, read back from disk rather than from memory.

    Read back deliberately: a decision that survives in ``self.rdf_graph`` but not through Turtle
    serialisation is not a decision this tool published.
    """
    from openapi_to_rdf import OpenAPIToSHACLConverter

    tmp_path = tmp_path_factory.mktemp("ttlprobe")
    spec = tmp_path / "TtlProbe.yaml"
    spec.write_text(yaml.safe_dump(PROBE, sort_keys=False), encoding="utf-8")
    converter = OpenAPIToSHACLConverter(
        str(spec), output_dir=str(tmp_path / "out"), base_namespace_prefix=BASE_PREFIX
    )
    converter.run()
    out = tmp_path / "out"
    rdf = Graph().parse(str(next((out / "rdf").glob("*.ttl"))), format="turtle")
    shacl = Graph().parse(str(next((out / "shacl").glob("*.ttl"))), format="turtle")
    return rdf, shacl


def _prop_shapes_for(shacl: Graph, local_name: str) -> list:
    """Every PropertyShape whose ``sh:path`` local name is ``local_name``.

    Splits on ``#`` **and** ``/``: the property separator changed from ``#`` to ``/`` on 2026-09-22
    (see ``property_uri.class_namespace``), and a helper that split on ``#`` alone returned the whole
    IRI, matched nothing, and failed two tests with an empty list — which reads as "the shape is
    missing" rather than "the lookup is wrong". Separator-agnostic so it cannot mislead that way
    again.
    """
    return [
        shape
        for shape, path in shacl.subject_objects(SH.path)
        if str(path).replace("#", "/").rsplit("/", 1)[-1] == local_name
    ]


# --- The probe is not vacuous ------------------------------------------------------------------


def test_the_probe_actually_produced_a_vocabulary(graphs) -> None:
    """Guard the guards: every assertion below is about absence, so presence must be established.

    Four classes are owed — `Addressable`, `Service`, `ServiceRef`, `ServiceOrder` — plus the
    envelope, and NOT the union. An exact set rather than a floor, so a schema going missing fails
    here rather than making a later absence assertion pass for the wrong reason.
    """
    rdf, shacl = graphs
    declared = {str(c).rsplit("#", 1)[-1].rsplit("/", 1)[-1] for c in rdf.subjects(RDF.type, RDFS.Class)}
    assert declared == {
        "Addressable",
        "Service",
        "ServiceRef",
        "ServiceOrder",
        "ServiceOrderCreateEvent",
    }, declared
    assert len(list(shacl.subject_objects(SH.targetClass))) == 5, list(
        shacl.subject_objects(SH.targetClass)
    )


# --- S2: a oneOf union gets no class, but still constrains the property -------------------------


def test_no_class_is_emitted_for_a_oneof_union(graphs) -> None:
    """The wrapper is absent from BOTH graphs, in every position, not merely un-typed.

    Counting occurrences rather than checking for the ``rdf:type`` triple: a leftover
    ``rdfs:subClassOf`` or ``sh:class`` naming the wrapper would be an axiom pointing at an IRI no
    document declares, which is the failure this determination exists to prevent.
    """
    rdf, shacl = graphs
    union = NS["ServiceRefOrValue"]
    occurrences = [
        (g_name, s, p, o)
        for g_name, g in (("rdf", rdf), ("shacl", shacl))
        for s, p, o in g
        if union in (s, p, o)
    ]
    assert occurrences == [], occurrences


def test_the_union_still_constrains_the_property_that_accepts_it(graphs) -> None:
    """The constraint survives on the property: ``sh:xone`` over the members, both of them."""
    _rdf, shacl = graphs
    shapes = _prop_shapes_for(shacl, "service")
    assert len(shapes) == 1, shapes
    xone_lists = list(shacl.objects(shapes[0], SH.xone))
    assert len(xone_lists) == 1, f"expected one sh:xone on ServiceOrder/service, got {xone_lists}"
    from rdflib.collection import Collection

    members = list(Collection(shacl, xone_lists[0]))
    assert len(members) == 2, f"expected the union's 2 members, got {members}"
    # And each member constrains to a real class, the reference resolved to its referent (S8).
    constrained = {
        str(c) for m in members for c in shacl.objects(m, URIRef(str(SH) + "class"))
    }
    assert constrained == {str(NS["Service"])}, constrained


def test_the_union_property_gets_no_rdfs_range(graphs) -> None:
    """A union has no single target, and ``rdfs:range`` propagates under entailment.

    After the reference collapse both members denote ``Service``, so a range WOULD be sound here —
    and it is emitted. This test pins the count, not the absence, so the reason is legible: one
    target after collapse means one range.
    """
    rdf, _shacl = graphs
    service_prop = URIRef(f"{BASE_PREFIX}rdf/TtlProbe/ServiceOrder/service")
    ranges = list(rdf.objects(service_prop, RDFS.range))
    assert ranges == [NS["Service"]], ranges


# --- S7: a transport envelope gets a namespace and a marker, never omission ---------------------


def test_the_envelope_class_is_emitted_under_the_transport_namespace(graphs) -> None:
    rdf, shacl = graphs
    envelope = TRANSPORT["ServiceOrderCreateEvent"]
    assert (envelope, RDF.type, RDFS.Class) in rdf, "the envelope must be emitted, not omitted"
    assert (NS["ServiceOrderCreateEvent"], RDF.type, RDFS.Class) not in rdf, (
        "the envelope must NOT also be emitted under the domain namespace"
    )
    assert len(list(shacl.subjects(SH.targetClass, envelope))) == 1


def test_the_envelope_carries_the_transport_marker(graphs) -> None:
    """Marked, so a consumer wanting a pure domain ontology can filter it out by predicate."""
    rdf, _shacl = graphs
    marked = set(rdf.subjects(TRANSPORT["isTransportEnvelope"], Literal(True)))
    assert marked == {TRANSPORT["ServiceOrderCreateEvent"]}, marked


def test_the_envelope_keeps_its_subclass_edge(graphs) -> None:
    """The subclass loop must iterate EVERY class, envelopes included.

    ``ServiceOrderCreateEvent`` declares ``type: object`` and ``allOf`` together, which is exactly
    the shape whose edges were being dropped. Inverting the guarded branch (removing the ``allOf``
    handling added to ``_handle_object_type``) makes this assertion fail — observed, not assumed.
    """
    rdf, _shacl = graphs
    parents = set(rdf.objects(TRANSPORT["ServiceOrderCreateEvent"], RDFS.subClassOf))
    assert parents == {NS["Addressable"]}, parents


def test_the_envelope_properties_live_with_their_class(graphs) -> None:
    """A property IRI must not claim a domain namespace its own ``rdfs:domain`` does not have."""
    rdf, _shacl = graphs
    envelope = TRANSPORT["ServiceOrderCreateEvent"]
    owned = [str(p) for p in rdf.subjects(RDFS.domain, envelope)]
    assert len(owned) == 1, owned
    assert owned[0] == f"{BASE_PREFIX}transport/ServiceOrderCreateEvent/event", owned


# --- S8: a reference contributes an edge to its referent, never a type --------------------------


def test_the_reference_class_is_declared_and_flagged(graphs) -> None:
    """Declared, because property domains and the inheritance chain depend on it; and flagged."""
    rdf, _shacl = graphs
    ref = NS["ServiceRef"]
    assert (ref, RDF.type, RDFS.Class) in rdf
    assert (ref, TRANSPORT["isSerialisationArtifact"], Literal(True)) in rdf
    assert set(rdf.objects(ref, TRANSPORT["refersTo"])) == {NS["Service"]}
    # Exactly one reference in the probe, so a rule that fired on everything would fail here.
    assert set(rdf.subjects(TRANSPORT["refersTo"], None)) == {ref}


def test_the_reference_also_keeps_its_subclass_edge(graphs) -> None:
    """`ServiceRef` is the second `type: object` + `allOf` schema: 1 edge owed, 1 emitted."""
    rdf, _shacl = graphs
    assert set(rdf.objects(NS["ServiceRef"], RDFS.subClassOf)) == {NS["Addressable"]}


def test_nothing_is_ever_typed_as_a_reference(graphs) -> None:
    """No ``sh:class`` and no ``rdfs:range`` names the ``*Ref``; the referent is what they name.

    Counting both sides: the ``place`` property points at ``Service``, and the ``ServiceRef`` IRI
    appears nowhere as a value type. Asserting the positive too, because "no sh:class anywhere" would
    also be satisfied by a projection that emitted no constraints at all.
    """
    rdf, shacl = graphs
    ref = NS["ServiceRef"]
    sh_class = URIRef(str(SH) + "class")
    typed_as_ref = [(s, p) for s, p, o in shacl if o == ref and p == sh_class]
    typed_as_ref += [(s, p) for s, p, o in rdf if o == ref and p == RDFS.range]
    assert typed_as_ref == [], typed_as_ref

    place = URIRef(f"{BASE_PREFIX}rdf/TtlProbe/ServiceOrder/place")
    assert list(rdf.objects(place, RDFS.range)) == [NS["Service"]], list(
        rdf.objects(place, RDFS.range)
    )
    shapes = _prop_shapes_for(shacl, "place")
    assert len(shapes) == 1, shapes
    assert list(shacl.objects(shapes[0], sh_class)) == [NS["Service"]]


# --- The routing itself -------------------------------------------------------------------------


def test_every_class_the_mapping_declares_is_emitted_and_no_others(graphs) -> None:
    """The projection's class set IS ``Mapping.classes``, reconciled by IRI rather than by count.

    This is the routing claim in one assertion: if the emitter re-derived which schemas are classes,
    the two sets could differ — and before Task 8 they did, because the emitter minted a class for a
    ``oneOf`` union and ``Mapping`` did not.
    """
    from openapi_to_rdf import OpenAPIToSHACLConverter

    rdf, _shacl = graphs
    import tempfile

    with tempfile.TemporaryDirectory() as tmp:
        spec = Path(tmp) / "TtlProbe.yaml"
        spec.write_text(yaml.safe_dump(PROBE, sort_keys=False), encoding="utf-8")
        converter = OpenAPIToSHACLConverter(
            str(spec), output_dir=str(Path(tmp) / "out"), base_namespace_prefix=BASE_PREFIX
        )
        expected = {fact.iri for fact in converter.mapping.classes.values()}

    emitted = {str(c) for c in rdf.subjects(RDF.type, RDFS.Class)}
    assert emitted == expected, {"only emitted": emitted - expected, "only mapped": expected - emitted}


# --- A pointer is resolved relative to the document that wrote it ------------------------------


def test_an_external_unions_members_resolve_against_their_own_document(tmp_path) -> None:
    """Expanding a union from another document must not resolve its members against ours.

    A bare ``#/components/schemas/X`` inside ``Sibling.yaml`` means *that file's* ``X``. Returning a
    union's members verbatim made the caller look for ``X`` locally, where it does not exist, so the
    expansion **manufactured** references that no document contains.

    Measured on the 3GPP corpus with all siblings loaded (38 specs): **206 unresolved external
    $refs over 14 specs**. These are genuine corpus gaps where a schema references a name declared
    in a sibling document (e.g., ``Dn``, ``PlmnInfo``, ``DnRo`` in ``TS28623_ComDefs.yaml``) but
    not successfully resolved. The union expansion mechanism can manufacture additional unresolved
    refs by returning members verbatim, making the caller look for ``X`` locally where it does not
    exist. TM Forum's 3 specs are self-contained with 0 unresolved refs.

    This is the fourth recorded instance of origin-blind ``$ref`` handling across these repositories
    (``snm-api-native``, ``docs/specs/2026-09-10-kiota-absorption.md`` §4.7), which is why it gets a
    test of its own rather than a comment.
    """
    from openapi_to_rdf import OpenAPIToSHACLConverter

    # The sibling owns both the union AND what its members point at, nested under `items` so the
    # recursive half of the rebase is exercised — a top-level-only rebase passes without it.
    sibling = {
        "openapi": "3.0.0",
        "info": {"title": "Sibling", "version": "1.0"},
        "components": {
            "schemas": {
                "Away": {"type": "object", "properties": {"k": {"type": "string"}}},
                "AwayUnion": {
                    "oneOf": [
                        {"$ref": "#/components/schemas/Away"},
                        {"type": "array", "items": {"$ref": "#/components/schemas/Away"}},
                    ]
                },
            }
        },
    }
    local = {
        "openapi": "3.0.0",
        "info": {"title": "Local", "version": "1.0"},
        "components": {
            "schemas": {
                "Holder": {
                    "type": "object",
                    # `Away` is declared ONLY in the sibling. If the members are resolved locally,
                    # this becomes an unresolved reference to a schema no document is missing.
                    "properties": {
                        "far": {"$ref": "Sibling.yaml#/components/schemas/AwayUnion"}
                    },
                }
            }
        },
    }
    sibling_path = tmp_path / "Sibling.yaml"
    sibling_path.write_text(yaml.safe_dump(sibling), encoding="utf-8")
    spec = tmp_path / "Local.yaml"
    spec.write_text(yaml.safe_dump(local), encoding="utf-8")

    converter = OpenAPIToSHACLConverter(
        str(spec),
        output_dir=str(tmp_path / "out"),
        base_namespace_prefix=BASE_PREFIX,
        external_refs=[str(sibling_path)],  # Load sibling so external union can resolve
    )
    converter.convert()

    assert converter.unresolved_references == [], (
        "union expansion manufactured references to a schema the sibling declares: "
        f"{converter.unresolved_references}"
    )
    # Not vacuous: the constraint must actually be there, and it must name the SIBLING's namespace.
    sh_class = URIRef(str(SH) + "class")
    named = {str(o) for _s, _p, o in converter.shacl_graph.triples((None, sh_class, None))}
    assert named == {f"{BASE_PREFIX}rdf/Sibling#Away"}, named
