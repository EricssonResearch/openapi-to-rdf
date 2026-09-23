"""One derived fact set that every artifact is a projection of.

An OpenAPI document supports at least four derived artifacts: an RDF/RDFS vocabulary, SHACL
shapes, an OpenAPI **Overlay** carrying ontology annotations, a JSON-LD ``@context``, and a
Hydra operation graph. Deriving each one by its own walk over the document means four
implementations of the same decisions — *which class declares this property*, *what is this
property's datatype*, *is this value an IRI or a literal* — and those implementations drift.

That is measured, not feared: two independently-derived class mappings over one corpus in the
consuming project (``snm-api-native``) agreed on the 75 schema names they shared and
**disagreed on 132**. Nothing detected it, because nothing compared them. This module exists so
there is one object to compare against: ``build_mapping`` decides once, and every projection
reads the decision rather than re-deriving it.

Decisions encoded here, each with its provenance
------------------------------------------------

``allOf`` **flattening.** A schema's properties come from its top-level ``properties`` block
*and* from its inline ``allOf`` members, because TM Forum and 3GPP both declare properties in an
``allOf`` alongside a ``$ref`` to the parent. Reading only the top-level block loses most of the
model. :func:`flattened_properties` is the single implementation; ``analyzer.checks`` consumes
it for its orphan-required check, which is where the determination was first made (the same
defect has been found in three separate repositories).

**Declaring-class attribution.** A property belongs to the highest ancestor that declares it,
not to the leaf class that mentions it — an ontology declares an inherited property once.
Measured on a TM Forum corpus (TMF641), declaring attribution resolved **93.9%** of
``(class, property)`` pairs against an independently-authored TBox where leaf attribution
resolved **68.5%**; the lower number was an artifact of the instrument, not a gap in the TBox.
``shacl_converter._find_declaring_class`` applies the same rule over an in-progress
``rdflib.Graph``; :func:`_resolve_declaring_class` applies it over ``ClassFact.parents``. The
rule is one rule, stated in both docstrings; see this module's report for why the graph-backed
walk could not simply be called from here.

**Injective local names.** Local names are passed through
:func:`openapi_to_rdf.property_uri.format_local_name`, which deliberately does *not* fold
``-`` to ``_``: that folding collapses ``my-prop`` and ``my_prop`` onto one IRI, which is a
silent collision rather than a normalisation.

**IRI-valued properties.** ``is_iri_valued`` is true when the property declares ``format: uri``
**or** is named ``href``. The second half is not a spelling heuristic, and it must not be
"cleaned up" into one: TM Forum declares ``href`` on ``Addressable`` as a bare
``{"type": "string", "description": "Hyperlink reference"}`` and applies ``format: uri``
inconsistently and *never* to ``href`` — the most important URL field in the standard. Measured
across three v5 documents: TMF641 marks only ``referenceError`` and ``serviceOrderHref``, TMF622
only ``@schemaLocation``, TMF620 ``url`` / ``parentSpecificationHref`` / ``@schemaLocation``,
while ``href`` appears on 9 TMF641 classes and is marked on none. Recorded as finding **F10** in
``snm-api-native`` (``docs/vendor-spec-findings.md``); the rule lives there as
``IRI_VALUED_NAMES`` in ``scripts/emit_tbox.py``. A purely derived rule would catch the scattering
and miss the obvious one. It is deliberately *not* "ends with Href" — ``serviceOrderHref`` is
already caught by its ``format``, and guessing from spelling is how a rule starts inventing
meaning. This half is a convention with a source (TM Forum's own base schema), not a derivation,
and saying so is the point.

**Transport envelopes are classified, never dropped** — and they get their own namespace and a
marker. Notification wrappers, event payloads and JSON Patch documents are wire plumbing rather
than domain concepts, so they are flagged (``ClassFact.is_transport``), minted under
:data:`DEFAULT_TRANSPORT_NAMESPACE` rather than the document's own namespace, and left in.
Excluding them outright broke nesting, measured on TMF641's own ``ServiceOrderCreateEvent``, where
the inner ``ServiceOrder`` lifted **76** triples standalone and **1** through the envelope, because
``event`` and ``serviceOrder`` had no terms and the path to the domain object was gone. So
*omission is not the safe option*: a consumer wanting a pure domain ontology filters on the marker,
and a consumer parsing wire format traverses through it. The classification mirrors
``snm_api.openapi_profile.is_plumbing`` and the namespace/marker mirror ``TRANSPORT_NS`` /
``TRANSPORT_MARKER`` in ``snm-api-native``'s ``scripts/emit_tbox.py``, which is in a different
distribution and cannot be imported here.

**A ``oneOf`` union gets no class.** A named schema whose body is a ``oneOf`` is a JSON Schema
workaround for two things RDF does not need: *serialisation* (embed the entity or point at it) and
*taxonomy* (enumerate the subclasses of a common ancestor, because JSON Schema cannot say "any
subclass of X"). Neither is a new kind of thing, and TM Forum's own ``discriminator`` maps
``@type`` to a member and **never** to the wrapper, so no conforming payload can select it.
Measured in ``snm-api-native``: emitting one produced **14 unreachable classes and 14 unreachable
context terms**. :func:`is_json_only_union` keys on the ``oneOf`` **shape**, not on the name — a
name test (``endswith("RefOrValue")``) missed ``PartyRefOrPartyRoleRef`` and minted a referent
called ``PartyRefOrPartyRole``, which names nothing in any TM Forum document. The union still
constrains the *property* that accepts it: :func:`target_classes_for` expands it into its members,
which a projection emits as ``sh:or``/``sh:xone``.

**A reference contributes an edge to its referent, never a type.** ``ServiceRef`` keeps its class,
because property domains (``EntityRef/href``) and the inheritance chain depend on it, but it is a
serialisation artifact: typing a node ``a tmf:ServiceRef`` asserts the *referenced entity* is a
reference, while the system that owns it says ``a svc:Service`` — one node, two classes,
disagreeing only over which side embedded it. So ``ClassFact.referent`` records what the reference
denotes (:func:`referent_name`), a projection emits an **edge** to it, and
:func:`target_classes_for` resolves a reference-valued property to the referent. The referent name
is minted **by convention, not looked up**: only one of TMF641/TMF622 declares a ``Party`` schema,
so a presence check resolved ``PartyRef`` to ``Party`` in one document and left it as ``PartyRef``
in the other — two specs disagreeing about one term, which is worse than the problem it fixed.

**Ranges are recorded as facts, not as axioms.** ``PropertyFact.target_classes`` is a tuple
because a property may constrain to several classes. ``rdfs:range`` may be emitted only for a
*single* target, because a range propagates under RDFS entailment and an invented range is a
false axiom rather than a loose constraint. That rule belongs to the projection that emits
``rdfs:range``; this module only records how many targets there are.

**Operations.** ``paths`` are in scope here, which the README's "schemas only" disclaimer
predates (Task 9c updates it). A list endpoint records the **item** class rather than an
anonymous array, and an operation with no body records ``None`` rather than guessing — measured,
12 of TMF641's operations and 32 of TMF620's return no body. Operation IRIs are minted by *this
project* from the method and path and assert no external authority.

Nothing here writes the input document or touches the filesystem.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Any
from urllib.parse import quote

from rdflib.namespace import XSD

from openapi_to_rdf.property_uri import (
    format_local_name,
    namespace_for_schema,
    property_uri,
)

#: HTTP methods an OpenAPI Path Item Object may carry (OpenAPI 3.1 §4.8.10). Every other key of a
#: path item (``parameters``, ``summary``, ``servers``, …) is not an operation.
HTTP_METHODS = ("get", "put", "post", "delete", "options", "head", "patch", "trace")

#: Property names that are IRI-valued regardless of their declared ``format``. See the module
#: docstring: this is TM Forum's own base-schema convention, recorded as finding F10, and NOT a
#: spelling heuristic. Adding a name here is a decision about a vendor's convention.
IRI_VALUED_NAMES = frozenset({"href"})

#: ``_FVO``/``_MVO`` are TM Forum's create/update projections of a resource: the same domain
#: class under another name. Stripped before classifying a name.
VARIANT_SUFFIXES = ("_FVO", "_MVO")

#: ``format`` → XSD datatype, for ``type: string``. The single copy: the emitter held four
#: hand-written duplicates of this map and now calls :func:`xsd_for_string_format`.
STRING_FORMAT_DATATYPES = {
    "date-time": XSD.dateTime,
    "full-time": XSD.time,
    "date-month": XSD.gMonth,
    "date-mday": XSD.gMonthDay,
}

#: Namespace for transport envelopes (see the module docstring). **This namespace is OURS** and
#: asserts no external authority; it exists so a consumer can tell a wire wrapper from a domain
#: class by IRI alone, and it is overridable per conversion
#: (``build_mapping(..., transport_namespace=...)``). Mirrors ``TRANSPORT_NS`` in
#: ``snm-api-native``'s ``scripts/emit_tbox.py``.
DEFAULT_TRANSPORT_NAMESPACE = "http://ericsson.com/models/transport/"

#: Marker predicate asserting that a class is a wire envelope rather than a domain concept.
#: Relative to the transport namespace in force. **Ours**, like the namespace.
TRANSPORT_MARKER_LOCAL = "isTransportEnvelope"

#: Marker predicate asserting that a class is a serialisation artifact — a *mention* of an entity
#: rather than a kind of entity — and the predicate pointing at what it mentions. **Ours.**
SERIALISATION_ARTIFACT_LOCAL = "isSerialisationArtifact"
REFERS_TO_LOCAL = "refersTo"

_MAX_REF_DEPTH = 10

_VARIANT_SUFFIX_RE = re.compile(r"_(FVO|MVO)$")


def xsd_for_string_format(fmt: Any, default: Any = XSD.string) -> Any:
    """The XSD datatype an OpenAPI ``format`` names for a ``type: string``.

    One implementation, because four hand-written copies of this map is four places for a new
    format to be added in three of them.
    """
    return STRING_FORMAT_DATATYPES.get(fmt, default)


@dataclass(frozen=True)
class ClassFact:
    """One named schema, as a class.

    Attributes:
        iri: The class IRI, minted under the schema's own namespace.
        parents: Names of the classes this schema composes via a top-level ``allOf`` ``$ref``.
            Recorded for *every* class, transport envelopes included: a projection that emits
            ``rdfs:subClassOf`` for domain classes only left 0 of 14 owed edges in the consuming
            project.
        is_transport: True for a wire envelope (see the module docstring). Its ``iri`` is minted
            under the transport namespace, so a consumer can filter plumbing by IRI alone.
        referent: For a ``*Ref`` schema, the name of what the reference *denotes*
            (``ServiceRef`` → ``Service``); None otherwise. A reference contributes an **edge** to
            this name, never a type — see the module docstring. Minted by convention, so it may
            name a class this document does not declare.
    """

    iri: str
    parents: tuple[str, ...]
    is_transport: bool
    referent: str | None = None


@dataclass(frozen=True)
class PropertyFact:
    """One property, attributed to the class that declares it.

    Attributes:
        iri: Class-scoped property IRI, minted under the *declaring* class.
        declaring_class: Name of the highest ancestor that declares this property.
        target_classes: Names of the classes this property may point at. Empty for a literal.
            A projection emitting ``rdfs:range`` must emit it only when there is exactly one.
        datatype: XSD datatype IRI as a string, or None where the value is not a literal.
        is_iri_valued: The value is a URL denoting a resource, not a string about one.
        min_count: Lower bound; 1 when required, ``minItems`` when stricter.
        max_count: Upper bound; 1 for a single-valued property, ``maxItems`` or None for a list.
    """

    iri: str
    declaring_class: str
    target_classes: tuple[str, ...]
    datatype: str | None
    is_iri_valued: bool
    min_count: int
    max_count: int | None


@dataclass(frozen=True)
class OperationFact:
    """One method on one path.

    ``returns_class`` and ``accepts_class`` are class *names*, so a projection resolves them
    through ``Mapping.classes`` and emits the IRI the vocabulary declares rather than re-deriving
    one from a schema name. Either is None when the operation has no body, or when its schema
    resolves to something that is not a class in this document — None, never a placeholder.

    ``has_request_body`` / ``has_response_body`` exist because that sentence describes **two
    different facts** and a consumer needs to tell them apart. A ``DELETE`` returning nothing and a
    response whose ``$ref`` could not be resolved both give ``returns_class = None``: the first is
    correct, the second is a defect. Without the flags, any report of resolution quality has to
    treat them alike, and the denominator it prints is wrong.

    The measurement that earned this, from ``snm-api-native``'s operation emitter: counting bodyless
    operations as unresolved reported **8 of 20** resolved on TMF641 where the truth is **8 of 8** —
    the other 12 are DELETEs and notification listeners that correctly return nothing. A 40% success
    rate and a 100% one, from the same graph.
    """

    iri: str
    method: str
    path_template: str
    returns_class: str | None
    accepts_class: str | None
    status_codes: tuple[int, ...]
    #: Whether the document declares a body at all, independent of whether its class resolved.
    has_request_body: bool = False
    has_response_body: bool = False


@dataclass(frozen=True)
class Mapping:
    """The derived fact set. Every artifact is a projection of this object.

    ``properties`` is keyed by local name because that is the key a JSON-LD context and an
    overlay need. Two unrelated classes may legitimately declare the same local name with
    different ranges (``startTime`` on both ``TimeWindow`` and ``PerfMetricJob`` in 3GPP
    TS 28.623), and those are distinct properties with distinct IRIs. So the by-name index keeps
    the first declaration in document order and ``properties_by_class`` — keyed
    ``(declaring_class, local_name)`` — keeps them all. Nothing is merged here: merging is an
    opinionated modelling step, the same determination ``property_index`` records for its
    collision report.
    """

    classes: dict[str, ClassFact]
    properties: dict[str, PropertyFact]
    operations: dict[str, OperationFact]
    properties_by_class: dict[tuple[str, str], PropertyFact]
    #: ``info.version`` verbatim, or None. Here rather than re-read by a projection, because this
    #: object is meant to be everything one walk of the document yields — a projection that opened
    #: the document again could disagree with the rest of the fact set about what it says.
    api_version: str | None = None

    def declaring_class(self, class_name: str, property_name: str) -> str:
        """The class that declares ``property_name`` for a node of ``class_name``.

        **The single entry point for declaring-class attribution.** The SHACL emitter used to own a
        second implementation that walked ``rdfs:subClassOf`` in the ``rdflib.Graph`` it was still
        building and probed for an already-emitted ``rdfs:domain``; because the graph was incomplete,
        its answer depended on the order schemas appear in the document, and where a parent was
        declared after its child it fell back to the leaf. Measured before the collapse: **23 of 57**
        non-trivial attributions differed on TMF641, **12 of 32** on TMF622 and **8 of 49** on
        TMF620 — 43 of 138 across the three — while 3GPP showed **0 of 2,822**, because that corpus
        contains no non-trivial attribution at all and so no 3GPP-only check could see the defect.

        Resolved against ``properties_by_class``, which is keyed by *declaring* class, so asking
        "does this ancestor own the property" is a lookup rather than a second walk over the
        document. Equivalent to :func:`_resolve_declaring_class` by construction and reconciled
        against it, over a different data path, by
        ``tests/test_mapping.py::test_both_declaring_class_implementations_agree``.

        Returns ``class_name`` when nothing in the ancestry declares the property, and for a name
        this mapping holds no class for — an absent fact, never a guess.
        """
        if class_name not in self.classes:
            return class_name
        ancestry: list[str] = []
        seen: set[str] = set()
        queue = [class_name]
        while queue:
            current = queue.pop(0)
            if current in seen:
                continue
            seen.add(current)
            ancestry.append(current)
            queue.extend(self.classes[current].parents if current in self.classes else ())
        ancestry.reverse()  # most general ancestor first
        for ancestor in ancestry:
            if (ancestor, property_name) in self.properties_by_class:
                return ancestor
        return class_name

    def property_collisions(self) -> dict[str, tuple[PropertyFact, ...]]:
        """Local names declared by more than one class, with every fact for each."""
        grouped: dict[str, list[PropertyFact]] = {}
        for (_declaring, local_name), fact in self.properties_by_class.items():
            grouped.setdefault(local_name, []).append(fact)
        return {
            name: tuple(facts) for name, facts in sorted(grouped.items()) if len(facts) > 1
        }


def base_name(schema_name: str) -> str:
    """``ServiceOrder_FVO`` → ``ServiceOrder``: the domain class both names denote."""
    for suffix in VARIANT_SUFFIXES:
        if schema_name.endswith(suffix):
            return schema_name[: -len(suffix)]
    return schema_name


def is_transport(schema_name: str) -> bool:
    """True for a wire envelope rather than a domain concept.

    Notification envelopes, event payload wrappers and RFC 6902 patch documents are transport
    plumbing. They are flagged rather than dropped — see the module docstring for the 76-vs-1
    triple measurement that decided that. Mirrors ``snm_api.openapi_profile.is_plumbing`` in
    ``snm-api-native``, which lives in a separate distribution and cannot be imported.
    """
    base = base_name(schema_name)
    return (
        base.endswith(("Event", "EventPayload"))
        or base.startswith("JsonPatch")
        or base in {"InformationRequiredArray", "Hub"}
    )


def is_json_only_union(schema_def: Any) -> bool:
    """True for a named schema that exists only because JSON Schema lacks a way to say something.

    A ``oneOf`` at the top of a named schema encodes either **serialisation** ("embed the entity or
    point at it", whose members co-denote) or **taxonomy** ("any subclass of X", enumerated because
    JSON Schema cannot quantify). Neither is a new kind of thing, and both are already expressed
    elsewhere — co-denotation by ``ClassFact.referent``, subsumption by ``rdfs:subClassOf``. So no
    class is declared for it. See the module docstring for the 14-unreachable-classes measurement
    and for why this is keyed on the **shape** rather than on the name.

    What the union genuinely carries is a constraint on the *property* that accepts it, and that is
    preserved: :func:`target_classes_for` expands the members, which a projection emits as
    ``sh:or``/``sh:xone``.
    """
    return isinstance(schema_def, dict) and bool(schema_def.get("oneOf"))


def referent_name(schema_name: str) -> str | None:
    """``ServiceRef`` → ``Service``, preserving any ``_FVO``/``_MVO`` suffix; None when not a ref.

    Declines when stripping the trailing ``Ref`` leaves another ``Ref`` behind. That guard is
    load-bearing rather than defensive: ``RelatedPartyRefOrPartyRoleRef`` is not a reference at all
    — it is TM Forum's reified related-party class — and it was being flagged a serialisation
    artifact and pointed at ``RelatedPartyRefOrPartyRole``, which names nothing. A compound
    union-style name cannot be turned into its referent by removing one suffix.
    """
    suffix = _VARIANT_SUFFIX_RE.search(schema_name)
    stem = _VARIANT_SUFFIX_RE.sub("", schema_name)
    if not stem.endswith("Ref") or stem == "Ref":
        return None
    remainder = stem[: -len("Ref")]
    if not remainder or "Ref" in remainder:
        return None
    return remainder + (suffix.group(0) if suffix else "")


def flattened_properties(schema_def: Any) -> dict[str, Any]:
    """Return a schema's own declared properties, ``allOf`` members included.

    Collects the top-level ``properties`` block AND the ``properties`` of every ``allOf`` member.
    TM Forum and 3GPP both declare properties inside an ``allOf`` alongside a ``$ref`` to the
    parent; reading only the top-level block loses them. ``$ref`` members are *not* followed —
    those properties belong to the referenced schema and are collected when it is visited, which
    is what makes declaring-class attribution possible.

    This is the single implementation of the flattening determination (first made in
    ``analyzer.checks.orphan_required``, which now calls it: 3 spurious errors on the TMF v5
    specs → 0 of 888 schemas with real orphans). Two callers, one rule.
    """
    if not isinstance(schema_def, dict):
        return {}
    properties: dict[str, Any] = {}
    for member in schema_def.get("allOf") or []:
        if isinstance(member, dict) and isinstance(member.get("properties"), dict):
            properties.update(member["properties"])
    if isinstance(schema_def.get("properties"), dict):
        properties.update(schema_def["properties"])
    return properties


def flattened_required(schema_def: Any) -> set[str]:
    """Return the required property names, ``allOf`` members included.

    Symmetric with :func:`flattened_properties`: a schema that declares a property in an
    ``allOf`` member declares its ``required`` there too, and reading only the top-level
    ``required`` list reports properties as optional that the contract makes mandatory.
    """
    if not isinstance(schema_def, dict):
        return set()
    required: set[str] = set(schema_def.get("required") or [])
    for member in schema_def.get("allOf") or []:
        if isinstance(member, dict):
            required |= set(member.get("required") or [])
    return required


def is_primitive_def(schema_def: Any, schemas: dict[str, Any], depth: int = 0) -> bool:
    """True when a schema definition resolves to a primitive (literal-valued) type.

    Follows ``$ref`` and ``items``, and treats an ``anyOf``/``oneOf`` as primitive only when
    every option is. Used to decide what is a class: a primitive named schema is a datatype, not
    a class, and minting a class for one produces a term no payload can instantiate.
    ``shacl_converter._is_primitive_def`` delegates here so both agree by construction.
    """
    if not isinstance(schema_def, dict) or depth > _MAX_REF_DEPTH:
        return False
    if "$ref" in schema_def:
        ref = schema_def["$ref"]
        if isinstance(ref, str) and ref.startswith("#/components/schemas/"):
            return is_primitive_def(schemas.get(ref.split("/")[-1]), schemas, depth + 1)
        return False
    declared = schema_def.get("type")
    if declared in ("string", "integer", "number", "boolean"):
        return True
    if declared == "array":
        return is_primitive_def(schema_def.get("items", {}), schemas, depth + 1)
    # An anyOf/oneOf is primitive only when every option is. Each key is tested independently
    # (rather than returning on the first present one) to preserve the behaviour of the emitter
    # predicate this replaced, for a schema that carries both.
    for key in ("anyOf", "oneOf"):
        if key in schema_def:
            if all(
                is_primitive_def(option, schemas, depth + 1) for option in schema_def[key]
            ):
                return True
    return False


def _resolve_ref(spec: Any, schemas: dict[str, Any], depth: int = 0) -> Any:
    """Follow an internal ``$ref`` to the schema it names; return ``spec`` unchanged otherwise.

    External (``file.yaml#/...``) references are left alone: this module never reads the
    filesystem, and an unresolvable reference must stay visible as an absent fact rather than
    become a guess.
    """
    if not isinstance(spec, dict) or depth > _MAX_REF_DEPTH:
        return spec if isinstance(spec, dict) else {}
    ref = spec.get("$ref")
    if isinstance(ref, str) and ref.startswith("#/components/schemas/"):
        target = schemas.get(ref.split("/")[-1])
        if isinstance(target, dict):
            return _resolve_ref(target, schemas, depth + 1)
    return spec


def _ref_name(spec: Any) -> str | None:
    """The schema name an internal ``$ref`` points at, or None."""
    if not isinstance(spec, dict):
        return None
    ref = spec.get("$ref")
    if isinstance(ref, str) and ref.startswith("#/components/schemas/"):
        return ref.split("/")[-1]
    return None


def _parse_external_ref(ref: str) -> tuple[str, str] | None:
    """Parse an external ``$ref`` into (document, schema_name), or None.

    OAS 3.x: *"each document in an OAD MUST be fully parsed in order to locate possible
    reference targets"* — the document qualifier is load-bearing, not a convenience.
    """
    if not isinstance(ref, str):
        return None
    # External ref: "file.yaml#/components/schemas/SchemaName"
    if ".yaml#/components/schemas/" in ref or ".yml#/components/schemas/" in ref:
        delimiter = ".yaml#" if ".yaml#" in ref else ".yml#"
        parts = ref.split(delimiter)
        if len(parts) == 2:
            doc = parts[0] + delimiter.rstrip("#")
            schema_part = parts[1]
            if schema_part.startswith("/components/schemas/"):
                schema_name = schema_part.split("/")[-1]
                return (doc, schema_name)
    return None


def datatype_for(spec: Any, schemas: dict[str, Any], depth: int = 0) -> str | None:
    """The XSD datatype of a property's value, or None where the value is not a literal.

    None is a fact, not a failure: an object-valued property, a multi-target ``anyOf`` and an
    array of unspecified items have no single datatype, and inventing ``xsd:string`` for them
    would be a false statement rather than a loose one.
    """
    if not isinstance(spec, dict) or depth > _MAX_REF_DEPTH:
        return None
    if "$ref" in spec:
        resolved = _resolve_ref(spec, schemas)
        if resolved is spec:
            return None
        return datatype_for(resolved, schemas, depth + 1)
    if any(key in spec for key in ("anyOf", "oneOf", "allOf")):
        return None
    declared = spec.get("type")
    if declared == "string":
        return str(xsd_for_string_format(spec.get("format")))
    if declared == "integer":
        return str(XSD.integer)
    if declared == "number":
        return str(XSD.float if spec.get("format") == "float" else XSD.double)
    if declared == "boolean":
        return str(XSD.boolean)
    if declared == "array":
        return datatype_for(spec.get("items", {}), schemas, depth + 1)
    return None


def target_classes_for(spec: Any, schemas: dict[str, Any]) -> tuple[str, ...]:
    """Class *names* a property may point at, in declaration order, deduplicated.

    Names rather than IRIs, so a projection resolves them through ``Mapping.classes``. A tuple
    rather than a single value because ``anyOf``/``oneOf`` genuinely has several targets; the
    rule that ``rdfs:range`` may only be emitted for exactly one of them belongs to the
    projection that emits it, not here.

    Two collapses happen here, and both are the module docstring's determinations rather than
    conveniences:

    * A ``$ref`` to a **``oneOf`` union** yields the union's *members*, not the union — there is no
      class for the union, so naming it would be an axiom pointing at an IRI no document declares.
      Recursive, because a union member may itself be one.
    * A ``$ref`` to a **``*Ref``** yields its **referent**: the property points at the thing, not at
      the mention of it. The referent is minted by convention, so a returned name is not guaranteed
      to be a key of ``schemas`` — a projection resolving it must mint the IRI rather than look it
      up, exactly as ``snm-api-native``'s ``emit_tbox`` does, and for the reason recorded there.

    Ordering and de-duplication survive both collapses, which is what lets a caller apply the
    one-target rule for ``rdfs:range``: ``ServiceRefOrValue`` = ``oneOf [Service, ServiceRef]``
    collapses to the single name ``Service``, so the range is emitted rather than deferred.
    """
    if not isinstance(spec, dict):
        return ()
    found: list[str] = []

    def add(name: str) -> None:
        if name not in found:
            found.append(name)

    def consider(candidate: Any, depth: int = 0) -> None:
        name = _ref_name(candidate)
        if name is None or name not in schemas or depth > _MAX_REF_DEPTH:
            return
        target = schemas[name]
        if is_primitive_def(target, schemas):
            return
        if is_json_only_union(target):
            # No class for the union; the constraint is its members. (Determination S2.)
            for member in target.get("oneOf") or []:
                consider(member, depth + 1)
            return
        referent = referent_name(name)
        if referent is not None:
            # A mention resolves to what it mentions. (Determination S8.)
            add(referent)
            return
        add(name)

    consider(spec)
    if spec.get("type") == "array" or "items" in spec:
        consider(spec.get("items"))
    for key in ("anyOf", "oneOf", "allOf"):
        for member in spec.get(key) or []:
            consider(member)
            if isinstance(member, dict) and ("items" in member):
                consider(member.get("items"))
    return tuple(found)


def is_iri_valued(property_name: str, spec: Any, schemas: dict[str, Any]) -> bool:
    """True when the value is a URL denoting a resource rather than a string about one.

    ``format: uri`` **or** the name ``href``. The second half is TM Forum's own base-schema
    convention, recorded as finding F10, and is deliberately not derived — see the module
    docstring for the measurement. Do not replace it with a name pattern, and do not delete it:
    ``href`` carries no machine-readable signal in any TMF document we have measured.
    """
    if property_name in IRI_VALUED_NAMES:
        return True
    resolved = _resolve_ref(spec, schemas)
    if isinstance(resolved, dict) and resolved.get("format") == "uri":
        return True
    if isinstance(resolved, dict) and resolved.get("type") == "array":
        items = _resolve_ref(resolved.get("items", {}), schemas)
        return isinstance(items, dict) and items.get("format") == "uri"
    return False


def _parents_of(
    schema_def: Any,
    schemas: dict[str, Any],
    external_schemas: dict[str, dict[str, Any]] | None = None,
) -> tuple[str, ...]:
    """Class names composed by a schema's top-level ``allOf`` ``$ref`` members.

    A ``$ref`` to a primitive schema is a datatype constraint, not inheritance, so it yields no
    parent — the same condition ``shacl_converter._handle_allof_as_inheritance`` applies before
    emitting ``rdfs:subClassOf``. A ``$ref`` to a ``oneOf`` union yields no parent either, because
    no class is declared for a union (:func:`is_json_only_union`) and an ``rdfs:subClassOf`` naming
    an IRI no document declares is a dangling axiom, not a weaker one.

    External refs are resolved against ``external_schemas`` as written: OAS 3.x requires that
    *"each document in an OAD MUST be fully parsed in order to locate possible reference targets"*.
    An unresolvable external ref yields no parent and is not tracked here — the caller records
    unresolved refs at the point an IRI is needed.
    """
    if not isinstance(schema_def, dict):
        return ()
    ext_schemas = external_schemas or {}
    parents: list[str] = []
    for member in schema_def.get("allOf") or []:
        # Try internal ref first
        name = _ref_name(member)
        if name is not None and name in schemas:
            if is_primitive_def(schemas[name], schemas) or is_json_only_union(schemas[name]):
                continue
            if name not in parents:
                parents.append(name)
            continue

        # Try external ref
        ref = member.get("$ref") if isinstance(member, dict) else None
        if ref:
            parsed = _parse_external_ref(ref)
            if parsed:
                doc, schema_name = parsed
                if doc in ext_schemas and schema_name in ext_schemas[doc]:
                    ext_schema = ext_schemas[doc][schema_name]
                    # Check if it's a class-like schema (not primitive, not union)
                    if not is_primitive_def(ext_schema, ext_schemas.get(doc, {})):
                        if not is_json_only_union(ext_schema):
                            if schema_name not in parents:
                                parents.append(schema_name)
    return tuple(parents)


def _resolve_declaring_class(
    class_name: str,
    parents: dict[str, tuple[str, ...]],
    declared_by: dict[str, dict[str, Any]],
    property_name: str,
) -> str:
    """The highest ancestor of ``class_name`` that declares ``property_name``.

    A subclass restating a parent's field must not mint a second IRI for it: an ontology declares
    an inherited property once, on the class that introduces it. Measured on TMF641, this
    attribution resolved 93.9% of (class, property) pairs against an independently-authored TBox
    where leaf attribution resolved 68.5%.

    Same rule as ``shacl_converter._find_declaring_class``, which walks ``rdfs:subClassOf`` in an
    in-progress ``rdflib.Graph`` and probes for an already-emitted ``rdfs:domain``. That walk is
    bound to graph state that does not exist here, so the rule is stated twice and the numbers are
    cited in both places; the ancestry order is the same (breadth-first, reversed, so the most
    general ancestor wins).

    **The two do not agree everywhere, and this one is the order-independent one.** Because the
    emitter probes a graph it is still building, its answer depends on the order schemas appear in
    the document: where a parent is declared after its child, it falls back to the leaf. Measured,
    23 of 57 non-trivial attributions differ on TMF641 and 8 of 49 on TMF620 — and 0 of 2,822 on
    the 3GPP corpus, which contains no non-trivial attribution at all, so no 3GPP-only check can
    see this. Guarded by ``tests/test_mapping.py::test_both_declaring_class_implementations_agree``
    and pinned by the strict xfail beside it; Task 8 collapses the two onto this one.
    """
    ancestry: list[str] = []
    seen: set[str] = set()
    queue = [class_name]
    while queue:
        current = queue.pop(0)
        if current in seen:
            continue
        seen.add(current)
        ancestry.append(current)
        queue.extend(parents.get(current, ()))
    ancestry.reverse()
    for ancestor in ancestry:
        if property_name in declared_by.get(ancestor, {}):
            return ancestor
    return class_name


def _status_codes(operation: Any) -> tuple[int, ...]:
    """Numeric response codes, ascending. ``default`` and range codes (``2XX``) are not numbers."""
    if not isinstance(operation, dict):
        return ()
    codes: list[int] = []
    for code in (operation.get("responses") or {}):
        text = str(code)
        if text.isdigit():
            codes.append(int(text))
    return tuple(sorted(codes))


def _resolve_document_ref(node: Any, document: dict, depth: int = 0) -> Any:
    """Follow an internal ``#/...`` pointer through the document, repeatedly.

    Needed because a Response Object, a Request Body Object and a Path Item Object may each *be*
    a ``$ref`` into ``components``, and TM Forum writes every one of them that way: measured on
    TMF641, all 20 operations reference ``#/components/responses/…`` and
    ``#/components/requestBodies/…``, so a reader that only looks for an inline ``content`` block
    resolves **0 of 8** response classes and **0 of 14** request classes. An unresolvable pointer
    returns the node unchanged, so the fact stays absent rather than becoming a guess.
    """
    while (
        isinstance(node, dict)
        and isinstance(node.get("$ref"), str)
        and node["$ref"].startswith("#/")
        and depth <= _MAX_REF_DEPTH
    ):
        target: Any = document
        for token in node["$ref"][2:].split("/"):
            token = token.replace("~1", "/").replace("~0", "~")  # RFC 6901 unescaping
            if isinstance(target, dict) and token in target:
                target = target[token]
            else:
                return node
        node = target
        depth += 1
    return node


def _body_schema(container: Any, document: dict) -> Any:
    """The schema of a request/response body, preferring JSON where several media types exist."""
    container = _resolve_document_ref(container, document)
    if not isinstance(container, dict):
        return None
    content = container.get("content")
    if not isinstance(content, dict) or not content:
        return None
    for media_type, media in content.items():
        if isinstance(media, dict) and "json" in str(media_type).lower():
            return media.get("schema")
    first = next(iter(content.values()))
    return first.get("schema") if isinstance(first, dict) else None


def _class_of_body(schema: Any, schemas: dict[str, Any], classes: dict[str, Any]) -> str | None:
    """The class a body carries: the item class for a list, None where there is no class.

    A list endpoint records the **item**, because an anonymous array is not a kind of thing.
    Resolution goes through ``classes``, so a name this document does not declare as a class
    yields None rather than a dangling name a projection would then mint an IRI for.
    """
    if not isinstance(schema, dict):
        return None
    name = _ref_name(schema)
    if name is None:
        resolved = _resolve_ref(schema, schemas)
        if isinstance(resolved, dict) and (
            resolved.get("type") == "array" or "items" in resolved
        ):
            return _class_of_body(resolved.get("items"), schemas, classes)
        return None
    target = schemas.get(name)
    if isinstance(target, dict) and (target.get("type") == "array" or "items" in target):
        # A named array schema is still a list of its item class.
        return _class_of_body(target.get("items"), schemas, classes)
    # Resolved THROUGH `classes`, never re-derived from the document pointer or the schema name:
    # that identity is why one Mapping replaces four conversions. Inverting this line to return
    # `schema["$ref"]` fails test_an_operation_reuses_the_class_iri_the_vocabulary_declares with
    # `assert '#/components/schemas/Order' in {...}` — observed, not assumed.
    return name if name in classes else None


def api_slug(title: str) -> str:
    """``info.title`` as one lowerCamelCase IRI segment. ``"Product Catalog Management"`` -> ``productCatalogManagement``."""
    words = [w for w in re.split(r"[^A-Za-z0-9]+", title or "") if w]
    if not words:
        return "api"
    head, *rest = words
    return head[0].lower() + head[1:] + "".join(w[0].upper() + w[1:] for w in rest)


def major_version(version: str) -> str:
    """``"5.0.0"`` -> ``"v5"``. The major only: a patch bump must not move every operation IRI."""
    leading = re.match(r"\d+", str(version or ""))
    return f"v{leading.group(0)}" if leading else "vUnversioned"


def _operation_iri(
    namespace: str, method: str, path_template: str, *, api: str, version: str
) -> str:
    """Mint an operation IRI from the API, its major version, the method and the path.

    **This IRI scheme is OURS** and asserts no external authority: no standard names operations in
    RDF, and Hydra (a W3C Community Group draft, not a Recommendation) gives a *type*, not an
    identifier scheme.

    Derived from method and path rather than from ``operationId``, which is **optional** in the
    OpenAPI Specification and duplicated in practice, while a method is unique within a Path Item
    Object by construction. Percent-encoded so distinct paths cannot collide — ``/order/{id}`` and
    ``/order/-id-`` must not land on one IRI.

    **The API and version segments were added 2026-09-22 to fix two measured collisions.** Without
    them the scheme is not an identifier at all when a store holds more than one document, and both
    failures are silent — they merge nodes rather than raising:

    * **Across versions.** Converting TMF641 v5 and v4.1 under one namespace produced **20 of 20
      identical** operation IRIs, so every operation of both versions was one node, carrying two
      conflicting ``dcterms:hasVersion`` literals.
    * **Across APIs.** Every TM Forum API defines ``/hub``, so ``delete/hub/{id}`` was **one node
      shared by TMF620, TMF622 and TMF641** — 2 collisions per pair, measured.

    ``info.title`` and ``info.version`` are **REQUIRED** by the OpenAPI Specification, so both
    segments are always derivable. That is why they are preferred to ``operationId`` even though an
    ``operationId``-based scheme would read more prettily: a scheme that needs an optional field has
    no defined behaviour on a document that omits it.
    """
    return (
        f"{namespace}operation/{api}/{version}/"
        f"{method.lower()}{quote(path_template, safe='/')}"
    )


def build_mapping(
    document: dict,
    *,
    namespace: str,
    operation_namespace: str | None = None,
    schema_namespaces: dict[str, str] | None = None,
    transport_namespace: str = DEFAULT_TRANSPORT_NAMESPACE,
    external_schemas: dict[str, dict[str, Any]] | None = None,
) -> Mapping:
    """Derive the fact set every projection reads, from one walk over the document.

    Args:
        document: A parsed OpenAPI document. Never modified.
        namespace: Default namespace for every class in the document.
        operation_namespace: Namespace for operation IRIs. Defaults to ``namespace``. Separate
            because an operation is a description of how to reach a resource, not a term in the
            vocabulary, and a consumer may publish the two under different bases.
        schema_namespaces: Optional ``{ClassName: namespace_uri}`` override, for a merged
            cross-domain spec where one document's schemas belong to several namespaces. Honoured
            through :func:`openapi_to_rdf.property_uri.namespace_for_schema`, the single point of
            decision for every URI this project mints, so class and property IRIs cannot disagree
            about where a class lives.
        transport_namespace: Namespace for wire envelopes. Defaults to
            :data:`DEFAULT_TRANSPORT_NAMESPACE`, which is **ours** and asserts no external
            authority. A distinct namespace rather than omission: see the module docstring for the
            76-vs-1 triple measurement that decided it.
        external_schemas: Optional ``{document_name: {schema_name: schema_def}}`` map of external
            documents' schemas. Used to resolve cross-document ``$ref`` like
            ``other.yaml#/components/schemas/Thing`` as written, per OAS 3.x: *"each document in
            an OAD MUST be fully parsed in order to locate possible reference targets"*. An
            unresolvable external ref is not an error here — it yields no parent and is reported
            where an IRI would be minted.

    Returns:
        A :class:`Mapping`. Classes come from ``components/schemas`` and operations from
        ``paths``; a document with neither yields an empty mapping rather than an error, so a
        caller can tell "nothing to derive" from "could not derive". A named ``oneOf`` union is
        **not** among the classes — see :func:`is_json_only_union`.
    """
    if not isinstance(document, dict):
        raise TypeError("document must be a parsed OpenAPI document (dict)")

    components = document.get("components") or {}
    schemas: dict[str, Any] = components.get("schemas") or {}
    overrides = dict(schema_namespaces or {})

    def namespace_of(schema_name: str) -> str:
        return namespace_for_schema(schema_name, namespace, overrides)

    # --- Classes. Every named schema that is not a primitive/datatype alias, and not a
    # --- JSON-only `oneOf` union (which is not a kind of thing — determination S2). -----------
    classes: dict[str, ClassFact] = {}
    parents: dict[str, tuple[str, ...]] = {}
    for schema_name, schema_def in schemas.items():
        if not isinstance(schema_def, dict) or is_primitive_def(schema_def, schemas):
            continue
        if is_json_only_union(schema_def):
            continue
        parents[schema_name] = _parents_of(schema_def, schemas, external_schemas)
        transport = is_transport(schema_name)
        # Transport envelopes are minted under their own namespace so plumbing is distinguishable
        # by IRI alone; domain classes keep the document's (possibly overridden) namespace.
        class_namespace_uri = transport_namespace if transport else namespace_of(schema_name)
        classes[schema_name] = ClassFact(
            iri=class_namespace_uri + format_local_name(schema_name),
            parents=parents[schema_name],
            is_transport=transport,
            referent=referent_name(schema_name),
        )

    # --- Properties, attributed to the class that declares them. ----------------------------
    declared_by = {name: flattened_properties(schemas[name]) for name in classes}
    required_by = {name: flattened_required(schemas[name]) for name in classes}

    properties: dict[str, PropertyFact] = {}
    properties_by_class: dict[tuple[str, str], PropertyFact] = {}
    for class_name in classes:
        for property_name, property_def in declared_by[class_name].items():
            declaring = _resolve_declaring_class(
                class_name, parents, declared_by, property_name
            )
            if declaring != class_name:
                # Attributed to an ancestor; the fact is built when that ancestor is visited,
                # so a restated property mints exactly one IRI.
                continue
            spec = property_def if isinstance(property_def, dict) else {}
            is_list = spec.get("type") == "array" or "items" in spec
            lower = max(
                1 if property_name in required_by[class_name] else 0,
                spec.get("minItems", 0) if is_list else 0,
            )
            # A transport envelope's properties live under the transport namespace with their
            # class: a property IRI must not claim a domain namespace its domain does not have.
            declaring_ns = (
                transport_namespace
                if classes[declaring].is_transport
                else namespace_of(declaring)
            )
            fact = PropertyFact(
                iri=str(property_uri(declaring_ns, declaring, property_name)),
                declaring_class=declaring,
                target_classes=target_classes_for(spec, schemas),
                datatype=datatype_for(spec, schemas),
                is_iri_valued=is_iri_valued(property_name, spec, schemas),
                min_count=lower,
                max_count=spec.get("maxItems") if is_list else 1,
            )
            properties_by_class[(declaring, property_name)] = fact
            # First declaration in document order owns the by-name index; every fact is kept in
            # properties_by_class. See Mapping's docstring.
            properties.setdefault(property_name, fact)

    # --- Operations. `paths` is in scope; the README disclaimer predates this. ---------------
    operations: dict[str, OperationFact] = {}
    # Read once: every operation IRI needs them, and info.title/info.version are
    # REQUIRED by the OpenAPI Specification so both are always present.
    _info = document.get("info") or {}
    _api = api_slug(_info.get("title", "") if isinstance(_info, dict) else "")
    _version = major_version(_info.get("version", "") if isinstance(_info, dict) else "")
    # Operations may live under a DIFFERENT namespace from the vocabulary, and a consumer may
    # need them to: an operation is not a resource OF the API, it is a description of how to
    # reach one, so putting it beside the classes conflates a vocabulary term with an
    # affordance description. Defaults to `namespace` so existing callers are unaffected.
    _op_ns = operation_namespace or namespace
    for path_template, path_item in (document.get("paths") or {}).items():
        path_item = _resolve_document_ref(path_item, document)
        if not isinstance(path_item, dict):
            continue
        for method in HTTP_METHODS:
            operation = path_item.get(method)
            if not isinstance(operation, dict):
                continue
            responses = operation.get("responses") or {}
            # YAML leaves an unquoted `200:` as an int, so compare on the text form but keep the
            # original key for the lookup.
            successes = sorted(
                (int(str(c)), c) for c in responses if str(c).isdigit() and 200 <= int(str(c)) < 300
            )
            returned = None
            for _code, key in successes:
                returned = _class_of_body(
                    _body_schema(responses[key], document), schemas, classes
                )
                if returned is not None:
                    break
            accepted = _class_of_body(
                _body_schema(operation.get("requestBody"), document), schemas, classes
            )
            key = f"{method.upper()} {path_template}"
            operations[key] = OperationFact(
                iri=_operation_iri(
                    _op_ns, method, path_template, api=_api, version=_version
                ),
                method=method.upper(),
                path_template=path_template,
                returns_class=returned,
                accepts_class=accepted,
                has_response_body=any(
                    _body_schema(responses[key], document) is not None for _code, key in successes
                ),
                has_request_body=_body_schema(operation.get("requestBody"), document) is not None,
                status_codes=_status_codes(operation),
            )

    info = document.get("info") or {}
    raw_version = info.get("version") if isinstance(info, dict) else None
    return Mapping(
        classes=classes,
        properties=properties,
        operations=operations,
        properties_by_class=properties_by_class,
        api_version=str(raw_version) if raw_version else None,
    )
