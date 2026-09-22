"""Class-scoped property URI minting.

OpenAPI property names are locally scoped to their containing schema: two
schemas that happen to use the same property name (e.g. ``status``) are
declaring two distinct fields, not the same one. The RDF/SHACL output of
this tool therefore mints each property under a per-class namespace, so a
schema ``TimeWindow`` with a ``startTime`` property yields the URI::

    <base>/TimeWindow#startTime

rather than sharing a single ``<base>#startTime`` with every other schema
that uses the same name. Any semantic merging is a modelling decision that
belongs in a separate, opinion-driven step (see the ``property_index``
sidecar manifest and the deferred Stage-2 ``merge`` workflow).

This module contains only pure functions used by both the SHACL and OWL
converters; it has no rdflib Graph dependencies and is cheap to test.
"""

from __future__ import annotations

from rdflib import URIRef


def format_local_name(name: str) -> str:
    """Normalise a class or property local name for RDF.

    Keeps characters that are legal in an IRI local name (RFC 3987). Dashes,
    underscores, and most alphanumerics are legal and preserved as-is.

    A lossy normalisation (e.g. folding ``my-prop`` and ``my_prop`` onto one
    IRI) is a silent collision: two OpenAPI fields land on one RDF property,
    which falsifies this tool's documented one-domain-one-range invariant.
    Where a character genuinely must be escaped, escape it reversibly (e.g.
    percent-encoding) so distinct source names cannot collide.

    Raises:
        ValueError: if ``name`` is empty or ``None``.
    """
    if not name:
        raise ValueError("local name must be a non-empty string")
    # Dash is legal in IRI local names; keep it as-is rather than folding to underscore.
    return name


def namespace_for_schema(
    schema_name: str,
    base_namespace: str,
    schema_namespaces: dict[str, str] | None = None,
) -> str:
    """Return the namespace URI a named schema's terms are minted under.

    When ``schema_namespaces`` has an entry for ``schema_name`` it wins; otherwise the
    file-level ``base_namespace`` is used. This is the **single point of decision** for every
    class, ``$ref`` and property URI this project mints, which is what lets one document holding
    several domains (e.g. a merged CTS topology spec, where ``Resource`` lives under ``.../ctc/``
    and ``WirelessNetFunction`` under ``.../ctw/`` but refers back to ``Resource`` via ``allOf``)
    convert in one pass without any two artifacts disagreeing about where a class lives.

    A module-level function rather than a converter method so every derivation path — the SHACL
    emitter, ``build_mapping``, and anything projected from it — consults the same decision
    instead of copying the ``dict.get``.
    """
    if schema_namespaces:
        override = schema_namespaces.get(schema_name)
        if override is not None:
            return override
    return base_namespace


def class_namespace(base_namespace: str, class_name: str) -> str:
    """Return the per-class namespace URI for a schema.

    ``class_namespace("http://x/TS28623/ComDefs#", "TimeWindow")`` yields
    ``"http://x/TS28623/ComDefs/TimeWindow/"``.

    **The separator is ``/``, changed from ``#`` on 2026-09-22.** The previous docstring called ``#``
    "the conventional hash-namespace shape", and that justification does not survive inspection:

    * **This tool did not apply it to classes.** Measured on TMF641: **0 of 205 class IRIs contained
      ``#`` while 714 of 714 property IRIs did.** The class ``BaseEvent`` was
      ``…/transport/BaseEvent`` and its own property was ``…/transport/BaseEvent#event`` — two
      conventions inside one vocabulary, which is not a scheme.
    * **The hash convention's purpose was defeated anyway.** A fragment is stripped before
      dereference (RFC 3986 §3.5), so a hash namespace's point is that **one document serves the
      whole vocabulary** — which is why W3C's own vocabularies have exactly one namespace each
      (``rdf-schema#label``, ``owl#Class``, ``shacl#targetClass``). This module minted **167 distinct
      hash namespaces for a single TMF641 document**, so nothing could serve them as documents: it
      paid the per-term dereferencing cost of slash URIs while keeping hash syntax.
    * The module docstring's actual argument was always about *scoping* — ``<base>/TimeWindow#start``
      rather than a shared ``<base>#start`` — and inserting ``/<Class>`` is what achieves that. The
      ``#`` was left over from the pre-scoping shape.

    Status of the references, because it is part of the claim: RFC 3986 is an **IETF Standard**;
    "Cool URIs for the Semantic Web", which is where hash-vs-slash is usually argued, is a **W3C
    Interest Group Note, not a Recommendation** — so neither form is mandated. This is a consistency
    decision, not a compliance one.

    Consequence: every property IRI this tool emits moves. That is deliberate; see the migration
    gate in ``snm-api-native`` (``scripts/gate_migration_delta.py``), which reported **508 of 508
    TMF641 property IRIs as `moved` and 0 as `unchanged`** against an independently-authored TBox
    that uses ``/``, while **101 of its class IRIs matched exactly**. The class agreement is the
    evidence that ``/`` is the shape the rest of this tool already assumes.

    Raises:
        ValueError: if ``base_namespace`` or ``class_name`` is empty/None.
    """
    if not base_namespace:
        raise ValueError("base_namespace must be a non-empty string")
    safe_class = format_local_name(class_name)
    trimmed = base_namespace.rstrip("#").rstrip("/")
    return f"{trimmed}/{safe_class}/"


def property_uri(base_namespace: str, class_name: str, property_name: str) -> URIRef:
    """Return the class-scoped URI for a property as an ``rdflib.URIRef``.

    Combines :func:`class_namespace` with a dash-normalised property local
    name. Two classes declaring the same property name produce two
    distinct URIs, one per class.

    Raises:
        ValueError: if any argument is empty/None.
    """
    safe_prop = format_local_name(property_name)
    return URIRef(class_namespace(base_namespace, class_name) + safe_prop)
