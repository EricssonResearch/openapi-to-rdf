"""Operation graph projection from the derived Mapping.

Each operation becomes a **hydra:Operation** carrying hydra:method, hydra:returns, the class it
accepts, and status codes. Hydra Core is a **W3C Community Group draft** — a permanent namespace
but not a Recommendation — and that status appears in the emitted prefix comment.
"""

from __future__ import annotations

from rdflib import RDF, Graph, Literal, Namespace, URIRef

from openapi_to_rdf.mapping import Mapping

HYDRA = Namespace("http://www.w3.org/ns/hydra/core#")

#: Dublin Core Terms. `dcterms:hasVersion` records the exact contract version on each operation, so
#: it stays recoverable when the operation IRI carries only the major. Reused rather than minted —
#: DCMI Metadata Terms is a **DCMI Recommendation** with a permanent namespace.
DCTERMS = Namespace("http://purl.org/dc/terms/")

# ─────────────────────────────────────────────────────────────────────────────────────────────────
#: **OURS. A MINTED TERM, NOT A STANDARD ONE.** `affordance:invokedAt` is defined by this project
#: and asserts no external authority. Nothing in Hydra Core, Dublin Core, or any W3C
#: Recommendation defines it, and it must not be presented as standard vocabulary.
#:
#: Why it exists. Hydra models "a templated link" (`hydra:IriTemplate` + `hydra:template`) and
#: "operations a class supports" (`hydra:supportedOperation`), but has **no term relating an
#: operation to the template it is invoked at**. The obvious shortcut — putting `hydra:template`
#: straight onto the operation — is unavailable: `hydra:template`'s domain is `hydra:IriTemplate`,
#: so it would entail *the operation IS a template*, which is false.
#:
#: This projection did exactly that until 2026-09-22, which is the defect this constant fixes.
#: The template is now its own `hydra:IriTemplate` node at `<operation>#template` — a real IRI
#: rather than a blank node, deliberately, so a consumer can address it — and `invokedAt` relates
#: the operation to it.
#:
#: Swapping Hydra for WoT means replacing the Hydra terms above; this one would have to be
#: re-justified against whatever the new vocabulary offers, not carried over unexamined.
# `semantics` (plural) as of 2026-09-25, when the host was settled. This term is OURS -- Hydra has no
# predicate relating an operation to its template, and `hydra:template`'s domain is `hydra:IriTemplate`,
# so putting it on the operation would entail the operation IS a template.
#
# It was singular until now, while `DEFAULT_TRANSPORT_NAMESPACE` in the same package was plural: two of
# this project's OWN vocabularies under two hosts differing by one letter. `snm-api-native` carries an
# independent copy of this same string in `snm_api/affordances.py`, and nothing gated that they agree.
AFFORDANCE_LOCAL_NAMESPACE = "https://semantics.ericsson.com/ontology/affordance/"
INVOKED_AT = URIRef(AFFORDANCE_LOCAL_NAMESPACE + "invokedAt")
# ─────────────────────────────────────────────────────────────────────────────────────────────────

# Hydra Core terms whitelist. An unknown term raises rather than minting, because a typo that
# silently minted `hydra:retruns` would produce a graph that parses, loads, and answers no query
# — the most expensive kind of wrong.
AFFORDANCE_TERMS = {
    "Operation": HYDRA.Operation,
    "method": HYDRA.method,
    "returns": HYDRA.returns,
    "expects": HYDRA.expects,
    "statusCode": HYDRA.statusCode,
    "template": HYDRA.template,
    "IriTemplate": HYDRA.IriTemplate,
    "supportedOperation": HYDRA.supportedOperation,
}


def term(concept: str) -> URIRef:
    """The Hydra Core term for an affordance concept.

    Raises:
        KeyError: When the concept is not in the whitelist.
    """
    if concept not in AFFORDANCE_TERMS:
        raise KeyError(f"unknown affordance concept: {concept}")
    return AFFORDANCE_TERMS[concept]


def operations_from_mapping(mapping: Mapping) -> Graph:
    """Project the operation graph from the Mapping.

    Args:
        mapping: The derived fact set. Operation IRIs come from it, already minted.

    **The `base` parameter was removed on 2026-09-22 because it did nothing.** It was documented as
    "base namespace for operation IRIs" while the body read ``op_fact.iri`` straight from the
    Mapping, so two calls differing only in `base` produced byte-identical graphs — verified. A
    parameter that silently ignores its argument is worse than an absent one, because a caller
    believes it took effect. To place operations under a different namespace, pass
    ``operation_namespace`` to :func:`openapi_to_rdf.mapping.build_mapping`, which is where the IRI
    is actually minted.

    Returns:
        An rdflib.Graph with one hydra:Operation per operation, carrying method, returns class,
        accepted class, and status codes. Hydra Core is a W3C Community Group draft.
    """
    """Hydra Core is a W3C Community Group draft, not a W3C Recommendation.
    See https://www.hydra-cg.com/spec/latest/core/"""

    graph = Graph()
    graph.bind("hydra", HYDRA)
    graph.bind("dcterms", DCTERMS)
    graph.bind("affordance", Namespace(AFFORDANCE_LOCAL_NAMESPACE))

    # Emit operations
    for op_fact in mapping.operations.values():
        op_uri = URIRef(op_fact.iri)

        # Type
        graph.add((op_uri, RDF.type, term("Operation")))

        # Method
        graph.add((op_uri, term("method"), Literal(op_fact.method)))

        # The exact contract version, so it survives an operation IRI that carries only the major.
        if mapping.api_version:
            graph.add((op_uri, DCTERMS.hasVersion, Literal(mapping.api_version)))

        # The URL to invoke it at, as its own IriTemplate node — NOT `hydra:template` on the
        # operation, whose domain is `hydra:IriTemplate` and would entail the operation is one.
        # See INVOKED_AT above for why the relating term is ours.
        if op_fact.path_template:
            template_node = URIRef(f"{op_fact.iri}#template")
            graph.add((template_node, RDF.type, term("IriTemplate")))
            graph.add((template_node, term("template"), Literal(op_fact.path_template)))
            graph.add((op_uri, INVOKED_AT, template_node))

        # `returns` / `expects`, plus the inverse so a consumer holding a class can find its
        # operations without a backwards property path.
        for concept, class_name in (("returns", op_fact.returns_class), ("expects", op_fact.accepts_class)):
            if not class_name or class_name not in mapping.classes:
                continue
            class_iri = URIRef(mapping.classes[class_name].iri)
            graph.add((op_uri, term(concept), class_iri))
            graph.add((class_iri, term("supportedOperation"), op_uri))

        # Status codes removed: hydra:statusCode has domain hydra:Status, not hydra:Operation.
        # Proper modeling would require hydra:possibleStatus -> hydra:Status node, which is
        # beyond current scope. Omitted rather than emitting ill-typed triples.

    return graph
