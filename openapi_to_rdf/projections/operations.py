"""Operation graph projection from the derived Mapping.

Each operation becomes a **hydra:Operation** carrying hydra:method, hydra:returns, the class it
accepts, and status codes. Hydra Core is a **W3C Community Group draft** — a permanent namespace
but not a Recommendation — and that status appears in the emitted prefix comment.
"""

from __future__ import annotations

from rdflib import RDF, Graph, Literal, Namespace, URIRef

from openapi_to_rdf.mapping import Mapping

HYDRA = Namespace("http://www.w3.org/ns/hydra/core#")

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
}


def term(concept: str) -> URIRef:
    """The Hydra Core term for an affordance concept.

    Raises:
        KeyError: When the concept is not in the whitelist.
    """
    if concept not in AFFORDANCE_TERMS:
        raise KeyError(f"unknown affordance concept: {concept}")
    return AFFORDANCE_TERMS[concept]


def operations_from_mapping(mapping: Mapping, *, base: str) -> Graph:
    """Project the operation graph from the Mapping.

    Args:
        mapping: The derived fact set.
        base: Base namespace for operation IRIs.

    Returns:
        An rdflib.Graph with one hydra:Operation per operation, carrying method, returns class,
        accepted class, and status codes. Hydra Core is a W3C Community Group draft.
    """
    """Hydra Core is a W3C Community Group draft, not a W3C Recommendation.
    See https://www.hydra-cg.com/spec/latest/core/"""

    graph = Graph()
    graph.bind("hydra", HYDRA)

    # Emit operations
    for op_fact in mapping.operations.values():
        op_uri = URIRef(op_fact.iri)

        # Type
        graph.add((op_uri, RDF.type, term("Operation")))

        # Method
        graph.add((op_uri, term("method"), Literal(op_fact.method)))

        # URL template (path_template from OperationFact)
        if op_fact.path_template:
            graph.add((op_uri, term("template"), Literal(op_fact.path_template)))

        # Returns class (only if present)
        if op_fact.returns_class:
            # Resolve through Mapping.classes to get the IRI the vocabulary declares
            if op_fact.returns_class in mapping.classes:
                returns_iri = URIRef(mapping.classes[op_fact.returns_class].iri)
                graph.add((op_uri, term("returns"), returns_iri))

        # Accepts class (only if present)
        if op_fact.accepts_class:
            if op_fact.accepts_class in mapping.classes:
                accepts_iri = URIRef(mapping.classes[op_fact.accepts_class].iri)
                graph.add((op_uri, term("expects"), accepts_iri))

        # Status codes removed: hydra:statusCode has domain hydra:Status, not hydra:Operation.
        # Proper modeling would require hydra:possibleStatus -> hydra:Status node, which is
        # beyond current scope. Omitted rather than emitting ill-typed triples.

    return graph
