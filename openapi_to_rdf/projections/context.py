"""JSON-LD context projection from the derived Mapping.

A JSON-LD 1.1 @context giving each class a type-scoped term, id → @id, @type: @id for IRI-valued
properties, and datatype coercion where the Mapping recorded one.
"""

from __future__ import annotations

from openapi_to_rdf.mapping import Mapping


def context_from_mapping(mapping: Mapping, *, base: str) -> dict:
    """Project a JSON-LD 1.1 @context from the Mapping.

    Args:
        mapping: The derived fact set.
        base: Emitted as ``@base``, so a relative ``"id": "order-42"`` in a payload resolves to
            ``<base>order-42``. **This argument was accepted and ignored until 2026-09-22** — two
            calls differing only in ``base`` produced byte-identical output, verified — which is the
            second parameter in this package found to do nothing, after
            ``operations_from_mapping(base=...)``. A caller has no way to notice.

    Returns:
        A JSON-LD 1.1 ``@context`` document: ``{"@context": {...}}``. Note the wrapper — a consumer
        wanting the term mapping reads ``result["@context"]``.

        Contains ``@version: 1.1``, ``@base``, and one type-scoped term per class carrying
        ``id → @id``, ``@type: @id`` for IRI-valued properties and datatype coercion for typed ones.

    **Why ``@version: 1.1`` is not decoration.** A term definition containing its own ``@context``
    is a **JSON-LD 1.1** feature. A processor in ``json-ld-1.0`` processing mode is required to
    signal an error for it, and the practical outcome is that every scoped term is dropped — 95 of
    them on TMF641, including every ``href`` coercion, which is what turns a string into a
    followable edge. Omitting the declaration made the whole context's most important content
    conditional on a processor's default mode.

    Raises:
        ValueError: When the Mapping has zero classes (refuse rather than return empty).
    """
    if not mapping.classes:
        raise ValueError("no classes to emit in context")

    # Declared first and deliberately: these two keys decide whether a processor reads the rest.
    context: dict = {"@version": 1.1, "@base": base}

    # For each class, create a type-scoped term
    for class_name, class_fact in mapping.classes.items():
        # Build the class-level context
        class_context: dict = {}

        # Add properties for this class (declared or inherited)
        for (declaring_class, prop_name), prop_fact in mapping.properties_by_class.items():
            if declaring_class == class_name or _is_ancestor(
                declaring_class, class_name, mapping
            ):
                # Special case: 'id' maps directly to '@id'
                if prop_name == "id":
                    class_context["id"] = "@id"
                # Add this property to the class context
                elif prop_fact.is_iri_valued:
                    class_context[prop_name] = {"@type": "@id", "@id": prop_fact.iri}
                elif prop_fact.datatype:
                    class_context[prop_name] = {"@type": prop_fact.datatype, "@id": prop_fact.iri}
                else:
                    # Just map to the property IRI
                    class_context[prop_name] = {"@id": prop_fact.iri}

        context[class_name] = {"@id": class_fact.iri, "@context": class_context}

    return {"@context": context}


def _is_ancestor(potential_ancestor: str, class_name: str, mapping: Mapping) -> bool:
    """Check if potential_ancestor is an ancestor of class_name."""
    if class_name not in mapping.classes:
        return False

    visited = set()
    queue = [class_name]

    while queue:
        current = queue.pop(0)
        if current in visited:
            continue
        visited.add(current)

        if current == potential_ancestor:
            return True

        if current in mapping.classes:
            queue.extend(mapping.classes[current].parents)

    return False
