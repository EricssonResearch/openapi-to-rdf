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
        base: Base namespace for the context.

    Returns:
        A JSON-LD 1.1 @context document with type-scoped terms for each class, id → @id mapping,
        @type: @id for IRI-valued properties, and datatype coercion for typed properties.

    Raises:
        ValueError: When the Mapping has zero classes (refuse rather than return empty).
    """
    if not mapping.classes:
        raise ValueError("no classes to emit in context")

    context: dict = {}

    # For each class, create a type-scoped term
    for class_name, class_fact in mapping.classes.items():
        # Build the class-level context
        class_context: dict = {}

        # Add properties for this class
        for (declaring_class, prop_name), prop_fact in mapping.properties_by_class.items():
            # We need to check if this property is relevant to class_name
            # It's relevant if class_name == declaring_class or class_name is a descendant

            # For now, let's check if the declaring_class is in the ancestry of class_name
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
