"""OpenAPI Overlay projection from the derived Mapping.

An OpenAPI Overlay 1.1.0 document (``overlay``, ``info``, ordered ``actions`` with RFC 9535
JSONPath targets) carrying ``x-jsonld-type`` / ``x-jsonld-context`` — keywords defined by
``draft-polli-restapi-ld-keywords``, an IETF **individual submission**, not an adopted standard.
"""

from __future__ import annotations

from openapi_to_rdf.mapping import Mapping


def overlay_from_mapping(
    mapping: Mapping, *, extends: str, title: str, version: str
) -> dict:
    """Project an OpenAPI Overlay 1.1.0 document from the Mapping.

    Args:
        mapping: The derived fact set.
        extends: The OpenAPI document this overlay annotates.
        title: Overlay title.
        version: Overlay version.

    Returns:
        An OpenAPI Overlay 1.1.0 document carrying JSON-LD annotations as defined by
        draft-polli-restapi-ld-keywords (an IETF individual submission).

    Raises:
        ValueError: When the Mapping has zero classes (S10: refuse rather than return empty).
    """
    if not mapping.classes:
        raise ValueError("no classes to annotate")

    actions: list[dict] = []

    # Document-level action: add x-jsonld-context reference (draft-polli-restapi-ld-keywords)
    context_url = extends.replace(".yaml", "-context.jsonld").replace(".yml", "-context.jsonld")

    actions.append({
        "target": "$",
        "description": "Add JSON-LD context reference (draft-polli-restapi-ld-keywords)",
        "update": {"x-jsonld-context": context_url},
    })

    # Per-schema actions: add x-jsonld-type
    for class_name, class_fact in mapping.classes.items():

        actions.append({
            "target": f"$.components.schemas['{class_name}']",
            "description": f"Annotate {class_name} with JSON-LD type (draft-polli-restapi-ld-keywords)",
            "update": {"x-jsonld-type": class_fact.iri},
        })

    return {
        "overlay": "1.1.0",
        "info": {"title": title, "version": version},
        "extends": extends,
        "actions": actions,
    }
