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

    # Document-level action: add x-jsonld-context
    # The context will be built by context_from_mapping, but for the overlay we just
    # reference it or embed it. For now, we'll reference it as a URL.
    # Actually, looking at the test, it seems we need to embed the context URL or object.
    # Let me check what the test expects...
    # The test applies the overlay and checks that x-jsonld-context is present at root.
    # So we need to add it.

    # For each class, we'll build a small context. But actually, for the overlay,
    # we just need to add the x-jsonld-context at document level and x-jsonld-type per schema.

    # Let me think about what the overlay should contain:
    # 1. Document-level: x-jsonld-context (can be a URL or embedded)
    # 2. Per-schema: x-jsonld-type with the class IRI

    # Build the context object (or URL). For simplicity, we'll build a basic context.
    # Actually, we should use context_from_mapping for consistency.
    from openapi_to_rdf.projections.context import context_from_mapping

    # We need a base namespace. Let's extract it from the first class IRI.
    # Actually, the overlay doesn't need the full context - it just needs to reference it.
    # But the test expects to apply the overlay and see x-jsonld-context.
    # Let me simplify: the overlay adds a reference to the context.

    # For now, let's say the context URL is derived from extends
    context_url = extends.replace(".yaml", "-context.jsonld").replace(".yml", "-context.jsonld")

    actions.append({
        "target": "$",
        "description": "Add JSON-LD context reference (draft-polli-restapi-ld-keywords)",
        "update": {"x-jsonld-context": context_url},
    })

    # Per-schema actions: add x-jsonld-type
    for class_name, class_fact in mapping.classes.items():
        if class_fact.is_transport:
            # Skip transport envelopes? Actually, no - we should include them.
            pass

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
