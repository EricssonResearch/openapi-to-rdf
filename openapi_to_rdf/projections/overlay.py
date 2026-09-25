"""OpenAPI Overlay projection from the derived Mapping.

An OpenAPI Overlay 1.1.0 document (``overlay``, ``info``, ordered ``actions`` with RFC 9535
JSONPath targets) carrying ``x-jsonld-type`` / ``x-jsonld-context`` — keywords defined by
``draft-polli-restapi-ld-keywords``, an IETF **individual submission**, not an adopted standard.
"""

from __future__ import annotations

from openapi_to_rdf.mapping import Mapping


def _base_name(class_name: str, variant_suffixes: tuple[str, ...]) -> str | None:
    """The base a variant name projects, or None when the name is not a variant.

    Longest suffix first, so a corpus configuring both `_MVO` and `_FVO_MVO` collapses to the right
    base rather than to whichever happened to match first.
    """
    for suffix in sorted(variant_suffixes, key=len, reverse=True):
        if suffix and class_name.endswith(suffix) and len(class_name) > len(suffix):
            return class_name[: -len(suffix)]
    return None


def overlay_from_mapping(
    mapping: Mapping,
    *,
    extends: str,
    title: str,
    version: str,
    variant_suffixes: tuple[str, ...] = (),
) -> dict:
    """Project an OpenAPI Overlay 1.1.0 document from the Mapping.

    Args:
        mapping: The derived fact set.
        extends: The OpenAPI document this overlay annotates.
        title: Overlay title.
        version: Overlay version.
        variant_suffixes: Schema-name suffixes marking a SERIALISATION VARIANT of another schema. A
            schema whose name ends with one of these, and whose base name is also a class, is annotated
            with the BASE's IRI rather than its own. Empty by default: this is a naming convention of a
            particular corpus, and the library must not assume one.

            Added 2026-09-25 at a consumer's request. TM Forum publishes `X_FVO` and `X_MVO` -- the
            create-view and update-view of one entity -- and annotating them with distinct class IRIs
            asserts that the same attachment is a different KIND of thing depending on which operation
            returned it. `snm-api-native` records this as settled with high confidence: "all 116
            variants share their base's IRI ... projections of one domain class". Measured there:
            **101 of 176 shared schemas disagreed** with this projection before the parameter existed.

            Deliberately a PARAMETER and not a built-in list. `_FVO`/`_MVO` is TM Forum's spelling, not
            a property of OpenAPI, and hardcoding it here would be the same error as defaulting a
            namespace to one organisation. The convention belongs to the caller; the mechanism belongs
            here.

            This affects the OVERLAY only. `Mapping.classes` still declares the variant class and the
            TBox still emits it -- that is a separate, separately-recorded decision. The overlay answers
            "what class should a payload of this schema be typed as", and for a variant the answer is
            its base.

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
        iri = class_fact.iri
        note = ""
        base = _base_name(class_name, variant_suffixes)
        if base is not None and base in mapping.classes:
            # The variant is typed as its BASE. Only when the base is itself a class: a schema merely
            # ENDING in a configured suffix, with no corresponding base, is its own class and keeping it
            # would be inventing a collapse the corpus does not support.
            iri = mapping.classes[base].iri
            note = f" (serialisation variant of {base}; typed as its base)"

        actions.append({
            "target": f"$.components.schemas['{class_name}']",
            "description": (
                f"Annotate {class_name} with JSON-LD type "
                f"(draft-polli-restapi-ld-keywords){note}"
            ),
            "update": {"x-jsonld-type": iri},
        })

    return {
        "overlay": "1.1.0",
        "info": {"title": title, "version": version},
        "extends": extends,
        "actions": actions,
    }
