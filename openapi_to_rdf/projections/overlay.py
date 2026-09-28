"""OpenAPI Overlay projection from the derived Mapping.

An OpenAPI Overlay 1.1.0 document (``overlay``, ``info``, ordered ``actions`` with RFC 9535
JSONPath targets) carrying ``x-jsonld-type`` / ``x-jsonld-context`` — keywords defined by
``draft-polli-restapi-ld-keywords``, an IETF **individual submission**, not an adopted standard.
"""

from __future__ import annotations

from openapi_to_rdf.mapping import Mapping
from openapi_to_rdf.projections.context import term_map_for_class


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
    context_url: str | None = None,
    context_url_key: str = "x-api-context-url",
) -> dict:
    """Project an OpenAPI Overlay 1.1.0 document from the Mapping.

    Args:
        mapping: The derived fact set.
        extends: The OpenAPI document this overlay annotates.
        title: Overlay title.
        version: Overlay version.
        context_url: Where the JSON-LD context is served, recorded at the document root under
            `context_url_key`. Defaults to `<extends>-context.jsonld`, which assumes the context sits
            beside the document. A caller that SERVES its context -- `snm-api-native` answers
            `/context.jsonld` -- must be able to say so, and the alternative was for it to rewrite this
            document afterwards: a second, non-standard step, which is what adopting the standard
            Overlay exists to remove. Pass `context_url=""` to emit no root action at all.
        context_url_key: The document-root key the context URL is recorded under. Defaults to
            `x-api-context-url`, and **must not** be `x-jsonld-context`, which this refuses.

            Verified against the primary source, not inferred: draft-polli-restapi-ld-keywords-09
            (`https://www.ietf.org/archive/id/draft-polli-restapi-ld-keywords-09.txt`, read 2026-09-27)
            titles its §2 "JSON Schema keywords" and opens "*A schema ... MAY use the following JSON
            Schema keywords*". Both keywords are scoped to a schemaObject, and the same section adds
            "*The schema MUST be of type object*". The OpenAPI document root is not a schema, so
            `x-jsonld-context` there is outside the keyword's defined scope -- this projection emitted it
            there until 2026-09-27, which was a placement the draft does not define.

            §2.2 makes it worse than a scope error: "*if the x-jsonld-context is a URL string, that URL
            needs to be dereferenced and processed to generate the instance context*". A generator
            reading a bare URL must fetch at build time, and hermetic builds break. `snm-api-native`
            records that as determination **P4** -- "a producer targeting generators MUST inline the
            context" -- and reserves the root key `x-api-context-url` explicitly "to avoid collision
            with the registered keys". Emitting the registered keyword there collided with the key that
            existed to prevent the collision.

            OURS, and it asserts no external authority: `x-api-context-url` is `snm-api-native`'s own
            key, chosen as the default here because it is the only established spelling for this fact
            that is known not to collide with a registered keyword. A caller with a different convention
            passes its own.
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

    # REFUSED, not coerced. `x-jsonld-context` at the document root is outside the keyword's scope
    # (draft-polli-restapi-ld-keywords-09 §2 defines it as a JSON Schema keyword on an OBJECT schema) and
    # a bare URL under it forces a generator to dereference at build time (§2.2). Silently renaming the
    # caller's key would hide a conformance decision they are entitled to make explicitly.
    if context_url_key == "x-jsonld-context":
        raise ValueError(
            "context_url_key='x-jsonld-context' is refused: draft-polli-restapi-ld-keywords-09 §2 "
            "scopes that keyword to a schemaObject of type `object`, so the document root is outside "
            "its definition, and §2.2 notes a URL-string value must be dereferenced at build time. "
            "Use a key of your own (the default `x-api-context-url`) or pass context_url='' to emit "
            "no root action."
        )

    actions: list[dict] = []

    # Document-level action: where the context is SERVED. Not a draft keyword -- the draft defines no
    # document-root pointer at all -- so it goes under a key the caller owns, and `context_url=""`
    # suppresses it entirely for a caller whose consumers read only the inlined per-schema contexts.
    if context_url is None:
        context_url = extends.replace(".yaml", "-context.jsonld").replace(".yml", "-context.jsonld")

    if context_url:
        actions.append({
            "target": "$",
            "description": f"Record where the JSON-LD context is served ({context_url_key})",
            "update": {context_url_key: context_url},
        })

    # Per-schema actions: add x-jsonld-type and x-jsonld-context
    for class_name, class_fact in mapping.classes.items():
        # `draft-polli-restapi-ld-keywords` requires an annotated schema to be of type OBJECT: JSON-LD
        # cannot carry semantics on a non-object value. Annotating `InformationRequiredArray`
        # (`type: array`) produced a document the draft forbids, and the consumer's conformance test is
        # what caught it. Skipped entirely rather than annotated with type-only, because the rule is
        # about annotating at all.
        if not class_fact.is_object:
            continue

        iri = class_fact.iri
        note = ""
        base = _base_name(class_name, variant_suffixes)
        if base is not None and base in mapping.classes:
            # The variant is typed as its BASE. Only when the base is itself a class: a schema merely
            # ENDING in a configured suffix, with no corresponding base, is its own class and keeping it
            # would be inventing a collapse the corpus does not support.
            iri = mapping.classes[base].iri
            note = f" (serialisation variant of {base}; typed as its base)"

        update: dict = {"x-jsonld-type": iri}

        # The schema-level `x-jsonld-context`, added 2026-09-25. `draft-polli-restapi-ld-keywords`
        # defines the keyword at schema level, so a conformant Overlay carries it and an adopter does
        # not have to fall back to a bespoke format to get it.
        #
        # A consumer needed it concretely: the Kiota fork reads per-schema `x-jsonld-context` to
        # generate a model's `ONTOLOGY_PROPERTIES`. An Overlay with only `x-jsonld-type` left those
        # EMPTY, so generated clients carried a class binding and no property bindings --
        # `AttributeError: type object 'NumberCharacteristic' has no attribute 'ONTOLOGY_PROPERTIES'`.
        #
        # Built by `term_map_for_class`, the same function `context_from_mapping` uses, so the overlay's
        # per-schema contexts and the context document cannot disagree. They were built by different
        # code in different repositories before this, which is how they came to.
        #
        # A variant takes its BASE's term map along with its base's IRI: the two have to agree about
        # which class the payload is, or the type says one thing and the properties another.
        term_map = term_map_for_class(mapping, base if note else class_name)
        if term_map:
            # `@version: 1.1` is required, not decoration: a term definition carrying its own
            # `@context` is a JSON-LD 1.1 scoped context, and a 1.0 processor silently ignores it. The
            # consumer asserts its presence on every inline context for exactly that reason.
            update["x-jsonld-context"] = {"@version": 1.1, **term_map}

        actions.append({
            "target": f"$.components.schemas['{class_name}']",
            "description": (
                f"Annotate {class_name} with JSON-LD type and context "
                f"(draft-polli-restapi-ld-keywords){note}"
            ),
            "update": update,
        })

    return {
        "overlay": "1.1.0",
        "info": {"title": title, "version": version},
        "extends": extends,
        "actions": actions,
    }
