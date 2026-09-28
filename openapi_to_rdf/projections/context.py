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
        context[class_name] = {
            "@id": class_fact.iri,
            "@context": term_map_for_class(mapping, class_name),
        }

    return {"@context": context}


def term_map_for_class(mapping: Mapping, class_name: str) -> dict:
    """The type-scoped term map for one class: its own properties AND its inherited ones, flattened.

    Extracted from `context_from_mapping` on 2026-09-25 so the OVERLAY projection can emit the same
    map as a schema-level `x-jsonld-context`. `draft-polli-restapi-ld-keywords` defines that keyword at
    schema level, so a conformant Overlay can carry it — and a consumer needed it: the Kiota fork reads
    per-schema `x-jsonld-context` to generate a model's `ONTOLOGY_PROPERTIES`, and an Overlay carrying
    only `x-jsonld-type` left those empty.

    Flattening the ancestry is the whole point and is not an optimisation. A JSON-LD type-scoped context
    does NOT inherit: without the walk, `NumberCharacteristic` loses `name` and `valueType` and a
    consumer reading that class alone never learns it lost half the properties.

    One definition, two projections. Before this the overlay's per-schema contexts and the context
    document were built by different code in different repositories, which is how they came to disagree.

    `@container: @set` on a multi-valued property, added 2026-09-27 because a consumer's generated code
    lost a member without it. JSON-LD 1.1 §9.15 defines a set container as "the term's value is always
    an array", so without it a single-element list round-trips as a scalar. Found by reading the
    CONSUMER rather than reasoning about the spec: `kiota-ld` reads
    `"@container": "@set"` in `OpenOpenApiJsonLdContextExtension.cs:122`, records it on
    `CodeProperty.IsOntologySet`, and emits `ONTOLOGY_SET_PROPERTIES` from all three language writers
    (C#, Java, Python). Dropping it silently removed that member from every generated client, which a
    committed report listing caught by refusing to regenerate.

    Driven by `max_count != 1`, a fact the Mapping already recorded -- no new field. Measured on TMF641:
    145 of 718 properties are multi-valued.
    """
    term_map: dict = {}
    for (declaring_class, prop_name), prop_fact in mapping.properties_by_class.items():
        if declaring_class != class_name and not _is_ancestor(declaring_class, class_name, mapping):
            continue
        if prop_name == "id":
            # `id` maps to `@id`: it is the node's identity, not a property of it. Dropping this is what
            # collapses every subject to a blank node, and blank nodes cannot be joined across
            # deployments, which is the entire point of doing this.
            term_map["id"] = "@id"
            continue
        if prop_fact.is_iri_valued:
            term: dict = {"@type": "@id", "@id": prop_fact.iri}
        elif prop_fact.datatype:
            term = {"@type": prop_fact.datatype, "@id": prop_fact.iri}
        else:
            term = {"@id": prop_fact.iri}
        if prop_fact.max_count != 1:
            term["@container"] = "@set"
        term_map[prop_name] = term
    return term_map


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
