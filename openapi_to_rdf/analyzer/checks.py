"""Schema validation checks for OpenAPI specifications.

This module provides analysis functions to detect common schema issues:
- Orphan required properties (properties listed in `required` but not declared)
- Dangling references (unresolvable $ref pointers)
"""

from __future__ import annotations

import logging
from typing import Any

from openapi_to_rdf.mapping import flattened_properties

logger = logging.getLogger(__name__)


def orphan_required(schemas: dict[str, Any]) -> list[str]:
    """Check for required properties that are not declared in any property set.

    Collects properties from the top-level `properties` block AND from inline
    (non-$ref) `allOf` members. TMF and 3GPP both declare properties in allOf
    alongside $ref to a parent; reading only top-level properties creates false
    positives where a schema appears to require undeclared fields.

    This is the same defect fixed in lifting-core ("Intent class losing all 17
    properties") and recorded as F12 in snm-api-native. Flattening before checking
    eliminates the false positives: 3 errors on TMF v5 specs → 0 of 888 schemas
    with real orphans.

    Args:
        schemas: Dictionary of schema definitions from components/schemas

    Returns:
        List of findings in format "Schema 'Name': required property 'field' not declared"

    Cost:
        O(n×m) where n = schema count, m = avg allOf member count.
        Typical: <1ms for 1000 schemas.
    """
    findings = []
    dangling_refs = []

    for schema_name, schema_def in schemas.items():
        if not isinstance(schema_def, dict):
            continue

        required = schema_def.get("required", [])
        if not required:
            continue

        # Collect properties from top-level AND inline allOf members
        properties = _collect_flattened_properties(schema_name, schema_def, schemas, dangling_refs)

        # Report any required property not in the flattened set
        for prop in required:
            if prop not in properties:
                findings.append(
                    f"Schema '{schema_name}': required property '{prop}' not declared "
                    f"(declared: {sorted(properties)})"
                )

    # Log dangling references
    if dangling_refs:
        logger.warning(
            f"Found {len(dangling_refs)} dangling $ref(s): "
            + ", ".join(f"{ref} in {schema}" for ref, schema in dangling_refs)
        )

    return findings


def _collect_flattened_properties(
    schema_name: str,
    schema_def: dict[str, Any],
    all_schemas: dict[str, Any],
    dangling_refs: list[tuple[str, str]],
) -> set[str]:
    """Collect all property names from top-level and inline allOf members.

    Traverses:
    1. Top-level `properties` dict
    2. Each inline (non-$ref-only) member of `allOf`
    3. Does NOT resolve $ref pointers (those properties come from the parent schema)

    Args:
        schema_name: Name of the schema being analyzed (for error reporting)
        schema_def: The schema definition dict
        all_schemas: All schemas in the spec (for $ref validation)
        dangling_refs: Accumulator for dangling $ref warnings

    Returns:
        Set of property names declared in this schema or its inline allOf members
    """
    # The flattening itself lives in openapi_to_rdf.mapping, because the same determination
    # drives every derived artifact and a second copy of it is exactly the drift this project
    # defends against. This function keeps only the $ref *validation* the checker needs.
    properties = set(flattened_properties(schema_def))

    for member in schema_def.get("allOf") or []:
        if not isinstance(member, dict):
            continue
        # A $ref member is checked for resolvability but not followed: the parent's own
        # properties are checked when the parent schema is visited.
        ref = member.get("$ref")
        if isinstance(ref, str) and ref.startswith("#/components/schemas/"):
            if ref.split("/")[-1] not in all_schemas:
                dangling_refs.append((ref, schema_name))

    return properties
