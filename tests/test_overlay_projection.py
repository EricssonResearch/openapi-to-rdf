"""Overlay projection from the derived Mapping.

An OpenAPI Overlay 1.1.0 document carrying JSON-LD annotations (x-jsonld-type, x-jsonld-context),
as defined by draft-polli-restapi-ld-keywords (an IETF individual submission).
"""

from __future__ import annotations

import copy
import hashlib
import tempfile
from pathlib import Path

import pytest
import yaml

from openapi_to_rdf import build_mapping
from openapi_to_rdf.projections.overlay import overlay_from_mapping

SPEC = {
    "openapi": "3.0.0",
    "info": {"title": "OverlayProbe", "version": "1.0"},
    "components": {
        "schemas": {
            "Order": {
                "type": "object",
                "properties": {
                    "id": {"type": "string"},
                    "orderDate": {"type": "string", "format": "date-time"},
                },
            }
        }
    },
}
NS = "https://example.org/ontology/"


def test_overlay_version_is_1_1_0() -> None:
    """The Overlay specification version."""
    from openapi_to_rdf.projections.overlay import overlay_from_mapping

    mapping = build_mapping(SPEC, namespace=NS)
    overlay = overlay_from_mapping(
        mapping, extends="openapi.yaml", title="Test", version="1.0"
    )
    assert overlay["overlay"] == "1.1.0"


def test_info_is_present() -> None:
    """Every overlay has an info block."""
    from openapi_to_rdf.projections.overlay import overlay_from_mapping

    mapping = build_mapping(SPEC, namespace=NS)
    overlay = overlay_from_mapping(
        mapping, extends="openapi.yaml", title="Test", version="1.0"
    )
    assert "info" in overlay
    assert overlay["info"]["title"] == "Test"
    assert overlay["info"]["version"] == "1.0"


def test_extends_names_the_input() -> None:
    """The extends field points to the OpenAPI document."""
    from openapi_to_rdf.projections.overlay import overlay_from_mapping

    mapping = build_mapping(SPEC, namespace=NS)
    overlay = overlay_from_mapping(
        mapping, extends="input.yaml", title="Test", version="1.0"
    )
    assert overlay["extends"] == "input.yaml"


def test_one_action_per_annotated_schema() -> None:
    """One action for each class."""
    from openapi_to_rdf.projections.overlay import overlay_from_mapping

    mapping = build_mapping(SPEC, namespace=NS)
    overlay = overlay_from_mapping(
        mapping, extends="openapi.yaml", title="Test", version="1.0"
    )
    # One action for the document-level context, one for the Order schema
    assert len(overlay["actions"]) == 2


def test_targets_match_jsonpath_syntax() -> None:
    """RFC 9535 JSONPath targets."""
    from openapi_to_rdf.projections.overlay import overlay_from_mapping

    mapping = build_mapping(SPEC, namespace=NS)
    overlay = overlay_from_mapping(
        mapping, extends="openapi.yaml", title="Test", version="1.0"
    )
    actions = overlay["actions"]
    # Document-level action
    assert any(a["target"] == "$" for a in actions)
    # Schema-level action
    assert any("$.components.schemas['Order']" in a["target"] for a in actions)


def test_applying_overlay_reproduces_annotated_document() -> None:
    """The load-bearing assertion: round-trip equivalence."""
    from openapi_to_rdf.projections.overlay import overlay_from_mapping

    mapping = build_mapping(SPEC, namespace=NS)
    overlay = overlay_from_mapping(
        mapping, extends="openapi.yaml", title="Test", version="1.0"
    )

    # Apply the overlay to the original document
    annotated = copy.deepcopy(SPEC)
    for action in overlay["actions"]:
        if action["target"] == "$":
            annotated.update(action["update"])
        elif "$.components.schemas[" in action["target"]:
            # Extract schema name from target like "$.components.schemas['Order']"
            schema_name = action["target"].split("['")[1].split("']")[0]
            annotated["components"]["schemas"][schema_name].update(action["update"])

    # The annotated document should have x-jsonld-context at the root
    assert "x-jsonld-context" in annotated
    # And x-jsonld-type on the Order schema
    assert "x-jsonld-type" in annotated["components"]["schemas"]["Order"]
    assert annotated["components"]["schemas"]["Order"]["x-jsonld-type"] == NS + "Order"


def test_zero_classes_raises() -> None:
    """S10: refuse a zero-class Mapping rather than returning an empty context."""
    from openapi_to_rdf.projections.overlay import overlay_from_mapping

    empty_spec = {
        "openapi": "3.0.0",
        "info": {"title": "Empty", "version": "1.0"},
        "components": {"schemas": {}},
    }
    mapping = build_mapping(empty_spec, namespace=NS)
    with pytest.raises(ValueError, match="no classes"):
        overlay_from_mapping(mapping, extends="empty.yaml", title="Empty", version="1.0")


def test_input_file_is_never_written() -> None:
    """N1: the input document is never written."""
    from openapi_to_rdf.projections.overlay import overlay_from_mapping

    with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
        yaml.dump(SPEC, f)
        input_path = Path(f.name)

    try:
        # Compute SHA-256 before
        sha_before = hashlib.sha256(input_path.read_bytes()).hexdigest()

        # Convert
        mapping = build_mapping(SPEC, namespace=NS)
        _ = overlay_from_mapping(
            mapping, extends=str(input_path), title="Test", version="1.0"
        )

        # Compute SHA-256 after
        sha_after = hashlib.sha256(input_path.read_bytes()).hexdigest()

        assert sha_before == sha_after, "input file was modified"
    finally:
        input_path.unlink()


# ─────────────────────────────────────────────────────────────────────────────────────────────────
# `variant_suffixes`: a serialisation variant is typed as its base. Added 2026-09-25 at a consumer's
# request, after `snm-api-native` measured 101 of 176 shared schemas disagreeing with this projection.
# ─────────────────────────────────────────────────────────────────────────────────────────────────

_VARIANT_DOC = {
    "openapi": "3.0.1",
    "info": {"title": "Variants", "version": "1.0.0"},
    "components": {
        "schemas": {
            "Attachment": {"type": "object", "properties": {"name": {"type": "string"}}},
            "Attachment_FVO": {"type": "object", "properties": {"name": {"type": "string"}}},
            "Attachment_MVO": {"type": "object", "properties": {"name": {"type": "string"}}},
            # Ends in a configured suffix but has NO base schema: it is its own class.
            "Orphan_FVO": {"type": "object", "properties": {"x": {"type": "string"}}},
        }
    },
}


def _typed(document: dict) -> dict[str, str]:
    out = {}
    for action in document["actions"]:
        target = action["target"]
        if target == "$":
            continue
        name = target.split("'")[1]
        out[name] = action["update"]["x-jsonld-type"]
    return out


def test_without_the_parameter_every_variant_keeps_its_own_iri() -> None:
    """The DEFAULT must not assume a naming convention.

    `_FVO`/`_MVO` is TM Forum's spelling, not a property of OpenAPI. A library that collapsed them by
    default would be making the same mistake as one defaulting a namespace to a single organisation.
    """
    mapping = build_mapping(_VARIANT_DOC, namespace="https://example.org/v/")
    typed = _typed(overlay_from_mapping(mapping, extends="v.yaml", title="t", version="1"))
    assert typed["Attachment_FVO"].endswith("Attachment_FVO")
    assert typed["Attachment_MVO"].endswith("Attachment_MVO")


def test_a_variant_is_typed_as_its_base_when_the_suffixes_are_supplied() -> None:
    """The consumer's convention, honoured because the consumer stated it.

    `X_FVO` and `X_MVO` are the create-view and update-view of ONE entity. Distinct class IRIs would
    assert that the same attachment is a different kind of thing depending on which operation returned
    it -- the federation failure this project exists to avoid.
    """
    mapping = build_mapping(_VARIANT_DOC, namespace="https://example.org/v/")
    typed = _typed(
        overlay_from_mapping(
            mapping, extends="v.yaml", title="t", version="1",
            variant_suffixes=("_FVO", "_MVO"),
        )
    )
    base = typed["Attachment"]
    assert typed["Attachment_FVO"] == base
    assert typed["Attachment_MVO"] == base
    # Not vacuous: the base must be the base's own IRI, not something both collapsed onto by accident.
    assert base.endswith("Attachment")


def test_a_suffixed_name_with_no_base_keeps_its_own_iri() -> None:
    """The guard on the collapse. Ending in a suffix is not enough.

    `Orphan_FVO` has no `Orphan` schema, so collapsing it would invent a class the document does not
    declare and type payloads as something absent from the vocabulary.
    """
    mapping = build_mapping(_VARIANT_DOC, namespace="https://example.org/v/")
    typed = _typed(
        overlay_from_mapping(
            mapping, extends="v.yaml", title="t", version="1",
            variant_suffixes=("_FVO", "_MVO"),
        )
    )
    assert typed["Orphan_FVO"].endswith("Orphan_FVO")


def test_the_longest_suffix_wins() -> None:
    """Order matters when a corpus configures overlapping suffixes.

    With `("_MVO", "_X_MVO")` the name `Thing_X_MVO` must collapse to `Thing`, not to `Thing_X` — and
    matching in declaration order would give whichever came first.
    """
    document = {
        "openapi": "3.0.1",
        "info": {"title": "L", "version": "1.0.0"},
        "components": {"schemas": {
            "Thing": {"type": "object", "properties": {"a": {"type": "string"}}},
            "Thing_X": {"type": "object", "properties": {"a": {"type": "string"}}},
            "Thing_X_MVO": {"type": "object", "properties": {"a": {"type": "string"}}},
        }},
    }
    mapping = build_mapping(document, namespace="https://example.org/l/")
    typed = _typed(
        overlay_from_mapping(
            mapping, extends="l.yaml", title="t", version="1",
            variant_suffixes=("_MVO", "_X_MVO"),
        )
    )
    assert typed["Thing_X_MVO"] == typed["Thing"], typed
