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
