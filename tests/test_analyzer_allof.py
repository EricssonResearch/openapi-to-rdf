"""Required-property checks must read the FLATTENED property set, including inline `allOf` members.

TMF and 3GPP both use two styles: properties at the top level, or nested inside an `allOf` member
alongside a `$ref` to the parent. Reading only the first makes a schema look as though it requires a
field it never declared. `lifting-core` fixed this ("the Intent class losing all 17 of its
properties"); `snm-api-native` recorded it as F12; this is the third copy.
"""

from __future__ import annotations

SPEC = {
    "openapi": "3.0.0",
    "info": {"title": "AnalyzerProbe", "version": "1.0"},
    "components": {
        "schemas": {
            "Parent": {"type": "object", "properties": {"id": {"type": "string"}}},
            "Inline": {
                "required": ["callback"],
                "allOf": [
                    {"$ref": "#/components/schemas/Parent"},
                    {"type": "object", "properties": {"callback": {"type": "string"}}},
                ],
            },
        }
    },
}


def test_a_property_declared_inside_allof_is_not_an_orphan() -> None:
    from openapi_to_rdf.analyzer.checks import orphan_required  # read the real name first

    findings = orphan_required(SPEC["components"]["schemas"])
    assert not findings, f"false orphan: {findings}"


def test_a_genuinely_undeclared_required_property_is_still_reported() -> None:
    """The control — the check must not be neutered into always passing."""
    from openapi_to_rdf.analyzer.checks import orphan_required

    broken = {"Bad": {"type": "object", "required": ["ghost"], "properties": {"real": {"type": "string"}}}}
    findings = orphan_required(broken)
    assert findings, "a real orphan must still be reported"
    assert any("ghost" in str(f) for f in findings), findings
