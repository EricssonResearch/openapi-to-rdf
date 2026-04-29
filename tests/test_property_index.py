"""Tests for the property index sidecar writer."""

import os
import tempfile
from pathlib import Path

import pytest
import yaml

from openapi_to_rdf.property_index import PropertyIndex
from openapi_to_rdf.shacl_converter import OpenAPIToSHACLConverter


# ── Unit tests on PropertyIndex itself ─────────────────────────────────


class TestPropertyIndexUnit:
    def test_round_trip(self, tmp_path):
        """An index serialized to YAML round-trips losslessly."""
        idx = PropertyIndex(source="foo.yaml", generated_by="openapi-to-rdf 0.2.0")
        idx.add(
            local_name="startTime",
            uri="http://x/A#startTime",
            owner_class="A",
            range_uri="http://x/#DateTime",
            description="start",
        )
        idx.add(
            local_name="endTime",
            uri="http://x/A#endTime",
            owner_class="A",
            range_uri="http://x/#DateTime",
            description=None,
        )
        out = tmp_path / "idx.yaml"
        idx.write(out)
        data = yaml.safe_load(out.read_text())
        assert data["source"] == "foo.yaml"
        assert data["generated_by"] == "openapi-to-rdf 0.2.0"
        names = [p["local_name"] for p in data["properties"]]
        assert set(names) == {"startTime", "endTime"}
        assert data.get("collisions", []) == []

    def test_collision_detected_on_range_difference(self, tmp_path):
        idx = PropertyIndex(source="foo.yaml", generated_by="t")
        idx.add(
            local_name="name",
            uri="http://x/A#name",
            owner_class="A",
            range_uri="http://www.w3.org/2001/XMLSchema#string",
            description="string name",
        )
        idx.add(
            local_name="name",
            uri="http://x/B#name",
            owner_class="B",
            range_uri="http://www.w3.org/2001/XMLSchema#integer",
            description="string name",
        )
        out = tmp_path / "idx.yaml"
        idx.write(out)
        data = yaml.safe_load(out.read_text())
        assert len(data["properties"]) == 2
        colls = data["collisions"]
        assert len(colls) == 1
        c = colls[0]
        assert c["local_name"] == "name"
        assert set(c["members"]) == {"http://x/A#name", "http://x/B#name"}
        assert "range" in c["differs_on"]

    def test_collision_detected_on_description_difference(self, tmp_path):
        idx = PropertyIndex(source="foo.yaml", generated_by="t")
        idx.add(
            local_name="id",
            uri="http://x/A#id",
            owner_class="A",
            range_uri="http://www.w3.org/2001/XMLSchema#string",
            description="A's id",
        )
        idx.add(
            local_name="id",
            uri="http://x/B#id",
            owner_class="B",
            range_uri="http://www.w3.org/2001/XMLSchema#string",
            description="B's id",
        )
        out = tmp_path / "idx.yaml"
        idx.write(out)
        data = yaml.safe_load(out.read_text())
        colls = data["collisions"]
        assert len(colls) == 1
        assert "description" in colls[0]["differs_on"]

    def test_no_collision_when_entries_agree(self, tmp_path):
        idx = PropertyIndex(source="foo.yaml", generated_by="t")
        idx.add(
            local_name="id",
            uri="http://x/A#id",
            owner_class="A",
            range_uri="http://www.w3.org/2001/XMLSchema#string",
            description="the id",
        )
        idx.add(
            local_name="id",
            uri="http://x/B#id",
            owner_class="B",
            range_uri="http://www.w3.org/2001/XMLSchema#string",
            description="the id",
        )
        out = tmp_path / "idx.yaml"
        idx.write(out)
        data = yaml.safe_load(out.read_text())
        # Two entries present, no collision flagged (structural identity only).
        assert len(data["properties"]) == 2
        assert data.get("collisions", []) == []


# ── Integration with the SHACL converter ───────────────────────────────

COLLISION_SPEC = {
    "components": {
        "schemas": {
            "A": {"type": "object", "properties": {"name": {"type": "string"}}},
            "B": {"type": "object", "properties": {"name": {"type": "integer"}}},
        }
    }
}


class TestShaclConverterEmitsIndex:
    @pytest.fixture(autouse=True)
    def setup(self, tmp_path):
        import yaml as _yaml
        spec_path = tmp_path / "TS00000_Sample.yaml"
        spec_path.write_text(_yaml.dump(COLLISION_SPEC))
        self.out_dir = tmp_path / "out"
        self.converter = OpenAPIToSHACLConverter(
            str(spec_path), output_dir=str(self.out_dir)
        )
        self.converter.run()
        self.index_path = self.out_dir / "index" / "TS00000_Sample_property_index.yaml"

    def test_index_file_written(self):
        assert self.index_path.exists(), f"Expected {self.index_path}"

    def test_index_has_both_entries(self):
        data = yaml.safe_load(self.index_path.read_text())
        locals_ = sorted(p["local_name"] for p in data["properties"])
        owners = sorted(p["owner_class"] for p in data["properties"])
        assert locals_ == ["name", "name"]
        assert owners == ["A", "B"]

    def test_index_flags_collision(self):
        data = yaml.safe_load(self.index_path.read_text())
        colls = data.get("collisions", [])
        assert len(colls) == 1
        c = colls[0]
        assert c["local_name"] == "name"
        assert "range" in c["differs_on"]
