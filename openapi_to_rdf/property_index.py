"""YAML sidecar "property index" produced alongside RDF/SHACL output.

The index records every ``(class, property, uri, range, description)``
tuple the converter produced for a single input file. Properties that
share a local name but differ on range or description are flagged as
collisions for downstream review — no merging is performed here. The
merging step (Stage 2) is a separate, opinionated tool that will
consume this file; this module only writes it.

YAML schema (stable; consumed by the future ``merge`` subcommand)::

    source: TS28623_ComDefs.yaml
    generated_by: openapi-to-rdf <version>
    properties:
      - local_name: startTime
        uri: http://.../TS28623/ComDefs/TimeWindow#startTime
        owner_class: TimeWindow
        range: http://.../TS28623/ComDefs#DateTime
        description: "..."
    collisions:
      - local_name: startTime
        members:
          - http://.../TS28623/ComDefs/TimeWindow#startTime
          - http://.../TS28623/ComDefs/PerfMetricJob#startTime
        differs_on: [range]
"""

from __future__ import annotations

import os
from collections import defaultdict
from pathlib import Path
from typing import Iterable, Optional

import yaml


class PropertyIndex:
    """Accumulator of per-property metadata, written out as YAML.

    Usage:

        idx = PropertyIndex(source="foo.yaml", generated_by="openapi-to-rdf 0.2.0")
        idx.add(local_name="x", uri=..., owner_class=..., range_uri=..., description=...)
        idx.write(Path("foo_property_index.yaml"))
    """

    def __init__(self, source: str, generated_by: str):
        self.source = source
        self.generated_by = generated_by
        # Keyed by URI so repeated calls are idempotent.
        self._entries: dict[str, dict] = {}

    def add(
        self,
        local_name: str,
        uri: str,
        owner_class: Optional[str],
        range_uri: Optional[str],
        description: Optional[str],
    ) -> None:
        """Record a property. Repeat calls for the same URI overwrite."""
        self._entries[str(uri)] = {
            "local_name": local_name,
            "uri": str(uri),
            "owner_class": owner_class,
            "range": str(range_uri) if range_uri is not None else None,
            "description": description,
        }

    def _collisions(self) -> list[dict]:
        """Group entries by ``local_name`` and flag differences.

        Two or more entries with the same ``local_name`` are treated as a
        collision when they disagree on ``range`` or ``description``. If
        all fields agree structurally, no collision is flagged — they are
        simply the same concept declared on multiple classes.
        """
        groups: dict[str, list[dict]] = defaultdict(list)
        for entry in self._entries.values():
            groups[entry["local_name"]].append(entry)

        out: list[dict] = []
        for name, group in sorted(groups.items()):
            if len(group) < 2:
                continue
            differs_on: list[str] = []
            ranges = {e["range"] for e in group}
            descriptions = {e["description"] for e in group}
            if len(ranges) > 1:
                differs_on.append("range")
            if len(descriptions) > 1:
                differs_on.append("description")
            if not differs_on:
                continue
            out.append(
                {
                    "local_name": name,
                    "members": sorted(e["uri"] for e in group),
                    "differs_on": differs_on,
                }
            )
        return out

    def to_dict(self) -> dict:
        """Return the YAML-shaped representation."""
        props = sorted(
            self._entries.values(),
            key=lambda e: (e["local_name"], e["owner_class"] or "", e["uri"]),
        )
        return {
            "source": self.source,
            "generated_by": self.generated_by,
            "properties": props,
            "collisions": self._collisions(),
        }

    def write(self, path: os.PathLike | str) -> None:
        """Serialize to the given path, creating parent directories."""
        p = Path(path)
        p.parent.mkdir(parents=True, exist_ok=True)
        with open(p, "w", encoding="utf-8") as fh:
            yaml.safe_dump(self.to_dict(), fh, sort_keys=False, allow_unicode=True)

    # Convenience for tests / inspection
    def __len__(self) -> int:
        return len(self._entries)

    def entries(self) -> Iterable[dict]:
        return iter(self._entries.values())
