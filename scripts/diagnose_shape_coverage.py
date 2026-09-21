#!/usr/bin/env python3
"""Which declared instance types have NO SHACL shape targeting them?

A `sh:targetClass` that matches no instance makes every constraint under it inert:
enum, min, max, pattern and cardinality all stop being enforced together, silently.
That is not a validation bug, it is a validation *hole*, and both directions of the
test suite go quiet when it opens:

  - `test_bad_instance_rejected` FAILS  — the violation is never reported.
  - `test_good_instance_accepted` PASSES — for the wrong reason. Nothing was checked.

The second is the dangerous one, which is why this script exists as a committed gate
rather than as a debugging aid. A suite can be green because it is correct or because
it is inert, and only this measurement distinguishes them.

Found 2026-09-21 while diagnosing 49 `test_bad_instance_rejected` failures whose
violation kinds spanned all four constraint families (29 enum, 7 below-min, 7
above-max, 4 pattern). One constraint family failing points at that family's emitter;
*every* family failing at once points at the target not matching at all.

Usage:
    uv run python scripts/diagnose_shape_coverage.py
    uv run python scripts/diagnose_shape_coverage.py --spec TS28623_TraceControlNrm
    uv run python scripts/diagnose_shape_coverage.py --json artifacts/shape-coverage.json

Exit status is 1 when any declared type is unreachable, so this can gate a push.
"""

from __future__ import annotations

import argparse
import collections
import json
import re
from pathlib import Path
from typing import Any

REPO = Path(__file__).resolve().parent.parent
SHACL_DIR = REPO / "output" / "shacl"
CASES_DIR = REPO / "test-cases"

#: `sh:targetClass PREFIX:LocalName`. Local names in the 3GPP corpus legitimately
#: contain `-` and `.` (`TraceJob-Single`, `loggedMDTConfig-Type`), so the character
#: class must admit them — a pattern that stopped at `\w` would under-report by
#: exactly the names this script exists to find.
TARGET_CLASS = re.compile(r"sh:targetClass\s+[A-Za-z0-9_]+:([A-Za-z0-9_.-]+)")
#: `a PREFIX:LocalName ;` or `... .` in an instance document.
RDF_TYPE = re.compile(r"\ba\s+[A-Za-z0-9_]+:([A-Za-z0-9_.-]+)\s*[;.]")


def targets_by_spec() -> dict[str, set[str]]:
    """Every class the shapes declare a target for, keyed by spec stem."""
    found: dict[str, set[str]] = collections.defaultdict(set)
    for path in sorted(SHACL_DIR.glob("*_shacl.ttl")):
        spec = path.name[: -len("_shacl.ttl")]
        found[spec] = set(TARGET_CLASS.findall(path.read_text()))
    return found


def declared_types(spec_filter: str | None) -> list[tuple[str, Path, str]]:
    """Every `rdf:type` the test instances declare, as (spec, file, local name)."""
    rows: list[tuple[str, Path, str]] = []
    for path in sorted(CASES_DIR.glob("*/*/*.ttl")):
        spec = path.relative_to(CASES_DIR).parts[0]
        if spec_filter and spec != spec_filter:
            continue
        for name in sorted(set(RDF_TYPE.findall(path.read_text()))):
            rows.append((spec, path, name))
    return rows


def run(spec_filter: str | None = None) -> dict[str, Any]:
    targets = targets_by_spec()
    rows = declared_types(spec_filter)

    matched: list[str] = []
    unreachable: list[dict[str, str]] = []
    for spec, path, name in rows:
        if name in targets.get(spec, set()):
            matched.append(name)
            continue
        # Why is it unreachable? A dash-variant present in the shapes means the two
        # sides disagree on NAME NORMALISATION, not on coverage — a different repair
        # than a class the converter never emitted at all.
        dashed = name.replace("_", "-")
        reason = "dash-variant exists in shapes" if dashed in targets.get(spec, set()) else "absent from shapes entirely"
        unreachable.append(
            {
                "spec": spec,
                "declared": name,
                "file": str(path.relative_to(REPO)),
                "reason": reason,
                "shape_target": dashed if reason.startswith("dash") else "",
            }
        )

    by_reason = collections.Counter(u["reason"] for u in unreachable)
    by_spec = collections.Counter(u["spec"] for u in unreachable)
    return {
        "instance_files_scanned": len({str(p) for _, p, _ in rows}),
        "specs_with_shapes": len(targets),
        "declared_types_examined": len(rows),
        "matched_by_a_target_class": len(matched),
        "unreachable": len(unreachable),
        "unreachable_by_reason": dict(by_reason),
        "unreachable_by_spec": dict(by_spec.most_common()),
        "detail": unreachable,
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--spec", help="restrict to one spec stem, e.g. TS28623_TraceControlNrm")
    parser.add_argument("--json", type=Path, help="also write the report as an artifact")
    parser.add_argument("--limit", type=int, default=12, help="detail rows to print (default 12)")
    args = parser.parse_args()

    report = run(args.spec)

    # Print the candidate-set size beside the finding. "231 unreachable" alone could
    # mean the scan found nothing to scan; "231 of 844 examined" is a measurement.
    print(f"instance files scanned         {report['instance_files_scanned']:>6}")
    print(f"specs with shapes              {report['specs_with_shapes']:>6}")
    print(f"declared types examined        {report['declared_types_examined']:>6}")
    print(f"matched by a sh:targetClass    {report['matched_by_a_target_class']:>6}")
    print(f"UNREACHABLE (no shape)         {report['unreachable']:>6}")
    for reason, count in report["unreachable_by_reason"].items():
        print(f"    {reason:<34} {count:>6}")

    if report["unreachable_by_spec"]:
        print("\nworst specs:")
        for spec, count in list(report["unreachable_by_spec"].items())[:8]:
            print(f"    {spec:<44} {count:>4}")

    if report["detail"]:
        print(f"\nfirst {min(args.limit, len(report['detail']))} of {len(report['detail'])}:")
        for row in report["detail"][: args.limit]:
            arrow = f" -> shapes have {row['shape_target']}" if row["shape_target"] else ""
            print(f"    {row['spec']}:{row['declared']}{arrow}")

    if args.json:
        args.json.parent.mkdir(parents=True, exist_ok=True)
        args.json.write_text(json.dumps(report, indent=2) + "\n")
        print(f"\nwrote {args.json}")

    if report["unreachable"]:
        print(
            f"\n{report['unreachable']} declared type(s) are validated by nothing. "
            "Every constraint on them — enum, min, max, pattern, cardinality — is inert."
        )
        return 1
    print("\nevery declared type is targeted by at least one shape")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
