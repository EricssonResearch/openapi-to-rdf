"""Output freshness gate: the committed deliverables must still be what the code produces.

`output/` is a published deliverable (the README advertises 114 tracked files), so a consumer who
diffs it against their own run should see nothing. This test regenerates the tree and compares.

**The equivalence relation is per-file type, and that is the whole design.** An earlier version
compared BYTES, via `git status --porcelain output/`, and it was unsatisfiable: rdflib's Turtle
serializer does not order blank nodes stably, so every regeneration rewrote ~27 SHACL files with
identical triples in a different order. Measured 2026-09-21: **27 of 27 dirty files were
graph-isomorphic** to their committed versions, differing only in where `],` versus `] ] ;` fell.
The test therefore failed after every regeneration, including immediately after committing the
regenerated tree — a loop with no termination condition, and one iteration of it was run before
anybody measured.

So:

- **`.ttl` files are compared as GRAPHS** (`rdflib.Graph.isomorphic`). Byte equality is the wrong
  equivalence relation for an RDF graph; two serializations of the same triples are the same
  deliverable. This is the fix chosen over making serialization byte-deterministic — the trade-off
  is recorded in `HYPOTHESES.md` under H5, and the thing given up is detection of gratuitous byte
  churn in files that consumers diff.
- **Everything else is compared as BYTES** (`output/index/*.yaml` and any future non-RDF artifact),
  because for those formats byte equality is exactly what the deliverable promises. Whether those
  are byte-deterministic was never measured — if one turns out not to be, measure it and decide,
  rather than extending the isomorphism exemption to cover it.

The comparison is a pure function (`compare_trees`) so it can be inverted in milliseconds with
fixtures, instead of through a three-minute regeneration. The slow integration test and the
fast guard-inversion test below are both required: the first proves the real tree is fresh, the
second proves this file can still tell freshness from staleness.
"""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path

import pytest
from rdflib import Graph

REPO_ROOT = Path(__file__).resolve().parent.parent

#: Below this, assume the comparison walked the wrong tree rather than that the deliverable
#: shrank. The README advertises 114 tracked files; a rename or a moved output path would
#: otherwise make this test silently inspect nothing and pass. Rule: a guard that enumerates
#: what it inspects must assert how much it inspected.
MIN_EXPECTED_FILES = 100


def _graphs_equivalent(left: Path, right: Path) -> bool:
    """True when two Turtle files carry the same triples, regardless of serialization order."""
    a, b = Graph(), Graph()
    a.parse(left, format="turtle")
    b.parse(right, format="turtle")
    return a.isomorphic(b)


def compare_trees(committed: Path, fresh: Path) -> tuple[list[str], int]:
    """Compare two output trees. Returns (findings, files_compared).

    A finding is a human-readable line naming a real difference. Serialization-only differences in
    `.ttl` files are not findings; see this module's docstring.
    """
    findings: list[str] = []
    compared = 0

    committed_files = {p.relative_to(committed) for p in committed.rglob("*") if p.is_file()}
    fresh_files = {p.relative_to(fresh) for p in fresh.rglob("*") if p.is_file()}

    # A file that regeneration no longer produces, or one it newly produces, is drift of the most
    # important kind — the byte/graph distinction does not arise, so check it first.
    # Wording matters here: this walks the working tree, not HEAD, so a file it finds may be
    # untracked scratch rather than a committed deliverable. Saying "committed" sent a reader
    # (me, 2026-09-21) looking for a stale commit when the real cause was three untracked files
    # a previous test run had left behind. Failing on them is still correct — stray files in the
    # deliverable tree are exactly how the last six got committed by accident — but the message
    # must not assert they are tracked.
    for missing in sorted(committed_files - fresh_files):
        findings.append(f"{missing}: present in output/ but NOT produced by a fresh run")
    for extra in sorted(fresh_files - committed_files):
        findings.append(f"{extra}: produced by a fresh run but absent from output/")

    for relative in sorted(committed_files & fresh_files):
        left, right = committed / relative, fresh / relative
        compared += 1
        if relative.suffix == ".ttl":
            try:
                if not _graphs_equivalent(left, right):
                    findings.append(f"{relative}: graphs differ (not just serialization)")
            except Exception as exc:  # a file that no longer parses is itself a finding
                findings.append(f"{relative}: failed to parse as Turtle — {exc}")
        elif left.read_bytes() != right.read_bytes():
            findings.append(f"{relative}: bytes differ")

    return findings, compared


def test_comparison_ignores_serialization_but_catches_semantics(tmp_path: Path) -> None:
    """The watched inversion for `compare_trees`, at fixture speed.

    Rule 12: a test not watched to fail is not a test. This asserts BOTH directions, because a
    comparison that ignored everything would pass the first half on its own and be worthless.
    """
    committed, fresh = tmp_path / "committed", tmp_path / "fresh"
    for directory in (committed, fresh):
        (directory / "shacl").mkdir(parents=True)
        (directory / "index").mkdir(parents=True)

    # Same two triples, deliberately different subject order and prefix style on disk.
    (committed / "shacl" / "a_shacl.ttl").write_text(
        "@prefix ex: <https://e.org/> .\n"
        "ex:A a ex:Shape ; ex:path ex:one .\n"
        "ex:B a ex:Shape ; ex:path ex:two .\n"
    )
    (fresh / "shacl" / "a_shacl.ttl").write_text(
        "<https://e.org/B> a <https://e.org/Shape> ; <https://e.org/path> <https://e.org/two> .\n"
        "<https://e.org/A> a <https://e.org/Shape> ; <https://e.org/path> <https://e.org/one> .\n"
    )
    (committed / "index" / "a.yaml").write_text("properties: [one, two]\n")
    (fresh / "index" / "a.yaml").write_text("properties: [one, two]\n")

    findings, compared = compare_trees(committed, fresh)
    assert findings == [], f"serialization-only difference was reported as drift: {findings}"
    assert compared == 2, compared  # not vacuous: it really did look at both files

    # Now break each equivalence in turn and watch it fail.
    (fresh / "shacl" / "a_shacl.ttl").write_text(
        "@prefix ex: <https://e.org/> .\nex:A a ex:Shape ; ex:path ex:CHANGED .\n"
    )
    findings, _ = compare_trees(committed, fresh)
    assert any("graphs differ" in f for f in findings), findings

    (fresh / "shacl" / "a_shacl.ttl").write_text(
        "@prefix ex: <https://e.org/> .\n"
        "ex:A a ex:Shape ; ex:path ex:one .\n"
        "ex:B a ex:Shape ; ex:path ex:two .\n"
    )
    (fresh / "index" / "a.yaml").write_text("properties: [one, two, THREE]\n")
    findings, _ = compare_trees(committed, fresh)
    assert any("bytes differ" in f for f in findings), findings

    # And a file that regeneration stops producing must be caught.
    (fresh / "index" / "a.yaml").unlink()
    findings, _ = compare_trees(committed, fresh)
    assert any("NOT produced by a fresh run" in f for f in findings), findings


@pytest.mark.slow
def test_output_matches_fresh_regeneration(tmp_path: Path) -> None:
    """The committed output/ must match a fresh run of regenerate_output.py.

    Catches stale artifacts, code changes that alter output without a regeneration, and IRI changes
    not reflected in the deliverable. Regenerates into a temporary tree via `--output-dir`, so it
    never touches the working copy — the previous version regenerated in place and left the tree
    dirty, which is how its own failure output got mistaken for the drift it was reporting.
    """
    regenerate_script = REPO_ROOT / "scripts" / "regenerate_output.py"
    committed = REPO_ROOT / "output"
    if not regenerate_script.exists():
        pytest.skip(f"regeneration script not found: {regenerate_script}")
    if not committed.exists():
        pytest.skip(f"output directory not found: {committed}")

    fresh = tmp_path / "output"
    result = subprocess.run(
        [sys.executable, str(regenerate_script), "--output-dir", str(fresh)],
        cwd=REPO_ROOT,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        pytest.fail(f"regeneration failed (exit {result.returncode}):\n{result.stdout}\n{result.stderr}")

    findings, compared = compare_trees(committed, fresh)

    # Before trusting a clean result, confirm the comparison could have produced a dirty one.
    assert compared >= MIN_EXPECTED_FILES, (
        f"only {compared} files compared, expected at least {MIN_EXPECTED_FILES} — "
        "the comparison probably walked the wrong tree, so a clean result means nothing"
    )

    if findings:
        pytest.fail(
            f"output/ has drifted from HEAD: {len(findings)} finding(s) across {compared} files "
            "compared.\nRegenerate with: uv run python scripts/regenerate_output.py\n"
            "Serialization-only reordering is NOT reported here, so every line below is a real "
            "difference.\n\n" + "\n".join(findings[:15])
        )
