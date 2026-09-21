"""Suite-wide guard: no test may write into `output/`.

`output/` is a published deliverable (the README advertises 114 tracked files). Converters default
their `output_dir` to a **cwd-relative** `output/`, so any test that constructs one without passing
`output_dir` silently writes its fixtures into the deliverable tree.

That is not hypothetical. On 2026-09-21 the freshness gate found **6 files committed but not produced
by a fresh regeneration** — `IdentityProbe_*` and `spec_*` across `rdf/`, `shacl/` and `index/` —
which made the committed tree 120 files against the README's 114. They were probe artifacts from
`tests/test_property_identity.py`, which put its input spec in `tmp_path` but let the converter
write its output to the repo, and they were then committed by a regeneration commit that could not
tell them apart from real deliverables.

Fixing the two known call sites would have left the invariant unguarded, which is the recurring
failure mode: a guard narrower than the thing it protects. So this checks the directory itself, and
any test — present or future — that writes there fails with its own name attached.
"""

from __future__ import annotations

from pathlib import Path

import pytest

OUTPUT_DIR = Path(__file__).resolve().parent.parent / "output"


def _snapshot() -> dict[Path, tuple[int, int]]:
    """Path -> (size, mtime_ns) for every file under output/."""
    if not OUTPUT_DIR.exists():
        return {}
    return {
        path: (stat.st_size, stat.st_mtime_ns)
        for path in OUTPUT_DIR.rglob("*")
        if path.is_file()
        for stat in (path.stat(),)
    }


@pytest.fixture(autouse=True)
def _output_dir_is_read_only(request: pytest.FixtureRequest):
    """Fail any test that creates or modifies a file under output/.

    Not vacuous: it was written against two known offenders and observed to catch them. The
    freshness test is exempt because regenerating the tree is its entire purpose — it writes to a
    temporary directory via `--output-dir`, but it is listed here so the exemption is explicit
    rather than accidental.
    """
    if request.node.get_closest_marker("writes_output"):
        yield
        return

    before = _snapshot()
    yield
    after = _snapshot()

    created = sorted(set(after) - set(before))
    modified = sorted(path for path in set(after) & set(before) if after[path] != before[path])

    if created or modified:
        lines = [f"  created:  {p.relative_to(OUTPUT_DIR)}" for p in created]
        lines += [f"  modified: {p.relative_to(OUTPUT_DIR)}" for p in modified]
        pytest.fail(
            f"{request.node.name} wrote into output/, which is a published deliverable.\n"
            + "\n".join(lines)
            + "\n\nPass output_dir=str(tmp_path) when constructing the converter. Its default is a "
            "cwd-relative 'output/', so omitting it targets the repo's deliverable tree.",
            pytrace=False,
        )
