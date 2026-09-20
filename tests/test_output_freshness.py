"""Output freshness gate: committed deliverables must match HEAD.

The output/ directory is a published deliverable (README.md advertises 114 tracked files).
If it drifts from the code, consumers see stale artifacts. This test fails if output/
differs from a fresh regeneration, making drift visible before push.
"""

from __future__ import annotations

import subprocess
from pathlib import Path

import pytest


def test_output_matches_fresh_regeneration():
    """The committed output/ must match a fresh run of regenerate_output.py.

    This catches:
    - Code changes that alter output format without regenerating
    - Stale artifacts from N commits ago
    - Class/property IRI changes not reflected in deliverables

    If this fails, run: uv run python scripts/regenerate_output.py
    then commit the changes.
    """
    repo_root = Path(__file__).resolve().parent.parent
    regenerate_script = repo_root / "scripts" / "regenerate_output.py"
    output_dir = repo_root / "output"

    if not regenerate_script.exists():
        pytest.skip(f"Regeneration script not found: {regenerate_script}")

    if not output_dir.exists():
        pytest.skip(f"Output directory not found: {output_dir}")

    # Save current git status of output/
    result = subprocess.run(
        ["git", "status", "--porcelain", "output/"],
        cwd=repo_root,
        capture_output=True,
        text=True,
    )

    # Regenerate using uv run
    result = subprocess.run(
        ["uv", "run", "python", str(regenerate_script)],
        cwd=repo_root,
        capture_output=True,
        text=True,
    )

    if result.returncode != 0:
        pytest.fail(f"Regeneration failed:\n{result.stdout}\n{result.stderr}")

    # Check if anything changed
    result = subprocess.run(
        ["git", "status", "--porcelain", "output/"],
        cwd=repo_root,
        capture_output=True,
        text=True,
    )
    final_status = result.stdout

    if final_status:
        # Get count of changed files
        changed_files = [line for line in final_status.strip().split("\n") if line]
        pytest.fail(
            f"Output/ has drifted from HEAD. {len(changed_files)} files differ.\n"
            f"Run: uv run python scripts/regenerate_output.py\n"
            f"Then commit the changes.\n\n"
            f"Changed files:\n" + "\n".join(changed_files[:10])
        )
