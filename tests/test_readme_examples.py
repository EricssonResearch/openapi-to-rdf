"""The README's "Example Output" must be real output, not a description of it.

Written 2026-09-24 after that section was found to be hand-typed and wrong on three counts at once:

  * a namespace the tool no longer mints — `http://ericsson.com/models/3gpp/TS28623/ComDefs#`,
    hash-separated, against the `https://example.org/TS28623_ComDefs/` it actually emits;
  * a class it does NOT emit. `TimeWindow` is a `oneOf` union, and determination S2/H10 says a union
    gets no class declared — so the README advertised output that contradicted the project's own
    rule, and `startTime` appeared nowhere in any generated file;
  * a fabricated `collisions` entry, in a document that has **zero** collisions, citing
    `PerfMetricJob`, which is not even in that document.

None of it was caught by anything, because a README is prose to every other check in this repository.
It is the first thing a reader copies from, so it is the worst place to be wrong.
"""

from __future__ import annotations

import re
from pathlib import Path

import pytest
import yaml

from scripts._namespace import BASE_STEM

REPO = Path(__file__).resolve().parent.parent
README = REPO / "README.md"
OUTPUT = REPO / "output"


def _turtle_blocks() -> list[str]:
    """Turtle blocks in the "Example Output" section ONLY.

    Scoped deliberately. The README has another Turtle block further down showing the Hydra operation
    graph, and it is about a TM FORUM document — which this repository does not redistribute, so there
    is no generated file to compare it against, and it elides IRIs with `…` on purpose. The first
    version of this helper took every block and reported that one as unverifiable output, which is a
    guard claiming more scope than it has.

    **Stated limitation: the operation-graph example is therefore NOT checked by anything.** It can
    only be verified by someone holding the TM Forum corpus. Narrowing the guard is right; pretending
    the gap does not exist would not be.
    """
    text = README.read_text(encoding="utf-8")
    start = text.index("## Example Output")
    end = text.index("\n## ", start + 1)
    return re.findall(r"```turtle\n(.*?)```", text[start:end], re.S)


def test_the_readme_quotes_no_retired_or_placeholder_namespace() -> None:
    """Two stems must not appear: the retired one, and the PLACEHOLDER.

    `https://example.org/` was worse than the retired stem, because it looked deliberate. 704
    committed artifacts were published under it — a namespace that asserts nothing and collides with
    every other example.org user, inherited by anyone who copied the vocabulary.

    Kept as its own narrow check because it catches a copy-paste from an old branch or an old issue,
    which is how the retired stem got into the README in the first place.
    """
    text = README.read_text(encoding="utf-8")
    for retired in ("ericsson.com/models/3gpp", "example.org", "example.com"):
        assert retired not in text, (
            f"{retired!r} must not appear: artifacts are minted under {BASE_STEM} "
            "(see scripts/_namespace.py, including the unresolved semantic/semantics split)"
        )
    assert BASE_STEM in text, "the README must show the stem the artifacts actually use"


@pytest.mark.skipif(not OUTPUT.is_dir(), reason="output/ not present")
def test_every_turtle_line_in_the_readme_appears_in_generated_output() -> None:
    """Line-by-line, against the real files. A prose description drifts; a copy cannot.

    Compares stripped non-trivial lines rather than whole blocks, because the README elides with
    `# ... 18 more` and shows excerpts. That is weaker than byte equality and it is the right
    trade: it still catches an invented class, an invented property and a wrong namespace, which
    are the three things that actually went wrong.
    """
    generated = "\n".join(
        path.read_text(encoding="utf-8") for path in sorted(OUTPUT.rglob("*.ttl"))
    )
    # Non-trivial: skip prefix declarations (they vary by document), comments, and punctuation-only
    # lines. What is left is the claims — classes, properties, domains, ranges, shapes.
    # NOT `("@prefix", "#", "]", "[", "")` -- every string starts with "", so that tuple skipped
    # every line and the check found 0 of them. The `checked >= 15` assertion below is what caught it,
    # which is the reason to assert how much a guard inspected rather than only what it concluded.
    skip = ("@prefix", "#", "]", "[")
    missing: list[str] = []
    checked = 0
    for block in _turtle_blocks():
        for raw in block.splitlines():
            line = raw.strip()
            if not line or line.startswith(skip) or line in {".", ";", ","}:
                continue
            checked += 1
            if line.rstrip(" ;,.") not in generated:
                missing.append(line)
    # The count is asserted so a README that stops showing Turtle cannot pass this vacuously.
    assert checked >= 15, f"only {checked} substantive Turtle lines found; is the section still there?"
    assert not missing, f"README shows Turtle that no generated file contains: {missing}"


@pytest.mark.skipif(not OUTPUT.is_dir(), reason="output/ not present")
def test_the_readme_collision_example_is_a_real_collision() -> None:
    """The fabricated one named a class from another document entirely.

    `attributes` in TS28105_AiMlNrm is the real case, and it is a better illustration anyway: 20
    classes declare it in ONE document, which is precisely what the per-class namespace exists for.
    """
    index = OUTPUT / "index" / "TS28105_AiMlNrm_property_index.yaml"
    if not index.exists():
        pytest.skip(f"{index} absent")
    collisions = yaml.safe_load(index.read_text(encoding="utf-8")).get("collisions") or []
    names = {entry["local_name"] for entry in collisions}
    assert "attributes" in names, (
        "the README quotes `attributes` as a real collision; it is no longer one"
    )

    text = README.read_text(encoding="utf-8")
    members = next(e for e in collisions if e["local_name"] == "attributes")["members"]
    quoted = [m for m in members if m in text]
    assert len(quoted) >= 2, "the README must quote real member URIs, not invented ones"

    # And the document the README says has none really has none, or that line is a new falsehood.
    com_defs = OUTPUT / "index" / "TS28623_ComDefs_property_index.yaml"
    if com_defs.exists():
        assert not (yaml.safe_load(com_defs.read_text(encoding="utf-8")).get("collisions") or []), (
            "the README states TS28623_ComDefs has no collisions; it now has some"
        )


@pytest.mark.skipif(not OUTPUT.is_dir(), reason="output/ not present")
def test_the_readme_shows_the_provenance_header() -> None:
    """A reader must see that generated files are labelled, not discover it by opening one.

    This is the documentation half of `tests/test_generated_file_provenance.py`: that one keeps the
    headers in the files, this one keeps the README from showing output without one.
    """
    text = README.read_text(encoding="utf-8")
    for phrase in (
        "GENERATED FILE",
        "did not produce, review or endorse",
        "Contact: Jean Martins <jean.martins@ericsson.com>",
        "Source document: TS28623_ComDefs.yaml",
    ):
        assert phrase in text, f"README's example output omits {phrase!r} from the header"
