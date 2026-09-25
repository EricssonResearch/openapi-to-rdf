"""Every generated artifact must say what produced it, and what did not.

The `.ttl` files under `output/` carried no provenance at all until 2026-09-24 — they opened straight
into `@prefix` lines. Nothing misattributed them to a standards body, but nothing attributed them to
this project either, so a reader holding one could not tell it was machine-generated, from which
document, or that the `rdfs:comment` text in it is the source document's prose rather than ours.

The freshness gate cannot cover this: it compares `.ttl` files as GRAPHS, and Turtle `#` comments are
invisible to a parser. So a header could vanish and every other check would stay green. That is what
this file is for.
"""

from __future__ import annotations

from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parent.parent
OUTPUT = REPO / "output"
CASES = REPO / "test-cases"

#: Both generated trees. `test-cases/` is the LARGER one and was missed when headers were first added
#: to `output/` alone -- reported at the time as covering "every generated file", which was 76 of 704.
#: Enumerated here so a third tree cannot be added without this list being updated.
TREES = (OUTPUT, CASES)

#: Sentences the header must carry, each for a different reason. Phrased so ONE set covers both a
#: vocabulary ("GENERATED FILE") and a fixture ("GENERATED TEST FIXTURE").
REQUIRED = {
    "GENERATED": "a reader must not hand-edit it",
    "openapi-to-rdf": "what produced it",
    "Contact: Jean Martins <jean.martins@ericsson.com>": (
        "who to write to about a generated artifact -- the WORK address, not the personal one in "
        "pyproject.toml"
    ),
    "Source document:": "which input it came from",
    "did not produce, review or endorse": (
        "the load-bearing disclaimer: a derived vocabulary that looks official may be cited as the "
        "standard's own model"
    ),
    "Regenerate:": "how to reproduce it",
}


def _generated_ttl() -> list[Path]:
    return sorted(path for tree in TREES for path in tree.rglob("*.ttl"))


@pytest.mark.skipif(not all(t.is_dir() for t in TREES), reason="a generated tree is absent")
def test_every_generated_ttl_carries_a_provenance_header() -> None:
    """All of them, in BOTH trees: one file without a header is the one someone will quote.

    The per-tree counts are asserted separately, because a single total would let one tree go missing
    while the other grew -- which is the shape of the mistake this test was extended to catch.
    """
    per_tree = {tree.name: sorted(tree.rglob("*.ttl")) for tree in TREES}
    # 76 -> 88 and 628 -> 927 on 2026-09-25, when the corpus became a FETCH at `Tag_Rel19_SA112`
    # instead of an un-derivable committed snapshot: 44 documents instead of 38.
    assert len(per_tree["output"]) == 88, "44 documents x (1 rdf + 1 shacl)"
    assert len(per_tree["test-cases"]) == 927, "247 good + 632 bad + 48 unchanged-name collisions"

    missing: list[str] = []
    for path in _generated_ttl():
        head = path.read_text(encoding="utf-8")[:1400]
        for phrase in REQUIRED:
            if phrase not in head:
                missing.append(f"{path.relative_to(REPO)}: missing {phrase!r}")
    assert not missing, missing[:10]


@pytest.mark.skipif(not all(t.is_dir() for t in TREES), reason="a generated tree is absent")
def test_the_header_names_the_document_it_came_from() -> None:
    """A header that names the WRONG source is worse than none — it sends a reader to the wrong spec.

    Load-bearing for `test-cases/`, where the generator reads the document name off the PATH
    (`test-cases/<document>/<good|bad>/<case>.ttl`) because `write_ttl` is not told it. That coupling
    breaks silently if the layout changes, and this is what would catch it.
    """
    wrong: list[str] = []
    for path in _generated_ttl():
        if OUTPUT in path.parents:
            stem = path.stem.rsplit("_", 1)[0]     # TS28623_ComDefs_rdf.ttl -> TS28623_ComDefs
        else:
            stem = path.parent.parent.name          # test-cases/<document>/good/X.ttl
        expected = f"Source document: {stem}.yaml"
        if expected not in path.read_text(encoding="utf-8")[:1400]:
            wrong.append(f"{path.relative_to(REPO)}: expected {expected!r}")
    assert not wrong, wrong[:10]


@pytest.mark.skipif(not OUTPUT.is_dir(), reason="output/ not present")
def test_the_header_does_not_change_the_graph() -> None:
    """Turtle comments are invisible to a parser — asserted rather than assumed.

    If a header ever became a triple, it would be an assertion about the vocabulary rather than a note
    about the file, and it would propagate into anything consuming the graph.
    """
    from rdflib import Graph

    sample = OUTPUT / "rdf" / "TS28623_ComDefs_rdf.ttl"
    if not sample.exists():
        pytest.skip(f"{sample} absent")
    text = sample.read_text(encoding="utf-8")
    header, body = text.split("\n\n", 1)
    assert "GENERATED FILE" in header
    assert all(line.startswith("#") or not line.strip() for line in header.splitlines())

    with_header = Graph().parse(data=text, format="turtle")
    without_header = Graph().parse(data=body, format="turtle")
    assert len(with_header) == len(without_header)


def test_the_notice_file_disclaims_the_generated_artifacts() -> None:
    """NOTICE carries the corpus-specific attribution the per-file headers deliberately do not.

    The headers stay generic because this converter also runs on TM Forum and on arbitrary documents,
    so naming 3GPP in them would be wrong for most inputs. That makes NOTICE the only place the bundled
    corpus is attributed, which is why its content is asserted rather than trusted.
    """
    notice = Path(__file__).resolve().parent.parent / "NOTICE"
    assert notice.exists(), "NOTICE is the only place the bundled 3GPP corpus is attributed"
    text = notice.read_text(encoding="utf-8")
    assert "3GPP Organizational Partners" in text
    assert "are NOT 3GPP's work" in text
    assert "Not redistributed" in text, "the TM Forum corpus's status must stay stated"
    # The open licensing question must not quietly disappear into a claim of compliance.
    assert "has **not** been verified" in text


def test_the_tool_names_its_own_vocabulary_not_the_callers() -> None:
    """The tool's marker terms must have ONE IRI, whoever runs it.

    `transport_namespace` used to default to `<base_namespace_prefix>transport/`, so a caller
    converting under `https://acme.example/` got `https://acme.example/transport/isIriValued`: a term
    that looks like theirs, carrying our semantics, and different from every other caller's IRI for
    the same predicate. Same tool, same meaning, two graphs that cannot join — which is the federation
    failure this project exists to avoid, arriving through the back door.

    Two callers with different document namespaces must agree on the marker, and that is what is
    asserted here rather than the constant's value.
    """
    import tempfile

    from openapi_to_rdf import OpenAPIToSHACLConverter
    from openapi_to_rdf.mapping import DEFAULT_TRANSPORT_NAMESPACE

    spec = REPO / "assets" / "MnS-Rel-19-OpenAPI" / "OpenAPI" / "TS28623_ComDefs.yaml"
    if not spec.exists():
        pytest.skip(
            f"{spec} absent; the corpus is fetched, not redistributed -- "
            "run `uv run python scripts/fetch_corpus.py`"
        )

    namespaces = []
    for prefix in ("https://acme.example/", "https://other.example/models/"):
        converter = OpenAPIToSHACLConverter(
            str(spec), base_namespace_prefix=prefix, output_dir=tempfile.mkdtemp()
        )
        namespaces.append(converter.transport_namespace)
    assert namespaces[0] == namespaces[1] == DEFAULT_TRANSPORT_NAMESPACE, namespaces

    # And an explicit override still wins -- snm-api-native depends on passing its own stem.
    explicit = OpenAPIToSHACLConverter(
        str(spec), transport_namespace="https://example.test/mine/", output_dir=tempfile.mkdtemp()
    )
    assert explicit.transport_namespace == "https://example.test/mine/"


def test_the_callers_namespace_default_is_an_obvious_placeholder() -> None:
    """The library must not invent an authority over someone else's document.

    The default was `http://ericsson.com/models/3gpp/`: it claimed an Ericsson namespace for the
    caller's document AND said "3gpp" whatever the input was. `example.org` is reserved by RFC 2606 §3
    for documentation and examples, so it cannot be mistaken for a real authority — which is the point.

    Not a contradiction of `scripts/_namespace.py`: artifacts THIS repository publishes use a real
    stem. The placeholder is for callers this project knows nothing about.
    """
    from openapi_to_rdf.mapping import DEFAULT_BASE_NAMESPACE_PREFIX

    assert "example.org" in DEFAULT_BASE_NAMESPACE_PREFIX
    assert "ericsson" not in DEFAULT_BASE_NAMESPACE_PREFIX, (
        "the default must not assert an Ericsson authority over a caller's document"
    )
    assert "3gpp" not in DEFAULT_BASE_NAMESPACE_PREFIX.lower(), (
        "the default must not name a corpus the caller may not be using"
    )
