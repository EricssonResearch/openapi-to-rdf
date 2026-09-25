"""The corpus fetch: its parsing, its refusals, and one live download.

Most of this runs offline against synthetic archives, because the parts that can go wrong — flattening,
zip-slip, digest comparison — are not network-dependent and should not need the forge to be reachable.
The one live test is marked and states its reason when it skips.
"""

from __future__ import annotations

import io
import json
import zipfile
from pathlib import Path

import pytest

from scripts.fetch_corpus import (
    MANIFEST,
    PINNED_REF,
    PINNED_REF_TYPE,
    archive_url,
    compare,
    digest,
    extract,
    on_disk,
)

REPO = Path(__file__).resolve().parent.parent


def _zip(members: dict[str, bytes]) -> bytes:
    buffer = io.BytesIO()
    with zipfile.ZipFile(buffer, "w") as zf:
        for name, data in members.items():
            zf.writestr(name, data)
    return buffer.getvalue()


def test_the_archive_url_pins_a_tag_and_quotes_the_ref() -> None:
    """A branch tip moves; a corpus that moves takes every figure in the repository with it.

    `?path=OpenAPI` is the other half: the MnS repository also holds `yang-models/`, which is
    `yang-to-rdf`'s input, and fetching the whole repository would pull a corpus this project does not
    convert.
    """
    url = archive_url()
    assert PINNED_REF_TYPE == "tags", "the pin must be a tag, not a branch"
    assert f"ref_type={PINNED_REF_TYPE}" in url
    assert "path=OpenAPI" in url
    assert PINNED_REF in url

    # Quoted, so a ref containing a slash or a space cannot alter the URL's structure.
    assert "%2F" in archive_url("Rel-19/odd", "heads")


def test_extract_flattens_the_ref_prefixed_directory() -> None:
    """The archive's top directory contains the REF, so keeping it would change every path on a re-pin."""
    archive = _zip({
        f"MnS-{PINNED_REF}-OpenAPI/": b"",
        f"MnS-{PINNED_REF}-OpenAPI/OpenAPI/": b"",
        f"MnS-{PINNED_REF}-OpenAPI/OpenAPI/TS12345_Thing.yaml": b"openapi: 3.0.1\n",
        f"MnS-{PINNED_REF}-OpenAPI/OpenAPI/README.md": b"(c) 3GPP\n",
    })
    got = extract(archive)
    assert set(got) == {"TS12345_Thing.yaml", "README.md"}
    assert got["TS12345_Thing.yaml"] == b"openapi: 3.0.1\n"


def test_extract_keeps_the_corpus_readme() -> None:
    """It carries 3GPP's copyright notice, and separating a notice from what it covers loses it."""
    archive = _zip({"a/OpenAPI/README.md": b"(c) 2023, 3GPP Organizational Partners\n"})
    assert "README.md" in extract(archive)


def test_extract_ignores_files_the_converter_does_not_read() -> None:
    archive = _zip({
        "a/OpenAPI/TS1_X.yaml": b"x",
        "a/OpenAPI/logo.png": b"\x89PNG",
        "a/OpenAPI/script.sh": b"#!/bin/sh",
    })
    assert set(extract(archive)) == {"TS1_X.yaml"}


@pytest.mark.parametrize(
    "member",
    ["/etc/passwd.yaml", "a/../../escape.yaml", "../outside.yaml"],
)
def test_extract_refuses_a_zip_slip_member(member: str) -> None:
    """Refused, not sanitised.

    A member resolving outside the tree means the archive is not what this code thinks it is, and
    repairing the path would hide that while still trusting the rest of the archive. Parametrised
    because an absolute path and a traversal are different bugs in an unzipper.
    """
    with pytest.raises(ValueError, match="zip-slip"):
        extract(_zip({member: b"x"}))


def test_extract_refuses_two_members_that_flatten_to_one_name() -> None:
    """Flattening is only safe while it is injective.

    Without this, `OpenAPI/A.yaml` and `OpenAPI/sub/A.yaml` would silently become one file and the
    corpus would be quietly incomplete — the failure mode being avoided is a MISSING document that
    nothing reports.
    """
    with pytest.raises(ValueError, match="flatten"):
        extract(_zip({"a/OpenAPI/A.yaml": b"one", "a/OpenAPI/sub/A.yaml": b"two"}))


def test_compare_separates_the_three_kinds_of_difference() -> None:
    """They mean different things, so one bucket would lose the distinction.

    `unexpected` is a corpus that grew, which is normal for a new release. `wrong_digest` is the one
    that matters: the same filename with different content is a moved tag or a tampered archive.
    """
    expected = {"keep.yaml": "aaa", "gone.yaml": "bbb", "changed.yaml": "ccc"}
    found = {"keep.yaml": "aaa", "changed.yaml": "DIFFERENT", "new.yaml": "ddd"}
    assert compare(found, expected) == {
        "missing": ["gone.yaml"],
        "unexpected": ["new.yaml"],
        "wrong_digest": ["changed.yaml"],
    }


def test_compare_is_clean_when_they_agree() -> None:
    """The negative control: without it, a `compare` that always reported differences would pass above."""
    same = {"a.yaml": "1", "b.yaml": "2"}
    assert compare(same, dict(same)) == {"missing": [], "unexpected": [], "wrong_digest": []}


def test_the_manifest_is_present_and_pins_the_tag() -> None:
    """The manifest is the reproducibility guarantee; a ref alone is only a promise.

    `yang-to-rdf` pins a ref and records a commit in its harness, but its fetch trusts what the ref
    serves. This records a digest per file so a moved tag is a failure rather than a silent corpus
    change.
    """
    assert MANIFEST.exists(), "run scripts/fetch_corpus.py --write-manifest"
    recorded = json.loads(MANIFEST.read_text(encoding="utf-8"))
    assert recorded["ref"] == PINNED_REF
    assert recorded["ref_type"] == PINNED_REF_TYPE
    assert len(recorded["archive_sha256"]) == 64
    # 45 at Tag_Rel19_SA112: 44 documents plus the corpus README.
    assert len(recorded["files"]) == 45
    assert all(len(d) == 64 for d in recorded["files"].values())


def test_on_disk_reports_nothing_for_an_absent_directory(tmp_path: Path) -> None:
    """So `--check` can tell "no corpus" apart from "a corpus that disagrees" and say which."""
    assert on_disk(tmp_path / "nope") == {}
    (tmp_path / "x.yaml").write_bytes(b"hello")
    assert on_disk(tmp_path) == {"x.yaml": digest(b"hello")}


@pytest.mark.network
def test_the_pinned_ref_still_serves_the_manifest_contents() -> None:
    """The live check: does the forge still serve what was pinned?

    This is the test that would catch a MOVED TAG, which is the whole reason the manifest exists, and it
    cannot be done offline. It skips with a stated reason rather than passing, because a network test
    that silently succeeds when the network is absent is worse than no test.

    Deliberately not run in the default suite via `-m "not network"`; it downloads ~200KB.
    """
    import urllib.error

    from scripts.fetch_corpus import download, extract as extract_archive

    try:
        archive = download(archive_url())
    except (urllib.error.URLError, OSError) as error:
        pytest.skip(f"3GPP forge unreachable, so the pin cannot be verified: {error}")

    recorded = json.loads(MANIFEST.read_text(encoding="utf-8"))
    served = {name: digest(data) for name, data in extract_archive(archive).items()}
    buckets = compare(served, recorded["files"])
    assert not any(buckets.values()), (
        f"the pinned ref {PINNED_REF} no longer serves the manifest contents: {buckets}"
    )
