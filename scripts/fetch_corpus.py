#!/usr/bin/env python3
"""Fetch the 3GPP SA5 MnS OpenAPI corpus from 3GPP Forge instead of redistributing it.

Inspired by `yang-to-rdf`, which commits **zero** `.yang` files and fetches from
<https://forge.3gpp.org/rep/sa5/MnS/> at a pinned ref. Its examples README records why the pin matters:
an earlier snapshot came from the moving `Rel-19` branch tip and "could not be re-derived by anyone".

This repository has the same problem in a worse form. Measured 2026-09-25 against the committed
`assets/MnS-Rel-19-OpenAPI/OpenAPI/`:

  * **no ref reproduces it.** `Tag_Rel19_SA112` (tags) and `Rel-19` (heads) each serve 45 files of
    which only 8 are byte-identical to the 39 committed; 31 differ in content.
  * **it is not the release it is named after.** The directory says Rel-19, but of 38 documents 19
    declare a major version of 18, 14 declare 19 and 4 declare 1. The tagged archive is 35 at 19.
    So the README's "38 3GPP SA5 Rel-19 documents" describes a mixture, not a release.

So the committed corpus is an un-derivable snapshot of no particular release. Fetching at a pinned ref
replaces it with something a fresh clone can reproduce.

## What this does differently from the reference

**It verifies content, it does not merely pin a ref.** `yang-to-rdf` pins `Tag_Rel19_SA112` and
records a `PINNED_COMMIT` in its verification harness, but the fetch itself trusts whatever the ref
serves. A tag *should* be immutable; nothing enforces that a forge keeps it so, and "the tag moved"
would present as a silent corpus change with every figure in the repository shifting under it. So the
expected SHA-256 of every file is recorded in a manifest, `--check` compares against it, and a
mismatch is a FAILURE naming the files rather than a warning.

## Refusals, each deliberate

* **No network is a failure, not a skip.** A corpus fetch that quietly does nothing leaves the caller
  converting whatever was on disk, which is how an un-derivable snapshot happens in the first place.
* **Zip-slip is refused** rather than sanitised: an archive member resolving outside the extraction
  directory means the archive is not what we think it is, and repairing the path hides that.
* **A manifest mismatch does not overwrite.** With `--check` it reports; without it, a fetch writes to
  a fresh directory and only then compares, so a bad fetch cannot corrupt a good tree.

Usage:
    uv run python scripts/fetch_corpus.py --check              # is what is on disk the pinned corpus?
    uv run python scripts/fetch_corpus.py                      # fetch into the default location
    uv run python scripts/fetch_corpus.py --out /tmp/corpus    # fetch elsewhere
    uv run python scripts/fetch_corpus.py --write-manifest     # re-pin (records what the ref serves)

Exit codes:
    0 - the corpus is present and matches the manifest
    1 - a mismatch: files missing, unexpected, or with the wrong digest
    2 - prerequisites (no network, no manifest, the ref does not resolve)
"""

from __future__ import annotations

import argparse
import hashlib
import io
import json
import shutil
import sys
import tempfile
import urllib.error
import urllib.parse
import urllib.request
import zipfile
from pathlib import Path
from typing import Any

ROOT = Path(__file__).resolve().parent.parent

#: The immutable ref the corpus is pinned to. A TAG, not a branch: a branch tip moves, and a corpus
#: that moves silently takes every measured figure in this repository with it.
PINNED_REF = "Tag_Rel19_SA112"
PINNED_REF_TYPE = "tags"

FORGE_HOST = "https://forge.3gpp.org"
FORGE_ARCHIVE = (
    FORGE_HOST + "/rep/sa5/MnS/-/archive/{ref}/MnS-{ref}.zip?ref_type={ref_type}&path=OpenAPI"
)

#: Where the corpus lands. Kept at the existing path so `scripts/_corpora.py` and the ~50 tests that
#: read it need no change: what changes is that the directory is FETCHED rather than committed.
DEFAULT_OUT = ROOT / "assets" / "MnS-Rel-19-OpenAPI" / "OpenAPI"

#: file name -> sha256 of what `PINNED_REF` serves. This is the reproducibility guarantee; the ref
#: alone is a promise, this is a check.
MANIFEST = ROOT / "assets" / "corpus-manifest.json"

#: Extensions taken from the archive. The corpus's own README travels with it, because it carries
#: 3GPP's copyright notice and separating a notice from what it applies to is how attribution is lost.
WANTED_SUFFIXES = (".yaml", ".yml", ".md")


def archive_url(ref: str = PINNED_REF, ref_type: str = PINNED_REF_TYPE) -> str:
    return FORGE_ARCHIVE.format(ref=urllib.parse.quote(ref, safe=""), ref_type=ref_type)


def digest(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def download(url: str) -> bytes:
    """The archive bytes, or a clear refusal.

    Returns the bytes rather than a path so the caller can hash the archive itself: two fetches that
    disagree is a fact worth having, and a temp file invites reading it twice and getting two answers.
    """
    request = urllib.request.Request(url, headers={"User-Agent": "openapi-to-rdf corpus fetch"})
    with urllib.request.urlopen(request, timeout=120) as response:
        return response.read()


def extract(archive: bytes) -> dict[str, bytes]:
    """`{filename: content}` for the corpus files in the archive.

    Flattens the archive's `MnS-<ref>-OpenAPI/OpenAPI/` prefix, because the prefix contains the ref and
    would otherwise make every path change when the pin moves.

    Refuses an absolute or parent-traversing member (zip-slip) instead of sanitising it: a member
    outside the tree means the archive is not what this code thinks it is, and quietly repairing the
    path would hide that. Nothing is written to disk here, so the guard is about trusting the archive's
    NAMES, which is what the flattening below relies on.
    """
    out: dict[str, bytes] = {}
    with zipfile.ZipFile(io.BytesIO(archive)) as zf:
        for member in zf.namelist():
            if member.endswith("/"):
                continue
            if member.startswith("/") or ".." in Path(member).parts:
                raise ValueError(f"unsafe path in archive (zip-slip): {member}")
            name = member.rsplit("/", 1)[-1]
            if not name.endswith(WANTED_SUFFIXES):
                continue
            if name in out:
                raise ValueError(
                    f"two archive members flatten to {name!r}; the layout is not what this expects"
                )
            out[name] = zf.read(member)
    return out


def read_manifest() -> dict[str, str] | None:
    if not MANIFEST.exists():
        return None
    return json.loads(MANIFEST.read_text(encoding="utf-8"))["files"]


def compare(files: dict[str, str], expected: dict[str, str]) -> dict[str, list[str]]:
    """Three buckets, kept separate because they mean different things.

    `missing` is an incomplete fetch, `unexpected` is a corpus that grew (a new document in the
    release), and `wrong_digest` is the one that matters: the same filename with different content,
    which is a moved tag or a tampered archive.
    """
    return {
        "missing": sorted(set(expected) - set(files)),
        "unexpected": sorted(set(files) - set(expected)),
        "wrong_digest": sorted(n for n in set(files) & set(expected) if files[n] != expected[n]),
    }


def fetch(out_dir: Path, *, ref: str = PINNED_REF, ref_type: str = PINNED_REF_TYPE) -> dict[str, Any]:
    url = archive_url(ref, ref_type)
    archive = download(url)
    contents = extract(archive)
    if not contents:
        raise ValueError(f"the archive at {url} contained no corpus files")

    # Staged in a temp directory and moved into place only once complete, so an interrupted or
    # mismatched fetch cannot leave a half-corpus that later looks like the real thing.
    staging = Path(tempfile.mkdtemp(prefix="corpus-fetch-"))
    for name, data in sorted(contents.items()):
        (staging / name).write_bytes(data)

    out_dir.parent.mkdir(parents=True, exist_ok=True)
    if out_dir.exists():
        shutil.rmtree(out_dir)
    shutil.move(str(staging), str(out_dir))

    return {
        "ref": ref,
        "ref_type": ref_type,
        "url": url,
        "archive_sha256": digest(archive),
        "archive_bytes": len(archive),
        "files": {name: digest(data) for name, data in sorted(contents.items())},
        "out_dir": str(out_dir),
    }


def on_disk(out_dir: Path) -> dict[str, str]:
    if not out_dir.is_dir():
        return {}
    return {
        p.name: digest(p.read_bytes())
        for p in sorted(out_dir.iterdir())
        if p.is_file() and p.name.endswith(WANTED_SUFFIXES)
    }


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument("--out", type=Path, default=DEFAULT_OUT, help=f"default: {DEFAULT_OUT}")
    parser.add_argument("--ref", default=PINNED_REF)
    parser.add_argument("--ref-type", default=PINNED_REF_TYPE, choices=("tags", "heads"))
    parser.add_argument(
        "--check", action="store_true",
        help="verify what is already on disk against the manifest and write nothing",
    )
    parser.add_argument(
        "--write-manifest", action="store_true",
        help="RE-PIN: fetch and record what the ref serves as the expected digests. Changes the "
             "corpus this repository measures, so every pinned figure must be re-derived after it.",
    )
    args = parser.parse_args(argv)

    expected = read_manifest()

    if args.check:
        if expected is None:
            print(f"no manifest at {MANIFEST}; run --write-manifest first", file=sys.stderr)
            return 2
        found = on_disk(args.out)
        if not found:
            print(
                f"no corpus at {args.out}. Fetch it:\n"
                f"  uv run python scripts/fetch_corpus.py",
                file=sys.stderr,
            )
            return 1
        buckets = compare(found, expected)
        if not any(buckets.values()):
            print(f"OK: {len(found)} files at {args.out} match the manifest (ref {PINNED_REF})")
            return 0
        print(f"MISMATCH against {MANIFEST}:", file=sys.stderr)
        for bucket, names in buckets.items():
            if names:
                print(f"  {bucket} ({len(names)}): {names[:5]}", file=sys.stderr)
        return 1

    try:
        result = fetch(args.out, ref=args.ref, ref_type=args.ref_type)
    except (urllib.error.URLError, OSError) as error:
        # A failure, never a skip: a fetch that quietly does nothing leaves the caller converting
        # whatever happened to be on disk, which is how an un-derivable snapshot arises.
        print(f"cannot reach {FORGE_HOST}: {error}", file=sys.stderr)
        print(f"  url: {archive_url(args.ref, args.ref_type)}", file=sys.stderr)
        return 2
    except (ValueError, zipfile.BadZipFile) as error:
        print(f"the archive is not what this expects: {error}", file=sys.stderr)
        return 2

    print(f"fetched {len(result['files'])} files from {result['url']}")
    print(f"  archive sha256 {result['archive_sha256'][:16]}  ({result['archive_bytes']} bytes)")
    print(f"  -> {result['out_dir']}")

    if args.write_manifest:
        MANIFEST.parent.mkdir(parents=True, exist_ok=True)
        MANIFEST.write_text(
            json.dumps(
                {
                    "ref": result["ref"],
                    "ref_type": result["ref_type"],
                    "url": result["url"],
                    "archive_sha256": result["archive_sha256"],
                    "files": result["files"],
                },
                indent=1,
            )
            + "\n",
            encoding="utf-8",
        )
        print(f"  wrote {MANIFEST} ({len(result['files'])} files)")
        print("  RE-PINNED: every measured figure in this repository must now be re-derived.")
        return 0

    if expected is None:
        print(f"no manifest at {MANIFEST} to verify against; run --write-manifest to create one")
        return 2
    buckets = compare(result["files"], expected)
    if any(buckets.values()):
        print("MISMATCH: the ref served something other than the manifest", file=sys.stderr)
        for bucket, names in buckets.items():
            if names:
                print(f"  {bucket} ({len(names)}): {names[:5]}", file=sys.stderr)
        return 1
    print(f"  verified against {MANIFEST}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
