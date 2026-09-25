"""Corpus resolution, shared by the measurement scripts in this directory.

A corpus is a label and a set of OpenAPI documents. Two are named here because every figure this
project quotes is measured on both, and a figure from one corpus is not a figure for the tool:

* **3GPP** — the 38 SA5 management specifications shipped in ``assets/``. Heavy cross-document
  ``$ref``, dashes in schema names, almost no inheritance.
* **TM Forum** — the three Open API v5 documents this tool's downstream consumer uses. Self-contained,
  no dashes, deep ``allOf`` inheritance.

They exercise disjoint parts of the converter, which is why several determinations measured on 3GPP
alone turned out to be invisible there: declaring-class attribution has **0 non-trivial cases in
2,822** on 3GPP and 138 in 3,033 on TM Forum.

The TM Forum documents are not redistributed with this repository, so that corpus is resolved from a
path and skipped **with a stated reason** when absent — never silently.
"""

from __future__ import annotations

import argparse
from pathlib import Path

#: The 3GPP corpus is FETCHED, not shipped (2026-09-25). `scripts/fetch_corpus.py` downloads it at the
#: pinned tag in `assets/corpus-manifest.json`; the documents are 3GPP's and this repository is no
#: longer a second copy of them. Anything that cannot find the directory should name that command in
#: its skip reason -- see `FETCH_HINT` -- because "corpus not found" without the remedy sends a reader
#: looking for a missing file rather than running one command.
GPP_DIR = Path("assets/MnS-Rel-19-OpenAPI/OpenAPI")

#: 44 documents at `Tag_Rel19_SA112`, up from the 38 that used to be committed.
#:
#: The old 38 were not a release: 19 of them declared a major version of 18 in a directory named
#: Rel-19, and NO ref reproduced the set -- 8 of 39 files matched the tag byte-for-byte. The 6 extra
#: documents here (EnergyInformationNrm, NdtNrm, CclNrm, PlanManagement, ExternalDataMgmtNrm,
#: FeatureNrm) are ones the un-derivable snapshot simply lacked.
GPP_EXPECTED = 44

#: Printed whenever the corpus is absent. One command, so a fresh clone is not a puzzle.
FETCH_HINT = "run `uv run python scripts/fetch_corpus.py` (downloads from 3GPP Forge at a pinned tag)"

#: The TM Forum v5 documents, by filename. Resolved relative to ``--tmforum-dir``.
TMF_FILENAMES = (
    "tmf620-product-catalog-management-v5.yaml",
    "tmf622-product-ordering-v5.yaml",
    "tmf641-service-ordering-v5.yaml",
)
TMF_DEFAULT_DIR = Path("/home/earejma/snm-api-native-src")


class Corpus:
    """A label, a document list, and why it is empty if it is."""

    def __init__(self, label: str, paths: list[Path], skip_reason: str | None = None) -> None:
        self.label = label
        self.paths = paths
        self.skip_reason = skip_reason

    def __bool__(self) -> bool:
        return bool(self.paths)


def add_arguments(parser: argparse.ArgumentParser) -> None:
    """Add the corpus options every measurement script in this directory accepts."""
    parser.add_argument(
        "--gpp-dir",
        type=Path,
        default=GPP_DIR,
        help=f"directory of 3GPP OpenAPI documents (default: {GPP_DIR})",
    )
    parser.add_argument(
        "--tmforum-dir",
        type=Path,
        default=TMF_DEFAULT_DIR,
        help=(
            "directory holding the three TM Forum v5 documents. Not redistributed with this "
            f"repository (default: {TMF_DEFAULT_DIR})"
        ),
    )
    parser.add_argument(
        "--corpus",
        action="append",
        metavar="LABEL=PATH",
        default=[],
        help=(
            "an extra corpus, as LABEL=PATH where PATH is a .yaml file or a directory of them. "
            "Repeatable. Given at least once, ONLY the corpora named this way are measured."
        ),
    )
    parser.add_argument("--json", type=Path, default=None, help="also write results as JSON")


def tmforum(directory: Path | None = None) -> Corpus:
    """The TM Forum corpus, resolved without an argparse namespace.

    Exists so `tests/test_tmforum_corpus.py` can reach the same document list the scripts use,
    instead of hardcoding three filenames a fourth time. The skip reason travels with the empty
    corpus: these documents are not redistributed with the repository, and a test that passed
    quietly on a machine without them would be exactly the false assurance AC-8 exists to prevent.
    """
    base = directory or TMF_DEFAULT_DIR
    paths = [base / name for name in TMF_FILENAMES]
    missing = [p for p in paths if not p.is_file()]
    if missing:
        return Corpus(
            "TMForum",
            [],
            f"{len(missing)} of {len(paths)} documents absent under {base} "
            "(not redistributed with this repository)",
        )
    return Corpus("TMForum", paths)


def resolve(args: argparse.Namespace) -> list[Corpus]:
    """Every corpus to measure, in report order, each either populated or carrying a skip reason.

    A corpus that cannot be found is returned with ``skip_reason`` set rather than dropped: a summary
    that quietly covers one corpus instead of two is worse than one that fails, because the number it
    prints still looks like a number for the tool.
    """
    if args.corpus:
        corpora = []
        for spec in args.corpus:
            if "=" not in spec:
                raise SystemExit(f"--corpus expects LABEL=PATH, got {spec!r}")
            label, _, raw = spec.partition("=")
            path = Path(raw)
            if path.is_dir():
                corpora.append(Corpus(label, sorted(path.glob("*.yaml"))))
            elif path.is_file():
                corpora.append(Corpus(label, [path]))
            else:
                corpora.append(Corpus(label, [], f"path not found: {path}"))
        return corpora

    gpp = sorted(args.gpp_dir.glob("*.yaml")) if args.gpp_dir.is_dir() else []
    if not gpp:
        gpp_corpus = Corpus("3GPP", [], f"no documents under {args.gpp_dir}; {FETCH_HINT}")
    else:
        if len(gpp) != GPP_EXPECTED:
            # Asserted rather than accepted: the count is quoted in commit messages and reports, and
            # a corpus that silently grew or shrank invalidates every one of them.
            raise SystemExit(
                f"expected {GPP_EXPECTED} 3GPP documents under {args.gpp_dir}, found {len(gpp)}. "
                f"Every 3GPP figure in this repository is quoted against {GPP_EXPECTED}; re-pin them "
                f"deliberately. If the corpus was hand-modified, {FETCH_HINT} to restore the pin."
            )
        gpp_corpus = Corpus("3GPP", gpp)

    # One definition of the TM Forum document list, used by scripts and tests alike.
    tmf_corpus = tmforum(args.tmforum_dir)

    return [gpp_corpus, tmf_corpus]


def report_skips(corpora: list[Corpus]) -> int:
    """Print why any corpus was skipped. Returns the number skipped, for the caller's exit status."""
    skipped = [c for c in corpora if not c]
    for corpus in skipped:
        print(f"SKIPPED {corpus.label}: {corpus.skip_reason}")
    return len(skipped)
