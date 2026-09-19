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

#: The 3GPP corpus ships with this repository.
GPP_DIR = Path("assets/MnS-Rel-19-OpenAPI/OpenAPI")
GPP_EXPECTED = 38

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
        gpp_corpus = Corpus("3GPP", [], f"no documents under {args.gpp_dir}")
    else:
        if len(gpp) != GPP_EXPECTED:
            # Asserted rather than accepted: the count is quoted in commit messages and reports, and
            # a corpus that silently grew or shrank invalidates every one of them.
            raise SystemExit(
                f"expected {GPP_EXPECTED} 3GPP documents under {args.gpp_dir}, found {len(gpp)}. "
                "Every 3GPP figure in this repository is quoted against 38; re-pin them deliberately."
            )
        gpp_corpus = Corpus("3GPP", gpp)

    tmf_paths = [args.tmforum_dir / name for name in TMF_FILENAMES]
    missing = [p for p in tmf_paths if not p.is_file()]
    if missing:
        tmf_corpus = Corpus(
            "TMForum",
            [],
            f"{len(missing)} of {len(tmf_paths)} documents absent under {args.tmforum_dir} "
            "(not redistributed with this repository; pass --tmforum-dir)",
        )
    else:
        tmf_corpus = Corpus("TMForum", tmf_paths)

    return [gpp_corpus, tmf_corpus]


def report_skips(corpora: list[Corpus]) -> int:
    """Print why any corpus was skipped. Returns the number skipped, for the caller's exit status."""
    skipped = [c for c in corpora if not c]
    for corpus in skipped:
        print(f"SKIPPED {corpus.label}: {corpus.skip_reason}")
    return len(skipped)
