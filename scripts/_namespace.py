#!/usr/bin/env python3
"""The IRI stem this project's COMMITTED artifacts are minted under.

Lives in `scripts/` and not in the package on purpose. `openapi_to_rdf` is a general-purpose converter
and takes `base_namespace` as an argument; it must not default to an Ericsson namespace for someone
converting their own documents. What belongs to this repository is the stem ITS OWN published
artifacts use, and that is a decision about our artifacts, not about the tool.

**Why this exists: the committed artifacts were minted under `https://example.org/`.** 704 files of
RDF and SHACL, published on GitHub, under a namespace that asserts nothing and collides with every
other example.org user. Anyone copying the vocabulary inherited a placeholder, and the more useful the
output is, the more likely they never notice.

## What this stem is, and is not

**It is OURS and asserts no external authority.** `semantics.ericsson.com` does not resolve, and
nothing here claims it does. An IRI need not dereference to identify something, but a reader must not
be left to infer that a name is official because it looks like a company's. These vocabularies are
derived from 3GPP documents and are not 3GPP's model of their own APIs -- see `NOTICE` and the header
in every generated file.

**It is a PROPOSAL, and every IRI is derived.** Adopting a standardised stem later is a regeneration
with a different value here, not a data migration.

## Where `example.org` is still correct, and stays

11 test files pass `https://example.org/` and should keep doing so. A unit test converting a synthetic
two-schema document is not publishing a vocabulary, and pinning it to an Ericsson stem would make the
tests assert this repository's naming policy alongside the behaviour they exist to check. The rule is
about ARTIFACTS THIS REPOSITORY PUBLISHES -- `output/`, `test-cases/`, and anything a reader copies --
not about every string in the tree.

## SETTLED 2026-09-25: `semantics` (plural) is the host

Decided by the repository owner. Everything this project mints now uses it:

    document namespaces   https://semantics.ericsson.com/openapi/<document>/
    marker vocabulary     https://semantics.ericsson.com/vocab/transport/
    affordance terms      https://semantics.ericsson.com/ontology/affordance/

`snm-api-native` still mints its TRANSPORT markers under the SINGULAR host, and that is deliberate
rather than forgotten: its frozen migration baseline is git history and carries markers under the old
stem, so renaming the constant makes `gate_lift_parity`'s before-arm match nothing and report
`coerced_iri 0 -> 0` as a PASS. Measured: 3 markers found under singular, 0 under plural. That gate now
REFUSES an empty monitored set rather than passing vacuously, so the migration is safe to attempt --
but it is a separate change with its own verification, not a find-and-replace.

The affordance term was migrated in both repositories, because unlike a document namespace it is the
SAME predicate meaning the same thing in both, and `tests/test_operation_graph.py` in the consumer now
gates that the two declarations agree.

## The inconsistency this replaced, kept for the reasoning

Two sibling Ericsson Research projects mint under hosts that differ by ONE LETTER:

    yang-to-rdf      https://semantics.ericsson.com/yang/<module>#
    snm-api-native   https://semantic.ericsson.com/ontology/...

That is a federation hazard and not a cosmetic one: the entire premise of minting IRIs this way is
that two independent systems produce the SAME IRI for the same thing, and one letter apart they never
join. Neither host resolves, so neither is "the live one", and no determination in either repository
says which is canonical.

This file follows **yang-to-rdf** (`semantics`, plural), because that is the closest sibling: same
organisation, same 3GPP corpus, same public GitHub channel, and the same shape of artifact. That is a
consistency argument, NOT a ruling on which host is right -- whoever owns the naming should settle it,
and when they do, this is the one line to change.
"""

from __future__ import annotations

#: The stem. Per-document namespaces hang off it; see `document_namespace`.
BASE_STEM = "https://semantics.ericsson.com/openapi/"


def document_namespace(document_stem: str) -> str:
    """The namespace for one OpenAPI document, e.g. ``TS28623_ComDefs``.

    Slash-separated, matching the class/property scheme the converter mints (`<ns>/<Class>` and
    `<ns>/<Class>/<property>`). Deliberately not hash-separated: a hash namespace per document would
    put every class of every document in one fragment space, which is what the separator change on
    2026-09-22 moved away from after the library minted 167 hash namespaces for a single document.
    """
    return f"{BASE_STEM}{document_stem}/"
