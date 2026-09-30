# From OpenAPI to an operable ontology: the method behind `openapi-to-rdf`

**Methodology companion.** Supporting material for the paper, and the citable record of *why* this
tool's outputs take the shape they do.

> **This document describes commit `5d87333` of `openapi-to-rdf` 0.2.0** (branch
> `feature/external-schema-ingestion`). An archived version of this document is immutable; the
> repository is not. Within one week of writing, the minted-IRI host changed, the 3GPP corpus went
> from 38 vendored documents to 44 fetched ones, and two design decisions reversed. Treat any claim
> here as scoped to that commit, and prefer `HYPOTHESES.md` in the repository for the current state.

## What this is, and what it is not

The paper covers *functionality* under a page limit. This companion carries what does not fit: the six
projections in technical detail, the multi-document handling, the corpus methodology, and the gaps
that remain open.

It is **not** a manual. `CONVERSION_DOC.md` in the repository already documents what maps to what,
construct by construct, across roughly a thousand lines — and carries almost no rationale. This
document carries the rationale and does not repeat the mapping. The test applied to every paragraph:
*if a reader could not disagree with it, it belongs in `CONVERSION_DOC.md` instead.*

## The problem, in one paragraph

An OpenAPI description says how bytes are arranged on a wire. An agent operating a network needs to
know what those bytes *denote*: which thing is being addressed, whether two documents are talking
about the same thing, and what actions are available on it. OpenAPI under-determines all three, and it
does so in ways that are identifiable and measurable rather than vague. This document characterises
the under-determination in three layers — **identity**, **meaning**, and **affordance** — states the
resolution this tool adopts for each, and reports what it cost when the resolution was wrong. The tool
is the instrument; the characterisation is the contribution.

## Contents

| file | covers |
|---|---|
| `01-method.md` | One derived fact set, five projections, and the reconciliation gate |
| `02-tbox-and-shapes.md` | The RDFS vocabulary and the SHACL shapes, and the line between them |
| `03-operations-and-context.md` | Hydra operation graph, JSON-LD context, Overlay, property index |
| `04-multi-document.md` | Multi-document descriptions, and identity that does not depend on file layout |
| `05-corpora-and-verification.md` | Why two corpora, and the gates that can actually observe a defect |
| `06-gaps.md` | What remains open, with status |
| `99-appendix.md` | Decision table, IRI grammar, glossary, and figure→command provenance |

## A note on the numbers

Every figure in this document is either produced by a committed script in the repository or marked as
not yet re-derivable. `99-appendix.md` carries that mapping in full, including the four figures that
**cannot** currently be reproduced from a fresh clone. A number whose provenance is unstated should be
read as unverified; we have tried to leave none, and to be explicit where we failed.

Two conventions used throughout:

* **"measured"** means a committed script produced the figure on a named corpus. **"observed"** means
  it was seen once in a diagnostic run and is not yet re-derivable.
* Where a decision is **ours** rather than a standard's — a minted term, an IRI scheme, a marker
  predicate — the text says so. No claim of external authority is implied by any IRI this tool emits.

## Status of the referenced standards

Part of the claim, because a reader who cannot tell these apart is misled by omission:

| | status |
|---|---|
| RDF 1.1, RDF Schema 1.1, SHACL, JSON-LD 1.1 | W3C Recommendations |
| OpenAPI Specification 3.x | Linux Foundation / OpenAPI Initiative specification |
| Hydra Core Vocabulary | W3C **Community Group draft** — not a Recommendation |
| OpenAPI Overlay Specification 1.1.0 | OpenAPI Initiative; the version this tool emits (`overlay: 1.1.0`). Recent |
| `draft-polli-restapi-ld-keywords` (`x-jsonld-type`, `x-jsonld-context`) | an Internet-Draft; by its name an *individual* submission (`draft-polli-…`, not `draft-ietf-…`) — status not independently checked here. Not a standard |
| `affordance:` terms (`invokedAt`) | **ours.** Minted by this project, asserts no external authority |

The tool emits into all of them. It does not treat them as equivalent in weight, and neither should a
consumer deciding what to depend on.
