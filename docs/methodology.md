# openapi-to-rdf — methodology companion

> **This file is an outline, not a draft.** Each section states what it must carry and where its
> numbers come from. Prose goes in as it is written; nothing below should be read as a finished claim.
>
> **Purpose.** Supporting material for the paper, and the citable record of *why* the outputs look
> the way they do. The paper covers functionality under a page limit; this carries the technical
> treatment it cannot fit — the six projections, the multi-document handling, the corpora argument and
> the gaps.
>
> **Division of labour, so this does not become a second manual.** `CONVERSION_DOC.md` already
> documents *what maps to what*, construct by construct, in 1,033 lines — and carries almost no
> rationale. This document carries the rationale and does not repeat the mapping. Test for every
> paragraph: **if a reader could not disagree with it, it belongs in `CONVERSION_DOC.md` instead.**
>
> **Pin the commit.** An arXiv version is immutable and this repository is not; the IRI host, the
> corpus and several decisions all moved inside one week. State the SHA this describes in the abstract
> and plan on a v2 rather than implying stability.

---

## Part I — Method

### 1. Scope, the commit described, and how to cite

What this is, what the paper covers instead, the commit SHA, and the licence/provenance of the corpora
it quotes.

### 2. One derived fact set, five projections

Why every artifact is a projection of a single `Mapping` rather than its own walk over the document,
and the reconciliation gate that holds them to one set of class IRIs.

*Motivation, measured:* two independently-derived class mappings over one corpus agreed on the 75
schema names they shared and **disagreed on 132** — and nothing detected it, because nothing compared
them.

---

## Part II — The projections

Each section opens with **what the projection is for and what it decides**, then the mechanics. That
order is what keeps it citable rather than a manual. One worked example — the same schema fragment
rendered six ways — runs through the whole of Part II.

### 3. TBox: the RDFS vocabulary

Class derivation; declaring-class attribution; the entailment budget; referents and serialisation
artifacts; transport envelopes.

Figures to carry: attribution resolves **93.9%** of (class, property) pairs against an
independently-authored reference TBox where leaf attribution resolves **68.5%**. One `rdfs:range` and
one `rdfs:domain` per property, because both propagate under entailment — `Addressable#href` had
carried 12 domains and `Event#event` 25. A `*Ref` contributes an edge to its referent, and the
referent is often declared nowhere (**39 of 56** on TMF620), so it is minted and marked as ours.
Transport envelopes are classified rather than dropped: omitting them cut a nested domain object from
**76 triples to 1**.

### 4. SHACL shapes

Targeting; the constraint families; cardinality; and what is deliberately expressed here rather than
entailed, because a shape binds without propagating.

Figures to carry: one NodeShape per class (`sh:targetClass` 2,399 → 1,770 against 1,769 classes when
`allOf` stopped being processed twice). Shape-vs-class coverage, and why an unshaped class means every
constraint on it is inert.

### 5. Hydra operation graph

**No standard gives an API operation an identity in RDF** — Hydra supplies a *type*, not an identifier
scheme. So the scheme is ours, derived from method, path, API and major version.

Figures to carry: `operationId` is rejected as a key because it is optional in OAS and duplicated in
practice. Omitting the API and version segments collapsed TMF641 v5 and v4.1 into **20 of 20 identical
operation IRIs**, and made `delete/hub/{id}` **one node shared by TMF620, TMF622 and TMF641**. TM Forum
writes every response and request body as a `$ref` into `components`, so a reader looking only for
inline `content` resolves **0 of 8** response classes and **0 of 14** request classes. Distinguishing
a bodyless `DELETE` from an unresolved body: counting bodyless operations as unresolved reported
**8 of 20** resolved on TMF641 where the truth is **8 of 8**.

### 6. JSON-LD context

Type-scoped terms; `@type: @id` coercion for IRI-valued properties; the ancestry walk that carries
inherited terms.

Figures to carry: a consumer deriving coercions from the vocabulary got **0 of 36** on TMF641 while
the fact lived only in the `Mapping`. And the contradiction that followed once it reached the TBox —
`isIriValued true` beside `rdfs:range xsd:string` on the same property, **15 of 15** IRI-valued
properties on TMF641, which entails typing a URL as a string.

### 7. OpenAPI Overlay

Annotating the source description in place, and why the annotation is separable from the description.

Figure to carry: deriving with an *empty* TBox produces a byte-identical overlay body — the adoption
argument in one line.

### 8. Property index

Collision reporting — same local name, different range or description — and the merge step that is
deliberately deferred because merging is an opinionated modelling decision.

---

## Part III — Corpora, verification, gaps

### 9. Multi-document descriptions

OAS treats a multi-document description as one description. What that requires of identity, and the
one thing the caller must supply because no document states it: whether a file boundary is a
*vocabulary* boundary. 3GPP and TM Forum need opposite answers.

Figures to carry: **175 dangling class targets** in a published output tree when a class's IRI followed
the referring document rather than the declaring one; **34** misattributed (class, property) pairs when
external ancestors went unregistered; **216 datatype disagreements** from a `$ref` the `Mapping` did
not follow across a document boundary.

### 10. Corpora: why two, not one

The table this section exists for — decisions implemented and validated against 3GPP that were
structurally **unreachable** there and live on TM Forum:

| decision | 3GPP | TM Forum |
|---|---|---|
| properties carrying two `rdfs:range` | 6 of 3,622 (0.17%) | 167 of 2,895 (5.8%) |
| properties carrying two `rdfs:domain` | 0 of 3,622 | 28 of 2,895 |
| non-trivial declaring-class attribution | 0 of 2,822 | 138 of 3,033 |
| IRI-valued properties (so the `xsd:string` contradiction) | 0 | 15 |

The claim: **structural diversity of the corpus, not its size, determines what a guard can see.**

Also here: corpus reproducibility as a methods finding — a committed "Rel-19" snapshot matched no
upstream ref (8 of 39 files), and 19 of its 38 documents declared a Rel-18 version.

### 11. Verification

The gates and what each actually asserts; the split-vs-whole differential test and why an equality
whose answer is known in advance catches defects nobody was looking for; and why **"100% conversion
success" is a vacuous metric** — every defect in this document converted successfully.

### 12. Known gaps, with status

The section that makes the citation credible. Each with a status, not a promise:

* whole-vs-split vocabularies not yet isomorphic;
* 128 of 965 property IRIs unscoped and carrying no `rdfs:domain`;
* **17 dangling class targets on 3GPP, in three distinct shapes** — 15 pure `$ref` aliases
  (`is_primitive_def` cannot see through an external `$ref` to a primitive), plus `JobDetails`
  carrying both `additionalProperties` and `properties`, plus `MdtAlignmentInfo` carrying `format`
  and `pattern` with **no `type`** at all. The last is the interesting one: a typeless schema with
  string facets is neither a class nor a recognisable primitive, and nothing in the standard requires
  `type`;
* 3 declared terms in `TS28572_PlanManagement.yaml` with no NodeShape, so inert;
* the deferred OWL emitter;
* provenance unresolved for one TM Forum document, so it is not redistributed.

---

## Appendices

A. Full decision table. B. IRI grammar. C. Glossary — referent, serialisation artifact, transport
envelope, declaring class.

---

## Figure → command

Every number above names the command that produces it. **Filling this table in is the task that
verifies the document**, and it is already known to be incomplete:

| figure | produced by | status |
|---|---|---|
| dangling class targets, per corpus | `uv run python scripts/measure_corpora.py` | **committed and current** (re-run 2026-09-30, 44/44 and 3/3 converted): 3GPP **17**, TM Forum **0**. The earlier 9 on 3GPP was measured on the 38-document snapshot and is not comparable — all 9 are still present, and 8 more arrived with the 6 added documents, uncharacterised. TM Forum's 0 supersedes the artifact's stale 98 |
| whole-vs-split divergence, isomorphism | `uv run python scripts/measure_split_isomorphism.py` | committed |
| corpus census: classes, properties, ranges, domains | `uv run python scripts/measure_corpora.py` | committed |
| declaring-class attribution 93.9% / 68.5% | — | **measured in `snm-api-native` against its reference TBox; no committed script in this repo** |
| 75 agreed / 132 disagreed class mappings | — | **historical, from the consolidation work; not re-derivable here** |
| 39 of 56 undeclared referents; 15 of 15 IRI-valued | ad-hoc | **needs promoting to a committed script before being quoted** |
| operation IRI collisions (20 of 20; `/hub`) | — | **recorded in `mapping.py` docstrings; no script** |

Anything still marked otherwise than "committed" when the document is submitted is a number the
reader cannot check.
