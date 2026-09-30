# 3. Operations, context, overlay, and the property index

These four projections share a theme the vocabulary and shapes do not: they exist to make an API
*operable* rather than merely describable. Each opens with what it decides.

## 3.1 The Hydra operation graph — an identity no standard supplies

**What it decides.** An API operation has no identity in RDF. Hydra Core supplies a *type*
(`hydra:Operation`), not an identifier scheme, and no standard names operations at all. So the IRI
scheme is **ours**, and asserts no external authority. Hydra itself is a W3C Community Group draft, not
a Recommendation; depending on it is a choice with weight.

For the worked example's single `GET`:

```turtle
<https://example.org/wx/Agreement> hydra:supportedOperation <…/operation/workedExample/v5/get/agreement/%7Bid%7D> .

<…/operation/workedExample/v5/get/agreement/%7Bid%7D> a hydra:Operation ;
    dcterms:hasVersion "5.0.0" ;
    hydra:method "GET" ;
    hydra:returns <https://example.org/wx/Agreement> ;
    affordance:invokedAt <…/get/agreement/%7Bid%7D#template> .

<…#template> a hydra:IriTemplate ;
    hydra:template "/agreement/{id}" .
```

Note `hydra:returns <…/Agreement>`: the operation reads the class IRI the vocabulary declares, from the
same `Mapping`, so an operation and a class cannot disagree about what a response is. That identity is
why one `Mapping` replaces four conversions (§1).

### The key: method, path, API, major version

The IRI is built from `<namespace>operation/<api-slug>/<major-version>/<method><path>`, percent-encoded.
It is **not** built from `operationId`, for a reason that is a fact about the standard: `operationId` is
*optional* in OpenAPI and duplicated in practice, while a method is unique within a Path Item Object by
construction. A scheme that needs an optional field has no defined behaviour on a document that omits
it. `info.title` and `info.version` are *required*, so both extra segments are always derivable.

The API and version segments were added to fix two measured collisions, and both failures are
**silent** — they merge nodes rather than raising:

* **Across versions.** Converting TMF641 v5 and v4.1 under one namespace produced **20 of 20 identical
  operation IRIs**: every operation of both versions was one node, carrying two conflicting
  `dcterms:hasVersion` literals.
* **Across APIs.** Every TM Forum API defines `/hub`, so `delete/hub/{id}` was **one node shared by
  TMF620, TMF622 and TMF641** — two collisions per pair.

*(Provenance: recorded in `mapping.py`'s docstrings from the original measurement. No committed script
reproduces the 20-of-20 figure; see `99-appendix.md`.)*

The major version only, not the full version: a patch bump must not move every operation IRI.

### Resolving what an operation returns

The class an operation returns is not always where a reader looks. TM Forum writes *every* response and
request body as a `$ref` into `components/responses` or `components/requestBodies`, so a reader that
only looks for an inline `content` block resolves **0 of 8** response classes and **0 of 14** request
classes on TMF641. The resolver follows document-internal `$ref`s through the whole chain, and a list
endpoint records its **item** class rather than an anonymous array — an array is not a kind of thing.

### Telling "no body" from "could not resolve"

A `DELETE` returning nothing, and a response whose `$ref` could not be resolved, both yield "no class".
The first is correct and the second is a defect. Without a flag distinguishing them, any report of
resolution quality has to treat them alike, and the denominator it prints is wrong.

> **Observed**, from the consuming project's operation emitter: counting bodyless operations as
> unresolved reported **8 of 20** resolved on TMF641, where the truth is **8 of 8** — the other twelve
> are `DELETE`s and notification listeners that correctly return nothing. A 40% success rate and a
> 100% one, from the same graph.

So `OperationFact` carries `has_request_body` and `has_response_body` alongside `returns_class` and
`accepts_class`. This generalises beyond operations: **a metric that cannot distinguish absence from
failure prints a wrong denominator**, and the error is invisible because the number still looks like a
number.

### `affordance:invokedAt` is ours

The template that says *where* an operation is invoked uses `affordance:invokedAt`, a term minted by
this project in `https://semantics.ericsson.com/ontology/affordance/`. It exists because Hydra has no
term for it, and it asserts no external authority. The namespace string is defined in this repository
and, per a comment at its definition, **independently in `snm-api-native`**, with — until noticed —
nothing gating that the two agree. A copy of a value the code owns is a copy that drifts.

## 3.2 The JSON-LD context — carrying meaning across the wire

**What it decides.** Which JSON keys are which terms, and which values are IRIs rather than strings.
Without it, a JSON payload is syntactically JSON and semantically nothing.

The context is *type-scoped* (JSON-LD 1.1): each class gets a term whose own `@context` maps the
properties valid on it. Quoted from the worked example:

```json
"Addressable": {
  "@id": "https://example.org/wx/Addressable",
  "@context": {
    "@type": { "@type": "http://www.w3.org/2001/XMLSchema#string",
               "@id":   "https://example.org/wx/Extensible/@type" },
    "id":   "@id",
    "href": { "@type": "@id", "@id": "https://example.org/wx/Addressable/href" }
  }
}
```

Three decisions are visible in those few lines.

**`id` maps to `@id`.** The JSON `id` becomes the node identifier, not an ordinary property.

**`href` carries `"@type": "@id"`.** This is the `isIriValued` fact reaching the context, coercing the
string into a followable edge. It is worth recording that this fact *lived only in the `Mapping`* for
some time, and a consumer deriving coercions from the vocabulary got **0 of 36** on TMF641: every `href`
lifted as a literal instead of an edge, and the consumer's own guard accepted the file because a
*different* marker family was present. The fact now reaches the TBox as well, which is how §2.5's
contradiction became visible.

**`Addressable` includes `Extensible`'s `@type` term.** An inherited term appears on the subclass. The
context walks `ClassFact.parents` — which now crosses document boundaries (§4) — so a class inherits
its ancestors' terms wherever they were declared.

A term definition that contains its own `@context` is a **JSON-LD 1.1** feature, so the context must
declare `"@version": 1.1`. A processor in `json-ld-1.0` mode is required to signal an error for such a
term, and the practical outcome is that the scoped terms are dropped — **95 of them on TMF641,
including every `href` coercion**, which is what turns a string into a followable edge. Omitting the
declaration made the context's most important content conditional on a processor's default mode.

## 3.3 The Overlay — annotating the source without editing it

**What it decides.** Where the semantic annotations live relative to the source description. An OpenAPI
Overlay (emitted as `overlay: 1.1.0`) is a list of `target`/`update` actions applied to a document, so
the vendor's description stays unmodified and the annotation is separable and re-derivable:

```json
{ "target": "$.components.schemas['Addressable']",
  "description": "Annotate Addressable with JSON-LD type and context (draft-polli-restapi-ld-keywords)",
  "update": { "x-jsonld-type": "https://example.org/wx/Addressable",
              "x-jsonld-context": { "@version": 1.1, … } } }
```

The `x-jsonld-*` keywords come from `draft-polli-restapi-ld-keywords`, an Internet-Draft that is by its
name an individual submission. They are a **draft convention**, not a standard, and the action
description names the source so a reader is never left to guess.

The adoption argument is one measurement: deriving the overlay against an *empty* TBox produces a
**byte-identical** overlay body. The conversion needs only the OpenAPI document; nothing about the
annotation depends on a hand-authored ontology existing first.

## 3.4 The property index — reporting what is not merged

**What it decides.** What to do when two classes declare the same local name. The answer this tool
gives is *nothing, but say so*. The index is a sidecar manifest listing every generated property IRI,
its owning class, its range, and any collisions (same local name, different range or description):

```yaml
source: example.yaml
generated_by: openapi-to-rdf 0.2.0
properties: [ … six entries … ]
collisions: []
```

Class-scoped IRIs keep same-named properties distinct, so a collision is not an error. It is a
*candidate for merging*, and merging is an opinionated modelling step — the kind of decision a tool
should surface rather than take. A real case from the current 3GPP output: `attributes` is declared by
**19** classes in `TS28105_AiMlNrm`, and the per-class namespace is what keeps them apart. (The
repository README says 20; that figure predates the move to the fetched 44-document corpus, and no test
reads it, so it went stale unnoticed.)

The merge step itself is deliberately **not implemented**. It is listed in `06-gaps.md`.
