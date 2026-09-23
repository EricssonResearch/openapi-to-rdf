# Design: ingest external schemas as first-class classes

_Spec for `docs/REQUEST-external-schema-ingestion.md`. Written 2026-09-23. Decisions taken with the
user in the brainstorming session of the same date._

## In one paragraph

A class's IRI must be a function of **the document that declares it**. Today it is a function of
**who is asking**: the declaring document's own conversion can be handed a namespace via
`base_namespace`, while every document that *refers* to it re-derives one from the filename and
cannot see that argument. When the two differ, the referring document emits an IRI that nothing
declares, and the graphs silently fail to join. A second, independent defect sits on top: external
schemas are never registered as classes, so ancestry and declaring-class attribution stop at the
document boundary and inherited properties are re-attributed to the leaf. This spec fixes both,
adds gates that can actually observe them, and regenerates the published `output/` tree.

## Corrections to the request

Two claims in `docs/REQUEST-external-schema-ingestion.md` do not survive measurement. They are
recorded here because the request is the document a future reader will start from.

1. **The request's reproduction snippet does not reproduce the defect it claims.** It pops half of
   `components/schemas` into `external_schemas` but does not rewrite the `$ref` strings, so
   `PolicyRef`'s `allOf` still reads `#/components/schemas/Addressable` — an *internal* ref to a
   now-absent schema. `_parse_external_ref` returns `None` for it and `external_schemas` is never
   consulted. The assertion does fail, for the wrong reason: this is the same
   absent-ancestor confound the request's own "Caution from the diagnosis" section warns about, one
   level subtler. Any test written from that snippet would be a decoration.

2. **`_parents_of` already resolves external parents.** On a correctly split TMF620 — refs rewritten
   to `common.yaml#/components/schemas/…` — `mapping.classes["PolicyRef"].parents == ('EntityRef',)`,
   not `()`. The break is one level up: `EntityRef` has no entry in `classes`, `parents` or
   `declared_by`, so the ancestry walk in `_resolve_declaring_class` terminates at the boundary. The
   request's mechanism section suspects this ("worth checking whether the external branch below it
   registers the name") but its evidence block states the stronger, false claim.

The request's **count is right and reproduces from an independent path**: a correctly split TMF620
yields **34** `(class, property)` pairs present in the split that do not exist in the whole-document
mapping — `href`, `id`, `@type`, `@baseType`, `@schemaLocation` on `PolicyRef` and its `_FVO`/`_MVO`
variants, rather than on `Addressable` and `Extensible`.

## The defects

### D1 — a document's namespace is computed twice, and the copies disagree

`shacl_converter._generate_base_namespace` (line 146) and
`shacl_converter._generate_namespace_for_file` (line 328) hold the **same derivation**: match
`(?P<num>TS\d*)_(?P<name>.*)`, yield `{prefix}{num}/{name}#`, else `{prefix}rdf/{stem}#`. The only
difference is that the first is overridable —
`self.base_namespace = base_namespace or self._generate_base_namespace()` — and the second is not.
So supplying `base_namespace` desynchronises a document from every document that refers to it.

Measured on the committed `output/rdf/` tree (38 files, 15,139 triples, loaded with rdflib; local
classes are declared under `https://example.org/<stem>/` because `scripts/regenerate_output.py`
passes that, while references to them are minted under
`http://ericsson.com/models/3gpp/<TS>/<Name>#`):

| target predicate | distinct non-W3C targets | naming an IRI no document in the tree declares |
|---|---|---|
| `rdfs:subClassOf` | 46 | **34** — 34 of 34 in the filename-derived family |
| `rdfs:range` | 797 | **141** — 132 of 141 in that family |
| `rdfs:domain` | 889 | **0** |

**175 distinct dangling class targets.** The `rdfs:domain` row is the control: domains are always
local, so an instrument that reports 0 there and non-zero above is one that can produce both
answers. The 9 non-family `rdfs:range` danglers are expected by design — `ClassFact.referent` is
minted by convention and may name a class the document does not declare (see `mapping.py`'s module
docstring).

This contradicts a determination already settled in `HYPOTHESES.md`: *"File structure is provenance,
never identity — the source document is recorded as a triple, never as an IRI segment."*

The same defect produces the consumer's TM Forum divergence. On a minimal two-document fixture under
one namespace, the whole-document conversion emits
`PolicyRef rdfs:subClassOf <https://tmforum.org/ontology/Addressable>` while the split conversion
emits `PolicyRef rdfs:subClassOf <http://ericsson.com/models/3gpp/rdf/common#Addressable>` — a
3GPP-prefixed IRI inside a TM Forum vocabulary, from the default `base_namespace_prefix`. Symmetric
difference of the two RDF graphs: **2 triples of 17**, i.e. that one edge, wrong on one side.

### D2 — external schemas are never registered, so attribution stops at the boundary

`build_mapping` builds `parents`, `classes` and `declared_by` by iterating **local** `schemas` only
(`mapping.py:904`, `922`, `923`). An external ancestor therefore has no entry in any of the three.
`_resolve_declaring_class` walks `parents` and probes `declared_by`, so the walk terminates at the
first external name and a restated inherited property is attributed to the leaf. 34 pairs on a split
TMF620, as above.

### D3 — `_parents_of` and `_resolve_reference` disagree on how an external document is keyed

`_external_schemas_map` is keyed by **basename** (`shacl_converter.py:198`).
`_resolve_reference` basenames the parsed document before looking it up (line 1652 region), but
`_parents_of` uses `_parse_external_ref`'s raw prefix verbatim (`mapping.py:664`). A ref carrying a
directory component — `../common/common.yaml#/components/schemas/X` — therefore resolves for range
purposes and fails for inheritance.

**Latent, not live: 0 of 2,313 external refs in the 3GPP corpus carry a directory component.** Fixed
here anyway, with a regression test, because the two functions must key the same way by construction
rather than by corpus accident.

### D4 — the OWL path has the same bug family (recorded, deferred)

`rdf_converter._bind_custom_namespaces` namespaces itself as
`base_namespace/<format_name(stem)>#` but a sibling as `base_namespace/<filename>#` — with the
`.yaml` extension left in, so a document and its referrers can never agree.

**Deferred by decision, RDFS/SHACL path first.** The grounds for deferring are verified rather than
assumed: `rdf_converter.py` is a separate emitter with its own namespace binding and is not reached
by any code this spec changes, so the D1 fix neither repairs nor worsens it. `HYPOTHESES.md` already
records this path as secondary (AC-3: "O4 is still live for inline properties and on the OWL path").
No test in this change asserts anything about it.

### D5 — the gates named in the request cannot observe any of this

* `scripts/measure_corpora.py::census_one` converts with `external_refs=[]` (line 75). **No metric it
  reports can move**, in either direction, no matter what this change does to cross-document
  resolution. The request names it as "the real gate".
* `tests/test_cross_document_refs.py::test_split_model_common_class_iri_is_stable` asserts
  `any("TimePeriod" in parent for parent in order_parents)` — a substring test. It passes today while
  the IRI is `http://ericsson.com/models/3gpp/rdf/common#TimePeriod`, which is precisely the wrong
  answer its name claims to guard against.
* `tests/test_cross_document_refs.py::test_external_ref_as_property_target_resolves` has the same
  shape: `any("CommonType" in str(r) for r in payload_ranges)`, with a comment conceding "the exact
  IRI depends on the namespace assigned to external schemas".

## Constraint from the corpora: the two conventions are real

The fix cannot assume one vocabulary per file or one vocabulary per API, because both corpora exist:

* **3GPP — one vocabulary per file.** `TimeWindow` belongs to `TS28623_ComDefs`' namespace.
  **49 of 1,741 schema names are declared in more than one of the 38 documents, and 44 of those 49
  have differing definitions** (SHA-1 over the canonicalised schema body). Same name in two
  documents is genuinely two classes.
* **TM Forum — one vocabulary per API, spread over files.** `Addressable` belongs to the shared
  namespace whichever file holds it. "One class, one IRI, referenced by N APIs" is the premise of
  their split model.

Consequence for D2: `Mapping.classes` is keyed by bare schema name, so it cannot hold both
`TimeWindow`s. That collision region has 44 measured members and must be handled explicitly, not
discovered later.

## The design

### Part 1 — one namespace resolver per document

Add to `openapi_to_rdf/property_uri.py`, beside `namespace_for_schema`, which is already documented
as "the single point of decision for every URI this project mints":

```python
def namespace_for_document(filename: str, base_namespace_prefix: str) -> str
```

the existing derivation, moved, with one copy. `_generate_base_namespace` and
`_generate_namespace_for_file` both call it; the latter is then deleted, since it exists only to be
the copy that could not be overridden.

`OpenAPIToSHACLConverter` gains `document_namespaces: dict[str, str] | None`, keyed by **basename**
to match `_external_schemas_map`. Resolution for a class declared in document `D`:

1. `schema_namespaces[class_name]`, if present — the existing per-class override, unchanged, and it
   already wins for external refs today.
2. `document_namespaces[basename(D)]`, if present.
3. `self.base_namespace`, when `D` is the document being converted. This is the step that closes D1:
   `base_namespace` stops being a value only the self path can see.
4. `namespace_for_document(basename(D), self.base_namespace_prefix)` otherwise.

`build_mapping` gains `external_namespaces: dict[str, str] | None`, the same map restricted to
external documents. When an external document has no entry, its classes fall back to the mapping's
own `namespace`. That is not a new guess: `namespace` is already documented as "Default namespace for
every class in the document", and extending that default is exactly right for the split model — which
is why the request's reproduction shape works with zero configuration. The converter, which is the
component that knows filenames, always supplies the full map, so 3GPP keeps per-document namespaces.

### Part 2 — register external schemas, reference-only

`ClassFact` gains provenance, not identity:

```python
declaring_document: str | None = None   # None means "the document being converted"

@property
def is_external(self) -> bool: ...
```

Recording the source document as a field is consistent with the settled determination — provenance
is a fact about a class, it is simply never an IRI segment.

Registration, after the local pass in `build_mapping`:

1. Seed a worklist with every `(document, schema_name)` an already-registered class reaches through
   a top-level `allOf` external `$ref`. This needs the document alongside the name, so `_parents_of`
   grows a companion that returns `list[tuple[str, str]]` rather than losing the qualifier.
2. For each `(doc, name)` not already in `classes`:
   * `parents[name] = _parents_of(ext_def, schemas=external_schemas[doc], external_schemas=…)` —
     **a `#/…` ref inside an external document resolves against that document's own schemas**, not
     the local ones. Getting this wrong is how A→B→C transitivity silently half-works.
   * `declared_by[name] = flattened_properties(ext_def)`;
     `required_by[name] = flattened_required(ext_def)`.
   * `classes[name] = ClassFact(iri=<Part 1 resolution for doc>, …, declaring_document=doc)`.
   * Push **both** its external parents and its within-document parents onto the worklist: a parent
     internal to `doc` is still external from the converted document's point of view.
   * Apply the existing exclusions unchanged — primitives and `oneOf` unions get no class.
3. Skip registration when `name` is already in `classes`. **Local always wins.** A skip whose schema
   body differs from the registered one is counted and surfaced, never silent (44 such cases exist on
   3GPP). Depth is bounded by `_MAX_REF_DEPTH` and by the `seen` set, so a cyclic `allOf` terminates.
4. Build `PropertyFact`s for external classes into `properties_by_class` — attribution needs them,
   because `Mapping.declaring_class` resolves against that index — but **not** into `properties`, the
   by-name index, where an external entry could shadow a local declaration.

### What each projection does with an external class

The acceptance criterion is that whole and split conversions yield isomorphic vocabularies, so the
governing rule is **reference, never re-declare**.

| projection | external classes |
|---|---|
| TTL vocabulary + SHACL | Referenced as the object of `rdfs:subClassOf` / `rdfs:range`. **Never** declared: no `a rdfs:Class`, no `rdfs:domain`, no `NodeShape`, no properties. |
| JSON-LD `@context` | Inherited property terms are included — the existing ancestry walk (`projections/context.py:75–91`) climbs `ClassFact.parents` and now reaches across the boundary. No type-scoped term block is added for the external class itself. |
| Overlay | Untouched. An overlay annotates this document's own schemas, and an external schema is not in it. |
| Operation graph | Untouched. |

Re-declaring is rejected for two measured reasons, not for tidiness: the declaring document already
emits those triples, so suppression is what makes the union equal the whole-document graph; and with
44 same-named-but-different schemas on 3GPP, re-declaration would publish two contradictory
definitions under one IRI.

`scripts/reconcile_projections.py` compares every projection against `expected = {name: fact.iri for
name, fact in mapping.classes.items()}` (line 93). With external classes in `mapping.classes` and
deliberately absent from the TTL, that gate breaks unless it is told. **It must exclude external
classes from `expected`**, and the exclusion needs its own assertion so the gate cannot go vacuous.

### Part 3 — gates that can observe the fix

1. **A dangling-target metric in `scripts/measure_corpora.py`.** Count `rdfs:subClassOf` and
   `rdfs:range` objects that no document in the corpus declares, over the union of the corpus's
   graphs. Requires a corpus arm that **loads siblings** — `census_one`'s `external_refs=[]` is why
   no existing metric can move. Today's baseline is 175 distinct targets on 3GPP; the expectation
   after the fix is 0. That is a **prediction until a run shows it**, and the run is a task, not an
   afterthought.
2. **A split-vs-whole isomorphism test in this repo**, not only in the consumer. Its fixture must
   **restate an inherited property** in the child schema. A fixture that does not cannot reach D2 at
   all: the minimal two-document case used during diagnosis showed the D1 namespace divergence and
   attributed `@type` perfectly correctly, because nothing was restated. TM Forum restates
   `href`/`id`/`@type` constantly, which is why the real corpus shows 34 misattributions.
3. **Promote the diagnosis probes to committed scripts.** The whole-vs-split comparison was written
   ad hoc twice during diagnosis; it becomes `scripts/measure_split_isomorphism.py`, so the figures
   this spec quotes can be re-derived by someone who is not their author.
4. **Repair the three decorative assertions** named in D5 so they compare IRIs rather than
   substrings. Each must be observed failing against the pre-fix code.
5. **Regenerate `output/`** in the same change, per decision. `test_output_freshness` fails
   otherwise, so the change is not complete without it.
6. **Update `HYPOTHESES.md`** as findings land: D1 and D2 are measured and belong in the settled
   section with their evidence, D3 and D4 as recorded-but-deferred, and the 175 → 0 expectation as an
   open hypothesis until the run in item 1 reports.

## Acceptance criteria

* `AC-1` On a correctly split TMF620 — refs rewritten, not merely popped — the set of
  `(declaring_class, property)` pairs from the split conversion contains **no pair absent from the
  whole-document conversion**. Pre-fix: 34 such pairs.
* `AC-2` Whole and split conversions of one document yield **isomorphic** RDF vocabularies, compared
  as graphs by `scripts/measure_split_isomorphism.py`.
* `AC-3` Dangling class targets over the 3GPP corpus, converted with siblings loaded: **0**.
  Pre-fix: 175 distinct.
* `AC-4` Transitivity holds: a class in document 1 whose parent is in document 2 whose parent is
  back in document 1 resolves all three, with each class's IRI taken from its own declaring
  document.
* `AC-5` A schema name declared in two documents registers **once**, the local declaration wins, and
  the skip is reported with a count.
* `AC-6` `scripts/reconcile_projections.py` is clean, and asserts the number of classes it excluded
  as external rather than silently excluding an unbounded set.
* `AC-7` No external class is declared in the referring document's TTL: zero `a rdfs:Class`,
  `rdfs:domain` or `sh:NodeShape` triples whose subject is an external class IRI.
* `AC-8` Both corpora. `scripts/measure_corpora.py` clean on 3GPP **and** TM Forum — five of six
  defects found in this library on 2026-09-22 were invisible on 3GPP.
* `AC-9` Every new gate has been **observed failing** against the pre-fix code. An assertion whose
  inversion cannot fail is a finding about the fixture, not a pass.
* `AC-10` D3: an external `$ref` carrying a directory component — `sub/common.yaml#/components/…` —
  resolves identically for inheritance and for range. Covered by a regression test, since the corpora
  cannot reach it (0 of 2,313).

## Out of scope

* D4, the OWL path — recorded above, deferred by decision.
* Fragment-less `$ref` (`Money.yaml`), which already raises with a spec citation. 0 occurrences in
  both corpora.
* Merging same-named schemas from different documents. Local-wins plus a reported skip is the
  behaviour; deciding that two `TimeWindow`s are one class is an opinionated modelling step, which
  this project consistently refuses to take silently.
* Re-keying `Mapping.classes` by `(document, name)`. It would ripple through every projection and the
  reconciliation gate; the 44 collisions are handled by local-wins-and-report instead. If that proves
  insufficient, it is a separate spec.

## Provenance of the figures

Every number above was measured on 2026-09-23 against `openapi-to-rdf` at `4d576b2`, using the TM
Forum v5 documents at `/home/earejma/snm-api-native-src` and the 38 3GPP documents in
`assets/MnS-Rel-19-OpenAPI/OpenAPI`. The `output/rdf` figures come from the committed tree. The
measurements were ad hoc; Part 3 items 1 and 3 exist so that they become re-derivable, and until
those land the figures in this spec should be read as one author's unreplicated run.
