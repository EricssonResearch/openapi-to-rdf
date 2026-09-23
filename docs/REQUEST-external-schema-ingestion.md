# Request: ingest external schemas as first-class classes

_Raised 2026-09-23 from `snm-api-native`, which is blocked on this. Diagnosed, not yet fixed._
> **SUPERSEDED 2026-09-23 — implemented, and two claims below were wrong.** Read
> `docs/superpowers/specs/2026-09-23-external-schema-ingestion-design.md` instead; it corrects this
> document and records a larger defect (D1) that this request missed entirely.
>
> 1. **The reproduction snippet does not reproduce the defect.** It pops schemas into
>    `external_schemas` without rewriting the `$ref` strings, so the `allOf` still reads
>    `#/components/schemas/Addressable` — an *internal* ref to an absent schema. It is the same
>    absent-ancestor confound this document's own "Caution" section warns about, one level subtler.
> 2. **`_parents_of` already resolved external parents.** On a correctly split TMF620,
>    `parents == ('EntityRef',)`, not `()`. The mechanism section guessed this correctly; the
>    evidence block stated the stronger, false claim, and led with it.
>
> The **count was right** and reproduced from an independent path: 34 misattributed pairs, now 0.
> What this request named as "the real gate" — `scripts/measure_corpora.py` — converted with
> `external_refs=[]`, so no metric it reported could move in either direction.


## The problem in one sentence

`build_mapping` resolves external `$ref`s well enough to find **range targets**, but never registers
the external schemas themselves as classes — so declaring-class attribution stops at the document
boundary and properties are attributed to the wrong class.

## Evidence (measured, TMF620, with `external_schemas` correctly supplied)

`PolicyRef` composes `Addressable` via `allOf: [{$ref: "<other-document>#/components/schemas/Addressable"}]`:

```
PolicyRef.parents                      == ()          # empty
"Addressable" in Mapping.classes       == False
any Addressable entry in properties_by_class == False
```

Consequence: **34 properties are attributed to the wrong class** when a description is split across
documents. `href` lands on `PolicyRef` instead of `Addressable`; `@baseType` on `PolicyRef` instead of
`Extensible`.

Downstream, in the consumer's own gate: converting one document whole versus converting it split into
(common + api) produces **377 divergent triples on TMF620 and 605 on TMF622** — the two paths
disagree about the ontology, so one of them is wrong.

Reproduce:

```python
doc = yaml.safe_load(open("tmf620-product-catalog-management-v5.yaml"))
names = sorted(doc["components"]["schemas"])
moved = {n: doc["components"]["schemas"].pop(n) for n in names[: len(names) // 2]}
m = build_mapping(doc, namespace="https://tmforum.org/ontology/",
                  external_schemas={"common.yaml": moved})
assert m.classes["PolicyRef"].parents != ()      # fails today
```

## Mechanism

`openapi_to_rdf/mapping.py`, in `build_mapping`:

```python
for schema_name, schema_def in schemas.items():          # LOCAL schemas only
    parents[schema_name] = _parents_of(schema_def, schemas, external_schemas)
    classes[schema_name] = ClassFact(...)

declared_by = {name: flattened_properties(schemas[name]) for name in classes}
```

`_parents_of` *is* passed `external_schemas`, but three indexes are built by iterating local
`schemas` only, so an external ancestor gets no entry in `parents`, none in `classes`, and none in
`declared_by`. The ancestry walk therefore terminates, and `_parents_of` additionally returns no
parent for a `$ref` it cannot find in `schemas` (see its "Try internal ref first" branch — worth
checking whether the external branch below it registers the name).

## What "correct" looks like

An external schema participates in attribution exactly as a local one does:

1. present in `Mapping.classes` with its own IRI,
2. its properties present in `properties_by_class`,
3. its `parents` resolved, so ancestry crosses document boundaries transitively (A in doc 1 →
   B in doc 2 → C in doc 1 must work).

## Acceptance criteria

- `m.classes["PolicyRef"].parents == ("Addressable",)` in the reproduction above.
- Converting a document whole and converting it split into (common + api) yield **isomorphic**
  vocabularies. This is the real gate; the consumer's `test_common_document::test_gate_2_*` is an
  existing implementation of it and currently reports 377/605 divergent triples.
- **Both corpora.** 3GPP uses heavy cross-document `$ref` (38 documents) and TM Forum uses the split
  model, so `scripts/measure_corpora.py` must be clean on both. Five of six defects found in this
  library on 2026-09-22 were invisible on 3GPP and appeared only on TM Forum.
- A test that would fail without the fix — not only one that passes with it.

## Scope notes

- **Not** a new resolution mechanism: `external_refs` loading and `_external_schemas_map` already
  work, and unresolved refs already report correctly. This is about what happens to the schemas
  *after* they are loaded.
- An external class's **namespace** is a real question: it should presumably follow the same
  `namespace_for_schema` / `schema_namespaces` rules as a local one, so the same class referenced
  from two documents mints one IRI. That is the property the consumer's split model depends on
  ("one class, one IRI, referenced by N APIs") and is worth deciding explicitly.
- Watch for **double registration**: if two local documents both `$ref` the same external schema,
  it must be registered once.
- This is a prerequisite for the consumer's G9 cost-curve claim regardless of its migration, so it
  has value independent of unblocking that work.

## Caution from the diagnosis

My first attempt to demonstrate this deleted schemas *without* supplying them via `external_schemas`,
which only proves that absent ancestors break attribution — a different and expected thing. It
happened to produce the same count. The numbers above come from the re-run with `external_schemas`
supplied; use that shape when writing the test.
