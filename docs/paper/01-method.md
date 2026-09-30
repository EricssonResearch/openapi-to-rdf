# 1. One derived fact set, five projections

## 1.1 The failure this structure exists to prevent

An OpenAPI description supports several useful derived artifacts: an RDF/RDFS vocabulary, SHACL
shapes, a JSON-LD `@context`, an OpenAPI Overlay carrying ontology annotations, and a Hydra operation
graph. The obvious implementation gives each its own walk over the document.

That is the mistake, and it is measurable rather than merely inelegant. Each walk must independently
decide the same questions — *which named schemas are classes, which class declares this property, what
does this property point at, is this value an IRI or a literal* — and independent implementations of
one decision drift.

> **Observed** in the consuming project (`snm-api-native`): two independently-derived class mappings
> over one corpus agreed on the 75 schema names they shared and **disagreed on 132**. Nothing detected
> it, because nothing compared them.

The disagreement was not exotic. It was the accumulation of small divergences in exactly the decisions
listed above, each defensible in isolation, each taken twice.

## 1.2 The structure

`openapi_to_rdf.mapping.build_mapping` performs **one** walk over the document and returns a `Mapping`:
a set of facts, with no opinion about serialisation. Every artifact is then a projection of that
object, and a projection reads decisions rather than re-deriving them.

```
                          ┌─────────────────────┐
  OpenAPI document ─────► │   build_mapping()   │ ─────► Mapping
                          └─────────────────────┘           │
                                                            │
      ┌──────────────┬──────────────┬──────────────┬─────────┴────────┐
      ▼              ▼              ▼              ▼                  ▼
  RDFS vocab    SHACL shapes   JSON-LD @context   Overlay      Hydra operations
      └──────────────┴──────────────┴──────────────┴──────────────────┘
                                    │
                            reconciliation gate
                    (all projections agree on every class IRI)
```

The `Mapping` holds four indexes and one piece of document metadata:

| member | what it records |
|---|---|
| `classes` | `{name: ClassFact}` — IRI, parents, transport flag, referent, declaring document |
| `properties` | `{local_name: PropertyFact}` — the by-name index, first local declaration wins |
| `properties_by_class` | `{(declaring_class, local_name): PropertyFact}` — every fact, nothing merged |
| `operations` | `{"METHOD /path": OperationFact}` |
| `api_version` | `info.version` verbatim, read once |

Two details of that table carry decisions rather than convenience.

**`properties_by_class` is keyed by *declaring* class**, which is what makes attribution a lookup
rather than a second walk. Asking "does this ancestor own this property" is a dictionary probe.

**Nothing is merged.** Two unrelated classes may legitimately declare the same local name with
different ranges — `startTime` on both `TimeWindow` and `PerfMetricJob` in 3GPP TS 28.623 — and those
are distinct properties with distinct IRIs. The by-name index keeps the first declaration in document
order so a context or overlay has a key to use; `properties_by_class` keeps them all. Merging two
same-named properties is an opinionated modelling step, and this tool refuses to take it silently. The
property index projection (§3.4) reports the collisions instead.

## 1.3 The reconciliation gate

A single fact set removes the *opportunity* for two projections to disagree; it does not prove they
don't. `scripts/reconcile_projections.py` extracts the class IRIs each projection actually emitted and
compares all of them against the `Mapping`. A projection that mints an IRI of its own — rather than
reading `ClassFact.iri` — fails here.

The gate has one limitation worth stating, because it is an instance of a failure mode this document
returns to in §5: **its external-class counter is currently vacuous.** It reports how many classes it
excluded as belonging to another document, and that count is always zero, because the gate builds its
`Mapping` without supplying external schemas — so no class is ever external in it. The exclusion logic
is correct and untested by its own report. Recorded in `06-gaps.md`.

## 1.4 A worked example

One document runs through the whole of the next two chapters. It is deliberately small and
deliberately shaped like TM Forum, because the decisions that matter are the ones TM Forum's shape
provokes:

```yaml
components:
  schemas:
    Extensible:
      type: object
      properties:
        '@type': { type: string, description: When sub-classing, this defines the sub-class name }
    Addressable:
      allOf:
        - $ref: '#/components/schemas/Extensible'
        - type: object
          properties:
            id:   { type: string, description: unique identifier }
            href: { type: string, description: Hyperlink reference }
    AgreementRef:
      allOf:
        - $ref: '#/components/schemas/Addressable'
        - type: object
          properties:
            name: { type: string, description: Name of the referred entity }
    Agreement:
      allOf:
        - $ref: '#/components/schemas/Addressable'
        - type: object
          properties:
            agreement: { $ref: '#/components/schemas/AgreementRef' }
            validFor:  { type: string, format: date-time }
paths:
  /agreement/{id}:
    get:
      responses:
        '200': { content: { application/json: { schema: { $ref: '#/components/schemas/Agreement' } } } }
```

Four schemas, one operation, and every construct that the rest of this document argues about: an
`allOf` chain, a property restated nowhere but *inherited* twice over, a `*Ref` schema, a
reference-valued property, a `format`-typed literal, and a path parameter.

The `Mapping` derived from it, in full:

```
classes          Extensible, Addressable, AgreementRef, Agreement
Agreement.parents            ('Addressable',)
AgreementRef.referent        'Agreement'
declaring_class(Agreement, 'href')   -> 'Addressable'
properties_by_class[('Addressable','href')].iri
                 -> https://example.org/wx/Addressable/href
properties_by_class[('Agreement','agreement')].target_classes
                 -> ('Agreement',)
operations['GET /agreement/{id}'].iri
                 -> https://example.org/wx/operation/workedExample/v5/get/agreement/%7Bid%7D
operations['GET /agreement/{id}'].returns_class -> 'Agreement'
```

Three of those lines are the whole argument of the next chapter in miniature:

* `href` is attributed to **`Addressable`**, not to `Agreement`, even though an `Agreement` instance
  carries it. An ontology declares an inherited property once, on the class that introduces it.
* the `agreement` property's target is **`Agreement`**, not `AgreementRef`. The reference is a
  serialisation artifact; what the property points at is the thing.
* the operation IRI carries the API slug and major version, not just method and path. §3.1 reports the
  two collisions that forced them in.

All output shown in this document was generated from this document's example or from a named corpus
document. None of it is hand-written; hand-written examples in this repository have drifted from the
tool's actual behaviour twice.
