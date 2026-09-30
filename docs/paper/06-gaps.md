# 6. Known gaps

Each entry states what is wrong, how it was found, how sure we are, and what is **not** known. A
companion document that omitted this chapter would read as marketing; one that includes it is worth
citing. The current state of each is in `HYPOTHESES.md`, which governs where the two differ.

Ordered by consequence to a *consumer of the output*, not by ease.

## 6.1 The SHACL projection rejects the correct form of an IRI-valued property

**What.** Where a property is marked `isIriValued true`, the SHACL projection still emits
`sh:datatype xsd:string`. Under SHACL that requires the value to be a literal, so an IRI — the form the
marker asserts — is **rejected**.

**Evidence.** `pyshacl`, RDFS inference on, vocabulary loaded, on the worked example: `href` as an IRI
→ *Value is not Literal with datatype xsd:string*; `href` as a string literal → accepted. A control
(`id` given the integer 5) is rejected, so the check can fail.

**Why it matters.** It is a false rejection of correct data, produced by a change (removing the
contradicting `rdfs:range`) that was reported as complete. That report said the datatype constraint
"constrains the lexical form, which is why it belongs in SHACL"; the constraint instead enforces the
wire form and rejects the RDF form.

**Not known.** How many properties are affected corpus-wide: 5 on 3GPP, 15 to 20 per TM Forum document
carry the marker, but whether every one carries the datatype constraint was not counted. **Not fixed.**

## 6.2 Whole and split conversions are not isomorphic

**What.** Splitting a description into common plus API documents changes the vocabulary. The current
classification (§4.6) attributes the difference to `range_dangling` (34 triples), `range_corrected` (23,
where the split is right), `vanished_unscoped` (5), `range_degraded` (4) and `other` (3). 14 of 44
documents diverge by that classification.

**Mechanism.** The SHACL emitter re-derives a range for an external `$ref` rather than reading the
`Mapping`. Rerouting it through the `Mapping` was tried and made divergence worse (69 → 984 triples,
917 unexplained); it was reverted. The drift is located, not resolved.

**Not known.** The 3 `other` triples; why the artifact's totals (69 lost) differ from the prose in
`HYPOTHESES.md` (79); whether a *third* definition of "diverges" — byte-level, or count-level — would
give a different picture. An earlier isomorphism run, over 47 documents, recorded seven with the same
triple count and different content — which a count-based gate would call clean; whether that still
holds on the current corpus was not re-measured.

## 6.3 The converter's own notes are published as the source's descriptions

**What.** Every generated file's provenance header states that the `rdfs:comment` descriptions *"are the
source document's own text"*. That is not true of all of them: the converter injects its own
implementation notes into `rdfs:comment` on classes — for example *"Note: Uses OpenAPI allOf — complex
logical constraints partially supported in SHACL"* and *"Note: Uses OpenAPI discriminator — consider
OWL union classes for full polymorphic semantics"*. `AgreementRef` carries both alongside its real
description.

**Why it matters.** It is the tool speaking in the vocabulary's documentation slot, in the standard's
voice, and it contradicts the header that disclaims exactly this. It also inflates comment counts:
after the declaring-class fix, TMF620 still has 26 subjects with more than one comment (44 surplus) and
TMF622 has 68 (119 surplus), led by `*Ref` classes carrying three.

**Not known.** How many of the surplus comments are injected notes rather than genuine
multiple-description cases — `AgreementRef` shows the former; the split for the rest was not measured.

## 6.4 Dangling class targets on 3GPP: 17, in three shapes

`scripts/measure_corpora.py`, 44 of 44 converted. 15 are **pure `$ref` aliases**
(`Name: {$ref: "other.yaml#/…"}`): the referrer mints a class IRI while the declaring document emits a
datatype or nothing, because `is_primitive_def` cannot see through an external `$ref`. Two are not:

* `JobDetails` carries both `additionalProperties` and `properties`;
* `MdtAlignmentInfo` carries `description`, `format` and `pattern` but **no `type`**, so it can be
  recognised neither as a primitive nor as an object. Nothing in the standard requires `type`.

The earlier belief that 3GPP's `-Single` suffix marked a distinct cause was wrong: all three
`CCO*Parameters-Single` entries share the alias shape.

## 6.5 Invented attribution pairs on two TM Forum documents

A split must invent no `(declaring class, property)` pair the whole document lacks. TMF620 reaches zero;
TMF622 has 6 and TMF641 has 9, all on `GeographicLocation` and its `_FVO`/`_MVO` variants, on `@type`,
`href` and `id`. The consuming project reported a separate, plausible cause in its own carve
(variant families split across the boundary); that is **reported, not verified here**, and this
document does not claim the library is or is not responsible. Last measured 2026-09-23.

## 6.6 Smaller open items

* **3 declared terms in one document have no NodeShape.** `TS28572_PlanManagement.yaml`: 42 declared
  against 39 targeted, so every constraint on those three is inert. Every other document of 44
  reconciles exactly. Not diagnosed.
* **The reconciliation gate's external-class counter is vacuous** (§5.4, item 4).
* **Shape duplication.** An inherited property's shape appears twice on a subclass, once under
  `sh:node` and once directly. Identical copies; validation unaffected; not traced to a commit.
* **The OWL emitter has the namespace defect of §4.2.** It namespaces the declaring document as
  `<base>/<stem>#` but a sibling as `<base>/<filename>#` with the `.yaml` extension left in, so a
  document and its referrers cannot agree. Deferred by decision; the RDFS/SHACL path was done first.
* **The property-index merge step is not implemented**, by design (§3.4).
* **65 failing tests, all one family.** The 3GPP test-instance generator mangles names the converter
  correctly preserves. A defect in test data, not the converter (`HYPOTHESES.md` H1–H3); 5 of the 65 are
  `test_good_instance_conforms`, which is those previously vacuous acceptance tests beginning to
  validate for real.
* **A typo-level upstream defect is tolerated.** One 3GPP enum contains a malformed member
  (`HSPA_EVOLUTION        - type: string`, a YAML indentation slip) that is dropped with a warning.

## 6.7 What has not been evaluated at all

Everything above measures the ontology's **internal consistency**. Nothing here measures whether the
generated semantics improve the task they exist for — whether an agent can construct a valid service
order, or navigate from one resource to an operation on it, better with them than without. There is no
downstream evaluation and no user study. The identity, meaning and affordance layers are argued from
defects found and measured, not from demonstrated benefit.

One provenance gap belongs here too: the TM Forum Service Ordering v5 document has no established
origin, so the corpus the operation-graph figures rest on partly cannot be redistributed or
independently re-fetched.
