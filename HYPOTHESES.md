# HYPOTHESES — openapi-to-rdf
_Last updated: 2026-09-23. Read this first: current beliefs + status._

This repo is becoming **the** OpenAPI → RDF extractor for the workspace, absorbing
`snm-api-native/scripts/emit_tbox.py` and `sid-lift-src/lifting-core`. The design record is
`snm-api-native/docs/specs/2026-09-18-consolidate-openapi-to-rdf.md` (determinations S1–S10 from
snm-api-native, L1–L9 from lifting-core, defects O1–O11 here, acceptance criteria AC-1…AC-8), and
the task plan is `snm-api-native/docs/superpowers/plans/2026-09-18-openapi-to-rdf-contribution.md`.

## Where the suite stands

**1126 passed, 66 failed, 1 xfailed** (2026-09-25, `-m "not network"`, 537s, `PYTEST_EXIT=1`).
Previously 791/50 (2026-09-21) and 844/49 (2026-09-24).

**The jump is the corpus, not a regression.** As of 2026-09-25 the 3GPP corpus is FETCHED at tag
`Tag_Rel19_SA112` (`scripts/fetch_corpus.py`) instead of being a committed snapshot, so it is 44
documents rather than 38 and every parametrised test grew. The snapshot it replaced was not
reproducible from any ref -- 8 of its 39 files matched the tag byte-for-byte, and 19 of its 38
documents declared a Rel-18 version inside a directory named Rel-19.

**All 66 failures are the SAME generator defect, in two shapes:**

* **61 `test_bad_instance_rejected`** -- the pre-existing 49, on a larger corpus. Unchanged in kind.
* **5 `test_good_instance_conforms`** -- NEW in 2026-09-25, and the same root cause seen from the other
  side. The generator emits instances using properties the shapes do not model: on
  `TS28623_ComDefs/GeoAreaToCellMapping` it produces `convexGeoPolygon`, a name that appears NOWHERE in
  that document, against a `sh:xone` whose two branches are `geoPolygon` and `geoCircle` -- which is
  exactly what the source declares. The shapes are right and the fixture is invented. Verified by
  reading the source schema rather than inferred from the failure.

So the note below still holds for all 66, and the count moving is not evidence about the converter.

**49 of 50 failures are one test — `test_3gpp_shacl_coverage.py::test_bad_instance_rejected` — and all
49 are a defect in the TEST DATA, not in the converter.** The converter is correct in all 260 cases.
See H1–H3, now resolved, and the repair plan below them. The 50th failure was
`test_output_freshness`, a pre-existing staleness (stale output/ from a previous commit that changed
the emitter without regenerating). Fixed by the 2026-09-23 regeneration.

**Read this before touching the 66:** they are not a blocker for anything. They do not affect
conversion, the TM Forum path, or any deliverable. A previous session spent two days on them on the
strength of their being red, while the TM Forum work they were mistaken for a blocker on took ten
minutes. Red is not the same as blocking.

**Correction to commit `b1ae672`:** its message claims committing 63 regenerated files "closes 1 of
the 50". That is **refuted** — see H5. The test failed again afterwards, and it will keep failing
no matter how many times the tree is committed.

**834 passed is NOT the target to return to.** An earlier run read 834/1, and that was a *less
honest* instrument, not a better state: the same validation holes existed and the corpus was too
stale to reach them. Regenerating the corpus made the failures visible. Do not "fix" this by
reverting the regeneration.

## Current working hypotheses

- **H1: 260 of 1,075 declared instance types have no `sh:targetClass` matching them, so every
  constraint on them is inert** — STATUS: **supported** (confidence: high)
  evidence: `uv run python scripts/diagnose_shape_coverage.py` → 927 instance files (628 before the
  corpus became a pinned fetch on 2026-09-25), 1,075 declared
  types, 815 matched, **260 unreachable**; artifact `artifacts/shape-coverage.json`.
  This is the cause of all 49 `test_bad_instance_rejected` failures. The tell was that the failures
  spanned **all four constraint families at once** (29 enum, 7 below-min, 7 above-max, 4 pattern,
  2 missing-property): one family failing implicates that family's emitter, every family failing
  implicates the target not matching at all.
  caveats: H1 is the *mechanism*. It decomposes into H2 and H3, which need different repairs.

- **H2: 190 of the 260 are a FIXTURE bug — `generate_test_cases.py` normalises `-` to `_` when
  minting instance types, and the converter (correctly) preserves the dash** — STATUS: **supported**
  (confidence: high)
  evidence: `scripts/generate_test_cases.py` lines 178, 188, 207, 278 all call
  `.replace("-", "_")`. In `TS28623_TraceControlNrm`, **0 of 64** `sh:targetClass` local names
  contain `_Type` while 59 of 64 contain a dash. 3GPP local names legitimately contain `-`
  (`TraceJob-Single`, `loggedMDTConfig-Type`), so the shapes are right and the instances are wrong.
  The converter's dash handling was deliberately fixed in `e847fa2` (property identity, defect O4);
  the generator was not updated to match.
  caveats: repairing the generator requires regenerating the corpus and a full suite run. Expect the
  49 to move, and expect **previously-vacuous good-instance tests to start really validating** —
  some may then legitimately fail, which is a finding, not a regression.

- **H3 RESOLVED: all 70 remaining occurrences are also fixture defects. The converter is correct.**
  — STATUS: **supported** (confidence: high), measured 2026-09-21
  The decision table that settled it, over `components/schemas` of all 38 corpus documents:

  | class | verdict |
  |---|---|
  | `TS29571_CommonData:GlobalRanNodeId`, `Area`, `GeraLocation`, `UtraLocation` | top-level `oneOf` |
  | `TS28541_5GcNrm:ImsiRange`, `PlmnRange` | top-level `oneOf` |
  | `TS28623_ComDefs:otherProblems`, `TS28623_GenericNrm:attributes`, `TS28623_TraceControlNrm:EventThreshold1F` | **not a schema — a property name** |
  | `TS28623_GenericNrm:EP_RP_Attr` | **not a schema and not a property — appears nowhere in the corpus** |

  6 of 10 are top-level `oneOf` unions, so determination **S2 (never mint a class for a `oneOf`
  union)** makes the converter right to emit no shape, and the generator wrong to emit instances of
  a union wrapper. The other 4 are not schemas at all: searched as a schema name across all 38
  documents, **declared nowhere**; three occur only as property names, and `EP_RP_Attr` does not
  occur at all.
  Note on method: the first lookup checked only top-level `components/schemas` of one document, which
  cannot distinguish "absent" from "nested elsewhere". The corpus-wide search is what licensed the
  verdict.

## The repair, and the part of it nobody can estimate

Three defects in `scripts/generate_test_cases.py`, which manufactures the RDF instance files under
`test-cases/<spec>/{good,bad}/`. It is test scaffolding: it does not ship and no conversion path uses
it.

1. **Dash mangling** — `.replace("-", "_")` at lines 178, 188, 207, 278. Not a blind delete: those
   names are also used as RDF prefixes, so each site needs checking.
2. **`oneOf` wrappers** — the schema loop already skips `type != object` and property-less schemas;
   it needs to skip top-level `oneOf` too, per S2.
3. **Classes minted from property names** — in `to_rdf`, a nested object with no `$ref` is typed
   `child_class or ns[safe_key]`, i.e. it invents a class from the PROPERTY name when the real class
   is unknown. It should leave the node untyped instead. This is the source of the 4 non-schema names.

Estimated mechanical cost: about an hour including regeneration and a commit.

**The unestimable part: once shapes match, 260 types get validated for the first time.** Each can go
three ways — the bad instance is now correctly rejected; it is still not rejected, exposing a genuine
converter gap that has been invisible until now; or a good instance starts being rejected. Nobody has
ever run real validation against those 260, so any predicted mix would be invention. Timebox it: do
the mechanical fix, run `tests/test_3gpp_shacl_coverage.py`, and report the triage table before
fixing anything in it.

- **H4: `test_good_instance_accepted` has been passing for the wrong reason across ~24% of the
  corpus** — STATUS: **supported** (confidence: high)
  evidence: falls out of H1 — a class no shape targets cannot fail an acceptance test. This is the
  textbook "guard whose inputs cannot reach the failure region" mode. It is why
  `scripts/diagnose_shape_coverage.py` is committed as a gate (exit 1 on any unreachable type)
  rather than kept as a debugging aid: a green suite can mean correct or inert, and only this
  measurement tells them apart.

- **H5: `test_output_freshness` is UNSATISFIABLE as written, because regeneration is not
  byte-deterministic while the test compares bytes** — STATUS: **supported** (confidence: high)
  evidence: after committing the 63 regenerated files, the test failed again with 27 files dirty.
  All 27 are `.ttl`, and **27 of 27 are graph-isomorphic** to their committed versions — identical
  triples, different serialization. The visible diff is where `],` versus `] ] ;` falls, i.e. which
  member of a blank-node list is emitted last; line counts are unchanged (155 = 155 on the file
  inspected). Cause: `openapi_to_rdf/shacl_converter.py:1864,1873` and
  `openapi_to_rdf/rdf_converter.py:487` call rdflib's `Graph.serialize(format="turtle")`, whose
  blank-node ordering is not stable across runs.
  Consequence: **committing output/ can never make this test pass.** Anyone who responds to this
  failure by regenerating and committing is in a loop that cannot terminate. I did exactly that once.
  caveats: measured on the 27 files that happened to be dirty, all of which were SHACL output. Byte
  determinism of `output/index/*.yaml` and `output/rdf/*` is therefore **not yet measured** — they
  may well be deterministic, which is why the repair below must not blanket-exempt them.

### H5 resolved: Option A, decided by the user 2026-09-21

`.ttl` files are compared by **graph isomorphism**, everything else by **bytes**
(`tests/test_output_freshness.py`). Byte equality is the wrong equivalence relation for an RDF
graph, so the test now asserts what it means. **What was given up:** detection of gratuitous byte
churn in files consumers diff — Option B (byte-deterministic serialization) would have kept that
and was rejected as the more expensive repair.

**Not measured:** whether `output/index/*.yaml` is byte-deterministic. It is compared by bytes and
has never drifted, but that is absence of evidence. If one day it does drift, measure it and decide
— do not widen the isomorphism exemption to cover it.

**Option A immediately earned its place**, which is the argument for fixing the equivalence rather
than deleting the gate. With the 27 serialization diffs no longer drowning the signal, the first run
reported 6 real findings the byte comparison had never been able to isolate: `IdentityProbe_*` and
`spec_*` under `rdf/`, `shacl/` and `index/` were **committed but not produced by a fresh run**. See
H6.

- **H6: a test was writing into the published deliverable tree** — STATUS: **supported, fixed**
  (confidence: high)
  evidence: the committed tree held **120** files against the README's **114**, and regeneration
  produced **114**. Three independent sources now agree at 114. The 6 extras were probe artifacts
  from `tests/test_property_identity.py`, whose fixture put its input spec in `tmp_path` but left
  the converter's `output_dir` at its default — a **cwd-relative `output/`**. They were then
  committed by `2aee21e`, a regeneration commit that could not distinguish them from deliverables.
  fix: the fixture passes `output_dir`, the 6 files are removed, and `tests/conftest.py` fails any
  test that creates or modifies a file under `output/`. The guard was inverted and observed to fail
  with the offending test's name and the created path.
  why a guard and not two edits: 18 of ~21 converter constructions in the suite already passed
  `output_dir`, so patching the stragglers would have left the invariant itself unguarded — the
  recurring failure mode where the guard is narrower than what it protects.

## Settled (established this project — don't relitigate)

- **One `rdfs:range` per property, enforced not just documented** — TM Forum had **167 of 2,895
  properties (5.8%)** carrying two ranges against **6 of 3,622 (0.17%)** on 3GPP, because the guard
  was `if range_uri is not None` while its comment claimed "single-target properties only". A
  class-scoped property URI can be reached twice (polymorphic `oneOf`) and `Graph.add` is a set
  insert. Now 0 on both corpora (`scripts/measure_corpora.py`, metric `properties_multi_range`).
  **The transferable lesson:** the guard was sound and its scope was right; its SAMPLE could not
  reach the failure region, because 3GPP barely uses polymorphic references. This is why AC-8 wants
  two standards bodies and not one.
- **TM Forum is covered by tests, at last** — `tests/test_tmforum_corpus.py`, 4 tests, where the
  suite previously had **0 of 27 modules** referencing TM Forum. Note that
  `scripts/measure_corpora.py` had been measuring TMF all along: it was the TESTS that were
  3GPP-only, which is a different gap and was briefly misreported as "TMF is unmeasured".

- **`rdfs:range` only where true** (S1) — datatype ranges 2,676 → 2,281, removing 395 invented
  triples; class ranges unchanged at 1,614. Range propagates under RDFS entailment, so a
  multi-target range asserts types about unobserved data. Commit `2f7e45d`.
- **One NodeShape per class** (defect O2) — `sh:targetClass` 2,399 → 1,770 against 1,769 classes.
  `allOf` was processed twice, and `allOf` *is* inheritance, so it hit most interesting classes.
  Commit `6b71d07`. The `anyOf` limitation is pinned with `xfail(strict=True)`.
- **No guessing, no placeholders** (S3, S4, O5, O6) — a fragment-less `$ref` is refused rather than
  given a placeholder IRI, which used to leak the build directory. Commit `6915f5b`.
- **File structure is not identity WITHIN one description** — the source document is recorded as a
  triple, never as an IRI segment. Argument: under file-derived identity a class in
  `common-tmf-v5.yaml` would get a different IRI than the same class inlined, contradicting the
  premise of the split model (one class, one IRI, N APIs).

  **Scope correction, 2026-09-23.** This was previously written as "file structure is provenance,
  never identity", with no qualifier, and read as a universal rule. It is not one, and 3GPP is a
  standing counterexample in this same repository: each TS document is its own published vocabulary,
  so `TS28623_ComDefs.yaml` → `…/TS28623/ComDefs#` is correct and load-bearing — the 38 documents
  only join up *because* a class carries its declaring document's namespace. The filename there is a
  proxy for the specification number, which is the real identifier, not for packaging.

  The distinction is whether the file boundary is a **published vocabulary boundary**: for 3GPP it is,
  for a carve of TM Forum's API family it is not. Nothing in an OpenAPI document states which case
  applies, so it is an input (`document_namespaces`), not a derivation.

  **Cost of the unqualified version:** I read it as universal while specifying external-schema
  ingestion, and it contributed to a zero-config default that took 3GPP from 16 dangling class
  targets to 79 before review caught it.
- **The `Mapping` spine: one derived object, four projections** — TTL (vocabulary + SHACL), Overlay
  1.1.0, JSON-LD `@context`, Hydra operation graph. Commits `278987a`, `5ead8a3`, then `928a884`…
  `73b8ff3`. A reconciliation gate checks the four agree on every class IRI (AC-4).
- **The conversion needs only the OpenAPI document** (AC-5) — proven, not asserted: deriving with an
  *empty* TBox produces a byte-identical overlay body. This is the adoption thesis in one line.
- **`output/` is a published deliverable**, not scratch — 132 tracked files (44 documents x rdf + shacl
  + property index; 114 before the corpus became a pinned fetch), advertised in the
  README. It was briefly gitignored by mistake; `test_output_freshness.py` now gates its currency.
- **uv, not poetry** — PEP 621 + hatchling, `requires-python >= 3.11`. Commit `9619606`.

- **A class's IRI comes from its declaring document, not the referring one (D1 in the spec)** — a
  document's namespace was derived twice, by two identical function bodies of which only one could be
  overridden by `base_namespace`, so supplying it desynchronised a document from every referrer.
  Evidence: `scripts/measure_corpora.py` metric `dangling_class_targets_corpuswide`, artifact
  `artifacts/external-schema-ingestion.json`. Commits `338c0f2` (one derivation), `4bbacae`…`3306ea8`
  (the converter wiring, over three review rounds).

- **External schemas are registered as classes, so attribution crosses documents (D2 in the spec)** —
  `build_mapping` indexed local `components/schemas` only, so an external ancestor had no entry in
  `classes`/`parents`/`declared_by` and the ancestry walk stopped at the boundary. Commits `9e0ea4b`,
  `9e476d6`. Real-corpus effect over the 38 3GPP documents: **144 properties gained a target class
  that was previously dropped, 0 lost one.**

  _(The D1/D2 labels were swapped in the first version of these two entries. The spec is the
  authority: D1 is the namespace defect, D2 is the registration defect.)_

- **A `*Ref`'s referent is declared** — `rdfs:range` names the referent, because in RDF an IRI already
  is a reference, but the referent itself was never declared, so the axiom pointed at nothing. TMF620:
  56 `*Ref` classes, **39 referents undeclared → declared and marked `isMintedByConvention`**; corpus
  `dangling_class_targets_corpuswide` on TM Forum **98 → 0**. Commit `24af6aa`.

  **This supersedes an earlier claim in this file that TM Forum's 98 dangling targets were "legitimate
  by design" because referent names are minted by convention.** That was wrong, and it was my ruling
  rather than a measurement: minting by convention is right, but it does not excuse leaving the minted
  term undeclared. The metric was reporting an incomplete ontology, and I proposed excluding those
  targets from the gate instead of fixing what the gate found. Do not reintroduce that exclusion.

## Open questions / next tests

- **H3's table (above) is the first thing to run.** It is ten schema lookups and it decides whether
  the next change touches the converter or the fixture generator. Do not start coding without it.
- **352 `sh:targetClass` triples point at `rdfs:Datatype`.** Noticed, never measured; the TM Forum
  count was never established.
> **Two different AC numberings appear in this file.** Those written `AC-n (consolidation)` come from
> `snm-api-native/docs/specs/2026-09-18-consolidate-openapi-to-rdf.md`; those written
> `AC-n (ingestion)` come from `docs/superpowers/specs/2026-09-23-external-schema-ingestion-design.md`.
> They are unrelated and their numbers collide. Always write which one you mean.

- **AC-2 (consolidation) is not met** — determinations L9, L8 and S9 have no test in this repo. It
  exists because "carried as a comment" is exactly what failed to stop defect F12 (`allOf` property
  loss) being independently rediscovered in three repos.
- **AC-3 (consolidation) is partial** — O4 is still live for inline properties and on the OWL path.

- **AC-1 (ingestion) is not met on two of three TM Forum documents** — STATUS: **open**
  (confidence: high). A split conversion must invent no `(declaring_class, property)` pair the whole
  document does not have. TMF620 reaches **0**; TMF622 has **6** and TMF641 has **9**, and all 15 are
  on `GeographicLocation` and its `_FVO`/`_MVO` variants. Measured by
  `scripts/measure_split_isomorphism.py`, artifact `artifacts/split-isomorphism.json`, 2026-09-23.
  Structurally uniform, so probably one cause; the cause is **not** identified. Earlier wording in this
  file cited "34 → 0" from TMF620 alone, which overstated it.

- **AC-2 (ingestion) is not met: whole and split vocabularies are not isomorphic** — STATUS: **open**
  (confidence: high). `graphs_isomorphic` is False on 35 of 38 3GPP documents and on all 3 TM Forum
  documents. Smallest case diagnosed by hand: `TS28104_MdaReport`, whole 33 triples vs split 32, the
  single missing triple being `attributes rdf:type rdf:Property`. Mechanism, **hypothesis not
  measurement**: the AC-7 (ingestion) suppression rule can orphan a term — where a property's
  declaring class is external in the document that would otherwise publish it, the referring half
  suppresses it and the declaring half never reaches it, so neither emits it. Found only because
  `measure_split_isomorphism.py` exists.

- **The reconciliation gate's external-class counter is vacuous** — STATUS: **open** (confidence:
  high). `scripts/reconcile_projections.py` reports `external_classes_excluded: 0` for every spec,
  because it never passes `external_schemas` to `build_mapping`, so no class is ever external in its
  mapping. The counter was added by this plan and cannot reach a non-zero value — the same
  "guard whose inputs cannot reach the failure region" mode this file records elsewhere.

- **128 of 965 property IRIs are unscoped, and carry no `rdfs:domain`** — STATUS: **open**
  (confidence: high). Measured on one TMF620 conversion: 727 class-scoped `<base>/<Class>/<prop>`
  against 128 unscoped `<base>/<prop>`. All 128 come from `shacl_converter.py`'s inline-sub-object
  fallback (an inline object has no schema name, so there is nothing to scope under); `version`
  collects 16 `rdfs:comment` values, `name` 15, `description` 12. A second site in the
  `anyOf`/`oneOf` branch mints the same unscoped shape and additionally dash-folds via `format_name`;
  it fired **0 times** across TMF620, TS28541_NrNrm and TS29571_CommonData, so it is reachable but
  unexercised by either corpus rather than dead. **Deciding what an inline object's scope should be is
  a vocabulary decision, not yet taken.** It also blocks finishing the comment rule: after the
  declaring-class fix, TMF620 surplus comments went 137 → 126 and **124 of the residual 126 sit on
  these unscoped IRIs**, where there is no declaring class to compare against.

- **D3 (new finding): pure `$ref` alias schemas mis-classified, causing 9 3GPP dangling class
  targets** — STATUS: **open** (confidence: high)
  evidence: all 9 remaining 3GPP `rdfs:range` dangling targets are pure `$ref` alias schemas (e.g.
  `ReportingTarget: {$ref: "other.yaml#/components/schemas/..."}`, 7 with only `$ref`, 2 with
  `$ref` + `description`). The referring document mints a class IRI for the alias while the declaring
  document emits it as a datatype or nothing, because `is_primitive_def` returns `False` for an
  **external** `$ref` and so cannot see that the alias resolves to a primitive. 0 of 9 are
  referent-backed (unlike TM Forum's convention-minted names), yet all 9 **are** declared in the
  corpus. This is a classification defect, not a missing-declaration defect. Measured 2026-09-23,
  artifact `artifacts/external-schema-ingestion.json`.
  Structural pattern: 9 of 9 on 3GPP are alias-only schemas, `is_primitive_def` external-`$ref`
  handling is the cause.

- **D4 (deferred): `rdf_converter.py` OWL emitter namespaces the declaring document as
  `base/<stem>#` but siblings as `base/<filename>#` with `.yaml` extension left in** — STATUS:
  **deferred** (confidence: high)
  evidence: the RDFS/SHACL path (fixed by this plan) and the OWL path (`rdf_converter.py`) are
  separate emitters. This plan's changes do not reach the OWL emitter, and the decision was to fix
  RDFS/SHACL first. The symptom is identical to D2: a document and its referrers cannot agree on
  where a class lives. Deferred by user decision 2026-09-23, RDFS/SHACL path first.
  (An earlier version of this entry called the symptom "identical to D2"; it is D1, the namespace
  defect. D2 is the registration defect.)

## Dead ends (tried, didn't work — don't repeat)

- **"The namespace mismatch between `generate_test_cases.py` and `regenerate_output.py` is the root
  cause of the failures."** It was a real bug and worth fixing (commit `2aee21e`), but the suite read
  **785/50 before and after** — so it was not the cause. Two further fix rounds on this theory
  produced no movement. The namespaces now agree; the *local names* were the problem.
- **"The `sh:in` emitter is dropping enum constraints."** Refuted by the failure breakdown: min, max
  and pattern fail too, and they share no emitter with enum.
- **"Commit the regenerated `output/` and the freshness test will pass."** Done in `b1ae672`; the
  test failed again immediately. Regeneration is not byte-deterministic (H5), so this loop cannot
  terminate. Do not regenerate-and-commit in response to that failure.
- **Reading a piped command's exit status.** I ran `pytest … | tail -15` and took the reported exit
  code 0 as a pass; it was `tail`'s status, and pytest had failed. Any `| grep`, `| tail`, `| head`
  hides the result of the thing being tested. Read the real status unpiped.
- **Estimating a suite result instead of measuring it.** A subagent reported "~830+ passed" from an
  incomplete run that had timed out; the real figure was 785. Run it or say you didn't.
- **Dropping per-task reviews to save cost.** The final whole-branch review then found 4 Criticals
  that the skipped Task 8 and Task 10 reviews were positioned to catch, and it cost 269k tokens —
  more than the reviews it replaced.
