# HYPOTHESES — openapi-to-rdf
_Last updated: 2026-09-21. Read this first: current beliefs + status._

This repo is becoming **the** OpenAPI → RDF extractor for the workspace, absorbing
`snm-api-native/scripts/emit_tbox.py` and `sid-lift-src/lifting-core`. The design record is
`snm-api-native/docs/specs/2026-09-18-consolidate-openapi-to-rdf.md` (determinations S1–S10 from
snm-api-native, L1–L9 from lifting-core, defects O1–O11 here, acceptance criteria AC-1…AC-8), and
the task plan is `snm-api-native/docs/superpowers/plans/2026-09-18-openapi-to-rdf-contribution.md`.

## Where the suite stands

**785 passed, 50 failed, 1 xfailed** (measured 2026-09-21, full run, 274s). Both failure groups now
have a measured cause: 49 are H1–H3 below, and the 1 freshness failure is H5.

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
  evidence: `uv run python scripts/diagnose_shape_coverage.py` → 628 instance files, 1,075 declared
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

- **H3: the remaining 70 occurrences (10 distinct classes) have no shape because the converter never
  emits one — and this may be CORRECT** — STATUS: **open** (confidence: low, and deliberately so)
  evidence: `TS29571_CommonData:GlobalRanNodeId`, `Area`, `GeraLocation`, `UtraLocation`;
  `TS28541_5GcNrm:ImsiRange`, `PlmnRange`; `TS28623_TraceControlNrm:EventThreshold1F`;
  `TS28623_ComDefs:otherProblems`; `TS28623_GenericNrm:EP_RP_Attr`, `attributes`.
  `GlobalRanNodeId` is named in an observed failure (`GlobalRanNodeId_missing_plmnId.ttl`).
  **The open question, and it must be answered before any code changes:** several of these are
  top-level `oneOf` unions. Determination **S2 says never mint a class for a `oneOf` union**
  (it produced 14 unreachable classes and 14 unreachable context terms in snm-api-native, and TMF's
  own discriminator never selects the wrapper). If S2 applies, the converter is right to emit no
  shape and the **fixture generator is wrong to emit instances of a union wrapper** — making H3 a
  second fixture bug rather than a converter gap. Check `components/schemas` for each of the 10
  before assuming either.
  cheapest experiment: for each of the 10, print whether its schema has a top-level `oneOf`, a
  top-level `required`, or both. That table decides converter-gap vs fixture-bug per class.

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

## Open decision (do not resolve by weakening the gate)

H5 needs a judgment call, and the wrong instinct is to relax the test so the suite goes green:

- **Option A — compare graph isomorphism for `.ttl`, bytes for everything else.** Byte equality is
  arguably the wrong equivalence relation for an RDF graph in the first place, so this makes the test
  assert what it actually means. Small change, one file.
- **Option B — make serialization byte-deterministic** (canonical/sorted emission before writing).
  Harder with rdflib blank nodes, but it preserves the stronger property, and `output/` is a
  *published deliverable* — consumers diff those files, so gratuitous byte churn has a real cost
  even when the graph is unchanged.

These differ in what they protect, not just in effort, which is why it is a decision rather than a
task. Option A stops detecting byte churn that Option B would prevent.

## Settled (established this project — don't relitigate)

- **`rdfs:range` only where true** (S1) — datatype ranges 2,676 → 2,281, removing 395 invented
  triples; class ranges unchanged at 1,614. Range propagates under RDFS entailment, so a
  multi-target range asserts types about unobserved data. Commit `2f7e45d`.
- **One NodeShape per class** (defect O2) — `sh:targetClass` 2,399 → 1,770 against 1,769 classes.
  `allOf` was processed twice, and `allOf` *is* inheritance, so it hit most interesting classes.
  Commit `6b71d07`. The `anyOf` limitation is pinned with `xfail(strict=True)`.
- **No guessing, no placeholders** (S3, S4, O5, O6) — a fragment-less `$ref` is refused rather than
  given a placeholder IRI, which used to leak the build directory. Commit `6915f5b`.
- **File structure is provenance, never identity** — the source document is recorded as a triple,
  never as an IRI segment. Decisive argument: under file-derived identity a class in
  `common-tmf-v5.yaml` would get a different IRI than the same class inlined, contradicting the
  premise of the split model (one class, one IRI, N APIs).
- **The `Mapping` spine: one derived object, four projections** — TTL (vocabulary + SHACL), Overlay
  1.1.0, JSON-LD `@context`, Hydra operation graph. Commits `278987a`, `5ead8a3`, then `928a884`…
  `73b8ff3`. A reconciliation gate checks the four agree on every class IRI (AC-4).
- **The conversion needs only the OpenAPI document** (AC-5) — proven, not asserted: deriving with an
  *empty* TBox produces a byte-identical overlay body. This is the adoption thesis in one line.
- **`output/` is a published deliverable**, not scratch — 114 tracked files, advertised in the
  README. It was briefly gitignored by mistake; `test_output_freshness.py` now gates its currency.
- **uv, not poetry** — PEP 621 + hatchling, `requires-python >= 3.11`. Commit `9619606`.

## Open questions / next tests

- **H3's table (above) is the first thing to run.** It is ten schema lookups and it decides whether
  the next change touches the converter or the fixture generator. Do not start coding without it.
- **AC-8 (both corpora pass) is not demonstrated.** 0 of 27 test modules reference TM Forum, and
  `scripts/_corpora.py:34` hardcodes a home path. The 3GPP corpus is well covered; TM Forum is
  asserted, not measured.
- **352 `sh:targetClass` triples point at `rdfs:Datatype`.** Noticed, never measured; the TM Forum
  count was never established.
- **AC-2 is not met** — determinations L9, L8 and S9 have no test in this repo. AC-2 exists because
  "carried as a comment" is exactly what failed to stop defect F12 (`allOf` property loss) being
  independently rediscovered in three repos.
- **AC-3 is partial** — O4 is still live for inline properties and on the OWL path.

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
