# 5. Corpora and verification

## 5.1 Why two corpora

The claim of this chapter is narrow and it is the one most likely to transfer: **the structural
diversity of a corpus, not its size, determines what a guard can see.** Three TM Forum documents found
defects that thirty-eight 3GPP documents could not, and the reason is mechanical rather than lucky.

| property of the corpus | 3GPP | TM Forum |
|---|---|---|
| properties carrying two `rdfs:range` | 6 of 3,622 (0.17%) | 167 of 2,895 (**5.8%**) |
| properties carrying two `rdfs:domain` | 0 of 3,622 | 28 of 2,895 (1.0%) |
| attributions landing on an ancestor rather than the class that mentions the property | **0** of 3,622 | 138 of 3,033 |
| IRI-valued properties | 5 across 4 of 44 documents (0.13% of 3,749 ranges) | 15 in TMF641 alone (~2% of 795 ranges) |
| reference schemas (`*Ref`) whose referent no document declares | not measured | 39 of 56 in TMF620 |

*(Provenance and staleness, stated because they matter. Rows 1–3 were measured on the 38-document
snapshot that predates the move to a fetched 44-document corpus; the defects behind them are fixed and
the counts are now zero. Row 3 is reproduced by the committed `scripts/measure_attribution.py`, whose
own docstring gives 3,622 and 138 — while a docstring in `mapping.py` says 2,822 for the same 3GPP
figure. One of the two is a drifted copy; this document uses the script's. Row 4 was measured
corpus-wide on the current 44 documents. Row 5 is from an ad hoc run.)*

The pattern in the table is the same in every row that has one. TM Forum's shape — heavy `allOf`
inheritance, subclasses that *restate* inherited fields, polymorphic `oneOf` references, `*Ref`
schemas, variant suffixes — is exactly the shape that provokes each decision in chapters 2 and 3. 3GPP
barely restates anything, barely uses polymorphic references, and has no `*Ref` pattern. A guard written
and validated against 3GPP was therefore *sound and correctly scoped and could not reach the failure
region*, which is a strictly worse situation than a wrong guard: it passes honestly.

The clearest instance: the range-uniqueness guard was written and passed on 3GPP; TM Forum then showed
167 violations. Of the six defects found in the library on 2026-09-22, five were invisible on 3GPP and
appeared only on TM Forum.

## 5.2 Corpora as a reproducibility problem

A corpus is an input, and an input that cannot be re-fetched makes every figure derived from it an
anecdote. Both corpora had this problem, in different forms.

**3GPP.** The committed snapshot was described as Release 19. Checked against the upstream forge:

* **no ref reproduces it.** The release tag and the release branch each serve 45 files, of which only 8
  are byte-identical to the 39 committed; 31 differ in content;
* **it is not the release it is named after.** Of its 38 documents, 19 declare a major version of 18,
  14 declare 19, and 4 declare 1.

So "38 Rel-19 documents" described a mixture. The corpus is now **fetched** rather than redistributed,
at a pinned tag, and every file is verified against a recorded SHA-256. A network failure is an error,
not a skip: a fetch that quietly does nothing leaves the caller converting whatever happens to be on
disk, which is how the un-derivable snapshot arose in the first place.

**TM Forum.** The three documents lived in a directory belonging to no working tree, reached by a
hardcoded absolute path, with no licence or source URL in their `info` blocks. No fresh clone could
reproduce a single TM Forum figure and nothing said so — a found file looks like it works where a
missing one is visible as a gap. Two of the three are published under Apache-2.0 by TM Forum's GitHub
organisation and were verified byte-identical to the working copies, so they are vendored with their
upstream commits recorded. The third, TMF641, could not be traced: it is absent from the Apache-2.0
organisation, whose Service Ordering repository holds no v5 document, and TM Forum's own site sits
behind a bot challenge. It is not redistributed, and a run over two documents prints that it is
*partial* rather than a figure that reads like a corpus figure.

The two corpora are handled differently on purpose. 3GPP is dozens of documents whose snapshot proved
re-derivable from no ref, so a pinned, digest-verified fetcher earned its keep. TM Forum is two
fetchable documents with no v5 tag to pin, so a fetcher would pin a commit on a moving branch to avoid
copying 1.1 MB that the licence permits copying.

## 5.3 The gates, and what each actually asserts

| gate | asserts | command |
|---|---|---|
| projection reconciliation | every projection emitted the `Mapping`'s class IRI | `scripts/reconcile_projections.py` |
| declared terms vs shapes | every declared term has exactly one NodeShape, *less* convention-minted referents, with the subtrahend printed | `scripts/measure_corpora.py` |
| dangling class targets | no `subClassOf`/`range` names a class no document in the corpus declares | `scripts/measure_corpora.py` |
| whole vs split, isomorphism | the two conversions give the same graph, provenance partitioned out | `scripts/measure_split_isomorphism.py` |
| whole vs split, cause | every differing triple is classified, and none falls in an unexplained bucket unreported | `scripts/diagnose_split_divergence.py` |
| attribution agreement | emitter and `Mapping` attribute every property to the same class, *and* the non-trivial count is reported beside it | `scripts/measure_attribution.py` |
| output freshness | the committed `output/` tree equals a fresh regeneration — graphs by isomorphism, other files by bytes | `tests/test_output_freshness.py` |

Two of those rows exist because of a specific failure. The **non-trivial count** in the attribution
gate is reported beside the disagreement count because a zero disagreement over zero non-trivial
attributions is a null from an instrument that can only produce nulls — which is exactly what 3GPP
supplies. And `.ttl` files are compared **by isomorphism, not bytes**, because `rdflib`'s blank-node
ordering is not stable across runs: 27 of 27 files that differed byte-wise were graph-isomorphic.

## 5.4 Guards that could not fail

The most reusable finding of this work is not about ontologies. It is that **the checks written to
protect the tool repeatedly could not detect the thing they were written for**, and were found only by
running them against the failure. Six instances, each with how it was caught:

1. **A test that checked a literal against itself.** A guard introduced so a new corpus-wide metric
   could not go unreported defined its own six-element tuple and asserted that tuple's length was six.
   It passed on the same day a metric was added without being wired in. *Caught by* noticing the tuple
   was not imported from the code it claimed to guard.
2. **A test that asserted a phrase, not a fact.** `"Not redistributed" in text` would have kept passing
   after two TM Forum documents were vendored and the statement became false. *Caught by* the change
   that made it false. Replaced by an assertion that every vendored file is named and its licence stated
   — and deliberately *not* by asserting the phrase's absence, because that failed on a legitimate
   historical quotation. A phrase check cannot tell a live claim from a quoted one.
3. **A gate unsatisfiable by construction.** The isomorphism check reported 0 of 46 documents
   isomorphic, and could not have reported otherwise: a provenance triple added to every output meant a
   split always carried exactly one more triple than the whole. *Caught by* the number being *exactly*
   one on every row.
4. **A counter whose input could not reach a non-zero value.** The reconciliation gate reports how many
   classes it excluded as external; it builds its `Mapping` without external schemas, so the answer is
   always zero. The exclusion logic is correct and its own report cannot show it working.
5. **A retyped count.** A test asserting `== 3` documents fired correctly when the corpus moved, and its
   docstring had predicted exactly that. A literal that restates a value the code owns is a copy, and
   copies drift; the count now lives with the file list everything reads.
6. **Assertions that accepted any namespace.** Two cross-document tests compared substrings of an IRI
   and passed while the IRI was the wrong one. Repairing them was still right — but when the wrong
   namespace was later reintroduced, *neither* caught it, because one exercised a path the regression
   never touched and the other had been given the very configuration that hid it. 21 of 22 tests in
   those two files passed against the broken code.

The remedy in each case is the same and cheap: **make the guard fail once, on purpose, before trusting
it** — delete the guarded line, remove a key, drop a file — and watch the assertion fire. It costs a
minute, and it is the entire difference between a gate and a decoration.

## 5.5 The recurring shape

Across chapters 2–4 and the six guards above, one defect appears more than a dozen times in different
clothes: **a decision is taken in one place and the other place that needs it is not told.**

* a document's namespace derived twice, only one copy overridable (§4.2);
* a determination correct on `#/components/schemas/X` and absent on `other.yaml#/…/X` (§4.5);
* the emitter re-deriving a range the `Mapping` already held (§4.7);
* a referent exemption given to a test and not to the script that reports the same equality — and then,
  having corrected the script's printed verdict, not to its *exit status* three screens below;
* a corpus vendored on one day and its resolver left pointing at the old path for five days;
* a namespace string defined in this repository and, independently, in the consuming project;
* a property's `rdfs:range` corrected while its `sh:datatype` was not (§2.5).

Every instance was individually reasonable, and none was caught by a test written for it. The
structural remedy is the one chapter 1 describes — one implementation, read by every consumer, next to
the constant it depends on — and the practical one is to treat *"which other place needs to know?"* as
a question to ask at the moment of every decision, not after the first symptom.

## 5.6 "Conversion succeeded" is a vacuous metric

Every defect in this document was found in the output of a conversion that **succeeded**. The
mis-attributed properties, the contradictory range and marker, the 175 dangling references, the
unshaped classes — none raised, none failed a schema check, and all of them would have been reported as
a 100% success rate. A conversion tool's headline number should be a count of what a successful
conversion still gets wrong, and this work's contribution is largely the instruments for producing it.
