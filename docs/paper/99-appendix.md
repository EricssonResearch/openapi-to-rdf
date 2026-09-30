# Appendix

## A. Decision table

One row per decision where a naive mapping is defensible and wrong. "Cost" is what getting it wrong
measured; a blank means no measurement is recorded.

| construct | decision | why | cost when wrong |
|---|---|---|---|
| property declared on an ancestor, restated by a subclass | attribute to the **highest declaring ancestor**; one IRI | an ontology declares an inherited property once | 34 misattributed pairs on a split TMF620; 68.5% vs 93.9% resolution against a reference TBox |
| a `*Ref` schema | keep the class, mark it a serialisation artifact, point ranges at the **referent** | an IRI already is a reference | 39 of 56 referents undeclared on TMF620 |
| the referent of a `*Ref` | **declare it**, mark it `isMintedByConvention` | the ontology needs the term though no schema defines it | 45 of 275 ranges pointing at nothing |
| referent lookup | mint **by convention**, never by presence check | a presence check makes the answer depend on which files are loaded | one specification resolving `PartyRef` two ways |
| a `oneOf` union | **no class**; expand into members on the accepting property | JSON Schema workaround for serialisation and taxonomy | 14 unreachable classes and context terms |
| a primitive alias | a datatype, not a class | no payload can instantiate it | |
| `_FVO` / `_MVO` variants | one class, three JSON shapes | variants share their base's IRI | |
| transport envelopes | classify and mint separately; **never drop** | omitting broke nesting | 76 triples → 1 through an envelope |
| `rdfs:range` | only where provably true; one per property | a range propagates under entailment | 167 of 2,895 properties with two ranges on TM Forum |
| `rdfs:domain` | the declaring class, exactly once | domains are conjunctive under entailment | 12 domains on `Addressable#href`, 25 on `Event#event` |
| IRI-valued property (`format: uri` **or** the name `href`) | mark it; **no** `rdfs:range xsd:string` | wire type is not object type | 15 of 15 on TMF641 carried both (§6.1: SHACL still open) |
| `rdfs:comment` | from the **declaring** schema only | a subclass's description is not a statement about the ancestor's property | 5 comments on `Addressable/id` |
| operation identity | method + path + API slug + major version | `operationId` is optional and duplicated | 20 of 20 identical IRIs across two versions; `/hub` shared across three APIs |
| a body that is a `$ref` into `components` | follow the chain | TM Forum writes every body that way | 0 of 8 responses, 0 of 14 requests resolved |
| an operation with no resolvable class | record **whether a body exists** separately | absence and failure are different facts | 8 of 20 reported where the truth is 8 of 8 |
| a class in another document | mint under the **declaring** document's namespace | identity follows the declaring document | 175 dangling targets (historical) |
| an unlisted external document | its own derived namespace, not the referrer's | the only answer needing no instruction | 16 → 79 dangling on 3GPP |
| a property with two same-named declarations | keep both, **report** the collision, do not merge | merging is an opinionated modelling step | |
| a fragment-less `$ref` | **refuse**, naming the spec section | a placeholder IRI leaked the build directory | |

## B. IRI grammar

**Every IRI in this section is minted by this project and asserts no external authority.** None is
claimed to dereference.

```
class        <namespace-of-declaring-document><LocalName>
property     <namespace-of-declaring-class><DeclaringClass>/<property>
operation    <operation-namespace>operation/<api-slug>/<vN><method><percent-encoded-path>
marker       https://semantics.ericsson.com/vocab/transport/{isTransportEnvelope | isIriValued |
                                                             isSerialisationArtifact | refersTo |
                                                             isMintedByConvention}
affordance   https://semantics.ericsson.com/ontology/affordance/invokedAt
```

* the local name is **injective**: `-` is kept, never folded to `_`, because folding collapses
  `Files-Single` and `Files_Single` onto one IRI;
* `<api-slug>` is `info.title` in lowerCamelCase; `<vN>` is the **major** version only;
* the namespace of a class is, in order: a per-class override, an explicit per-document mapping, the
  converted document's own base namespace when it is that document, and otherwise a derivation from the
  filename;
* transport envelopes and their properties live under the transport namespace regardless of document.

The two markers' host is shared but their path prefixes differ (`/vocab/` and `/ontology/`); the
project describes itself as using one host, which is accurate for the host and not for the path.

## C. Glossary

**declaring class** — the highest ancestor whose schema declares a property.
**referent** — what a `*Ref` denotes: `AgreementRef` → `Agreement`.
**serialisation artifact** — a class that exists because JSON must choose between embedding and
pointing; it contributes an edge to its referent, never a type.
**minted by convention** — a term inferred from a naming rule rather than published by any document.
**transport envelope** — wire plumbing (notification wrappers, event payloads, JSON Patch) as opposed
to a domain concept.
**external class** — a class declared by a different document of the same description.
**vocabulary boundary** — a file boundary that is also a published-vocabulary boundary. True of a
3GPP specification, false of a carve of TM Forum's API family. Not stated by any document.
**`Mapping`** — the single derived fact set every projection reads.

### Label collisions to avoid

This repository has reused labels for unrelated things, and the collisions have caused real confusion:

* **`AC-n`** means acceptance criterion *n* of the consolidation specification **and** of the external-
  schema specification, unrelated to each other. Always write which.
* **`D1`–`D4`** are defects in the external-schema specification; `HYPOTHESES.md` separately calls the
  `$ref`-alias defect `D3`, where the specification's `D3` is a document-keying inconsistency. This
  document uses descriptions, not labels, for that reason.

## D. Figure provenance

Every number in this document and where it comes from. **Re-derivable** means a committed command
produces it from a fresh clone.

| figure | source | re-derivable? |
|---|---|---|
| 17 dangling class targets (3GPP), 0 (TM Forum) | `scripts/measure_corpora.py` | **yes** — artifact current 2026-09-30 |
| declared terms vs `sh:targetClass`; +3 in one document | `scripts/measure_corpora.py` | **yes** |
| whole-vs-split cause classification (34 / 23 / 5 / 4 / 3 / 0) | `scripts/diagnose_split_divergence.py` | **yes** — artifact 2026-09-28; totals disagree with `HYPOTHESES.md` prose |
| 6 and 9 invented pairs; 0 on TMF620 | `scripts/measure_split_isomorphism.py` | **yes**, last run 2026-09-23 |
| 0 of 3,622 vs 138 of 3,033 non-trivial attributions | `scripts/measure_attribution.py` | **yes**, on the historical snapshot; `mapping.py` says 2,822 |
| 167 of 2,895 vs 6 of 3,622 multiple ranges; 28 of 2,895 multiple domains | `scripts/measure_corpora.py` (`properties_multi_range`, `_domain`) | **yes** as a metric; the quoted values are pre-fix |
| corpus is not the named release (8 of 39; 19 of 38 at Rel-18) | recorded in `scripts/fetch_corpus.py` | the pin and digests are; the *comparison* was not scripted |
| 27 of 27 differing `.ttl` files graph-isomorphic | `HYPOTHESES.md`, behind `tests/test_output_freshness.py` | count not scripted |
| 216 datatype disagreements → 0 | commit message | **harness not identified in this document** |
| 5 IRI-valued properties across 4 of 44 documents (3GPP); 15 / 20 (TM Forum) | ad hoc, this work | **no — promote to a script** |
| 39 of 56 undeclared referents; 45 of 275 dangling ranges (TMF620) | ad hoc | **no** |
| `sh:datatype` rejects an IRI-valued property (§6.1) | ad hoc `pyshacl` run | **no** |
| comment surplus 44 / 119; injected notes on `*Ref` classes | ad hoc | **no** |
| 34 misattributed pairs; 144 properties gaining a range, 0 losing | ad hoc and a reviewer's script | **no** — pre-fix values |
| 175 dangling targets in the old committed output tree | ad hoc | **no** — the tree was replaced |
| 16 → 79 dangling on 3GPP | a reviewer's ad hoc script | **no** |
| 93.9% vs 68.5% attribution against a reference TBox | `snm-api-native` | **no** — not in this repository |
| 75 agreed / 132 disagreed class mappings | consolidation history | **no** |
| operation IRI collisions (20 of 20; `/hub`); 0 of 8 / 0 of 14; 8 of 20 vs 8 of 8 | docstrings in `mapping.py` | **no** |
| 76 triples vs 1 through an envelope; 14 unreachable classes; 95 dropped terms | docstrings, from the consuming project | **no** |

**Counted from the table above: of twenty rows, six are fully re-derivable from a fresh clone, two
partly, and twelve not.** Until that changes, a reader should treat the twelve as observations reported
in good faith rather than results they can check. The cheapest single improvement to this document's
standing is promoting the four rows marked "ad hoc, this work" — IRI-valued counts, undeclared
referents, the SHACL rejection, and the comment surplus — to committed scripts, since they were measured
in this work, are still true of the current code, and are the ones a reader is most likely to want to
re-run.
