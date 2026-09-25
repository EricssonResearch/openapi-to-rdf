# OpenAPI to RDF Converter

Convert OpenAPI YAML **schema definitions and operations** to RDF vocabularies, SHACL validation shapes, JSON-LD contexts, OpenAPI Overlays, and Hydra operation graphs for telecom intent-based automation and 3GPP standards. Adheres to W3C standards and best practices.

The tool converts `components/schemas` to RDF/SHACL vocabularies and `paths` to Hydra Core operation graphs (W3C Community Group draft). All projections derive from a single Mapping, ensuring the operation graph and vocabulary share class IRIs automatically.

## Features

- **Dual Output Formats**: Generate separate RDF vocabulary + SHACL shapes (default) or traditional RDF/OWL
- **Schema Coverage**: Converts OpenAPI schemas including objects, arrays, enums, and logical operators
- **Uses W3C vocabularies**: `rdfs:domain` and `rdfs:range` on properties, constraints expressed in SHACL. Not a compliance claim — no conformance suite is run, and `rdfs:range` is emitted only where a property has exactly one target class, because a range propagates under RDFS entailment and a multi-target range would assert something the document does not say.
- **Tested on two corpora**: the 44 3GPP SA5 documents at tag `Tag_Rel19_SA112`, **fetched** rather than redistributed (`uv run python scripts/fetch_corpus.py`; pinned and digest-verified in `assets/corpus-manifest.json`), and three TM Forum Open API v5 documents -- two of them (TMF620, TMF622) redistributed here under Apache-2.0 with upstream commits recorded in `assets/tmforum/PROVENANCE.md`, and TMF641 absent because its origin could not be established (see that file). Other OpenAPI documents are expected to work but are not exercised — and the two corpora repeatedly disagree about what they cover, so "works on 3GPP" has more than once turned out not to mean "works".
- **3GPP SA5 RDF/SHACL**: We provide pre-generated RDF/SHACL output for the 3GPP SA5 documents at `Tag_Rel19_SA112`. The source documents are NOT redistributed here -- `scripts/fetch_corpus.py` downloads them from `https://forge.3gpp.org/rep/sa5/MnS/` at that pinned tag and verifies every file against a recorded SHA-256, so a fresh clone reproduces the same corpus. (Until 2026-09-25 a snapshot was committed instead, and no ref reproduced it: 8 of 39 files matched the tag, and 19 of the 38 documents declared a Rel-18 version in a directory named Rel-19.)

## Installation

```bash
pip install openapi-to-rdf
```

## Quick Start

### Get OpenAPI Specifications

First, obtain OpenAPI YAML files from your preferred source:

**3GPP Specifications:**
```bash
# Download from 3GPP Forge
curl -O https://forge.3gpp.org/rep/sa5/MnS/-/raw/Rel-18/OpenAPI/TS28623_ComDefs.yaml
```


### Convert to RDF/SHACL

```bash
# Convert single file
openapi-to-rdf openapi-spec.yaml

# Convert multiple files
openapi-to-rdf file1.yaml file2.yaml file3.yaml

# Convert all YAML files in a directory
openapi-to-rdf /path/to/openapi/specs/

# Use custom namespace prefix
openapi-to-rdf openapi-spec.yaml --namespace-prefix "https://myorg.com/models/"

# Convert to OWL format instead of SHACL
openapi-to-rdf openapi-spec.yaml --format owl
```

### Complete Example

```bash
# 1. Download 3GPP specifications
mkdir specs && cd specs
curl -O https://forge.3gpp.org/rep/sa5/MnS/-/raw/Rel-18/OpenAPI/TS28623_ComDefs.yaml
curl -O https://forge.3gpp.org/rep/sa5/MnS/-/raw/Rel-18/OpenAPI/TS28623_GenericNrm.yaml

# 2. Convert to RDF/SHACL
openapi-to-rdf *.yaml --namespace-prefix "https://myorg.com/models/3gpp/"
```

## Tested Sources

This tool has been tested and validated with:
- **3GPP SA5 MnS specifications** from https://forge.3gpp.org/rep/sa5/MnS/
- **Releases**: Rel-18, Rel-19
- **38+ different schema files** covering various network management domains

## Output Formats

### SHACL Format (Default)

Generates three files per input:
- **RDF Vocabulary** (`output/rdf/*_rdf.ttl`): Classes and properties with proper domain/range.
- **SHACL Shapes** (`output/shacl/*_shacl.ttl`): Validation constraints and cardinality rules.
- **Property Index** (`output/index/*_property_index.yaml`): Sidecar manifest listing every generated property URI, its owning class, its range, and any collisions (same local name, different range or description). See the "Property identity and merging" section of [CONVERSION_DOC.md](CONVERSION_DOC.md) for the schema and its intended use by a future merge step.


## Example Output

> Every excerpt below is **copied from `output/`**, not written by hand. The previous version of this
> section was hand-typed and had drifted on three counts: a namespace the tool no longer mints
> (`http://ericsson.com/.../ComDefs#`, hash-separated), and a class — `TimeWindow` — that is a `oneOf`
> union and therefore gets **no class declared** at all, so the example advertised output that
> contradicted the project's own determination. `tests/test_readme_examples.py` now keeps these honest.

### RDF Vocabulary (`*_rdf.ttl`)

Properties are minted under a **per-class namespace**, so two schemas
that happen to use the same property name produce two distinct URIs.

Every generated file opens with a provenance header, because a derived vocabulary that looks official
is worse than one that looks unfinished:

```turtle
# GENERATED FILE -- do not edit; your changes will be overwritten.
#
# RDF vocabulary produced by openapi-to-rdf
#   https://github.com/EricssonResearch/openapi-to-rdf
# Contact: Jean Martins <jean.martins@ericsson.com>
# Source document: TS28623_ComDefs.yaml
#
# The CONVERSION is this project's work. The class and property names below, and the
# `rdfs:comment` descriptions, are the source document's own text. The publisher of that
# document did not produce, review or endorse this file, and it must not be cited as
# their model. See NOTICE for the corpora this repository bundles and their terms.
#
# Regenerate: uv run python scripts/regenerate_output.py

@prefix TS28623_ComDefs: <https://semantics.ericsson.com/openapi/TS28623_ComDefs/> .
@prefix TS28623_ComDefs_DayInYear: <https://semantics.ericsson.com/openapi/TS28623_ComDefs/DayInYear/> .

TS28623_ComDefs:DayInYear a rdfs:Class .

TS28623_ComDefs_DayInYear:month a rdf:Property ;
    rdfs:domain TS28623_ComDefs:DayInYear ;
    rdfs:range xsd:gMonth .

TS28623_ComDefs_DayInYear:monthDay a rdf:Property ;
    rdfs:domain TS28623_ComDefs:DayInYear ;
    rdfs:range xsd:gMonthDay .
```

### SHACL Shapes (`*_shacl.ttl`)
```turtle
[] a sh:NodeShape ;
    sh:property [ a sh:PropertyShape ;
            sh:datatype xsd:gMonth ;
            sh:maxCount 1 ;
            sh:path TS28623_ComDefs_DayInYear:month ],
        [ a sh:PropertyShape ;
            sh:datatype xsd:gMonthDay ;
            sh:maxCount 1 ;
            sh:path TS28623_ComDefs_DayInYear:monthDay ] ;
    sh:targetClass TS28623_ComDefs:DayInYear .
```

### Property Index (`*_property_index.yaml`)

```yaml
source: TS28623_ComDefs.yaml
generated_by: openapi-to-rdf 0.2.0
properties:
  - local_name: month
    uri: https://semantics.ericsson.com/openapi/TS28623_ComDefs/DayInYear/month
    owner_class: DayInYear
    range: http://www.w3.org/2001/XMLSchema#gMonth
    description: null
  # ... one entry per (class, property) pair
collisions: []   # none in this document
```

A real collision, from `output/index/TS28105_AiMlNrm_property_index.yaml` — `attributes` is declared by
20 classes in one document and the per-class namespace is what keeps them distinct:

```yaml
collisions:
  - local_name: attributes
    members:
      - https://semantics.ericsson.com/openapi/TS28105_AiMlNrm/AIMLInferenceFunction-Single/attributes
      - https://semantics.ericsson.com/openapi/TS28105_AiMlNrm/MLModel-Single/attributes
      # ... 18 more
    differs_on:
      - range
```

The `collisions` section flags same-named properties that disagree on
range or description and is meant to drive a future, opinion-driven
merging step (see [CONVERSION_DOC.md](CONVERSION_DOC.md)).

📖 **For comprehensive conversion examples and detailed explanations of all OpenAPI patterns, see [CONVERSION_DOC.md](CONVERSION_DOC.md)**


## Vocabulary this tool uses, and what is ours

Everything below is stated so a reader can tell a standard from a convention from an invention.
Presenting any of it as more authoritative than it is would be the failure mode this section exists
to prevent.

| term | status |
|---|---|
| `rdf:`, `rdfs:`, `owl:`, `xsd:`, `sh:` | **W3C Recommendations** |
| `dcterms:hasVersion` | **DCMI Recommendation**, permanent namespace |
| `hydra:` (`Operation`, `method`, `returns`, `expects`, `supportedOperation`, `IriTemplate`, `template`) | **W3C Community Group draft**, *not* a Recommendation. Chosen for its permanent `w3.org/ns/` namespace; may be called a convention with a stable namespace, may **not** be called a standard |
| **`affordance:invokedAt`** | **OURS. Minted by this project. Asserts no external authority.** |
| the operation IRI scheme | **OURS.** No standard names operations in RDF |
| the class and property IRI scheme | **OURS** |

### `affordance:invokedAt` — a term we had to invent

`https://semantics.ericsson.com/ontology/affordance/invokedAt`

Relates a `hydra:Operation` to the `hydra:IriTemplate` it is invoked at. **Nothing in Hydra Core,
Dublin Core or any W3C Recommendation defines it**, and it must not be presented as standard
vocabulary in a paper, a report or a dataset description.

It exists because Hydra models *a templated link* (`hydra:IriTemplate` + `hydra:template`) and
*operations a class supports* (`hydra:supportedOperation`), but has **no term relating an operation
to its template**. The obvious shortcut is unavailable: `hydra:template`'s domain is
`hydra:IriTemplate`, so putting it straight on the operation would entail *the operation IS a
template*, which is false. This projection did exactly that until 2026-09-22.

The emitted shape is therefore:

```turtle
<…/operation/serviceOrdering/v5/get/serviceOrder>
    a hydra:Operation ;
    hydra:method "GET" ;
    dcterms:hasVersion "5.0.0" ;
    hydra:returns tmf:ServiceOrder ;
    affordance:invokedAt <…/operation/serviceOrdering/v5/get/serviceOrder#template> .

<…/operation/serviceOrdering/v5/get/serviceOrder#template>
    a hydra:IriTemplate ;
    hydra:template "/serviceOrder" .
```

The template node is a real IRI rather than a blank node, deliberately, so a consumer can address
it. Swapping Hydra for WoT means replacing the Hydra terms; this one must be **re-justified**
against whatever the replacement offers rather than carried over unexamined.

### Operation IRIs: `operation/<api>/<majorVersion>/<method><path>`

Derived from `info.title` and `info.version` — both **REQUIRED** by the OpenAPI Specification — plus
the method and path template. Not from `operationId`, which is **optional** and duplicated in
practice: a scheme that needs an optional field has no defined behaviour on a document omitting it.

The API and version segments fix two silently-merging collisions measured on 2026-09-22:

* **across versions** — TMF641 v5 and v4.1 under one namespace produced **20 of 20 identical**
  operation IRIs, carrying two conflicting `dcterms:hasVersion` literals on one node;
* **across APIs** — every TM Forum API defines `/hub`, so `delete/hub/{id}` was **one node shared by
  TMF620, TMF622 and TMF641**.

Both now measure 0.
