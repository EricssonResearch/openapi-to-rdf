# OpenAPI to RDF Converter

Convert OpenAPI YAML **schema definitions** to RDF vocabularies and SHACL validation shapes for telecom intent-based automation and 3GPP standards. Adheres to W3C standards and best practices.

**Note:** This tool converts only the `components/schemas` section of OpenAPI specifications, not endpoints, paths, or operations.

## Features

- **Dual Output Formats**: Generate separate RDF vocabulary + SHACL shapes (default) or traditional RDF/OWL
- **Schema Coverage**: Converts OpenAPI schemas including objects, arrays, enums, and logical operators
- **W3C Standards Compliant**: Assigns proper `rdfs:domain`, `rdfs:range` to properties, and constraints via SHACL vocabulary
- **Universality**: Should works with any OpenAPI schema specification, althoug only tested with 3GPP input.
- **3GPP SA5 RDF/SHACL**: We provide pre-generated RDF/SHACL output for 3GPP SA5 Release-19 OpenAPI schemas (downloaded from `https://forge.3gpp.org/rep/sa5/MnS/` into `assets/`)

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

### RDF Vocabulary (`*_rdf.ttl`)

Properties are minted under a **per-class namespace**, so two schemas
that happen to use the same property name produce two distinct URIs.

```turtle
@prefix TS28623_ComDefs: <http://ericsson.com/models/3gpp/TS28623/ComDefs#> .
@prefix TS28623_ComDefs_TimeWindow: <http://ericsson.com/models/3gpp/TS28623/ComDefs/TimeWindow#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

TS28623_ComDefs:TimeWindow a rdfs:Class .

TS28623_ComDefs_TimeWindow:startTime a rdf:Property ;
    rdfs:domain TS28623_ComDefs:TimeWindow ;
    rdfs:range TS28623_ComDefs:DateTime .
```

### SHACL Shapes (`*_shacl.ttl`)
```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix TS28623_ComDefs: <http://ericsson.com/models/3gpp/TS28623/ComDefs#> .
@prefix TS28623_ComDefs_TimeWindow: <http://ericsson.com/models/3gpp/TS28623/ComDefs/TimeWindow#> .

[] a sh:NodeShape ;
    sh:targetClass TS28623_ComDefs:TimeWindow ;
    sh:property [ a sh:PropertyShape ;
        sh:path TS28623_ComDefs_TimeWindow:startTime ;
        sh:class TS28623_ComDefs:DateTime ;
        sh:maxCount 1 ] .
```

### Property Index (`*_property_index.yaml`)

```yaml
source: TS28623_ComDefs.yaml
generated_by: openapi-to-rdf 0.2.0
properties:
  - local_name: startTime
    uri: http://ericsson.com/models/3gpp/TS28623/ComDefs/TimeWindow#startTime
    owner_class: TimeWindow
    range: http://ericsson.com/models/3gpp/TS28623/ComDefs#DateTime
    description: null
  # ... one entry per (class, property) pair
collisions:
  - local_name: startTime
    members:
      - http://ericsson.com/models/3gpp/TS28623/ComDefs/TimeWindow#startTime
      - http://ericsson.com/models/3gpp/TS28623/ComDefs/PerfMetricJob#startTime
    differs_on: [range]
```

The `collisions` section flags same-named properties that disagree on
range or description and is meant to drive a future, opinion-driven
merging step (see [CONVERSION_DOC.md](CONVERSION_DOC.md)).

📖 **For comprehensive conversion examples and detailed explanations of all OpenAPI patterns, see [CONVERSION_DOC.md](CONVERSION_DOC.md)**

