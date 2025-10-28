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

Generates two separate files:
- **RDF Vocabulary** (`*_rdf.ttl`): Classes and properties with proper domain/range
- **SHACL Shapes** (`*_shacl.ttl`): Validation constraints and cardinality rules


## Example Output

### RDF Vocabulary (`*_rdf.ttl`)
```turtle
@prefix TS28623_ComDefs: <http://ericsson.com/models/3gpp/TS28623/ComDefs#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

TS28623_ComDefs:TimeWindow a rdfs:Class ;
    rdfs:comment "Note: Uses OpenAPI xone - complex logical constraints partially supported in SHACL" .

TS28623_ComDefs:startTime a rdf:Property ;
    rdfs:domain TS28623_ComDefs:TimeWindow ;
    rdfs:range TS28623_ComDefs:DateTime .
```

### SHACL Shapes (`*_shacl.ttl`)
```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix TS28623_ComDefs: <http://ericsson.com/models/3gpp/TS28623/ComDefs#> .

[] a sh:NodeShape ;
    sh:targetClass TS28623_ComDefs:TimeWindow ;
    sh:property [ a sh:PropertyShape ;
        sh:path TS28623_ComDefs:startTime ;
        sh:class TS28623_ComDefs:DateTime ;
        sh:maxCount 1 ] .
```

📖 **For comprehensive conversion examples and detailed explanations of all OpenAPI patterns, see [CONVERSION_DOC.md](CONVERSION_DOC.md)**

