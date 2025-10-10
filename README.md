# OpenAPI RDF Converter

A Python tool for converting OpenAPI YAML specifications to RDF vocabularies and SHACL validation shapes, following W3C standards and best practices.

## Features

- **Dual Output Formats**: Generate separate RDF vocabulary + SHACL shapes (default) or traditional RDF/OWL
- **100% Schema Coverage**: Converts all OpenAPI schemas including objects, arrays, enums, and logical operators
- **W3C Standards Compliant**: Uses proper `rdfs:domain`, `rdfs:range`, and SHACL vocabulary
- **Professional Quality**: Generates clean, professional-grade RDF with proper semantic comments
- **Comprehensive Testing**: Built-in test suite validates conversion completeness and semantic correctness

## Quick Start

### Prerequisites

- Python 3.8+
- Poetry (for dependency management)

### Installation

```bash
# Clone the repository
git clone <repository-url>
cd openapi-rdf-converter

# Install dependencies
poetry install
```

### Basic Usage

```bash
# Convert to SHACL format (separate RDF vocabulary + SHACL shapes)
poetry run python main.py path/to/openapi.yaml

# Convert to OWL format (traditional RDF/OWL)
poetry run python main.py path/to/openapi.yaml --format owl

# Process entire directory
poetry run python main.py assets/MnS-Rel-19-OpenAPI/OpenAPI/ --format shacl
```

### Example

```bash
# Convert 3GPP ComDefs specification
poetry run python main.py assets/MnS-Rel-19-OpenAPI/OpenAPI/TS28623_ComDefs.yaml

# Output files:
# - output/TS28623_ComDefs_rdf.ttl    (RDF vocabulary)
# - output/TS28623_ComDefs_shacl.ttl (SHACL validation shapes)
```

## Output Formats

### SHACL Format (Default)

Generates two separate files:
- **RDF Vocabulary** (`*_rdf.ttl`): Classes and properties with proper domain/range
- **SHACL Shapes** (`*_shacl.ttl`): Validation constraints and cardinality rules

### OWL Format

Generates a single RDF/OWL file (`*.ttl`) with traditional ontological modeling.

## Testing

The project includes a comprehensive testing framework:

```bash
# Run complete test suite
cd tests
poetry run python run_tests.py ../assets/MnS-Rel-19-OpenAPI/OpenAPI/TS28623_ComDefs.yaml

# Run individual tests
poetry run python test_completeness.py <yaml_file> <rdf_file> <shacl_file>
poetry run python test_semantic_correctness.py <yaml_file> <rdf_file> <shacl_file>
```

### Test Reports

The testing framework provides:
- **Schema Coverage**: Percentage of OpenAPI schemas converted to RDF classes
- **Property Coverage**: Percentage of OpenAPI properties converted to RDF properties  
- **Description Preservation**: Percentage of descriptions preserved as `rdfs:comment`
- **W3C Compliance**: Validation against RDF/RDFS/SHACL standards
- **Semantic Correctness**: Detection of ambiguous OpenAPI→RDF conversions

## Architecture

### Core Modules

- **`main.py`**: Command-line interface supporting both SHACL and OWL formats
- **`yaml2rdf/shacl_converter.py`**: Main SHACL converter implementation
- **`yaml2rdf/converter.py`**: Legacy OWL converter
- **`tests/`**: Comprehensive testing framework

### Key Features

#### Schema Type Support
- **Objects**: Converted to `rdfs:Class` with property constraints
- **Arrays**: Handled with `dash:ListShape` and item validation
- **Simple Types**: String, number, integer with format support
- **Logical Operators**: `oneOf`, `allOf`, `anyOf` with SHACL constraints
- **Enumerations**: Proper enum value validation

#### Standards Compliance
- Uses standard W3C namespaces (`rdf:`, `rdfs:`, `sh:`, `xsd:`)
- Proper `rdfs:domain` and `rdfs:range` specifications
- SHACL constraint language for validation
- Semantic comments for OpenAPI features that don't translate directly

#### Namespace Management
- Auto-generates namespaces from filenames (e.g., `TS28623_ComDefs.yaml` → `http://ericsson.com/models/3gpp/TS28623/ComDefs#`)
- Handles external references between OpenAPI files
- Consistent cross-file referencing

## Docker Support

```bash
# Build image
docker build -t openapi-rdf-converter .

# Run converter
docker run -v $(pwd)/assets:/input -v $(pwd)/output:/output openapi-rdf-converter \
  python main.py /input/TS28623_ComDefs.yaml --format shacl
```

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
        sh:minCount 1 ] .
```

## Quality Metrics

Recent test results on TS28623_ComDefs.yaml:
- **Schema Coverage**: 100% (64/64 schemas)
- **Property Coverage**: 100% (30/30 properties)  
- **Description Preservation**: 100% (35/35 descriptions)
- **Overall Quality Score**: 100% ✅ EXCELLENT

## Contributing

1. Make changes to the converter code
2. Run the test suite to verify quality metrics
3. Update documentation as needed
4. Ensure all tests pass before submitting

## License

[License information]