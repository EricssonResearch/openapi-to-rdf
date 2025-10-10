# OpenAPI RDF Converter

A Python tool for converting OpenAPI YAML specifications to RDF vocabularies and SHACL validation shapes, following W3C standards and best practices.

## Features

- **Dual Output Formats**: Generate separate RDF vocabulary + SHACL shapes (default) or traditional RDF/OWL
- **100% Schema Coverage**: Converts all OpenAPI schemas including objects, arrays, enums, and logical operators
- **W3C Standards Compliant**: Uses proper `rdfs:domain`, `rdfs:range`, and SHACL vocabulary
- **Professional Quality**: Generates clean, professional-grade RDF with proper semantic comments
- **Comprehensive Testing**: Built-in test suite validates conversion completeness and semantic correctness

## Installation

```bash
# Install directly from Git repository
pip install git+https://github.com/your-username/openapi-rdf-converter.git
```

## Quick Start

### Step 1: Download 3GPP OpenAPI Specifications

```bash
# Download all files from a specific release
openapi-rdf-converter download --release Rel-18 --output-dir assets/

# Preview what would be downloaded (dry run)
openapi-rdf-converter download --release Rel-19 --dry-run

# List available releases
openapi-rdf-converter download --list-releases
```

### Step 2: Convert to RDF/SHACL

```bash
# Convert single file
openapi-rdf-converter convert path/to/openapi.yaml

# Convert all files in directory
openapi-rdf-converter convert assets/MnS-Rel-18-OpenAPI/OpenAPI/

# Use custom namespace prefix
openapi-rdf-converter convert assets/MnS-Rel-18-OpenAPI/OpenAPI/ --namespace-prefix "https://myorg.com/models/"

# Convert to OWL format (single file)
openapi-rdf-converter convert path/to/openapi.yaml --format owl
```

### Complete Workflow Example

```bash
# 1. Download Rel-18 specifications
openapi-rdf-converter download --release Rel-18 --output-dir assets/

# 2. Convert to RDF/SHACL with custom namespace
openapi-rdf-converter convert assets/MnS-Rel-18-OpenAPI/OpenAPI/ --namespace-prefix "https://myorg.com/models/"
```

### Download Features

- **Multiple Discovery Methods**: Uses GitLab API, web scraping, and pattern matching
- **Robust Error Handling**: Falls back to alternative methods if one fails
- **Release Support**: Supports Rel-15 through Rel-19 and future releases
- **Dry Run Mode**: Preview downloads without actually downloading files
- **Organized Output**: Creates structured directories matching the repository layout

## Output Formats

### SHACL Format (Default)

Generates two separate files:
- **RDF Vocabulary** (`*_rdf.ttl`): Classes and properties with proper domain/range
- **SHACL Shapes** (`*_shacl.ttl`): Validation constraints and cardinality rules

### OWL Format

Generates a single RDF/OWL file (`*.ttl`) with traditional ontological modeling.

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

## Development Mode

For developers who want to contribute or modify the code:

### Prerequisites

- Python 3.8+
- Poetry (for dependency management)

### Development Installation

```bash
# Clone the repository
git clone <repository-url>
cd openapi-rdf-converter

# Install dependencies
poetry install
```

### Development Usage

```bash
# Download 3GPP specifications
poetry run python main.py download --release Rel-18 --output-dir assets/

# Convert OpenAPI files
poetry run python main.py convert path/to/openapi.yaml
poetry run python main.py convert assets/directory/

# Backward compatibility
poetry run python main.py path/to/openapi.yaml
```

### Testing

The project includes a comprehensive testing framework:

```bash
# Run complete test suite
cd tests
poetry run python run_tests.py ../assets/MnS-Rel-19-OpenAPI/OpenAPI/TS28623_ComDefs.yaml

# Run individual tests
poetry run python test_completeness.py <yaml_file> <rdf_file> <shacl_file>
poetry run python test_semantic_correctness.py <yaml_file> <rdf_file> <shacl_file>
```

### Architecture

#### Core Modules

- **`main.py`**: Command-line interface supporting both SHACL and OWL formats
- **`openapi_rdf_converter/shacl_converter.py`**: Main SHACL converter implementation
- **`openapi_rdf_converter/rdf_converter.py`**: Legacy OWL converter
- **`openapi_rdf_converter/download_3gpp_openapi.py`**: 3GPP specification downloader
- **`tests/`**: Comprehensive testing framework

#### Key Features

- **Schema Type Support**: Objects, arrays, enums, logical operators
- **Standards Compliance**: W3C namespaces, proper domain/range specifications
- **Namespace Management**: Auto-generated from filenames, configurable prefixes
- **Input Processing**: Single files or entire directories
- **Multiple Discovery Methods**: GitLab API, web scraping, pattern matching

## Contributing

1. Make changes to the converter code
2. Run the test suite to verify quality metrics
3. Update documentation as needed
4. Ensure all tests pass before submitting

## License

[License information]