# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Common Commands

### Python Development
- **Install dependencies**: `poetry install`
- **Run SHACL converter (default)**: `poetry run python main.py convert <input_yaml_file_or_directory>`
  - Generates separate `*_rdf.ttl` (vocabulary) and `*_shacl.ttl` (shapes) files
- **Run OWL converter**: `poetry run python main.py convert <input> --format owl`
- **Download 3GPP specs**: `poetry run python main.py download --release Rel-19 --output-dir assets/`
- **List available releases**: `poetry run python main.py download --list-releases`

### Backward Compatibility
- **Legacy syntax**: `poetry run python main.py <input> [--format shacl|owl]` (still supported)

### Testing
- **Run complete test suite**: `cd tests && poetry run python run_tests.py ../assets/MnS-Rel-19-OpenAPI/OpenAPI/TS28623_ComDefs.yaml`
- **Individual tests**: `poetry run python test_completeness.py <yaml> <rdf> <shacl>`
- **SHACL syntax validation**: `poetry run python test_shacl_syntax.py <shacl_file>`

## Architecture Overview

This project converts OpenAPI YAML specifications to RDF formats with **two conversion approaches**:

### Core Architecture (`openapi_to_rdf/`)
- **Entry point**: `main.py` - Modern CLI with subcommands (convert, download) + backward compatibility
- **SHACL converter**: `openapi_to_rdf/shacl_converter.py` - `OpenAPIToSHACLConverter` class (default)
- **OWL converter**: `openapi_to_rdf/rdf_converter.py` - `OpenAPIToRDFConverter` class (legacy)
- **3GPP downloader**: `openapi_to_rdf/download_3gpp_openapi.py` - `ThreeGPPDownloader` class for spec retrieval
- **Testing framework**: `tests/` - Comprehensive validation suite with quality metrics


### Data Flow
1. **Input**: OpenAPI YAML files from `assets/MnS-Rel-19-OpenAPI/OpenAPI/` (3GPP specifications)
2. **Processing**: Python converter parses YAML structures and converts to either:
   - **SHACL**: Separate RDF vocabulary (`*_rdf.ttl`) + SHACL validation shapes (`*_shacl.ttl`) (default)
   - **OWL**: Single OWL ontological model file (legacy)
3. **Output**: RDF Turtle files in `output/` directory with format-specific suffixes

### Key Conversion Rules

#### SHACL Approach (Default)
**RDF Vocabulary File (`*_rdf.ttl`)**:
- Object schemas → `rdfs:Class`
- Properties → `rdf:Property` with `cc:definedProperty` relationships
- Subclass relationships → `rdfs:subClassOf`
- Comments and descriptions → `rdfs:comment`

**SHACL Shapes File (`*_shacl.ttl`)**:
- Validation shapes → `sh:NodeShape` with `sh:targetClass`
- Property constraints → `sh:PropertyShape` with `sh:path`
- String constraints → `sh:pattern`, `sh:minLength`, `sh:maxLength`
- Numeric constraints → `sh:minInclusive`, `sh:maxInclusive`
- Required fields → `sh:minCount 1`
- Enumerations → `sh:in` with RDF lists
- Arrays → `dash:ListShape` with complex path expressions
- External references resolved via namespace prefixes
- Naming: dashes converted to underscores for RDF compatibility

#### OWL Approach (Legacy)
- Object schemas → `owl:Class`
- String enums → `owl:Class` with `owl:oneOf` 
- Object properties → `owl:ObjectProperty` or `owl:DatatypeProperty`
- Required fields → `owl:minCardinality 1`
- External references resolved via namespace prefixes
- Naming: dashes converted to underscores for RDF compatibility

## CLI Structure and Package Management

### Modern CLI Interface
The tool supports both new subcommand syntax and backward compatibility:
- **New syntax**: `openapi-to-rdf download|convert [options]` (preferred)
- **Legacy syntax**: `openapi-to-rdf <input> [--format shacl|owl]` (maintained for backward compatibility)
- **Console script**: Defined in `pyproject.toml` as `openapi-to-rdf = "main:main"`

### Package Dependencies (pyproject.toml)
- **Python**: ^3.11.6 (minimum version requirement)
- **Core dependencies**: rdflib ^7.1.3, PyYAML ^6.0.2, requests ^2.32.5
- **Build system**: Poetry with modern pyproject.toml configuration
- **Package structure**: `openapi_to_rdf/` module with proper `__init__.py`

## Development Notes

### Input Data Sources
1. **Local assets**: 3GPP specifications in `assets/MnS-Rel-19-OpenAPI/OpenAPI/`
2. **Remote download**: Automated retrieval via `ThreeGPPDownloader` with GitLab API, web scraping fallbacks
3. **Release support**: Rel-15 through Rel-19 and future releases with pattern-based discovery

### Multi-Method Download Strategy
The downloader implements robust retrieval with fallback mechanisms:
1. **GitLab API**: Primary method for structured file discovery
2. **Web scraping**: Secondary method for HTML page parsing
3. **Pattern matching**: Tertiary method for predictable URL structures
4. **Dry run mode**: Preview downloads without filesystem changes

### Testing Framework
Comprehensive test suite with automated quality metrics and validation:

**Test Components**:
- `test_completeness.py` - Schema, property, and description coverage analysis
- `test_semantic_correctness.py` - RDF semantic validation and consistency checks  
- `test_shacl_syntax.py` - SHACL syntax validation and constraint verification
- `run_tests.py` - Unified test runner with quality scoring

**Quality Metrics**:
- Schema Coverage: Percentage of OpenAPI schemas converted to RDF classes
- Property Coverage: Percentage of object properties converted to RDF properties
- Description Preservation: Percentage of descriptions preserved as `rdfs:comment`
- Overall Quality Score: Composite score with EXCELLENT/GOOD/FAIR/POOR ratings

**Quick test workflow:**
```bash
# Convert and test a single file
poetry run python main.py convert assets/MnS-Rel-19-OpenAPI/OpenAPI/TS28623_ComDefs.yaml
cd tests && poetry run python run_tests.py ../assets/MnS-Rel-19-OpenAPI/OpenAPI/TS28623_ComDefs.yaml

# Validate SHACL syntax only
poetry run python test_shacl_syntax.py ../output/TS28623_ComDefs_shacl.ttl
```

**Output validation patterns:**
- SHACL RDF files: `rdfs:Class`, `rdf:Property`, `cc:definedProperty`, proper namespace bindings
- SHACL shapes files: `sh:NodeShape`, `sh:PropertyShape`, `sh:targetClass`, constraint properties
- OWL files: `owl:Class`, `owl:ObjectProperty`, `owl:DatatypeProperty`, cardinality restrictions