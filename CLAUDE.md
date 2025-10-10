# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Common Commands

### Python Development
- **Run SHACL converter (default)**: `python main.py <input_yaml_file_or_directory> [--base_namespace <namespace>]`
  - Generates separate `*_rdf.ttl` (vocabulary) and `*_shacl.ttl` (shapes) files
- **Run OWL converter**: `python main.py <input> --format owl [--base_namespace <namespace>]`
- **Install dependencies**: `poetry install` 
- **Run with Poetry**: `poetry run python main.py <input>`

### Docker Development  
- **Build image**: `docker build --build-arg USER=<USERNAME> --build-arg TOKEN=<TOKEN> -t knowledgebase .`
- **Run container**: `docker run -i -p 3020:3020 -t knowledgebase`

## Architecture Overview

This project converts OpenAPI YAML specifications to RDF formats with **two conversion approaches**:

### Python Component (`openapi_rdf_converter/`)
- **Entry point**: `main.py` - CLI interface supporting both SHACL and OWL formats
- **SHACL converter**: `openapi_rdf_converter/shacl_converter.py` - `OpenAPIToSHACLConverter` class (default, mimics Prolog approach)
- **OWL converter**: `openapi_rdf_converter/converter.py` - `OpenAPIToRDFConverter` class (legacy ontological approach)
- **Parser**: `openapi_rdf_converter/parser.py` - `OpenAPIParser` for parsing OpenAPI YAML files (removed)
- **AI prompts**: `openapi_rdf_converter/prompts.py` - LangChain prompts for AI-assisted conversion (removed)


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

## Development Notes

### Input Data
The project primarily works with 3GPP Management and Service (MnS) OpenAPI specifications located in `assets/MnS-Rel-19-OpenAPI/OpenAPI/`. These are telecommunications standard specifications that define network management interfaces.

### Dependencies
- **Python**: rdflib, PyYAML for RDF manipulation and YAML parsing  
- **Docker**: Containerized deployment option available

### Testing
No formal test suite is currently implemented. Manual testing is done by running conversions on sample YAML files and validating the generated RDF output.

**Quick test commands:**
```bash
# Test SHACL conversion (default)
poetry run python main.py assets/MnS-Rel-19-OpenAPI/OpenAPI/TS28623_ComDefs.yaml

# Test OWL conversion
poetry run python main.py assets/MnS-Rel-19-OpenAPI/OpenAPI/TS28623_ComDefs.yaml --format owl

# Test directory conversion
poetry run python main.py assets/MnS-Rel-19-OpenAPI/OpenAPI/
```

**Output validation:**
- SHACL RDF files: Look for `rdfs:Class`, `rdf:Property`, `cc:definedProperty` patterns
- SHACL shapes files: Look for `sh:NodeShape`, `sh:PropertyShape`, `sh:targetClass` patterns  
- OWL files: Look for `owl:Class`, `owl:ObjectProperty`, `owl:DatatypeProperty` patterns
- All files should have proper namespace bindings