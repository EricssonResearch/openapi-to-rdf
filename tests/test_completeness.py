#!/usr/bin/env python3
"""
Completeness tests to ensure all OpenAPI schemas are converted to RDF/SHACL.
"""

import yaml
import re
from rdflib import Graph
from pathlib import Path

class CompletenessValidator:
    """Validates that all OpenAPI schemas are represented in RDF output."""
    
    def __init__(self, yaml_file, rdf_file, shacl_file):
        self.yaml_file = yaml_file
        self.rdf_file = rdf_file  
        self.shacl_file = shacl_file
        
        # Load YAML
        with open(yaml_file, 'r') as f:
            self.yaml_data = yaml.safe_load(f)
            
        # Load RDF graphs
        self.rdf_graph = Graph()
        self.rdf_graph.parse(rdf_file, format='turtle')
        
        self.shacl_graph = Graph()
        self.shacl_graph.parse(shacl_file, format='turtle')
    
    def get_yaml_schemas(self):
        """Extract all schema names from OpenAPI YAML."""
        schemas = set()
        if 'components' in self.yaml_data and 'schemas' in self.yaml_data['components']:
            schemas = set(self.yaml_data['components']['schemas'].keys())
        return schemas
    
    def get_rdf_classes(self):
        """Extract all class names from RDF graph."""
        classes = set()
        query = """
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        SELECT ?class WHERE {
            ?class a rdfs:Class .
        }
        """
        for row in self.rdf_graph.query(query):
            class_name = str(row[0]).split('#')[-1]
            classes.add(class_name)
        return classes
    
    def get_yaml_properties(self):
        """Extract all property names from OpenAPI schemas."""
        properties = set()
        schemas = self.yaml_data.get('components', {}).get('schemas', {})
        
        for schema_name, schema_def in schemas.items():
            if isinstance(schema_def, dict) and 'properties' in schema_def:
                for prop_name in schema_def['properties'].keys():
                    properties.add(prop_name)
        return properties
    
    def get_rdf_properties(self):
        """Extract all property names from RDF graph."""
        properties = set()
        query = """
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT ?prop WHERE {
            ?prop a rdf:Property .
        }
        """
        for row in self.rdf_graph.query(query):
            prop_name = str(row[0]).split('#')[-1]
            properties.add(prop_name)
        return properties
    
    def check_schema_completeness(self):
        """Check if all YAML schemas are represented as RDF classes."""
        yaml_schemas = self.get_yaml_schemas()
        rdf_classes = self.get_rdf_classes()
        
        # Convert to comparable names (handle naming conventions)
        def normalize_name(name):
            return name.replace('-', '_')
        
        yaml_normalized = {normalize_name(s) for s in yaml_schemas}
        rdf_normalized = {normalize_name(c) for c in rdf_classes}
        
        missing_in_rdf = yaml_normalized - rdf_normalized
        extra_in_rdf = rdf_normalized - yaml_normalized
        
        return {
            'missing_schemas': missing_in_rdf,
            'extra_classes': extra_in_rdf,
            'total_yaml_schemas': len(yaml_schemas),
            'total_rdf_classes': len(rdf_classes),
            'coverage_percentage': (len(yaml_normalized & rdf_normalized) / len(yaml_normalized)) * 100 if yaml_normalized else 0
        }
    
    def check_property_completeness(self):
        """Check if all YAML properties are represented as RDF properties."""
        yaml_properties = self.get_yaml_properties()
        rdf_properties = self.get_rdf_properties()
        
        def normalize_name(name):
            return name.replace('-', '_')
        
        yaml_normalized = {normalize_name(p) for p in yaml_properties}
        rdf_normalized = {normalize_name(p) for p in rdf_properties}
        
        missing_in_rdf = yaml_normalized - rdf_normalized
        extra_in_rdf = rdf_normalized - yaml_normalized
        
        return {
            'missing_properties': missing_in_rdf,
            'extra_properties': extra_in_rdf,
            'total_yaml_properties': len(yaml_properties),
            'total_rdf_properties': len(rdf_properties),
            'coverage_percentage': (len(yaml_normalized & rdf_normalized) / len(yaml_normalized)) * 100 if yaml_normalized else 0
        }
    
    def check_descriptions_preserved(self):
        """Check if descriptions are preserved as rdfs:comment."""
        schemas_with_descriptions = 0
        descriptions_preserved = 0
        
        schemas = self.yaml_data.get('components', {}).get('schemas', {})
        
        for schema_name, schema_def in schemas.items():
            if isinstance(schema_def, dict) and 'description' in schema_def:
                schemas_with_descriptions += 1
                
                # Check if rdfs:comment exists for this class
                class_name = schema_name.replace('-', '_')
                query = f"""
                PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                ASK {{
                    ?class rdfs:comment ?comment .
                    FILTER(STRENDS(STR(?class), "#{class_name}"))
                }}
                """
                if self.rdf_graph.query(query).askAnswer:
                    descriptions_preserved += 1
        
        return {
            'schemas_with_descriptions': schemas_with_descriptions,
            'descriptions_preserved': descriptions_preserved,
            'preservation_percentage': (descriptions_preserved / schemas_with_descriptions) * 100 if schemas_with_descriptions else 100
        }
    
    def generate_report(self):
        """Generate comprehensive completeness report."""
        schema_check = self.check_schema_completeness()
        property_check = self.check_property_completeness()
        description_check = self.check_descriptions_preserved()
        
        report = f"""
# OpenAPI to RDF/SHACL Conversion Completeness Report

## Schema Coverage
- Total YAML schemas: {schema_check['total_yaml_schemas']}
- Total RDF classes: {schema_check['total_rdf_classes']}
- Coverage: {schema_check['coverage_percentage']:.1f}%

### Missing schemas in RDF:
{chr(10).join(f"- {s}" for s in sorted(schema_check['missing_schemas']))}

### Extra classes in RDF:
{chr(10).join(f"- {c}" for c in sorted(schema_check['extra_classes']))}

## Property Coverage  
- Total YAML properties: {property_check['total_yaml_properties']}
- Total RDF properties: {property_check['total_rdf_properties']}
- Coverage: {property_check['coverage_percentage']:.1f}%

### Missing properties in RDF:
{chr(10).join(f"- {p}" for p in sorted(property_check['missing_properties']))}

## Description Preservation
- Schemas with descriptions: {description_check['schemas_with_descriptions']}
- Descriptions preserved: {description_check['descriptions_preserved']}
- Preservation rate: {description_check['preservation_percentage']:.1f}%
"""
        return report

if __name__ == "__main__":
    import sys
    if len(sys.argv) != 4:
        print("Usage: python test_completeness.py <yaml_file> <rdf_file> <shacl_file>")
        sys.exit(1)
    
    validator = CompletenessValidator(sys.argv[1], sys.argv[2], sys.argv[3])
    print(validator.generate_report())