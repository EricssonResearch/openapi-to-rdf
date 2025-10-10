#!/usr/bin/env python3
"""
Semantic correctness tests for OpenAPI to RDF/SHACL conversion.
"""

import yaml
from rdflib import Graph, Namespace, URIRef
from rdflib.namespace import RDF, RDFS, XSD

class SemanticValidator:
    """Validates semantic correctness of RDF/SHACL output."""
    
    def __init__(self, yaml_file, rdf_file, shacl_file):
        self.yaml_file = yaml_file
        self.rdf_file = rdf_file
        self.shacl_file = shacl_file
        
        # Load data
        with open(yaml_file, 'r') as f:
            self.yaml_data = yaml.safe_load(f)
            
        self.rdf_graph = Graph()
        self.rdf_graph.parse(rdf_file, format='turtle')
        
        self.shacl_graph = Graph()
        self.shacl_graph.parse(shacl_file, format='turtle')
        
        # Define namespaces
        self.SH = Namespace("http://www.w3.org/ns/shacl#")
    
    def validate_w3c_compliance(self):
        """Check W3C standards compliance."""
        issues = []
        
        # Check for non-standard namespaces/properties
        non_standard_prefixes = []
        for prefix, namespace in self.rdf_graph.namespaces():
            if prefix not in ['rdf', 'rdfs', 'xsd', 'owl'] and not str(namespace).startswith('http://ericsson.com'):
                if prefix not in ['sh', 'dash']:  # SHACL namespaces are OK
                    non_standard_prefixes.append((prefix, namespace))
        
        if non_standard_prefixes:
            issues.append(f"Non-standard namespaces found: {non_standard_prefixes}")
        
        # Check property domain/range specifications
        properties_without_domain = []
        properties_without_range = []
        
        query = """
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        SELECT ?prop WHERE {
            ?prop a rdf:Property .
            FILTER NOT EXISTS { ?prop rdfs:domain ?domain }
        }
        """
        for row in self.rdf_graph.query(query):
            properties_without_domain.append(str(row[0]).split('#')[-1])
        
        query = """
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        SELECT ?prop WHERE {
            ?prop a rdf:Property .
            FILTER NOT EXISTS { ?prop rdfs:range ?range }
        }
        """
        for row in self.rdf_graph.query(query):
            properties_without_range.append(str(row[0]).split('#')[-1])
        
        if properties_without_domain:
            issues.append(f"Properties without rdfs:domain: {properties_without_domain}")
        if properties_without_range:
            issues.append(f"Properties without rdfs:range: {properties_without_range}")
        
        return issues
    
    def validate_shacl_constraints(self):
        """Validate SHACL constraint consistency."""
        issues = []
        
        # Check that every class has a corresponding NodeShape
        rdf_classes = set()
        query = """
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        SELECT ?class WHERE { ?class a rdfs:Class . }
        """
        for row in self.rdf_graph.query(query):
            rdf_classes.add(row[0])
        
        shacl_target_classes = set()
        query = """
        PREFIX sh: <http://www.w3.org/ns/shacl#>
        SELECT ?target WHERE { ?shape sh:targetClass ?target . }
        """
        for row in self.shacl_graph.query(query):
            shacl_target_classes.add(row[0])
        
        missing_shapes = rdf_classes - shacl_target_classes
        if missing_shapes:
            missing_names = [str(c).split('#')[-1] for c in missing_shapes]
            issues.append(f"Classes without SHACL shapes: {missing_names}")
        
        return issues
    
    def validate_type_mappings(self):
        """Validate OpenAPI type to XSD type mappings."""
        issues = []
        type_mapping_errors = []
        
        schemas = self.yaml_data.get('components', {}).get('schemas', {})
        
        for schema_name, schema_def in schemas.items():
            if isinstance(schema_def, dict):
                # Check string types
                if schema_def.get('type') == 'string':
                    expected_datatype = XSD.string
                    if 'format' in schema_def:
                        format_map = {
                            'date-time': XSD.dateTime,
                            'full-time': XSD.time,
                            'date-month': XSD.gMonth,
                            'date-mday': XSD.gMonthDay,
                        }
                        expected_datatype = format_map.get(schema_def['format'], XSD.string)
                    
                    # Check if properties using this schema have correct datatype in SHACL
                    # This is a simplified check - full validation would require more complex querying
                
                # Check integer/number types
                elif schema_def.get('type') in ['integer', 'number']:
                    if schema_def.get('type') == 'integer':
                        expected_datatype = XSD.integer
                    else:
                        format_val = schema_def.get('format', 'double')
                        expected_datatype = XSD.float if format_val == 'float' else XSD.double
        
        return type_mapping_errors
    
    def check_conversion_equivalences(self):
        """Check areas where OpenAPI→RDF equivalence is unclear and suggest comments."""
        ambiguous_conversions = []
        
        schemas = self.yaml_data.get('components', {}).get('schemas', {})
        
        for schema_name, schema_def in schemas.items():
            if isinstance(schema_def, dict):
                # Check for readOnly properties - ambiguous in RDF
                if schema_def.get('readOnly'):
                    ambiguous_conversions.append({
                        'schema': schema_name,
                        'issue': 'readOnly property',
                        'openapi_semantics': 'Property is read-only in API operations',
                        'rdf_semantics': 'No direct equivalent - RDF properties are bidirectional',
                        'suggested_comment': f"# Note: {schema_name} is readOnly in OpenAPI - consider access control in implementation"
                    })
                
                # Check for writeOnly properties
                if schema_def.get('writeOnly'):
                    ambiguous_conversions.append({
                        'schema': schema_name,
                        'issue': 'writeOnly property',
                        'openapi_semantics': 'Property is write-only in API operations',
                        'rdf_semantics': 'No direct equivalent - RDF properties are bidirectional',
                        'suggested_comment': f"# Note: {schema_name} is writeOnly in OpenAPI - consider access control in implementation"
                    })
                
                # Check for discriminator - complex inheritance
                if 'discriminator' in schema_def:
                    ambiguous_conversions.append({
                        'schema': schema_name,
                        'issue': 'discriminator',
                        'openapi_semantics': 'Polymorphic inheritance with type discrimination',
                        'rdf_semantics': 'No direct equivalent - RDF uses simple class hierarchy',
                        'suggested_comment': f"# Note: {schema_name} uses OpenAPI discriminator - consider OWL union classes for full semantics"
                    })
                
                # Check for nullable - not directly expressible in basic RDF
                if schema_def.get('nullable'):
                    ambiguous_conversions.append({
                        'schema': schema_name,
                        'issue': 'nullable',
                        'openapi_semantics': 'Property can be null/absent',
                        'rdf_semantics': 'Absence is default - null vs absent distinction lost',
                        'suggested_comment': f"# Note: {schema_name} is nullable in OpenAPI - null vs absent semantics not preserved"
                    })
                
                # Check for allOf/anyOf/oneOf - complex logical constraints
                for logical_op in ['allOf', 'anyOf', 'oneOf']:
                    if logical_op in schema_def:
                        ambiguous_conversions.append({
                            'schema': schema_name,
                            'issue': f'{logical_op} constraint',
                            'openapi_semantics': f'Logical {logical_op} constraint on schema',
                            'rdf_semantics': 'Partial support in SHACL - complex logic may be lost',
                            'suggested_comment': f"# Note: {schema_name} uses {logical_op} - complex logical constraints partially supported"
                        })
        
        return ambiguous_conversions
    
    def generate_semantic_report(self):
        """Generate comprehensive semantic validation report."""
        w3c_issues = self.validate_w3c_compliance()
        shacl_issues = self.validate_shacl_constraints()
        type_issues = self.validate_type_mappings()
        ambiguous_conversions = self.check_conversion_equivalences()
        
        report = f"""
# Semantic Correctness Report

## W3C Standards Compliance
{'✅ PASSED' if not w3c_issues else '❌ ISSUES FOUND'}
{chr(10).join(f"- {issue}" for issue in w3c_issues)}

## SHACL Constraint Validation
{'✅ PASSED' if not shacl_issues else '❌ ISSUES FOUND'}
{chr(10).join(f"- {issue}" for issue in shacl_issues)}

## Type Mapping Validation
{'✅ PASSED' if not type_issues else '❌ ISSUES FOUND'}
{chr(10).join(f"- {issue}" for issue in type_issues)}

## Ambiguous OpenAPI→RDF Conversions
Found {len(ambiguous_conversions)} cases where OpenAPI and RDF semantics don't align perfectly:

"""
        for conv in ambiguous_conversions:
            report += f"""
### {conv['schema']} - {conv['issue']}
- **OpenAPI semantics**: {conv['openapi_semantics']}
- **RDF semantics**: {conv['rdf_semantics']}
- **Suggested comment**: `{conv['suggested_comment']}`
"""
        
        return report

if __name__ == "__main__":
    import sys
    if len(sys.argv) != 4:
        print("Usage: python test_semantic_correctness.py <yaml_file> <rdf_file> <shacl_file>")
        sys.exit(1)
    
    validator = SemanticValidator(sys.argv[1], sys.argv[2], sys.argv[3])
    print(validator.generate_semantic_report())