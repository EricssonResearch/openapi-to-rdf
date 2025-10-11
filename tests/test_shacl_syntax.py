#!/usr/bin/env python3
"""
SHACL Syntax Validation Tests

This test suite verifies that generated SHACL files are syntactically sound
and don't contain invalid anonymous IDs or other syntax issues.
"""

import os
import re
from pathlib import Path
from rdflib import Graph
from rdflib.namespace import SH


class SHACLSyntaxValidator:
    """Validates SHACL syntax and structure."""
    
    def __init__(self, shacl_file):
        self.shacl_file = shacl_file
        self.graph = Graph()
        self.errors = []
        self.warnings = []
        
    def load_and_validate(self):
        """Load the SHACL file and run all validation checks."""
        try:
            self.graph.parse(self.shacl_file, format='turtle')
            self._check_anonymous_ids()
            self._check_blank_node_syntax()
            self._check_shacl_structure()
            self._check_namespace_usage()
            return len(self.errors) == 0
        except Exception as e:
            self.errors.append(f"Failed to parse SHACL file: {e}")
            return False
    
    def _check_anonymous_ids(self):
        """Check for invalid anonymous IDs (long hex strings)."""
        with open(self.shacl_file, 'r') as f:
            content = f.read()
            
        # Look for patterns like "N207cbc4fc6dd42b082d0954ddd76b103"
        anonymous_id_pattern = r'"N[a-f0-9]{32}"'
        matches = re.findall(anonymous_id_pattern, content)
        
        if matches:
            self.errors.append(f"Found {len(matches)} invalid anonymous IDs: {matches}")
    
    def _check_blank_node_syntax(self):
        """Check that blank nodes use proper syntax."""
        with open(self.shacl_file, 'r') as f:
            content = f.read()
            
        # Check for proper blank node syntax
        # Should see patterns like: [] a sh:NodeShape
        # Should NOT see patterns like: "N..." a sh:NodeShape
        
        # Look for quoted strings that look like blank node IDs
        quoted_bnode_pattern = r'"[N_][a-zA-Z0-9_]{20,}"'
        matches = re.findall(quoted_bnode_pattern, content)
        
        if matches:
            self.errors.append(f"Found {len(matches)} improperly quoted blank node IDs: {matches}")
    
    def _check_shacl_structure(self):
        """Check basic SHACL structure validity."""
        # Check for required SHACL namespaces
        shacl_namespace_found = False
        for prefix, namespace in self.graph.namespaces():
            if str(namespace) == str(SH):
                shacl_namespace_found = True
                break
        
        if not shacl_namespace_found:
            self.errors.append("SHACL namespace not found")
        
        # Check for NodeShapes and PropertyShapes
        node_shapes = list(self.graph.subjects(SH.targetClass))
        if not node_shapes:
            self.warnings.append("No NodeShapes with targetClass found")
        
        # Check for valid SHACL properties
        valid_shacl_props = {
            SH.targetClass, SH.targetNode, SH.targetSubjectsOf, SH.targetObjectsOf,
            SH.property, SH.path, SH.datatype, getattr(SH, 'class'), SH.node, SH.minCount,
            SH.maxCount, SH.minLength, SH.maxLength, SH.pattern, SH.minInclusive,
            SH.maxInclusive, getattr(SH, 'in'), getattr(SH, 'or'), SH.xone, getattr(SH, 'and'),
            SH.zeroOrMorePath, SH.zeroOrOnePath, SH.oneOrMorePath, SH.alternativePath,
            SH.inversePath, SH.hasValue, SH.equals, SH.disjoint, SH.lessThan, SH.lessThanOrEquals
        }
        
        invalid_props = set()
        for s, p, o in self.graph:
            if str(p).startswith(str(SH)) and p not in valid_shacl_props:
                # Check if it's a valid SHACL property we might have missed
                prop_name = str(p).split('#')[-1]
                if not any(prop_name in str(valid_prop) for valid_prop in valid_shacl_props):
                    invalid_props.add(str(p))
        
        if invalid_props:
            self.warnings.append(f"Potentially invalid SHACL properties: {invalid_props}")
    
    def _check_namespace_usage(self):
        """Check that namespaces are properly declared and used."""
        # Get all prefixes used in the file
        used_prefixes = set()
        for s, p, o in self.graph:
            if hasattr(s, 'prefix') and s.prefix:
                used_prefixes.add(s.prefix)
            if hasattr(p, 'prefix') and p.prefix:
                used_prefixes.add(p.prefix)
            if hasattr(o, 'prefix') and o.prefix:
                used_prefixes.add(o.prefix)
        
        # Check that all used prefixes are declared
        declared_prefixes = set()
        for prefix, namespace in self.graph.namespaces():
            declared_prefixes.add(prefix)
        
        undeclared_prefixes = used_prefixes - declared_prefixes
        if undeclared_prefixes:
            self.errors.append(f"Undeclared prefixes used: {undeclared_prefixes}")
    
    def generate_report(self):
        """Generate a validation report."""
        report = f"""
# SHACL Syntax Validation Report

## File: {self.shacl_file}

## Validation Results
- **Status**: {'✅ PASSED' if len(self.errors) == 0 else '❌ FAILED'}
- **Errors**: {len(self.errors)}
- **Warnings**: {len(self.warnings)}

## Errors
"""
        for error in self.errors:
            report += f"- ❌ {error}\n"
        
        if not self.errors:
            report += "- None\n"
        
        report += "\n## Warnings\n"
        for warning in self.warnings:
            report += f"- ⚠️ {warning}\n"
        
        if not self.warnings:
            report += "- None\n"
        
        # Add statistics
        report += f"""
## Statistics
- **Total triples**: {len(self.graph)}
- **NodeShapes**: {len(list(self.graph.subjects(SH.targetClass)))}
- **PropertyShapes**: {len(list(self.graph.subjects(SH.path)))}
- **Namespaces**: {len(list(self.graph.namespaces()))}
"""
        
        return report


def test_shacl_syntax():
    """Test SHACL syntax for all generated files."""
    output_dir = Path("output")
    shacl_files = list(output_dir.glob("*_shacl.ttl"))
    
    if not shacl_files:
        print("No SHACL files found in output directory")
        return
    
    print(f"Testing {len(shacl_files)} SHACL files...")
    
    all_passed = True
    for shacl_file in shacl_files:
        print(f"\n{'='*60}")
        print(f"Testing: {shacl_file.name}")
        print('='*60)
        
        validator = SHACLSyntaxValidator(shacl_file)
        passed = validator.load_and_validate()
        
        if not passed:
            all_passed = False
        
        print(validator.generate_report())
    
    print(f"\n{'='*60}")
    print(f"Overall Result: {'✅ ALL TESTS PASSED' if all_passed else '❌ SOME TESTS FAILED'}")
    print('='*60)
    
    return all_passed


def test_specific_file(shacl_file):
    """Test a specific SHACL file."""
    validator = SHACLSyntaxValidator(shacl_file)
    passed = validator.load_and_validate()
    print(validator.generate_report())
    return passed


if __name__ == "__main__":
    import sys
    
    if len(sys.argv) > 1:
        # Test specific file
        test_specific_file(sys.argv[1])
    else:
        # Test all files
        test_shacl_syntax()
