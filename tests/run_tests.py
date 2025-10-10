#!/usr/bin/env python3
"""
Comprehensive test runner for OpenAPI to RDF/SHACL conversion.
"""

import os
import sys
import subprocess
from pathlib import Path

# Add parent directory to path to import modules
sys.path.insert(0, str(Path(__file__).parent.parent))

from test_completeness import CompletenessValidator
from test_semantic_correctness import SemanticValidator

def run_conversion_tests(yaml_file):
    """Run comprehensive tests on a YAML file conversion."""
    
    # Get output file paths
    base_name = Path(yaml_file).stem
    output_dir = Path(__file__).parent.parent / "output"
    rdf_file = output_dir / f"{base_name}_rdf.ttl"
    shacl_file = output_dir / f"{base_name}_shacl.ttl"
    
    print(f"🧪 Testing conversion of {yaml_file}")
    print(f"📊 RDF output: {rdf_file}")
    print(f"📊 SHACL output: {shacl_file}")
    print("=" * 60)
    
    # Check if output files exist
    if not rdf_file.exists() or not shacl_file.exists():
        print("❌ Output files not found. Run conversion first:")
        print(f"   poetry run python main.py {yaml_file} --format shacl")
        return False
    
    # Run completeness tests
    print("\n📋 COMPLETENESS TESTS")
    print("-" * 30)
    completeness_validator = CompletenessValidator(yaml_file, rdf_file, shacl_file)
    completeness_report = completeness_validator.generate_report()
    print(completeness_report)
    
    # Run semantic correctness tests
    print("\n🔍 SEMANTIC CORRECTNESS TESTS") 
    print("-" * 30)
    semantic_validator = SemanticValidator(yaml_file, rdf_file, shacl_file)
    semantic_report = semantic_validator.generate_semantic_report()
    print(semantic_report)
    
    # Generate summary scores
    schema_check = completeness_validator.check_schema_completeness()
    property_check = completeness_validator.check_property_completeness()
    description_check = completeness_validator.check_descriptions_preserved()
    
    print("\n📊 SUMMARY SCORES")
    print("-" * 30)
    print(f"Schema Coverage: {schema_check['coverage_percentage']:.1f}%")
    print(f"Property Coverage: {property_check['coverage_percentage']:.1f}%") 
    print(f"Description Preservation: {description_check['preservation_percentage']:.1f}%")
    
    # Overall assessment
    overall_score = (
        schema_check['coverage_percentage'] + 
        property_check['coverage_percentage'] + 
        description_check['preservation_percentage']
    ) / 3
    
    print(f"\n🎯 OVERALL QUALITY SCORE: {overall_score:.1f}%")
    
    if overall_score >= 95:
        print("✅ EXCELLENT - Conversion quality is excellent")
    elif overall_score >= 85:
        print("✅ GOOD - Conversion quality is good") 
    elif overall_score >= 70:
        print("⚠️  FAIR - Conversion quality needs improvement")
    else:
        print("❌ POOR - Conversion quality is poor")
    
    return True

def main():
    """Main test runner."""
    if len(sys.argv) < 2:
        print("Usage: python run_tests.py <yaml_file1> [yaml_file2] ...")
        print("\nExample:")
        print("  python run_tests.py assets/MnS-Rel-19-OpenAPI/OpenAPI/TS28623_ComDefs.yaml")
        print("  python run_tests.py assets/MnS-Rel-19-OpenAPI/OpenAPI/*.yaml")
        sys.exit(1)
    
    yaml_files = sys.argv[1:]
    
    print("🚀 OpenAPI to RDF/SHACL Conversion Test Suite")
    print("=" * 60)
    
    for yaml_file in yaml_files:
        if not Path(yaml_file).exists():
            print(f"❌ File not found: {yaml_file}")
            continue
        
        success = run_conversion_tests(yaml_file)
        if not success:
            continue
        
        print("\n" + "=" * 60 + "\n")

if __name__ == "__main__":
    main()