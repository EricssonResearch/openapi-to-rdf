import argparse
import logging
import os
import sys
from pathlib import Path

from openapi_to_rdf.shacl_converter import OpenAPIToSHACLConverter
from openapi_to_rdf.rdf_converter import OpenAPIToRDFConverter


def convert_files(args):
    """Convert OpenAPI YAML files to RDF/SHACL."""
    input_paths = args.input
    base_namespace = args.base_namespace
    output_format = args.format
    namespace_prefix = args.namespace_prefix

    # Collect all YAML files from all input paths
    yaml_files = []
    
    for input_path in input_paths:
        # Validate input path exists
        if not os.path.exists(input_path):
            logging.error(f"Input path does not exist: {input_path}")
            sys.exit(1)

        if os.path.isdir(input_path):
            # Find all YAML files in directory and subdirectories
            dir_yaml_files = list(Path(input_path).rglob("*.yaml"))
            dir_yaml_files = [str(f) for f in dir_yaml_files]
            yaml_files.extend(dir_yaml_files)
            print(f"Found {len(dir_yaml_files)} YAML files in {input_path}")
        elif os.path.isfile(input_path) and input_path.endswith(".yaml"):
            yaml_files.append(input_path)
            print(f"Processing file: {input_path}")
        else:
            logging.error(
                f"Invalid input: {input_path} must be a YAML file or a directory containing YAML files."
            )
            sys.exit(1)

    if not yaml_files:
        logging.error("No YAML files found to process.")
        sys.exit(1)

    # Remove duplicates while preserving order
    yaml_files = list(dict.fromkeys(yaml_files))

    # Process each YAML file
    successful_conversions = 0
    failed_conversions = 0
    
    for i, yaml_file in enumerate(yaml_files, 1):
        print(f"[{i}/{len(yaml_files)}] Converting {yaml_file} to {output_format.upper()}...")
        
        try:
            if output_format == "shacl":
                converter = OpenAPIToSHACLConverter(
                    yaml_file, 
                    base_namespace=base_namespace, 
                    external_refs=[],
                    base_namespace_prefix=namespace_prefix
                )
            else:  # owl
                # Fallback to original namespace format for OWL if not provided
                if base_namespace is None:
                    base_namespace = f"{namespace_prefix}rdf/"
                converter = OpenAPIToRDFConverter(yaml_file, base_namespace, external_refs=[])
            
            converter.run()
            successful_conversions += 1
            print(f"✓ Successfully converted {yaml_file}")
            
        except Exception as e:
            failed_conversions += 1
            print(f"✗ Failed to convert {yaml_file}: {str(e)}")
            logging.error(f"Conversion failed for {yaml_file}: {str(e)}")
    
    # Summary
    print(f"\nConversion Summary:")
    print(f"  Total files processed: {len(yaml_files)}")
    print(f"  Successful conversions: {successful_conversions}")
    print(f"  Failed conversions: {failed_conversions}")
    
    if failed_conversions > 0:
        sys.exit(1)


def main():
    """Main entry point for the openapi-to-rdf CLI."""
    parser = argparse.ArgumentParser(
        description="Convert OpenAPI YAML specifications to RDF vocabularies and SHACL validation shapes",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Convert single file
  openapi-to-rdf openapi-spec.yaml
  
  # Convert multiple files
  openapi-to-rdf file1.yaml file2.yaml file3.yaml
  
  # Convert all YAML files in a directory
  openapi-to-rdf /path/to/openapi/specs/
  
  # Convert with custom namespace prefix
  openapi-to-rdf openapi-spec.yaml --namespace-prefix "https://myorg.com/models/"
  
  # Convert to OWL format instead of SHACL
  openapi-to-rdf openapi-spec.yaml --format owl

Common OpenAPI sources:
  - 3GPP specifications: https://forge.3gpp.org/rep/sa5/MnS/-/tree/Rel-18/OpenAPI
  - TMForum APIs: https://github.com/tmforum-oda
        """
    )
    
    parser.add_argument(
        "input", 
        nargs="+",
        help="Path(s) to YAML file(s) or directory(ies) containing YAML files"
    )
    parser.add_argument(
        "--base-namespace",
        default=None,
        help="Base namespace for RDF output (auto-generated if not provided)",
    )
    parser.add_argument(
        "--format",
        choices=["shacl", "owl"],
        default="shacl",
        help="Output format: 'shacl' for separate RDF vocabulary + SHACL shapes (default), 'owl' for RDF/OWL"
    )
    parser.add_argument(
        "--namespace-prefix",
        default="http://ericsson.com/models/3gpp/",
        help="Base namespace prefix for generated URIs (default: http://ericsson.com/models/3gpp/)"
    )
    parser.add_argument(
        "--version",
        action="version",
        version="%(prog)s 0.1.0"
    )
    
    args = parser.parse_args()
    convert_files(args)


if __name__ == "__main__":
    main()
