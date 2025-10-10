import argparse
import logging
import os
import sys

from openapi_rdf_converter.shacl_converter import OpenAPIToSHACLConverter
from openapi_rdf_converter.converter import OpenAPIToRDFConverter

if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="Convert OpenAPI YAML to separate RDF vocabulary + SHACL shapes (default) or RDF/OWL.")
    parser.add_argument(
        "input", help="Path to a YAML file or a directory containing YAML files."
    )
    parser.add_argument(
        "--base_namespace",
        default=None,
        help="Base namespace for RDF output (auto-generated if not provided)",
    )
    parser.add_argument(
        "--format",
        choices=["shacl", "owl"],
        default="shacl",
        help="Output format: 'shacl' for separate RDF vocabulary + SHACL shapes (default), 'owl' for RDF/OWL"
    )
    args = parser.parse_args()

    input_path = args.input
    base_namespace = args.base_namespace
    output_format = args.format

    yaml_files = []
    if os.path.isdir(input_path):
        yaml_files = [
            os.path.join(input_path, f)
            for f in os.listdir(input_path)
            if f.endswith(".yaml")
        ]
    elif os.path.isfile(input_path) and input_path.endswith(".yaml"):
        yaml_files.append(input_path)
    else:
        logging.error(
            "Invalid input: must be a YAML file or a directory containing YAML files."
        )
        sys.exit(1)

    for yaml_file in yaml_files:
        print(f"Converting {yaml_file} to {output_format.upper()}...")
        
        if output_format == "shacl":
            converter = OpenAPIToSHACLConverter(
                yaml_file, 
                base_namespace=base_namespace, 
                external_refs=[]
            )
        else:  # owl
            # Fallback to original namespace format for OWL if not provided
            if base_namespace is None:
                base_namespace = "http://ericsson.com/models/3gpp/rdf/"
            converter = OpenAPIToRDFConverter(yaml_file, base_namespace, external_refs=[])
        
        converter.run()
