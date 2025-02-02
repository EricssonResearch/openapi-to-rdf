import argparse
import os

from yaml2rdf.converter import OpenAPIToRDFConverter

if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="Convert OpenAPI YAML to RDF Turtle.")
    parser.add_argument(
        "input", help="Path to a YAML file or a directory containing YAML files."
    )
    parser.add_argument(
        "--base_namespace",
        default="http://ericsson.com/models/3gpp/rdf/",
        help="Base namespace for RDF output",
    )
    args = parser.parse_args()

    input_path = args.input
    base_namespace = args.base_namespace

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
        converter = OpenAPIToRDFConverter(yaml_file, base_namespace, external_refs=[])
        converter.run()
