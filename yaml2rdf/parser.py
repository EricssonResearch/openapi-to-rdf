import re

import yaml


class OpenAPIParser:
    """Reads an OpenAPI YAML file and parse its Header, references, and schemas."""

    def __init__(self):
        """Initialize with no specific file. The file can be loaded later."""
        self.data = {}
        self.header_part = {}
        self.schemas_part = {}
        self.references = []

    def load_yaml_file(self, yaml_file):
        """Load a new YAML file into the parser."""
        with open(yaml_file, "r", encoding="utf-8") as f:
            self.data = yaml.safe_load(f)
        self.header_part, self.schemas_part = self.__split_yaml_parts()

        return (
            self.header_part,
            self.schemas_part,
            self.__retrieve_external_references(),
        )

    def __split_yaml_parts(self):
        """Splits the loaded OpenAPI file into header part and schemas part."""
        for key, value in self.data.items():
            if key == "components" and "schemas" in value:
                self.schemas_part = value["schemas"]  # Extract only the 'schemas' part
            else:
                self.header_part[key] = value
        return self.header_part, self.schemas_part

    def schema_generator(self):
        """A generator to yield each first-level schema in the 'schemas' part."""
        for schema_name, schema_definition in self.schemas_part.items():
            yield schema_name, schema_definition

    def __retrieve_external_references(self):
        """Extracts external file references from the loaded OpenAPI YAML file."""
        _external_files = set()

        def find_refs(node):
            if isinstance(node, dict):
                for key, value in node.items():
                    if key == "$ref" and isinstance(value, str):
                        match = re.match(r"([^#]+)#", value)
                        if match:
                            _external_files.add(match.group(1))
                    else:
                        find_refs(value)
            elif isinstance(node, list):
                for item in node:
                    find_refs(item)

        find_refs(self.data)
        return list(_external_files)

    def display_header(self):
        """Displays the header part of the YAML file."""
        print("Header Part:")
        print(yaml.dump(self.header_part, default_flow_style=False))

    def display_schemas(self):
        """Displays the schemas part of the YAML file."""
        print("\nSchemas Part:")
        print(yaml.dump(self.schemas_part, default_flow_style=False))


from rich import print

if __name__ == "__main__":

    # Example usage
    parser = OpenAPIParser()

    # Load the first YAML file
    yaml_file_1 = "assets/MnS-Rel-19-OpenAPI/OpenAPI/TS28623_GenericNrm.yaml"
    header_par, schemas_part, references = parser.load_yaml_file(yaml_file_1)

    print(references)
