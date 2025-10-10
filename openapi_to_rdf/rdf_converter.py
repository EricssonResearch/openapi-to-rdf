import os
import sys

import yaml
from rdflib import BNode, Graph, Literal, Namespace, URIRef
from rdflib.collection import Collection
from rdflib.namespace import OWL, RDF, RDFS, XSD


class OpenAPIToRDFConverter:
    def __init__(
        self, yaml_file, base_namespace, output_dir="output", external_refs=None
    ):
        """Initialize the converter with the YAML file, base namespace, output directory, and an optional list of external YAML reference files."""
        self.yaml_file = yaml_file
        self.base_namespace = base_namespace  # e.g., "https://forge.3gpp.org/rep/sa5/MnS/-/tree/Rel-19/OpenAPI"
        self.output_dir = output_dir
        self.external_refs = external_refs if external_refs is not None else []
        self.data = None
        self.graph = Graph()
        self.prefixes = {}  # Mapping from prefix string to Namespace object
        self._load_yaml()
        self._bind_standard_prefixes()
        self._bind_custom_namespaces()

    def _load_yaml(self):
        """Load the YAML file into a Python dictionary."""
        with open(self.yaml_file, "r", encoding="utf-8") as file:
            self.data = yaml.safe_load(file)

    def _bind_standard_prefixes(self):
        """Bind the standard RDF/OWL prefixes to the graph."""
        self.graph.bind("owl", OWL)
        self.graph.bind("rdfs", RDFS)
        self.graph.bind("rdf", RDF)
        self.graph.bind("xsd", XSD)

    def _bind_custom_namespaces(self):
        """
        Bind a custom namespace for the current YAML file and for any external references.
        The namespace URI is generated from the base_namespace and the filename.
        """
        # Main file namespace
        # filename = os.path.basename(self.yaml_file)
        # file_prefix = self.format_name(os.path.splitext(filename)[0])
        # ns_uri = self.base_namespace.rstrip("/") + "/" + filename + "#"
        # main_ns = Namespace(ns_uri)

        filename = os.path.basename(self.yaml_file)
        file_prefix = self.format_name(
            os.path.splitext(filename)[0]
        )  # Corrects filename format
        ns_uri = (
            self.base_namespace.rstrip("/") + "/" + file_prefix + "#"
        )  # Use file_prefix instead of filename
        main_ns = Namespace(ns_uri)

        self.prefixes[file_prefix] = main_ns
        self.graph.bind(file_prefix, main_ns)
        self.main_prefix = main_ns

        # Bind namespaces for external references, if any.
        for ext in self.external_refs:
            ext_filename = os.path.basename(ext)
            ext_prefix = self.format_name(os.path.splitext(ext_filename)[0])
            ext_ns_uri = self.base_namespace.rstrip("/") + "/" + ext_filename + "#"
            ext_ns = Namespace(ext_ns_uri)
            self.prefixes[ext_prefix] = ext_ns
            self.graph.bind(ext_prefix, ext_ns)

    def convert(self):
        """
        Convert the loaded YAML content into RDF Turtle.
        Handles OpenAPI document header conversion and schema definitions.
        """
        # Convert OpenAPI header (document metadata) if present.
        if isinstance(self.data, dict) and "openapi" in self.data:
            self._convert_openapi_header()

        # Convert components/schemas if available.
        if (
            isinstance(self.data, dict)
            and "components" in self.data
            and "schemas" in self.data["components"]
        ):
            self._parse_schemas(self.data["components"]["schemas"])

    def _convert_openapi_header(self):
        """Convert the OpenAPI header (info, externalDocs) into RDF classes and properties."""
        info = self.data.get("info", {})
        title = info.get("title", "Unknown")
        version = info.get("version", "Unknown")
        description = info.get("description", "")
        external_docs = self.data.get("externalDocs", {})

        # Create a class for the document metadata.
        class_name = self.format_name(title.replace(" ", ""))
        class_uri = self.main_prefix[class_name]
        self.graph.add((class_uri, RDF.type, OWL.Class))
        self.graph.add((class_uri, RDFS.label, Literal(title)))
        comment = f"Class representing the {title} document metadata."
        self.graph.add((class_uri, RDFS.comment, Literal(comment)))

        # Define the 'version' property as a functional property.
        prop_version = self.main_prefix["version"]
        self.graph.add((prop_version, RDF.type, OWL.FunctionalProperty))
        self.graph.add((prop_version, RDFS.domain, class_uri))
        self.graph.add((prop_version, RDFS.range, XSD.string))
        self.graph.add((prop_version, RDFS.label, Literal("Version")))
        self.graph.add(
            (prop_version, RDFS.comment, Literal("The version of the document."))
        )

        # Define the 'description' property.
        prop_description = self.main_prefix["description"]
        self.graph.add((prop_description, RDF.type, OWL.DatatypeProperty))
        self.graph.add((prop_description, RDFS.domain, class_uri))
        self.graph.add((prop_description, RDFS.range, XSD.string))
        self.graph.add((prop_description, RDFS.label, Literal("Description")))
        self.graph.add(
            (prop_description, RDFS.comment, Literal("A description of the document."))
        )

        # Define the 'url' property if externalDocs provides one.
        if "url" in external_docs:
            prop_url = self.main_prefix["url"]
            self.graph.add((prop_url, RDF.type, OWL.DatatypeProperty))
            self.graph.add((prop_url, RDFS.domain, class_uri))
            self.graph.add((prop_url, RDFS.range, XSD.string))
            self.graph.add((prop_url, RDFS.label, Literal("URL")))
            self.graph.add(
                (
                    prop_url,
                    RDFS.comment,
                    Literal(
                        "The URL for external documentation related to the document."
                    ),
                )
            )

        # Create an instance of the metadata class.
        instance_uri = self.main_prefix[class_name + "Instance"]
        self.graph.add((instance_uri, RDF.type, class_uri))
        self.graph.add((instance_uri, prop_version, Literal(version)))
        self.graph.add((instance_uri, prop_description, Literal(description)))
        if "url" in external_docs:
            self.graph.add((instance_uri, prop_url, Literal(external_docs["url"])))

    def _parse_schemas(self, schemas):
        """Parse each schema definition in the OpenAPI components."""
        for schema_name, schema_def in schemas.items():
            self._process_schema(schema_name, schema_def)

    def _process_schema(self, schema_name, schema_def):
        """
        Process an individual schema. Converts object definitions, enumerations,
        and union types (oneOf) into RDF classes with appropriate properties.
        """
        safe_name = self.format_name(schema_name)
        class_uri = self.main_prefix[safe_name]

        # Process object type schemas.
        if schema_def.get("type") == "object":
            self.graph.add((class_uri, RDF.type, OWL.Class))
            self.graph.add(
                (class_uri, RDFS.label, Literal(self.human_readable(safe_name)))
            )
            if "description" in schema_def:
                self.graph.add(
                    (class_uri, RDFS.comment, Literal(schema_def["description"]))
                )
            properties = schema_def.get("properties", {})
            required_props = schema_def.get("required", [])
            for prop, details in properties.items():
                self._process_property(class_uri, prop, details, required_props)

        # Process enumeration schemas.
        elif schema_def.get("type") == "string" and "enum" in schema_def:
            self.graph.add((class_uri, RDF.type, OWL.Class))
            self.graph.add(
                (class_uri, RDFS.label, Literal(self.human_readable(safe_name)))
            )
            self.graph.add(
                (
                    class_uri,
                    RDFS.comment,
                    Literal(f"Enumeration of {self.human_readable(safe_name)} values."),
                )
            )
            enum_values = schema_def["enum"]
            enum_individuals = []
            for enum_val in enum_values:
                ind_name = self.format_name(str(enum_val))
                ind_uri = self.main_prefix[ind_name]
                self.graph.add((ind_uri, RDF.type, OWL.NamedIndividual))
                self.graph.add((ind_uri, RDFS.label, Literal(str(enum_val))))
                self.graph.add(
                    (
                        ind_uri,
                        RDFS.comment,
                        Literal(
                            f"Indicates that the {self.human_readable(safe_name)} value is {enum_val}."
                        ),
                    )
                )
                enum_individuals.append(ind_uri)
            bnode = BNode()
            Collection(self.graph, bnode, enum_individuals)
            self.graph.add((class_uri, OWL.oneOf, bnode))

        # Process union types (oneOf).
        elif "oneOf" in schema_def:
            self.graph.add((class_uri, RDF.type, OWL.Class))
            self.graph.add(
                (class_uri, RDFS.label, Literal(self.human_readable(safe_name)))
            )
            self.graph.add(
                (
                    class_uri,
                    RDFS.comment,
                    Literal(
                        f"Represents a resource that can be one of several types related to {self.human_readable(safe_name)}."
                    ),
                )
            )
            oneof_items = []
            for option in schema_def["oneOf"]:
                if "$ref" in option:
                    ref_uri = self.resolve_reference(option["$ref"])
                    if ref_uri is not None:
                        oneof_items.append(ref_uri)
            if oneof_items:
                bnode = BNode()
                Collection(self.graph, bnode, oneof_items)
                self.graph.add((class_uri, OWL.oneOf, bnode))
        # Other types can be extended here as needed.

    def _process_property(self, domain_uri, prop_name, prop_def, required_list):
        """
        Process an individual property of an object schema.
        Determines the property type (object, datatype) and range,
        and applies cardinality constraints if the property is required.
        """
        safe_prop = self.format_name(prop_name)
        prop_uri = self.main_prefix[safe_prop]

        # Determine property type and range.
        if "$ref" in prop_def:
            ref_uri = self.resolve_reference(prop_def["$ref"])
            # Heuristic: if the referenced name implies a simple type, use a DatatypeProperty.
            if any(
                x in ref_uri.split("/")[-1].lower() for x in ["float", "int", "string"]
            ):
                prop_type = OWL.DatatypeProperty
            else:
                prop_type = OWL.ObjectProperty
            range_uri = ref_uri
        elif prop_def.get("type") == "string":
            prop_type = OWL.DatatypeProperty
            range_uri = XSD.string
        elif prop_def.get("type") == "integer":
            prop_type = OWL.DatatypeProperty
            range_uri = XSD.integer
        elif prop_def.get("type") == "number":
            prop_type = OWL.DatatypeProperty
            range_uri = XSD.double
        elif prop_def.get("type") == "boolean":
            prop_type = OWL.DatatypeProperty
            range_uri = XSD.boolean
        elif prop_def.get("type") == "object":
            # Inline object definition.
            self._process_schema(prop_name, prop_def)
            prop_type = OWL.ObjectProperty
            range_uri = self.main_prefix[self.format_name(prop_name)]
        elif prop_def.get("type") == "array":
            items = prop_def.get("items", {})
            if "$ref" in items:
                ref_uri = self.resolve_reference(items["$ref"])
                if any(
                    x in ref_uri.split("/")[-1].lower()
                    for x in ["float", "int", "string"]
                ):
                    prop_type = OWL.DatatypeProperty
                else:
                    prop_type = OWL.ObjectProperty
                range_uri = ref_uri
            elif "type" in items:
                prop_type = OWL.DatatypeProperty
                range_uri = self.map_xsd_type(items["type"])
            else:
                prop_type = OWL.DatatypeProperty
                range_uri = XSD.string
        else:
            prop_type = OWL.DatatypeProperty
            range_uri = XSD.string

        # Add property triple definitions.
        self.graph.add((prop_uri, RDF.type, prop_type))
        self.graph.add((prop_uri, RDFS.domain, domain_uri))
        self.graph.add((prop_uri, RDFS.range, range_uri))
        self.graph.add((prop_uri, RDFS.label, Literal(self.human_readable(safe_prop))))
        self.graph.add(
            (
                prop_uri,
                RDFS.comment,
                Literal(f"Property representing the {self.human_readable(safe_prop)}."),
            )
        )

        # Cardinality constraints: if the property is required, add a minimum cardinality.
        if required_list and prop_name in required_list:
            self.graph.add((prop_uri, OWL.minCardinality, Literal(1)))
            # For non-array properties, add a functional constraint or maximum cardinality.
            if prop_def.get("type") != "array":
                if prop_type == OWL.DatatypeProperty:
                    self.graph.add((prop_uri, RDF.type, OWL.FunctionalProperty))
                else:
                    self.graph.add((prop_uri, OWL.maxCardinality, Literal(1)))

    def resolve_reference(self, ref):
        """
        Resolve a $ref reference to an RDF URI.
        Handles internal references (starting with "#/components/schemas/")
        and external references (containing ".yaml#").
        """
        # Internal reference.
        if ref.startswith("#/components/schemas/"):
            ref_name = ref.split("/")[-1]
            return self.main_prefix[self.format_name(ref_name)]
        # External reference.
        elif ".yaml#" in ref:
            filename, remainder = ref.split("#/components/schemas/")
            ref_name = remainder
            ns = self.extract_namespace(filename)
            ext_prefix = self.format_name(
                os.path.splitext(os.path.basename(filename))[0]
            )
            if ext_prefix not in self.prefixes:
                ext_ns = Namespace(ns)
                self.prefixes[ext_prefix] = ext_ns
                self.graph.bind(ext_prefix, ext_ns)
            return self.prefixes[ext_prefix][self.format_name(ref_name)]
        else:
            return None

    def extract_namespace(self, yaml_file):
        """
        Generate a namespace URI from a YAML filename.
        For example, given "TS28623_ComDefs.yaml", it returns a namespace URI
        based on the base_namespace and the filename.
        """
        base = self.base_namespace.rstrip("/") + "/"
        filename = os.path.basename(yaml_file).replace(".yaml", "")
        return f"{base}{filename}#"

    def format_name(self, name):
        """
        Ensure names use underscores instead of dashes and remove file extensions if present.
        """
        name = os.path.splitext(name)[0]
        return name.replace("-", "_")

    def human_readable(self, name):
        """
        Convert a safe name into a human-readable label by replacing underscores with spaces.
        """
        return name.replace("_", " ")

    def map_xsd_type(self, yaml_type):
        """
        Map YAML primitive types to XSD datatypes.
        """
        type_map = {
            "string": XSD.string,
            "integer": XSD.integer,
            "number": XSD.double,
            "boolean": XSD.boolean,
        }
        return type_map.get(yaml_type, XSD.string)

    def save_rdf(self):
        """Serialize the RDF graph as Turtle and save it to the output directory."""
        output_filename = os.path.basename(self.yaml_file).replace(".yaml", ".ttl")
        output_path = os.path.join(self.output_dir, output_filename)
        os.makedirs(self.output_dir, exist_ok=True)
        self.graph.serialize(destination=output_path, format="turtle")
        print(f"✅ RDF file saved: {output_path}")

    def run(self):
        """Run the full conversion process."""
        self.convert()
        self.save_rdf()


# Example Usage
if __name__ == "__main__":
    yaml_files = [
        "assets/MnS-Rel-19-OpenAPI/OpenAPI/TS28541_SliceNrm.yaml"
    ]  # Replace with actual YAML files
    for yaml_file in yaml_files:
        converter = OpenAPIToRDFConverter(yaml_file, "http://teste#", external_refs=[])
        converter.run()
