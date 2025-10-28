import os
import re
import yaml
from rdflib import BNode, Graph, Literal, Namespace, URIRef
from rdflib.collection import Collection
from rdflib.namespace import RDF, RDFS, XSD


class OpenAPIToSHACLConverter:
    """Converts OpenAPI YAML to RDF/RDFS + SHACL, mimicking the Prolog implementation."""
    
    def __init__(self, yaml_file, base_namespace=None, output_dir="output", external_refs=None, base_namespace_prefix="http://ericsson.com/models/3gpp/"):
        """Initialize the converter with SHACL-based approach."""
        self.yaml_file = yaml_file
        self.base_namespace_prefix = base_namespace_prefix
        self.base_namespace = base_namespace or self._generate_base_namespace()
        self.output_dir = output_dir
        self.external_refs = external_refs if external_refs is not None else []
        self.data = None
        
        # Separate graphs for RDF vocabulary and SHACL shapes
        self.rdf_graph = Graph()
        self.shacl_graph = Graph()
        self.prefixes = {}  # Mapping from prefix string to Namespace object
        
        self._load_yaml()
        self._bind_standard_prefixes()
        self._bind_custom_namespaces()

    def _generate_base_namespace(self):
        """Generate namespace from filename using configurable prefix: TS28xxx_Name -> {prefix}TSxxx/Name#"""
        filename = os.path.basename(self.yaml_file)
        name_without_ext = os.path.splitext(filename)[0]
        
        # Match pattern like TS28623_ComDefs
        match = re.match(r"(?P<num>TS\d*)_(?P<name>.*)", name_without_ext)
        if match:
            num_part = match.group('num')
            name_part = match.group('name')
            return f"{self.base_namespace_prefix}{num_part}/{name_part}#"
        else:
            return f"{self.base_namespace_prefix}rdf/{name_without_ext}#"

    def _load_yaml(self):
        """Load the YAML file into a Python dictionary."""
        try:
            with open(self.yaml_file, "r", encoding="utf-8") as file:
                self.data = yaml.safe_load(file)
        except FileNotFoundError:
            raise ValueError(f"YAML file not found: {self.yaml_file}")
        except yaml.YAMLError as e:
            raise ValueError(f"Invalid YAML file: {self.yaml_file}. Error: {e}")
        except Exception as e:
            raise ValueError(f"Error loading YAML file: {self.yaml_file}. Error: {e}")
        
        if self.data is None:
            raise ValueError(f"YAML file is empty: {self.yaml_file}")

    def _bind_standard_prefixes(self):
        """Bind standard RDF/RDFS/SHACL prefixes to both graphs."""
        # RDF graph - basic vocabulary
        self.rdf_graph.bind("rdf", RDF)
        self.rdf_graph.bind("rdfs", RDFS)
        self.rdf_graph.bind("xsd", XSD)
        
        # SHACL graph - shapes and constraints
        self.shacl_graph.bind("rdf", RDF)
        self.shacl_graph.bind("rdfs", RDFS)
        self.shacl_graph.bind("xsd", XSD)
        
        # SHACL namespace
        self.SH = Namespace("http://www.w3.org/ns/shacl#")
        self.shacl_graph.bind("sh", self.SH)
        
        # DASH namespace for additional SHACL features
        self.DASH = Namespace("http://datashapes.org/dash#")
        self.shacl_graph.bind("dash", self.DASH)
        
        # No longer using custom cc: namespace - using standard W3C patterns

    def _bind_custom_namespaces(self):
        """Bind custom namespace for the current YAML file."""
        filename = os.path.basename(self.yaml_file)
        file_prefix = self.format_name(os.path.splitext(filename)[0])
        
        main_ns = Namespace(self.base_namespace)
        self.prefixes[file_prefix] = main_ns
        self.rdf_graph.bind(file_prefix, main_ns)
        self.shacl_graph.bind(file_prefix, main_ns)
        self.main_prefix = main_ns

        # Bind namespaces for external references
        for ext in self.external_refs:
            ext_filename = os.path.basename(ext)
            ext_prefix = self.format_name(os.path.splitext(ext_filename)[0])
            ext_ns_uri = self._generate_namespace_for_file(ext_filename)
            ext_ns = Namespace(ext_ns_uri)
            self.prefixes[ext_prefix] = ext_ns
            self.rdf_graph.bind(ext_prefix, ext_ns)
            self.shacl_graph.bind(ext_prefix, ext_ns)

    def _generate_namespace_for_file(self, filename):
        """Generate namespace URI for external file using configurable prefix."""
        name_without_ext = os.path.splitext(filename)[0]
        match = re.match(r"(?P<num>TS\d*)_(?P<name>.*)", name_without_ext)
        if match:
            num_part = match.group('num')
            name_part = match.group('name')
            return f"{self.base_namespace_prefix}{num_part}/{name_part}#"
        else:
            return f"{self.base_namespace_prefix}rdf/{name_without_ext}#"

    def convert(self):
        """Convert the loaded YAML content into RDF/RDFS + SHACL."""
        if isinstance(self.data, dict) and "components" in self.data:
            if "schemas" in self.data["components"]:
                self._parse_schemas(self.data["components"]["schemas"])

    def _parse_schemas(self, schemas):
        """Parse each schema definition in the OpenAPI components."""
        for schema_name, schema_def in schemas.items():
            self._process_schema(schema_name, schema_def)

    def _process_schema(self, schema_name, schema_def):
        """Process an individual schema following Prolog SHACL pattern."""
        safe_name = self.format_name(schema_name)
        subject_uri = self.main_prefix[safe_name]

        self._type_clause(subject_uri, None, schema_def)

    def _type_clause(self, subject, property_shape, spec):
        """Main type processing clause, mirrors Prolog type_clause/4."""
        
        # Handle $ref references
        if '$ref' in spec:
            ref = spec['$ref']
            class_uri, _ = self._resolve_reference(ref)
            # Only create SHACL constraints for property shapes, not inheritance for classes
            if property_shape is not None and class_uri is not None:
                if self._is_object_type_from_ref(ref):
                    self.shacl_graph.add((property_shape, getattr(self.SH, 'class'), class_uri))
                else:
                    # Handle datatype reference
                    datatype = self._get_datatype_from_ref(ref)
                    if datatype is not None:
                        self.shacl_graph.add((property_shape, self.SH.datatype, datatype))
            return

        # Handle object type
        if spec.get("type") == "object":
            self._handle_object_type(subject, property_shape, spec)

        # Handle array type  
        elif spec.get("type") == "array":
            self._handle_array_type(subject, property_shape, spec)

        # Handle string type
        elif spec.get("type") == "string":
            self._handle_string_type(subject, property_shape, spec)

        # Handle numeric types (integer, number)
        elif spec.get("type") in ["integer", "number"]:
            self._handle_numeric_type(subject, property_shape, spec)

        # Handle logical operators (anyOf, oneOf, allOf)
        elif "anyOf" in spec:
            self._handle_logical_operator(subject, property_shape, spec["anyOf"], self.SH.or_)
        elif "oneOf" in spec:
            self._handle_logical_operator(subject, property_shape, spec["oneOf"], self.SH.xone)
        elif "allOf" in spec:
            self._handle_logical_operator(subject, property_shape, spec["allOf"], self.SH.and_)

    def _handle_object_type(self, subject, property_shape, spec):
        """Handle object type schemas (type: object)."""
        if subject is not None:
            # Create rdfs:Class in RDF graph
            self.rdf_graph.add((subject, RDF.type, RDFS.Class))
            
            # Add description if present
            if "description" in spec:
                self.rdf_graph.add((subject, RDFS.comment, Literal(spec["description"])))
            
            # Add semantic comments for OpenAPI features that don't translate directly to RDF
            semantic_comments = self._generate_semantic_comments(spec)
            for comment in semantic_comments:
                self.rdf_graph.add((subject, RDFS.comment, Literal(comment)))

            # Create NodeShape with targetClass in SHACL graph
            node_shape = self._create_bnode()
            self.shacl_graph.add((node_shape, RDF.type, self.SH.NodeShape))
            self.shacl_graph.add((node_shape, self.SH.targetClass, subject))

        elif property_shape is not None:
            # Create anonymous NodeShape for property in SHACL graph
            node_shape = self._create_bnode()
            self.shacl_graph.add((node_shape, RDF.type, self.SH.NodeShape))
            self.shacl_graph.add((property_shape, self.SH.node, node_shape))
        else:
            node_shape = None

        # Process properties
        if "properties" in spec:
            properties = spec["properties"]
            required_props = spec.get("required", [])
            
            for prop_name, prop_def in properties.items():
                self._process_property(subject, node_shape, prop_name, prop_def, required_props)

    def _handle_array_type(self, subject, property_shape, spec):
        """Handle array type schemas."""
        # Handle top-level array schemas (like DnList, ConvexGeoPolygon)
        if subject is not None and property_shape is None:
            # Create rdfs:Class in RDF graph
            self.rdf_graph.add((subject, RDF.type, RDFS.Class))
            
            # Add description if present
            if "description" in spec:
                self.rdf_graph.add((subject, RDFS.comment, Literal(spec["description"])))
            
            # Create NodeShape for SHACL validation
            node_shape = self._create_bnode()
            self.shacl_graph.add((node_shape, RDF.type, self.SH.NodeShape))
            self.shacl_graph.add((node_shape, self.SH.targetClass, subject))
            property_shape = node_shape
            
        elif property_shape is None:
            return

        # Add description if present for property-level arrays
        if property_shape is not None and subject is None and "description" in spec:
            self.shacl_graph.add((property_shape, RDFS.comment, Literal(spec["description"])))

        # Use dash:ListShape for array validation
        self.shacl_graph.add((property_shape, self.SH.node, self.DASH.ListShape))

        # Create property shape for array items
        item_shape = self._create_bnode()
        self.shacl_graph.add((property_shape, self.SH.property, item_shape))

        # Create path for array items: ( [ sh:zeroOrMorePath rdf:rest ] rdf:first )
        rest_path = self._create_bnode()
        self.shacl_graph.add((rest_path, self.SH.zeroOrMorePath, RDF.rest))
        path_list = self._create_bnode()
        Collection(self.shacl_graph, path_list, [rest_path, RDF.first])
        self.shacl_graph.add((item_shape, self.SH.path, path_list))

        # Add cardinality constraints
        if "minItems" in spec:
            self.shacl_graph.add((item_shape, self.SH.minCount, Literal(spec["minItems"])))
        if "maxItems" in spec:
            self.shacl_graph.add((item_shape, self.SH.maxCount, Literal(spec["maxItems"])))

        # Process items type
        if "items" in spec:
            self._type_clause(subject, item_shape, spec["items"])

    def _handle_string_type(self, subject, property_shape, spec):
        """Handle string type schemas."""
        # Handle top-level string schemas (like enums) as classes
        if subject is not None and property_shape is None:
            # Create rdfs:Class in RDF graph
            self.rdf_graph.add((subject, RDF.type, RDFS.Class))
            
            # Add description if present
            if "description" in spec:
                self.rdf_graph.add((subject, RDFS.comment, Literal(spec["description"])))
                
            # For enum types, we could create individuals, but for now just create the class
            return
        
        # Handle property-level string constraints
        if property_shape is None:
            return

        # Determine datatype based on format
        datatype = XSD.string
        if "format" in spec:
            format_val = spec["format"]
            format_map = {
                "date-time": XSD.dateTime,
                "full-time": XSD.time,
                "date-month": XSD.gMonth,
                "date-mday": XSD.gMonthDay,
            }
            datatype = format_map.get(format_val, XSD.string)

        self.shacl_graph.add((property_shape, self.SH.datatype, datatype))

        # Add description
        if "description" in spec:
            self.shacl_graph.add((property_shape, RDFS.comment, Literal(spec["description"])))

        # Add string constraints (only for xsd:string)
        if datatype == XSD.string:
            if "pattern" in spec:
                self.shacl_graph.add((property_shape, self.SH.pattern, Literal(spec["pattern"])))
            if "minLength" in spec:
                self.shacl_graph.add((property_shape, self.SH.minLength, Literal(spec["minLength"])))
            if "maxLength" in spec:
                self.shacl_graph.add((property_shape, self.SH.maxLength, Literal(spec["maxLength"])))

        # Handle enumerations
        if "enum" in spec:
            # Convert Python None values back to "NULL" strings (YAML parsing artifact)
            processed_enum = []
            for value in spec["enum"]:
                if value is None:
                    processed_enum.append("NULL")
                else:
                    processed_enum.append(value)
            enum_list = self._create_rdf_list(processed_enum)
            self.shacl_graph.add((property_shape, getattr(self.SH, 'in'), enum_list))

    def _handle_numeric_type(self, subject, property_shape, spec):
        """Handle numeric type schemas (integer, number)."""
        # Handle top-level numeric schemas as classes
        if subject is not None and property_shape is None:
            # Create rdfs:Class in RDF graph
            self.rdf_graph.add((subject, RDF.type, RDFS.Class))
            
            # Add description if present
            if "description" in spec:
                self.rdf_graph.add((subject, RDFS.comment, Literal(spec["description"])))
                
            return
        
        # Handle property-level numeric constraints
        if property_shape is None:
            return

        # Determine datatype
        if spec["type"] == "integer":
            datatype = XSD.integer
        else:  # number
            format_val = spec.get("format", "double")
            if format_val == "float":
                datatype = XSD.float
            else:
                datatype = XSD.double

        self.shacl_graph.add((property_shape, self.SH.datatype, datatype))

        # Add description
        if "description" in spec:
            self.shacl_graph.add((property_shape, RDFS.comment, Literal(spec["description"])))

        # Add numeric constraints
        if "minimum" in spec:
            self.shacl_graph.add((property_shape, self.SH.minInclusive, Literal(spec["minimum"])))
        if "maximum" in spec:
            self.shacl_graph.add((property_shape, self.SH.maxInclusive, Literal(spec["maximum"])))

    def _handle_logical_operator(self, subject, property_shape, specs_list, operator):
        """Handle logical operators (anyOf, oneOf, allOf)."""
        # Handle top-level logical schemas as classes
        if subject is not None and property_shape is None:
            # Create rdfs:Class in RDF graph
            self.rdf_graph.add((subject, RDF.type, RDFS.Class))
            
            # Add semantic comment about logical constraint
            operator_name = str(operator).split('#')[-1]
            comment = f"Note: Uses OpenAPI {operator_name} - complex logical constraints partially supported in SHACL"
            self.rdf_graph.add((subject, RDFS.comment, Literal(comment)))
            
            # Create NodeShape for SHACL validation
            node_shape = self._create_bnode()
            self.shacl_graph.add((node_shape, RDF.type, self.SH.NodeShape))
            self.shacl_graph.add((node_shape, self.SH.targetClass, subject))
            property_shape = node_shape
        
        if property_shape is None:
            return

        # For oneOf/anyOf with mixed types, we need to handle them differently
        # Check if we have mixed datatypes and classes
        has_datatypes = False
        has_classes = False
        
        for spec in specs_list:
            if '$ref' in spec:
                ref = spec['$ref']
                if self._is_object_type_from_ref(ref):
                    has_classes = True
                else:
                    has_datatypes = True
            elif spec.get('type') in ['string', 'number', 'integer', 'boolean']:
                has_datatypes = True
            else:
                has_classes = True
        
        # If we have mixed types, we need to create separate constraints
        if has_datatypes and has_classes:
            # Create separate constraints for datatypes and classes
            datatype_constraints = []
            class_constraints = []
            
            for spec in specs_list:
                if '$ref' in spec:
                    ref = spec['$ref']
                    if self._is_object_type_from_ref(ref):
                        class_uri, _ = self._resolve_reference(ref)
                        if class_uri is not None:
                            class_constraints.append(class_uri)
                    else:
                        datatype = self._get_datatype_from_ref(ref)
                        if datatype is not None:
                            datatype_constraints.append(datatype)
                elif spec.get('type') in ['string', 'number', 'integer', 'boolean']:
                    datatype = self._get_datatype_from_spec(spec)
                    datatype_constraints.append(datatype)
                else:
                    # Handle other types as classes
                    shape = self._create_bnode()
                    self._type_clause(subject, shape, spec)
                    class_constraints.append(shape)
            
            # Add datatype constraints
            if datatype_constraints:
                if len(datatype_constraints) == 1:
                    self.shacl_graph.add((property_shape, self.SH.datatype, datatype_constraints[0]))
                else:
                    datatype_list = self._create_rdf_list(datatype_constraints)
                    self.shacl_graph.add((property_shape, self.SH.datatype, datatype_list))
            
            # Add class constraints
            if class_constraints:
                if len(class_constraints) == 1:
                    self.shacl_graph.add((property_shape, getattr(self.SH, 'class'), class_constraints[0]))
                else:
                    class_list = self._create_rdf_list(class_constraints)
                    self.shacl_graph.add((property_shape, getattr(self.SH, 'class'), class_list))
        else:
            # For homogeneous types, inline the constraints instead of creating separate shapes
            # This avoids the problem of undefined blank node references
            if operator == self.SH.and_:
                # For allOf, we can inline all constraints directly on the property_shape
                for spec in specs_list:
                    if "description" in spec:
                        self.shacl_graph.add((property_shape, RDFS.comment, Literal(spec["description"])))
                    self._type_clause(subject, property_shape, spec)
            else:
                # For oneOf/anyOf, we need to create separate shapes but avoid RDF lists
                # Create individual property shapes for each constraint
                valid_constraints = []
                
                for spec in specs_list:
                    # For object types in logical operators, create a NodeShape instead of PropertyShape
                    if spec.get("type") == "object" and "properties" in spec:
                        # Create a NodeShape for object constraints in logical operators
                        constraint_shape = self._create_bnode()
                        self.shacl_graph.add((constraint_shape, RDF.type, self.SH.NodeShape))
                        
                        if "description" in spec:
                            self.shacl_graph.add((constraint_shape, RDFS.comment, Literal(spec["description"])))
                        
                        # Process properties as SHACL property constraints
                        properties = spec.get("properties", {})
                        required_props = spec.get("required", [])
                        
                        for prop_name, prop_def in properties.items():
                            safe_prop = self.format_name(prop_name)
                            predicate_uri = self.main_prefix[safe_prop]
                            
                            # Create PropertyShape for this property
                            prop_shape = self._create_bnode()
                            self.shacl_graph.add((prop_shape, RDF.type, self.SH.PropertyShape))
                            self.shacl_graph.add((constraint_shape, self.SH.property, prop_shape))
                            self.shacl_graph.add((prop_shape, self.SH.path, predicate_uri))
                            
                            # Add cardinality if required
                            if prop_name in required_props:
                                self.shacl_graph.add((prop_shape, self.SH.minCount, Literal(1)))
                            
                            # Process the property constraints
                            self._type_clause(subject, prop_shape, prop_def)
                        
                        valid_constraints.append(constraint_shape)
                    else:
                        # Create a PropertyShape for non-object constraints
                        constraint_shape = self._create_bnode()
                        self.shacl_graph.add((constraint_shape, RDF.type, self.SH.PropertyShape))
                        
                        if "description" in spec:
                            self.shacl_graph.add((constraint_shape, RDFS.comment, Literal(spec["description"])))
                        
                        # Track triples before processing
                        triples_before = len(self.shacl_graph)
                        
                        self._type_clause(subject, constraint_shape, spec)
                        
                        # Check if any meaningful constraints were added
                        triples_after = len(self.shacl_graph)
                        has_constraints = self._has_meaningful_constraints(constraint_shape)
                        
                        # Also check if we added any sh:property constraints
                        has_properties = len(list(self.shacl_graph.objects(constraint_shape, self.SH.property))) > 0
                        
                        if has_constraints or has_properties or (triples_after > triples_before + 1):  # +1 for the PropertyShape type
                            valid_constraints.append(constraint_shape)
                        else:
                            # Remove the empty PropertyShape and all its triples
                            for p, o in list(self.shacl_graph.predicate_objects(constraint_shape)):
                                self.shacl_graph.remove((constraint_shape, p, o))
                
                # Only add logical operator if we have valid constraints
                if valid_constraints:
                    if len(valid_constraints) == 1:
                        # If only one valid constraint, don't use logical operator
                        # Copy constraints directly to property_shape
                        constraint = valid_constraints[0]
                        for p, o in self.shacl_graph.predicate_objects(constraint):
                            if p != RDF.type:  # Don't copy the PropertyShape type
                                self.shacl_graph.add((property_shape, p, o))
                        # Remove the now-redundant constraint shape
                        for p, o in list(self.shacl_graph.predicate_objects(constraint)):
                            self.shacl_graph.remove((constraint, p, o))
                    else:
                        # Add all valid constraints to the logical operator
                        for constraint_shape in valid_constraints:
                            self.shacl_graph.add((property_shape, operator, constraint_shape))

    def _process_property(self, domain_class, node_shape, prop_name, prop_def, required_list):
        """Process a property within an object schema."""
        safe_prop = self.format_name(prop_name)
        predicate_uri = self.main_prefix[safe_prop]

        # Determine property type and range for proper domain/range specification
        prop_type, range_uri = self._determine_property_type_and_range(prop_def)
        
        # Create property with proper type in RDF graph
        self.rdf_graph.add((predicate_uri, RDF.type, prop_type))
        
        # Add standard W3C domain/range relationships
        if domain_class is not None:
            self.rdf_graph.add((predicate_uri, RDFS.domain, domain_class))
        
        if range_uri is not None:
            self.rdf_graph.add((predicate_uri, RDFS.range, range_uri))

        # Add description
        if "description" in prop_def:
            self.rdf_graph.add((predicate_uri, RDFS.comment, Literal(prop_def["description"])))

        # Create PropertyShape
        property_shape = self._create_bnode()
        self.shacl_graph.add((property_shape, RDF.type, self.SH.PropertyShape))
        if node_shape is not None:
            self.shacl_graph.add((node_shape, self.SH.property, property_shape))
        self.shacl_graph.add((property_shape, self.SH.path, predicate_uri))

        # Add cardinality constraints
        if prop_name in required_list:
            self.shacl_graph.add((property_shape, self.SH.minCount, Literal(1)))
        
        # Add maxCount 1 for non-array properties to ensure single-valued semantics
        if prop_def.get("type") != "array" and "items" not in prop_def:
            self.shacl_graph.add((property_shape, self.SH.maxCount, Literal(1)))

        # Process the property type
        self._type_clause(domain_class, property_shape, prop_def)

    def _determine_property_type_and_range(self, prop_def):
        """Determine the appropriate RDF property type and range for a property definition."""
        
        # Handle $ref references
        if "$ref" in prop_def:
            ref = prop_def["$ref"]
            class_uri, _ = self._resolve_reference(ref)
            if class_uri is not None:
                if self._is_object_type_from_ref(ref):
                    return RDF.Property, class_uri
                else:
                    # Assume datatype property for simple types
                    datatype = self._get_datatype_from_ref(ref)
                    return RDF.Property, datatype if datatype is not None else XSD.string
            else:
                # Fallback if reference cannot be resolved
                return RDF.Property, XSD.string
        
        # Handle basic types
        elif prop_def.get("type") == "string":
            datatype = XSD.string
            if "format" in prop_def:
                format_val = prop_def["format"]
                format_map = {
                    "date-time": XSD.dateTime,
                    "full-time": XSD.time,
                    "date-month": XSD.gMonth,
                    "date-mday": XSD.gMonthDay,
                }
                datatype = format_map.get(format_val, XSD.string)
            return RDF.Property, datatype
            
        elif prop_def.get("type") == "integer":
            return RDF.Property, XSD.integer
            
        elif prop_def.get("type") == "number":
            format_val = prop_def.get("format", "double")
            if format_val == "float":
                return RDF.Property, XSD.float
            else:
                return RDF.Property, XSD.double
                
        elif prop_def.get("type") == "boolean":
            return RDF.Property, XSD.boolean
            
        elif prop_def.get("type") == "object":
            # Inline object definition - use generic object range
            # The actual structure will be defined in SHACL constraints
            return RDF.Property, RDFS.Resource
            
        elif prop_def.get("type") == "array":
            items = prop_def.get("items", {})
            if "$ref" in items:
                ref_uri = self._resolve_reference(items["$ref"])[0]
                return RDF.Property, ref_uri
            else:
                # Array of simple types
                return RDF.Property, XSD.string
        
        # Default fallback
        return RDF.Property, XSD.string

    def _get_datatype_from_ref(self, ref):
        """Get appropriate XSD datatype from a reference (heuristic)."""
        if ref is None:
            return XSD.string
        ref_name = ref.split("/")[-1].lower()
        if "float" in ref_name:
            return XSD.float
        elif "int" in ref_name or "integer" in ref_name:
            return XSD.integer
        elif "bool" in ref_name:
            return XSD.boolean
        else:
            return XSD.string
    
    def _get_datatype_from_spec(self, spec):
        """Get XSD datatype from a specification."""
        spec_type = spec.get('type', 'string')
        
        if spec_type == 'string':
            if 'format' in spec:
                format_val = spec['format']
                format_map = {
                    'date-time': XSD.dateTime,
                    'full-time': XSD.time,
                    'date-month': XSD.gMonth,
                    'date-mday': XSD.gMonthDay,
                }
                return format_map.get(format_val, XSD.string)
            return XSD.string
        elif spec_type == 'integer':
            return XSD.integer
        elif spec_type == 'number':
            return XSD.double
        elif spec_type == 'boolean':
            return XSD.boolean
        else:
            return XSD.string

    def _resolve_reference(self, ref):
        """Resolve a $ref reference to an RDF URI."""
        # Internal reference
        if ref.startswith("#/components/schemas/"):
            ref_name = ref.split("/")[-1]
            return self.main_prefix[self.format_name(ref_name)], None
        
        # External reference
        elif ".yaml#" in ref:
            filename, remainder = ref.split("#/components/schemas/")
            ref_name = remainder
            ext_prefix = self.format_name(os.path.splitext(os.path.basename(filename))[0])
            
            if ext_prefix not in self.prefixes:
                ext_ns_uri = self._generate_namespace_for_file(filename)
                ext_ns = Namespace(ext_ns_uri)
                self.prefixes[ext_prefix] = ext_ns
                self.rdf_graph.bind(ext_prefix, ext_ns)
                self.shacl_graph.bind(ext_prefix, ext_ns)
            
            return self.prefixes[ext_prefix][self.format_name(ref_name)], None
        
        # Handle unresolvable references by creating a placeholder URI
        print(f"Warning: Could not resolve reference '{ref}', creating placeholder")
        safe_ref = self.format_name(ref.replace("/", "_").replace("#", "_"))
        return self.main_prefix[f"UnresolvedRef_{safe_ref}"], None

    def _is_object_type_from_ref(self, ref):
        """Determine if a reference points to an object type (heuristic)."""
        # This is a simplified heuristic - in practice, you'd need to load the referenced schema
        # For now, assume most references are to object types unless they contain type indicators
        ref_name = ref.split("/")[-1].lower()
        return not any(x in ref_name for x in ["float", "int", "string", "bool"])

    def _create_bnode(self):
        """Create a new blank node."""
        return BNode()

    def _has_meaningful_constraints(self, property_shape):
        """Check if a PropertyShape has meaningful SHACL constraints."""
        meaningful_properties = {
            self.SH.datatype, getattr(self.SH, 'class'), self.SH.node, self.SH.minCount,
            self.SH.maxCount, self.SH.minLength, self.SH.maxLength, self.SH.pattern,
            self.SH.minInclusive, self.SH.maxInclusive, getattr(self.SH, 'in'),
            self.SH.hasValue, self.SH.equals, self.SH.disjoint, self.SH.lessThan,
            self.SH.lessThanOrEquals, self.SH.path
        }
        
        for predicate in self.shacl_graph.predicates(property_shape):
            if predicate in meaningful_properties:
                return True
        return False

    def _create_rdf_list(self, items):
        """Create an RDF list from Python list."""
        if not items:
            return RDF.nil
        
        if len(items) == 1:
            # For single items, return the item directly instead of creating a list
            item = items[0]
            if isinstance(item, (str, int, float, bool)):
                return Literal(item)
            else:
                return item
        
        # For multiple items, create a proper RDF list structure
        list_node = self._create_bnode()
        current_node = list_node
        
        for i, item in enumerate(items):
            if isinstance(item, (str, int, float, bool)):
                processed_item = Literal(item)
            else:
                processed_item = item
            
            # Add the item to the current node
            self.shacl_graph.add((current_node, RDF.first, processed_item))
            
            # Create next node if not the last item
            if i < len(items) - 1:
                next_node = self._create_bnode()
                self.shacl_graph.add((current_node, RDF.rest, next_node))
                current_node = next_node
            else:
                # Last item points to rdf:nil
                self.shacl_graph.add((current_node, RDF.rest, RDF.nil))
        
        return list_node

    def _generate_semantic_comments(self, spec):
        """Generate comments for OpenAPI features that don't translate directly to RDF."""
        comments = []
        
        # Check for readOnly properties
        if spec.get('readOnly'):
            comments.append("Note: This property is readOnly in OpenAPI - consider access control in implementation")
        
        # Check for writeOnly properties  
        if spec.get('writeOnly'):
            comments.append("Note: This property is writeOnly in OpenAPI - consider access control in implementation")
        
        # Check for nullable
        if spec.get('nullable'):
            comments.append("Note: This property is nullable in OpenAPI - null vs absent semantics not preserved in RDF")
        
        # Check for discriminator
        if 'discriminator' in spec:
            comments.append("Note: Uses OpenAPI discriminator - consider OWL union classes for full polymorphic semantics")
        
        # Check for logical operators
        for logical_op in ['allOf', 'anyOf', 'oneOf']:
            if logical_op in spec:
                comments.append(f"Note: Uses OpenAPI {logical_op} - complex logical constraints partially supported in SHACL")
        
        # Check for deprecated
        if spec.get('deprecated'):
            comments.append("Note: This schema is deprecated in OpenAPI")
        
        # Check for format constraints that might not translate
        if 'format' in spec and spec.get('type') == 'string':
            format_val = spec['format']
            if format_val not in ['date-time', 'full-time', 'date-month', 'date-mday']:
                comments.append(f"Note: OpenAPI format '{format_val}' constraint not directly expressible in RDF/SHACL")
        
        return comments

    def format_name(self, name):
        """Format names to use underscores instead of dashes."""
        name = os.path.splitext(name)[0]
        return name.replace("-", "_")

    def save_rdf(self):
        """Serialize both RDF and SHACL graphs as separate Turtle files."""
        base_filename = os.path.basename(self.yaml_file).replace(".yaml", "")
        
        try:
            os.makedirs(self.output_dir, exist_ok=True)
        except OSError as e:
            raise ValueError(f"Cannot create output directory {self.output_dir}: {e}")
        
        # Save RDF vocabulary file
        rdf_filename = f"{base_filename}_rdf.ttl"
        rdf_path = os.path.join(self.output_dir, rdf_filename)
        try:
            self.rdf_graph.serialize(destination=rdf_path, format="turtle")
            print(f"✅ RDF vocabulary file saved: {rdf_path}")
        except Exception as e:
            raise ValueError(f"Failed to serialize RDF graph to {rdf_path}: {e}")
        
        # Save SHACL shapes file
        shacl_filename = f"{base_filename}_shacl.ttl"
        shacl_path = os.path.join(self.output_dir, shacl_filename)
        try:
            self.shacl_graph.serialize(destination=shacl_path, format="turtle")
            print(f"✅ SHACL shapes file saved: {shacl_path}")
        except Exception as e:
            raise ValueError(f"Failed to serialize SHACL graph to {shacl_path}: {e}")

    def run(self):
        """Run the full conversion process."""
        try:
            self.convert()
            self.save_rdf()
        except Exception as e:
            import traceback
            print(f"Error during conversion: {e}")
            print("Full traceback:")
            traceback.print_exc()
            raise


# Example Usage
if __name__ == "__main__":
    yaml_files = [
        "assets/MnS-Rel-19-OpenAPI/OpenAPI/TS28623_ComDefs.yaml"
    ]
    for yaml_file in yaml_files:
        converter = OpenAPIToSHACLConverter(yaml_file, external_refs=[])
        converter.run()