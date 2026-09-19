import os
import re
import yaml
from rdflib import BNode, Graph, Literal, Namespace, URIRef
from rdflib.collection import Collection
from rdflib.namespace import RDF, RDFS, XSD
from rdflib.term import URIRef as URIRefTerm

from openapi_to_rdf.mapping import (
    REFERS_TO_LOCAL,
    SERIALISATION_ARTIFACT_LOCAL,
    STRING_FORMAT_DATATYPES,
    TRANSPORT_MARKER_LOCAL,
    build_mapping,
    is_json_only_union,
    is_primitive_def,
    target_classes_for,
    xsd_for_string_format,
)
from openapi_to_rdf.property_index import PropertyIndex
from openapi_to_rdf.property_uri import (
    class_namespace,
    format_local_name,
    namespace_for_schema,
    property_uri,
)


def _package_version() -> str:
    """Best-effort lookup of the installed package version."""
    try:
        from importlib.metadata import PackageNotFoundError, version

        return version("openapi-to-rdf")
    except Exception:
        return "unknown"


class OpenAPIToSHACLConverter:
    """Projects an OpenAPI document into an RDF/RDFS vocabulary and SHACL shapes.

    **This class owns serialisation; :mod:`openapi_to_rdf.mapping` owns decisions.** Which named
    schemas are classes, what a class is called, which class declares a property, what a property
    points at, whether a schema is a wire envelope or a reference — every one of those is read from
    the :class:`~openapi_to_rdf.mapping.Mapping` built in ``__init__``, so this projection and any
    other (an Overlay, a JSON-LD context, an operation graph) cannot disagree.

    That is not a tidiness preference. Before the routing, this emitter attributed properties by
    probing the ``rdflib.Graph`` it was still building, so its answer depended on the order schemas
    appear in the document: it disagreed with ``Mapping`` on **43 of 138** non-trivial attributions
    across the three TM Forum v5 documents (23 of 57 on TMF641, 12 of 32 on TMF622, 8 of 49 on
    TMF620), each disagreement minting two IRIs for one field. On the 3GPP corpus it disagreed on
    **0 of 2,822** — that corpus has no non-trivial attribution at all, so no 3GPP-only check could
    have seen the defect.
    """

    def __init__(self, yaml_file, base_namespace=None, output_dir="output", external_refs=None, base_namespace_prefix="http://ericsson.com/models/3gpp/", schema_namespaces=None, transport_namespace=None):
        """Initialize the converter with SHACL-based approach.

        Args:
            yaml_file: Path to the OpenAPI YAML spec to convert.
            base_namespace: Default namespace URI for every schema in the
                spec. Used for all classes unless overridden by
                ``schema_namespaces``.
            output_dir: Directory for the ``output/rdf`` and
                ``output/shacl`` subtrees.
            external_refs: List of paths to sibling OpenAPI YAML files
                whose schemas may appear as ``$ref`` targets.
            base_namespace_prefix: URI prefix used when auto-deriving
                per-file namespaces (e.g. from ``TS28623_ComDefs`` →
                ``<prefix>TS28623/ComDefs#``).
            schema_namespaces: Optional ``{ClassName: namespace_uri}``
                map. When supplied, schemas listed here are emitted under
                their declared namespace instead of ``base_namespace``,
                and ``$ref`` resolutions honour the same map. This
                enables a single-pass conversion over a merged multi-
                domain spec (e.g. CTS topology), where ``Resource`` lives
                under ``.../ctc/`` while ``WirelessNetFunction`` lives
                under ``.../ctw/`` but refers back to ``Resource`` via
                ``allOf``. Namespaces must end in ``#`` or ``/``.
            transport_namespace: Namespace for wire envelopes (notification
                wrappers, event payloads, JSON Patch documents). Defaults to
                ``<base_namespace_prefix>transport/``, so it follows
                ``--namespace-prefix``. Envelopes are emitted under this
                namespace and marked, never omitted: omitting them broke
                nesting, measured on TMF641's ``ServiceOrderCreateEvent``
                where the inner ``ServiceOrder`` lifted 76 triples standalone
                and 1 through the envelope. **This namespace is ours** and
                asserts no external authority.
        """
        self.yaml_file = yaml_file
        self.base_namespace_prefix = base_namespace_prefix
        self.base_namespace = base_namespace or self._generate_base_namespace()
        self.transport_namespace = transport_namespace or f"{base_namespace_prefix}transport/"
        self.output_dir = output_dir
        self.external_refs = external_refs if external_refs is not None else []
        # Per-schema namespace overrides for cross-domain merged specs.
        # See the ``schema_namespaces`` parameter docstring above.
        self.schema_namespaces = dict(schema_namespaces or {})
        self.data = None
        
        # Separate graphs for RDF vocabulary and SHACL shapes
        self.rdf_graph = Graph()
        self.shacl_graph = Graph()
        self.prefixes = {}  # Mapping from prefix string to Namespace object

        # Sidecar property index — populated during _process_property and
        # flushed to disk alongside the TTL files in save_rdf.
        self.property_index = PropertyIndex(
            source=os.path.basename(yaml_file),
            generated_by=f"openapi-to-rdf {_package_version()}",
        )

        # Track how many properties carried no value constraint and were
        # omitted (logged once per conversion).
        self.properties_without_constraints = 0

        # Track unresolved $ref targets rather than inventing placeholders.
        # A filesystem-relative string serialises to a file:/// IRI that
        # leaks the build directory and breaks cross-machine reproducibility,
        # so an unmatched ref is a parser gap, not vocabulary data.
        self.unresolved_references = []

        self._load_yaml()
        # The derived fact set this projection reads. Built once, before any triple is emitted, so
        # no decision is ever answered by probing a half-built graph.
        self.mapping = build_mapping(
            self.data if isinstance(self.data, dict) else {},
            namespace=self.base_namespace,
            schema_namespaces=self.schema_namespaces,
            transport_namespace=self.transport_namespace,
        )
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
        
        # Transport envelopes live in their own namespace and are marked, never omitted; see the
        # `transport_namespace` parameter docstring for the 76-vs-1 triple measurement.
        self.TRANSPORT = Namespace(self.transport_namespace)
        self.TRANSPORT_MARKER = self.TRANSPORT[TRANSPORT_MARKER_LOCAL]
        self.SERIALISATION_ARTIFACT = self.TRANSPORT[SERIALISATION_ARTIFACT_LOCAL]
        self.REFERS_TO = self.TRANSPORT[REFERS_TO_LOCAL]
        self.rdf_graph.bind("transport", self.TRANSPORT)
        self.shacl_graph.bind("transport", self.TRANSPORT)

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

        # Bind per-schema override namespaces so their prefixes are
        # serialised in the output Turtle rather than full IRIs.
        for override_ns in set(self.schema_namespaces.values()):
            ns_obj = Namespace(override_ns)
            # Derive a readable prefix from the last non-empty path segment
            # of the namespace URI (e.g. ".../cts/ctc/" → "ctc"). Falls
            # back to a numbered prefix on collision.
            candidate = override_ns.rstrip("#").rstrip("/").rsplit("/", 1)[-1] or "ns"
            prefix = self.format_name(candidate)
            i = 1
            while prefix in self.prefixes and self.prefixes[prefix] != ns_obj:
                i += 1
                prefix = f"{self.format_name(candidate)}{i}"
            if prefix not in self.prefixes:
                self.prefixes[prefix] = ns_obj
                self.rdf_graph.bind(prefix, ns_obj)
                self.shacl_graph.bind(prefix, ns_obj)

    def _namespace_for_schema(self, schema_name):
        """Return the namespace URI to use for a named schema.

        Delegates to :func:`openapi_to_rdf.property_uri.namespace_for_schema`,
        which is the single point of decision for every class/ref/property
        URI this project mints — the emitter and ``build_mapping`` must not
        each own a copy of it.
        """
        return namespace_for_schema(
            schema_name, self.base_namespace, self.schema_namespaces
        )

    def _class_namespace_uri(self, schema_name):
        """The namespace a named schema's class and property IRIs are minted under.

        Transport for a wire envelope, the schema's own (possibly overridden) namespace otherwise.
        One function, so a class and the properties it declares cannot land in two namespaces.
        """
        if self.mapping is not None:
            fact = self.mapping.classes.get(schema_name)
            if fact is not None and fact.is_transport:
                return self.transport_namespace
        return self._namespace_for_schema(schema_name)

    def _class_iri(self, schema_name):
        """The IRI for a named schema, as a class. **The single place a class IRI is minted.**

        Every other path — ``_process_schema``, ``$ref`` resolution, inheritance edges, reference
        edges — goes through here, so a class cannot be named one thing when it is declared and
        another when it is referred to. A transport envelope resolves to the transport namespace;
        everything else to the schema's own (possibly overridden) namespace.

        **The local name is injective.** It goes through
        :func:`openapi_to_rdf.property_uri.format_local_name`, which keeps ``-`` rather than folding
        it to ``_``. ``format_name`` used to do that folding here, which collapses ``Files-Single``
        and ``Files_Single`` onto one class IRI: a silent collision, not a normalisation, and in the
        identifier that matters most, because a class IRI is what data gets typed with. Task 4 made
        property local names injective and left class names folded. Measured on the 3GPP corpus,
        **629 of 1,801** schema names contain a ``-`` and **0** collide under the folding, so the
        change costs nothing today; the point is that it stops being free the first time a document
        declares both spellings, and by then it is a data migration. 0 of 888 TM Forum v5 names are
        affected.

        Read from ``Mapping`` where it has the class, so the vocabulary and any other projection
        cannot name a class two different things.
        """
        if self.mapping is not None and schema_name in self.mapping.classes:
            return URIRef(self.mapping.classes[schema_name].iri)
        ns_uri = self._class_namespace_uri(schema_name)
        if ns_uri == self.base_namespace:
            return self.main_prefix[format_local_name(schema_name)]
        return Namespace(ns_uri)[format_local_name(schema_name)]

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

    def _get_schemas(self):
        """Return the components/schemas dict, or empty dict."""
        if isinstance(self.data, dict) and "components" in self.data:
            return self.data["components"].get("schemas", {})
        return {}

    def _is_primitive_schema(self, schema_name):
        """Check if a schema name refers to a primitive (non-object, non-array) type."""
        schemas = self._get_schemas()
        return self._is_primitive_def(schemas.get(schema_name), schemas, depth=0)

    def _is_primitive_def(self, schema_def, schemas, depth=0):
        """Recursively check if a schema definition resolves to a primitive type.

        Delegates to :func:`openapi_to_rdf.mapping.is_primitive_def`. "Is this a
        class or a datatype" is a decision, and ``Mapping`` owns decisions while
        the emitter owns serialisation; two copies of this predicate is how a
        vocabulary and a projection come to disagree about what a class is.
        """
        return is_primitive_def(schema_def, schemas, depth)

    def _get_xsd_for_schema(self, schema_name):
        """Get the XSD datatype for a primitive schema by looking up its definition."""
        schemas = self._get_schemas()
        schema_def = schemas.get(schema_name)
        if schema_def is None:
            return None
        return self._get_datatype_from_spec(schema_def)

    def convert(self):
        """Convert the loaded YAML content into RDF/RDFS + SHACL."""
        if isinstance(self.data, dict) and "components" in self.data:
            if "schemas" in self.data["components"]:
                self._parse_schemas(self.data["components"]["schemas"])

        if self.properties_without_constraints > 0:
            print(f"ℹ️  {self.properties_without_constraints} properties carried no value constraint and were omitted from SHACL shapes")

    def _parse_schemas(self, schemas):
        """Parse each schema definition in the OpenAPI components."""
        for schema_name, schema_def in schemas.items():
            self._process_schema(schema_name, schema_def)

    def _process_schema(self, schema_name, schema_def):
        """Project one named schema.

        Three of the decisions here are read from ``Mapping``, not taken here:

        * **A ``oneOf`` union gets no class.** Its members either co-denote or enumerate the
          subclasses of a common ancestor, and the vendor's own ``discriminator`` maps ``@type`` to a
          member and never to the wrapper, so no conforming payload can select it. Emitting one
          produced 14 unreachable classes and 14 unreachable context terms in the consuming project.
          The union still constrains whatever *property* accepts it — see ``_type_clause``, which
          expands the members into ``sh:xone`` rather than pointing ``sh:class`` at a wrapper.
        * **A transport envelope is emitted under the transport namespace and marked**, never
          omitted. Omission broke nesting: TMF641's ``ServiceOrderCreateEvent`` lifted 1 triple
          through the envelope against 76 for the inner ``ServiceOrder`` standalone, because the path
          to the domain object was gone.
        * **A reference contributes an edge to its referent, never a type.** ``ServiceRef`` keeps its
          class — property domains and the inheritance chain depend on it — but is flagged a
          serialisation artifact and pointed at ``Service``, because typing a node
          ``a ...ServiceRef`` asserts the referenced entity *is* a reference while the system that
          owns it says it is a ``Service``.
        """
        if is_json_only_union(schema_def):
            return

        subject_uri = self._class_iri(schema_name)
        self._type_clause(subject_uri, None, schema_def)

        fact = self.mapping.classes.get(schema_name) if self.mapping else None
        if fact is None:
            return
        if fact.is_transport:
            self.rdf_graph.add((subject_uri, self.TRANSPORT_MARKER, Literal(True)))
        if fact.referent is not None:
            # Minted by convention, not looked up: only one of TMF641/TMF622 declares a `Party`
            # schema, so a presence check resolved `PartyRef` to `Party` in one document and left it
            # as `PartyRef` in the other — two specs disagreeing about one term.
            self.rdf_graph.add((subject_uri, self.SERIALISATION_ARTIFACT, Literal(True)))
            self.rdf_graph.add((subject_uri, self.REFERS_TO, self._class_iri(fact.referent)))

    def _type_clause(self, subject, property_shape, spec):
        """Main type processing clause, mirrors Prolog type_clause/4."""
        
        # Handle $ref references
        if '$ref' in spec:
            ref = spec['$ref']
            class_uri, _ = self._resolve_reference(ref)
            # Only create SHACL constraints for property shapes, not inheritance for classes
            if property_shape is not None and class_uri is not None:
                # Check if ref points to an array type — delegate to array handler
                ref_schema = None
                if ref.startswith("#/components/schemas/"):
                    ref_schema = self._get_schemas().get(ref.split("/")[-1])
                elif ".yaml#" in ref:
                    ref_schema = self._load_external_schema(ref)
                members = self._union_members(ref)
                if members is not None:
                    # No class exists for a `oneOf` union, so `sh:class` here would name an IRI no
                    # document declares. The union's content is a constraint on THIS property, and
                    # that is what gets emitted: sh:xone over the members.
                    #
                    # The note travels with it. It used to sit on the union's own class as an
                    # `rdfs:comment`, and dropping the class would have dropped the only record that
                    # this constraint came from a `oneOf` rather than from an `anyOf` — the reader
                    # needs it where the constraint is, which is here.
                    for comment in self._generate_semantic_comments({"oneOf": members}):
                        self.shacl_graph.add((property_shape, RDFS.comment, Literal(comment)))
                    self._handle_logical_operator(None, property_shape, members, self.SH.xone)
                elif isinstance(ref_schema, dict) and ref_schema.get("type") == "array":
                    self._handle_array_type(None, property_shape, ref_schema)
                elif self._is_object_type_from_ref(ref):
                    # A VALUE constraint, so it resolves a reference to its referent (S8).
                    value_class = self._value_class_iri(ref, class_uri)
                    if value_class is not None:
                        self.shacl_graph.add(
                            (property_shape, getattr(self.SH, 'class'), value_class)
                        )
                else:
                    # Handle datatype reference — inline constraints from the referenced schema
                    datatype = self._get_datatype_from_ref(ref)
                    if datatype is not None:
                        self.shacl_graph.add((property_shape, self.SH.datatype, datatype))
                    # Inline constraints (pattern, enum, min/max) from the referenced primitive
                    self._inline_primitive_constraints(property_shape, ref_schema)
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
            # Also process allOf/anyOf/oneOf if present alongside type.
            # For top-level schemas (subject is not None, property_shape is None),
            # _handle_string_type already created a NodeShape, so skip logical operators
            # to avoid creating a second one.
            is_top_level = subject is not None and property_shape is None
            if not is_top_level:
                if "allOf" in spec:
                    self._handle_logical_operator(subject, property_shape, spec["allOf"], self.SH["and"])
                elif "anyOf" in spec:
                    self._handle_logical_operator(subject, property_shape, spec["anyOf"], self.SH["or"])
                elif "oneOf" in spec:
                    self._handle_logical_operator(subject, property_shape, spec["oneOf"], self.SH.xone)

        # Handle numeric types (integer, number)
        elif spec.get("type") in ["integer", "number"]:
            self._handle_numeric_type(subject, property_shape, spec)

        # Handle boolean type
        elif spec.get("type") == "boolean":
            if property_shape is not None:
                self.shacl_graph.add((property_shape, self.SH.datatype, XSD.boolean))

        # Handle logical operators (anyOf, oneOf, allOf) without type
        elif "anyOf" in spec:
            # At top level (subject not None, property_shape None), create NodeShape first
            # to avoid _handle_logical_operator creating a duplicate.
            if subject is not None and property_shape is None:
                self.rdf_graph.add((subject, RDF.type, RDFS.Class))
                comment = "Note: Uses OpenAPI anyOf - complex logical constraints partially supported in SHACL"
                self.rdf_graph.add((subject, RDFS.comment, Literal(comment)))
                # Check if a NodeShape already exists for this subject to avoid duplicates
                existing = list(self.shacl_graph.subjects(self.SH.targetClass, subject))
                if existing:
                    node_shape = existing[0]
                else:
                    node_shape = self._create_bnode()
                    self.shacl_graph.add((node_shape, RDF.type, self.SH.NodeShape))
                    self.shacl_graph.add((node_shape, self.SH.targetClass, subject))
                self._handle_logical_operator(subject, node_shape, spec["anyOf"], self.SH["or"])
            else:
                self._handle_logical_operator(subject, property_shape, spec["anyOf"], self.SH["or"])
        elif "oneOf" in spec:
            # At top level (subject not None, property_shape None), create NodeShape first
            # to avoid _handle_logical_operator creating a duplicate.
            if subject is not None and property_shape is None:
                self.rdf_graph.add((subject, RDF.type, RDFS.Class))
                comment = "Note: Uses OpenAPI oneOf - complex logical constraints partially supported in SHACL"
                self.rdf_graph.add((subject, RDFS.comment, Literal(comment)))
                # Check if a NodeShape already exists for this subject to avoid duplicates
                existing = list(self.shacl_graph.subjects(self.SH.targetClass, subject))
                if existing:
                    node_shape = existing[0]
                else:
                    node_shape = self._create_bnode()
                    self.shacl_graph.add((node_shape, RDF.type, self.SH.NodeShape))
                    self.shacl_graph.add((node_shape, self.SH.targetClass, subject))
                self._handle_logical_operator(subject, node_shape, spec["oneOf"], self.SH.xone)
            else:
                self._handle_logical_operator(subject, property_shape, spec["oneOf"], self.SH.xone)
        elif "allOf" in spec:
            # At the top of a named schema, allOf expresses class composition:
            # every `$ref` item is a parent (emit rdfs:subClassOf) and inline
            # object items contribute properties directly to this class.
            # The inheritance path (_handle_allof_as_inheritance) creates
            # exactly one NodeShape, which the logical operator path then
            # augments with sh:class/sh:and validation constraints.
            if subject is not None and property_shape is None:
                # Create NodeShape and add inheritance edges and properties.
                node_shape = self._handle_allof_as_inheritance(subject, spec["allOf"])
                # Add SHACL validation constraints to the same NodeShape.
                if node_shape is not None:
                    self._handle_logical_operator(subject, node_shape, spec["allOf"], self.SH["and"])
            else:
                # Property-level allOf: use logical operator semantics.
                self._handle_logical_operator(subject, property_shape, spec["allOf"], self.SH["and"])

        # Handle bare constraint specs (e.g. {pattern: "..."} without type) inside allOf/oneOf
        elif property_shape is not None and not spec.get("type"):
            if "pattern" in spec:
                self.shacl_graph.add((property_shape, self.SH.pattern, Literal(spec["pattern"])))
            if "minLength" in spec:
                self.shacl_graph.add((property_shape, self.SH.minLength, Literal(spec["minLength"])))
            if "maxLength" in spec:
                self.shacl_graph.add((property_shape, self.SH.maxLength, Literal(spec["maxLength"])))
            if "minimum" in spec:
                self.shacl_graph.add((property_shape, self.SH.minInclusive, Literal(spec["minimum"])))
            if "maximum" in spec:
                self.shacl_graph.add((property_shape, self.SH.maxInclusive, Literal(spec["maximum"])))
            if "enum" in spec:
                processed_enum = ["NULL" if v is None else v for v in spec["enum"]]
                enum_list = self._create_rdf_list(processed_enum)
                self.shacl_graph.add((property_shape, getattr(self.SH, 'in'), enum_list))
        
        # Per SHACL (W3C Recommendation), a PropertyShape must have at least one
        # constraint component to be meaningful. If we're processing a property_shape
        # and haven't added any value constraints, remove the PropertyShape entirely
        # rather than inventing one — a shape says what the schema said, and the empty
        # schema says nothing. If a downstream triple store requires every PropertyShape
        # to carry a constraint, that store's loader should handle the requirement,
        # not the converter baking a false assertion into published artifacts.
        if property_shape is not None and not list(
            self.shacl_graph.objects(property_shape, self.SH.targetClass)
        ):
            # **A shape carrying sh:targetClass is a class's NodeShape and is never disposable.**
            # The cleanup below deletes a shape that gained no value constraint, which is right for
            # the anonymous PropertyShapes it was written for and catastrophic for a NodeShape: it
            # removes the sh:targetClass too, so the class silently loses its shape entirely.
            #
            # Found on real data, not by a test. `_handle_object_type` now routes a schema declaring
            # both `type: object` and `allOf` through `_handle_logical_operator`, passing the class's
            # own NodeShape; where the `allOf` members carry no value constraint — TMF and 3GPP both
            # write `allOf: [{oneOf: [{required: [a]}, {required: [b]}]}]`, a pure co-occurrence rule
            # — this cleanup then deleted the NodeShape. Measured: **10 of 1,698** declared 3GPP terms
            # lost their shape across 4 documents (`SpecificAnalyticsSubscription`,
            # `SpecificDataSubscription`, `ServiceAreaRestriction`, `AreaScope` and 6 more), against
            # 0 before. The suite was green throughout, which is why this is guarded by an assertion
            # over the real corpus rather than by a fixture.
            #
            # Check if we've added any value constraints by checking predicates directly.
            # Value constraints per SHACL (W3C Recommendation) are: sh:datatype,
            # sh:class, sh:node, sh:in, sh:hasValue, sh:nodeKind, sh:or, sh:xone, sh:and.
            has_value_constraint = False

            # Check all triples with this property_shape as subject
            predicates_for_shape = list(self.shacl_graph.predicates(property_shape))

            # Value constraint predicates to check
            value_constraint_predicates = [
                self.SH.datatype,
                getattr(self.SH, 'class'),
                self.SH.node,
                getattr(self.SH, 'in'),
                self.SH.hasValue,
                self.SH.nodeKind,
                self.SH["or"],
                self.SH.xone,
                self.SH["and"]
            ]

            # Check if any predicate is a value constraint
            for pred in predicates_for_shape:
                if pred in value_constraint_predicates:
                    has_value_constraint = True
                    break

            # If no value constraint was added, remove the PropertyShape
            if not has_value_constraint:
                # Remove all triples where this property_shape is the subject
                for p, o in list(self.shacl_graph.predicate_objects(property_shape)):
                    self.shacl_graph.remove((property_shape, p, o))
                # Remove all triples where this property_shape is the object
                for s, p in list(self.shacl_graph.subject_predicates(property_shape)):
                    self.shacl_graph.remove((s, p, property_shape))
                self.properties_without_constraints += 1

    def _value_class_iri(self, ref, resolved_uri):
        """The class a **value** of this ``$ref`` should be typed as, or None when undecidable.

        Differs from ``_resolve_reference`` in exactly one case, and that case is determination S8: a
        ``$ref`` to ``ServiceRef`` yields ``Service``. ``sh:class X`` requires the value node to have
        ``rdf:type X``, so pointing it at a ``*Ref`` asserts that the *referenced entity* is a
        reference, while the system that owns that entity says it is a ``Service`` — one node, two
        classes, disagreeing only over which side embedded it. A reference contributes an **edge** to
        its referent, never a type.

        The ``*Ref`` class itself is still declared and still carries its own shape: property domains
        (``EntityRef/href``) and the inheritance chain depend on it. What never happens is a node
        being typed as one.

        ``resolved_uri`` is the IRI the caller already obtained from ``_resolve_reference``, passed
        in rather than re-derived, so one reference is resolved once per decision:
        ``_resolve_reference`` appends to ``self.unresolved_references``, and a second call for the
        same ref would inflate that count.

        Recorded because I got it wrong: the 3GPP unresolved-reference count rose 193 → 327 with this
        task's changes and I attributed it to double resolution here. Measured, that was not the
        cause — disabling union expansion alone brings it to **192**, so **135** of the occurrences
        are unions being opened. The emitter used to point ``sh:class`` at the wrapper and never look
        inside; it now resolves the members, and some of them name schemas the document does not
        declare locally. That is a pre-existing gap in those documents becoming visible, not a new
        one, and the distinct unresolved targets number 50 rather than 327.
        """
        if not isinstance(ref, str) or not ref.startswith("#/components/schemas/"):
            return resolved_uri
        name = ref.split("/")[-1]
        fact = self.mapping.classes.get(name) if self.mapping else None
        if fact is not None and fact.referent is not None:
            return self._class_iri(fact.referent)
        return resolved_uri

    def _union_members(self, ref):
        """The ``oneOf`` members of a ``$ref``'d union, or None when the target is not one.

        No class is declared for a union (``mapping.is_json_only_union``), so every site that would
        otherwise point at the wrapper — ``sh:class``, ``rdfs:range``, ``rdfs:subClassOf`` — asks
        here first and constrains against the members instead. Local refs only: an external
        document's unions are resolved through ``_load_external_schema`` by the same rule.
        """
        if not isinstance(ref, str):
            return None
        if ref.startswith("#/components/schemas/"):
            target = self._get_schemas().get(ref.split("/")[-1])
        elif ".yaml#" in ref:
            target = self._load_external_schema(ref)
        else:
            return None
        if is_json_only_union(target):
            return [m for m in target["oneOf"] if isinstance(m, dict)]
        return None

    def _expand_union_refs(self, specs_list, depth=0):
        """Replace any ``$ref``-to-a-union in a logical operator's member list by its own members.

        Flattening rather than nesting, because ``sh:xone (A B)`` and ``sh:xone (sh:xone (A B))``
        differ in SHACL, and because the wrapper has no class to constrain against. Bounded depth so
        a union that references itself cannot spin.
        """
        if depth > 5:
            return list(specs_list)
        expanded = []
        for spec in specs_list:
            members = self._union_members(spec.get("$ref")) if isinstance(spec, dict) else None
            if members is None:
                expanded.append(spec)
            else:
                expanded.extend(self._expand_union_refs(members, depth + 1))
        return expanded

    def _inline_primitive_constraints(self, property_shape, ref_schema):
        """Inline constraints from a referenced primitive schema onto a PropertyShape."""
        if not isinstance(ref_schema, dict):
            return
        if "pattern" in ref_schema:
            self.shacl_graph.add((property_shape, self.SH.pattern, Literal(ref_schema["pattern"])))
        if "minLength" in ref_schema:
            self.shacl_graph.add((property_shape, self.SH.minLength, Literal(ref_schema["minLength"])))
        if "maxLength" in ref_schema:
            self.shacl_graph.add((property_shape, self.SH.maxLength, Literal(ref_schema["maxLength"])))
        # Only add numeric constraints for numeric types (not strings with erroneous min/max)
        if ref_schema.get("type") in ("integer", "number"):
            if "minimum" in ref_schema:
                self.shacl_graph.add((property_shape, self.SH.minInclusive, Literal(ref_schema["minimum"])))
            if "maximum" in ref_schema:
                self.shacl_graph.add((property_shape, self.SH.maxInclusive, Literal(ref_schema["maximum"])))
        if "enum" in ref_schema:
            processed = ["NULL" if v is None else v for v in ref_schema["enum"]]
            self.shacl_graph.add((property_shape, getattr(self.SH, 'in'), self._create_rdf_list(processed)))

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

            # A named schema may declare BOTH `type: object` and a top-level `allOf`, and `allOf` is
            # still class composition when it does. `_type_clause` dispatches on `type` first, so
            # every such schema lost its inheritance edges and its allOf-declared properties: 179 of
            # the 816 parent edges the three TM Forum v5 documents owe were unreachable (0 of 56 on
            # the 3GPP corpus, which has 9 such schemas but none with a `$ref` parent — so a
            # 3GPP-only check could not see this). The subclass loop must iterate EVERY class.
            # `_handle_allof_as_inheritance` reuses the NodeShape just created rather than adding a
            # second one.
            if isinstance(spec.get("allOf"), list):
                self._handle_allof_as_inheritance(subject, spec["allOf"])
                self._handle_logical_operator(
                    subject, node_shape, spec["allOf"], self.SH["and"]
                )

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
        """Handle array type schemas.
        
        Arrays are represented as repeated properties in RDF.
        Cardinality: minItems/maxItems → sh:minCount/sh:maxCount.
        Item type constraints go directly on the property shape.
        """
        # Handle top-level array schemas (like DnList, ConvexGeoPolygon)
        if subject is not None and property_shape is None:
            self.rdf_graph.add((subject, RDF.type, RDFS.Class))
            if "description" in spec:
                self.rdf_graph.add((subject, RDFS.comment, Literal(spec["description"])))
            node_shape = self._create_bnode()
            self.shacl_graph.add((node_shape, RDF.type, self.SH.NodeShape))
            self.shacl_graph.add((node_shape, self.SH.targetClass, subject))
            property_shape = node_shape
        elif property_shape is None:
            return

        if property_shape is not None and subject is None and "description" in spec:
            self.shacl_graph.add((property_shape, RDFS.comment, Literal(spec["description"])))

        # Cardinality from minItems/maxItems (only on PropertyShapes, not top-level NodeShapes)
        # minCount is now computed once in _process_property, combining required and minItems.
        if subject is None:  # property-level array, not top-level
            if "maxItems" in spec:
                self.shacl_graph.add((property_shape, self.SH.maxCount, Literal(spec["maxItems"])))

        # Item type constraints applied directly to the property shape
        if "items" in spec:
            self._type_clause(None, property_shape, spec["items"])

    def _handle_string_type(self, subject, property_shape, spec):
        """Handle string type schemas."""
        # Handle top-level string schemas as datatypes
        if subject is not None and property_shape is None:
            self.rdf_graph.add((subject, RDF.type, RDFS.Datatype))
            
            if "description" in spec:
                self.rdf_graph.add((subject, RDFS.comment, Literal(spec["description"])))

            # Create NodeShape with constraints for top-level string types
            node_shape = self._create_bnode()
            self.shacl_graph.add((node_shape, RDF.type, self.SH.NodeShape))
            self.shacl_graph.add((node_shape, self.SH.targetClass, subject))

            datatype = xsd_for_string_format(spec.get("format"))
            self.shacl_graph.add((node_shape, self.SH.datatype, datatype))

            if "pattern" in spec:
                self.shacl_graph.add((node_shape, self.SH.pattern, Literal(spec["pattern"])))
            if "minLength" in spec:
                self.shacl_graph.add((node_shape, self.SH.minLength, Literal(spec["minLength"])))
            if "maxLength" in spec:
                self.shacl_graph.add((node_shape, self.SH.maxLength, Literal(spec["maxLength"])))
            if "enum" in spec:
                processed_enum = ["NULL" if v is None else v for v in spec["enum"]]
                enum_list = self._create_rdf_list(processed_enum)
                self.shacl_graph.add((node_shape, getattr(self.SH, 'in'), enum_list))

            return
        
        # Handle property-level string constraints
        if property_shape is None:
            return

        # Determine datatype based on format
        datatype = xsd_for_string_format(spec.get("format"))

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
        # Handle top-level numeric schemas as datatypes
        if subject is not None and property_shape is None:
            self.rdf_graph.add((subject, RDF.type, RDFS.Datatype))
            
            if "description" in spec:
                self.rdf_graph.add((subject, RDFS.comment, Literal(spec["description"])))

            # Create NodeShape with constraints
            node_shape = self._create_bnode()
            self.shacl_graph.add((node_shape, RDF.type, self.SH.NodeShape))
            self.shacl_graph.add((node_shape, self.SH.targetClass, subject))

            if spec["type"] == "integer":
                datatype = XSD.integer
            else:
                datatype = XSD.float if spec.get("format") == "float" else XSD.double
            self.shacl_graph.add((node_shape, self.SH.datatype, datatype))

            if "minimum" in spec:
                self.shacl_graph.add((node_shape, self.SH.minInclusive, Literal(spec["minimum"])))
            if "maximum" in spec:
                self.shacl_graph.add((node_shape, self.SH.maxInclusive, Literal(spec["maximum"])))

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

    def _handle_allof_as_inheritance(self, subject, allof_items):
        """Express a schema's top-level `allOf` as class inheritance.

        For every item that is a `$ref` to a named object schema, emit
        ``subject rdfs:subClassOf <referenced class>`` in the RDF graph.
        For every inline ``type: object`` item, merge its ``properties``
        onto the child class via a single NodeShape.

        Always creates exactly one NodeShape for the subject, which
        _handle_logical_operator can then augment with sh:class/sh:and
        constraints for validation semantics.

        Returns the NodeShape so the caller knows one was created.
        """
        if not isinstance(allof_items, list):
            return None
        # Ensure the subject is declared as a class even if no inline
        # `type: object` item exists (pure `$ref` composition).
        self.rdf_graph.add((subject, RDF.type, RDFS.Class))

        # Create exactly one NodeShape for this class. Even pure-ref allOf
        # (no inline objects) needs a NodeShape so SHACL constraints from
        # _handle_logical_operator have somewhere to attach.
        # Check if a NodeShape already exists to avoid duplicates.
        existing = list(self.shacl_graph.subjects(self.SH.targetClass, subject))
        if existing:
            node_shape = existing[0]
        else:
            node_shape = self._create_bnode()
            self.shacl_graph.add((node_shape, RDF.type, self.SH.NodeShape))
            self.shacl_graph.add((node_shape, self.SH.targetClass, subject))

        for item in allof_items:
            if not isinstance(item, dict):
                continue
            # Case 1: $ref to another named schema → inheritance edge.
            if "$ref" in item:
                ref = item["$ref"]
                if not self._is_object_type_from_ref(ref):
                    # Only emit subClassOf when the parent is itself an
                    # object-type class. Datatype refs stay at SHACL level.
                    continue
                parent_uri, _ = self._resolve_reference(ref)
                if parent_uri is not None:
                    self.rdf_graph.add((subject, RDFS.subClassOf, parent_uri))
                continue
            # Case 2: inline object — merge properties onto the subject.
            if item.get("type") == "object":
                properties = item.get("properties")
                if isinstance(properties, dict):
                    required_props = item.get("required", []) or []
                    for prop_name, prop_def in properties.items():
                        self._process_property(
                            subject, node_shape, prop_name, prop_def, required_props
                        )

        return node_shape

    def _handle_logical_operator(self, subject, property_shape, specs_list, operator):
        """Handle logical operators (anyOf, oneOf, allOf)."""
        # A member that is a `$ref` to a `oneOf` union is replaced by that union's own members:
        # there is no class for the wrapper, so constraining against it would name an IRI no
        # document declares. See `_union_members`.
        specs_list = self._expand_union_refs(specs_list)
        # Handle top-level logical schemas as classes.
        # After the uniqueness fix, anyOf/oneOf/allOf in _type_clause create NodeShapes
        # first and pass them in, so this path should only be reached for edge cases or
        # property-level logical operators with subject still set from outer context.
        if subject is not None and property_shape is None:
            # Create rdfs:Class in RDF graph
            self.rdf_graph.add((subject, RDF.type, RDFS.Class))

            # Add semantic comment about logical constraint
            operator_map = {str(self.SH.xone): "oneOf", str(self.SH["or"]): "anyOf", str(self.SH["and"]): "allOf"}
            openapi_name = operator_map.get(str(operator), str(operator).split('#')[-1])
            comment = f"Note: Uses OpenAPI {openapi_name} - complex logical constraints partially supported in SHACL"
            self.rdf_graph.add((subject, RDFS.comment, Literal(comment)))

            # Create NodeShape for SHACL validation (check if one exists first)
            existing = list(self.shacl_graph.subjects(self.SH.targetClass, subject))
            if existing:
                node_shape = existing[0]
            else:
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
                        # A VALUE constraint, so a reference resolves to its referent (S8).
                        class_uri = self._value_class_iri(ref, self._resolve_reference(ref)[0])
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
            # SHACL spec: sh:datatype must be a single IRI, not a list
            # For multiple datatypes, use sh:or with separate PropertyShapes
            if datatype_constraints:
                # Remove duplicates
                datatype_constraints = list(dict.fromkeys(datatype_constraints))
                
                if len(datatype_constraints) == 1:
                    self.shacl_graph.add((property_shape, self.SH.datatype, datatype_constraints[0]))
                else:
                    # Create separate NodeShapes for each datatype, then use sh:or
                    # Use NodeShape instead of PropertyShape since these are constraint shapes without paths
                    datatype_shapes = []
                    for datatype in datatype_constraints:
                        dt_shape = self._create_bnode()
                        self.shacl_graph.add((dt_shape, RDF.type, self.SH.NodeShape))
                        self.shacl_graph.add((dt_shape, self.SH.datatype, datatype))
                        datatype_shapes.append(dt_shape)
                    
                    # Use sh:or to combine multiple datatype constraints
                    or_list = self._create_bnode()
                    Collection(self.shacl_graph, or_list, datatype_shapes)
                    self.shacl_graph.add((property_shape, self.SH["or"], or_list))
            
            # Add class constraints
            # SHACL spec: sh:class must be a single IRI, not a list
            # For multiple classes, use sh:or with separate NodeShapes
            # Note: class_constraints can contain URIRefs (classes) or NodeShapes (from anyOf/oneOf)
            if class_constraints:
                # Separate URIRefs (actual classes) from NodeShapes (constraint shapes)
                class_uris = []
                constraint_shapes = []
                for constraint in class_constraints:
                    if isinstance(constraint, URIRef):
                        class_uris.append(constraint)
                    else:
                        # It's a NodeShape from anyOf/oneOf - use as constraint shape
                        constraint_shapes.append(constraint)
                
                # Remove duplicate URIs
                class_uris = list(dict.fromkeys(class_uris))
                
                # Combine all constraints (class URIs and NodeShapes) in sh:or
                all_shapes = []
                
                # Add NodeShapes for each class URI
                for class_uri in class_uris:
                    class_shape = self._create_bnode()
                    self.shacl_graph.add((class_shape, RDF.type, self.SH.NodeShape))
                    self.shacl_graph.add((class_shape, getattr(self.SH, 'class'), class_uri))
                    all_shapes.append(class_shape)
                
                # Add existing constraint shapes (NodeShapes from anyOf/oneOf)
                all_shapes.extend(constraint_shapes)
                
                # If we have only one shape and it's a class URI (no constraint shapes), use it directly
                if len(all_shapes) == 1 and len(class_uris) == 1 and len(constraint_shapes) == 0:
                    self.shacl_graph.add((property_shape, getattr(self.SH, 'class'), class_uris[0]))
                elif len(all_shapes) > 0:
                    # Use sh:or to combine all constraints
                    or_list = self._create_bnode()
                    Collection(self.shacl_graph, or_list, all_shapes)
                    self.shacl_graph.add((property_shape, self.SH["or"], or_list))
        else:
            # For homogeneous types, inline the constraints instead of creating separate shapes
            # This avoids the problem of undefined blank node references
            if operator == self.SH["and"]:
                # For allOf, inline constraints directly on the property_shape.
                # $refs become sh:class constraints; inline objects add their properties.
                for spec in specs_list:
                    if "description" in spec:
                        self.shacl_graph.add((property_shape, RDFS.comment, Literal(spec["description"])))

                    # Handle $ref: add sh:class constraint without creating a new NodeShape.
                    # _handle_allof_as_inheritance already emitted rdfs:subClassOf for these.
                    if "$ref" in spec:
                        ref = spec["$ref"]
                        if self._is_object_type_from_ref(ref):
                            parent_uri, _ = self._resolve_reference(ref)
                            if parent_uri is not None:
                                self.shacl_graph.add((property_shape, getattr(self.SH, 'class'), parent_uri))
                    else:
                        # Inline constraint/object: pass subject=None so _handle_object_type
                        # doesn't create a duplicate NodeShape.
                        self._type_clause(None, property_shape, spec)
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
                        # Create a NodeShape for non-object constraints in logical operators
                        # PropertyShapes require sh:path in GraphDB, but these constraint shapes
                        # are used in logical operators (sh:xone/sh:or/sh:and) and don't validate
                        # a specific property, so NodeShape is more appropriate
                        constraint_shape = self._create_bnode()
                        self.shacl_graph.add((constraint_shape, RDF.type, self.SH.NodeShape))
                        
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
                        
                        if has_constraints or has_properties or (triples_after > triples_before + 1):  # +1 for the NodeShape type
                            valid_constraints.append(constraint_shape)
                        else:
                            # Remove the empty NodeShape and all its triples
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
                        # Add all valid constraints to the logical operator as an RDF list
                        # GraphDB requires sh:xone, sh:or, sh:and to use RDF lists, not blank nodes
                        constraint_list = self._create_bnode()
                        Collection(self.shacl_graph, constraint_list, valid_constraints)
                        self.shacl_graph.add((property_shape, operator, constraint_list))

    def _find_declaring_class(self, current_class, prop_name):
        """The class that declares ``prop_name``, read from ``Mapping``. Never re-derived here.

        A subclass restating a parent's field must not mint a duplicate IRI: on the TMF641 corpus,
        declaring attribution resolved **93.9%** of (class, property) pairs against an
        independently-authored TBox where leaf attribution resolved **68.5%**.

        **This method used to own a second implementation of that rule and it was wrong.** It walked
        ``rdfs:subClassOf`` in ``self.rdf_graph`` and asked whether the ancestor's property IRI had
        already been emitted — a probe of a graph still being built, so the answer depended on the
        order schemas appear in the document, and where a parent was declared after its child it
        fell back to the leaf. Measured against ``Mapping``: **43 of 138** non-trivial attributions
        differed across the three TM Forum v5 documents (23 of 57 on TMF641, 12 of 32 on TMF622,
        8 of 49 on TMF620) and **0 of 2,822** on the 3GPP corpus, which contains no non-trivial
        attribution at all. Pinned by
        ``tests/test_mapping.py::test_the_two_paths_agree_even_when_the_parent_is_declared_after_the_child``,
        which was a strict xfail until this collapse and is a plain assertion now.

        Takes and returns a class *IRI* rather than a name, because that is what the emitter holds.
        """
        local_name = str(current_class).rsplit('#', 1)[-1].rsplit('/', 1)[-1]
        if self.mapping is None or local_name not in self.mapping.classes:
            return current_class
        declaring = self.mapping.declaring_class(local_name, prop_name)
        if declaring == local_name:
            return current_class
        return self._class_iri(declaring)

    def _process_property(self, domain_class, node_shape, prop_name, prop_def, required_list):
        """Process a property within an object schema.

        Every property is minted under a per-class namespace so that two
        schemas declaring the same property name produce distinct URIs.
        See :mod:`openapi_to_rdf.property_uri` for the URI shape and the
        rationale.

        If ``domain_class`` is ``None`` (inline anonymous sub-object, no
        owning schema class), we fall back to the file-level base
        namespace for the property URI.
        """
        safe_prop = self.format_name(prop_name)

        # Determine property type and range for proper domain/range specification
        prop_type, range_uri = self._determine_property_type_and_range(prop_def)

        # Mint a class-scoped property URI whenever we have an owning class.
        if domain_class is not None:
            # Attribute the property to the highest ancestor that declares it,
            # not the class that mentions it. Inherited restatements collapse
            # onto one IRI.
            declaring_class = self._find_declaring_class(domain_class, prop_name)
            class_local = str(declaring_class).rsplit('#', 1)[-1].rsplit('/', 1)[-1]
            # Use the class's own namespace (which may differ from the
            # file-level base_namespace when schema_namespaces overrides
            # are in play, and is the transport namespace for a wire
            # envelope) so the property URI stays under its owning class's
            # namespace. A property IRI must not claim a domain namespace
            # that its own rdfs:domain does not have.
            class_base = self._class_namespace_uri(class_local)
            predicate_uri = property_uri(class_base, class_local, prop_name)

            # Bind a readable prefix for the per-class namespace on first sight.
            class_ns_uri = class_namespace(class_base, class_local)
            class_ns = Namespace(class_ns_uri)
            file_prefix = self.format_name(os.path.splitext(os.path.basename(self.yaml_file))[0])
            class_ns_prefix = f"{file_prefix}_{class_local}"
            # rdflib's bind is idempotent for an identical prefix/namespace pair.
            self.rdf_graph.bind(class_ns_prefix, class_ns)
            self.shacl_graph.bind(class_ns_prefix, class_ns)
        else:
            # No owning class — inline sub-object without its own schema name.
            predicate_uri = self.main_prefix[safe_prop]

        # Create property with proper type in RDF graph
        self.rdf_graph.add((predicate_uri, RDF.type, prop_type))

        # Single rdfs:domain per property — guaranteed because the URI is
        # class-scoped and therefore unique to this (class, property) pair.
        if domain_class is not None:
            self.rdf_graph.add((predicate_uri, RDFS.domain, domain_class))

        # Per RDF Schema (W3C Recommendation), rdfs:range propagates under
        # entailment rather than validating. An invented range is not a loose
        # constraint, it is a false axiom. Emit rdfs:range only where it is
        # provably true: datatype ranges always, class ranges only for
        # single-target properties. Multi-target and un-analyzable properties
        # defer their constraint to SHACL, where it binds without entailing.
        if range_uri is not None:
            self.rdf_graph.add((predicate_uri, RDFS.range, range_uri))

        # Add description
        if "description" in prop_def:
            self.rdf_graph.add((predicate_uri, RDFS.comment, Literal(prop_def["description"])))

        # Record this property in the sidecar index.
        owner_name = None
        if domain_class is not None:
            owner_name = str(domain_class).rsplit('#', 1)[-1].rsplit('/', 1)[-1]
        self.property_index.add(
            local_name=prop_name,
            uri=str(predicate_uri),
            owner_class=owner_name,
            range_uri=range_uri,
            description=prop_def.get("description"),
        )

        # Create PropertyShape
        property_shape = self._create_bnode()
        self.shacl_graph.add((property_shape, RDF.type, self.SH.PropertyShape))
        if node_shape is not None:
            self.shacl_graph.add((node_shape, self.SH.property, property_shape))
        self.shacl_graph.add((property_shape, self.SH.path, predicate_uri))

        # Add cardinality constraints
        # Compute lower bound once, combining required (implies min 1) and minItems.
        # The stricter bound wins: minItems 2 beats required's implied 1.
        is_array = prop_def.get("type") == "array" or "items" in prop_def
        min_items = prop_def.get("minItems", 0) if is_array else 0
        is_required = 1 if prop_name in required_list else 0
        lower_bound = max(is_required, min_items)
        if lower_bound > 0:
            self.shacl_graph.add((property_shape, self.SH.minCount, Literal(lower_bound)))

        # Add maxCount 1 for non-array properties to ensure single-valued semantics
        if not is_array and "$ref" in prop_def:
            ref = prop_def["$ref"]
            ref_name = ref.split("/")[-1]
            if ref.startswith("#/components/schemas/"):
                ref_schema = self._get_schemas().get(ref_name, {})
                if isinstance(ref_schema, dict) and ref_schema.get("type") == "array":
                    is_array = True
        if not is_array:
            self.shacl_graph.add((property_shape, self.SH.maxCount, Literal(1)))

        # Process the property type
        self._type_clause(None, property_shape, prop_def)

    def _determine_property_type_and_range(self, prop_def):
        """Determine the appropriate RDF property type and range for a property definition.

        Returns (RDF.Property, range_uri) where range_uri may be None.
        Per RDFS (W3C Recommendation), rdfs:range propagates under entailment
        rather than validating, so we emit it only where it is provably true:
        datatype ranges always, class ranges only for single-target properties.
        Multi-target (anyOf/oneOf) and un-analyzable properties return None,
        deferring their constraint to SHACL where it binds without entailing.
        """

        # Multi-target properties (anyOf, oneOf, allOf) cannot have a single
        # rdfs:range — the constraint belongs in SHACL.
        if any(k in prop_def for k in ("anyOf", "oneOf", "allOf")):
            return RDF.Property, None

        # Handle $ref references
        if "$ref" in prop_def:
            ref = prop_def["$ref"]
            if ref.startswith("#/components/schemas/"):
                # Read the class targets from `Mapping`, which applies the two collapses this
                # emitter must not re-derive: a `oneOf` union yields its members (no class exists
                # for the wrapper) and a `*Ref` yields its referent (a property points at the
                # thing, not at the mention of it). One target is a range; several are a SHACL
                # constraint only, because rdfs:range propagates under entailment.
                targets = target_classes_for(prop_def, self._get_schemas())
                if len(targets) == 1:
                    return RDF.Property, self._class_iri(targets[0])
                if targets:
                    return RDF.Property, None
                datatype = self._get_datatype_from_ref(ref)
                return RDF.Property, datatype if datatype is not None else None
            # An EXTERNAL `$ref` is still resolved by the emitter: `Mapping` reads one document and
            # never touches the filesystem, so it sees no external schema at all. The 3GPP corpus
            # depends on this path — its 38 documents cross-reference `TS28623_ComDefs.yaml` and
            # friends heavily — so routing this branch through `Mapping` would silently delete
            # ranges rather than consolidate them. A real gap in `Mapping`, recorded here rather
            # than papered over.
            class_uri, _ = self._resolve_reference(ref)
            if class_uri is not None:
                if self._is_object_type_from_ref(ref):
                    return RDF.Property, class_uri
                datatype = self._get_datatype_from_ref(ref)
                return RDF.Property, datatype if datatype is not None else None
            # Unresolved reference — no range can be determined.
            return RDF.Property, None

        # Handle basic types
        elif prop_def.get("type") == "string":
            return RDF.Property, xsd_for_string_format(prop_def.get("format"))
            
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
                if items["$ref"].startswith("#/components/schemas/"):
                    # Same two collapses as the scalar case, read from `Mapping`.
                    targets = target_classes_for(prop_def, self._get_schemas())
                    if len(targets) == 1:
                        return RDF.Property, self._class_iri(targets[0])
                    if targets:
                        return RDF.Property, None
                    return RDF.Property, self._get_datatype_from_ref(items["$ref"])
                return RDF.Property, self._resolve_reference(items["$ref"])[0]
            else:
                # Array of unspecified item type — no determinate range.
                return RDF.Property, None

        # Default fallback: type could not be analyzed, no range determined.
        return RDF.Property, None

    def _get_datatype_from_ref(self, ref):
        """Get appropriate XSD datatype from a reference by looking up the schema.

        Derive from the declared type/format only. Guessing from spelling is
        how a rule starts inventing meaning (snm-api-native determination S3).
        """
        if ref is None:
            return None
        ref_name = ref.split("/")[-1]
        if ref.startswith("#/components/schemas/"):
            xsd = self._get_xsd_for_schema(ref_name)
            if xsd is not None:
                return xsd
        # For external refs, load the file
        ext_schema = self._load_external_schema(ref) if ".yaml#" in ref else None
        if ext_schema is not None:
            return self._get_datatype_from_spec(ext_schema)
        # Could not determine datatype from declared schema — return None
        # rather than guessing from the name.
        return None
    
    def _get_datatype_from_spec(self, spec):
        """Get XSD datatype from a specification."""
        if not isinstance(spec, dict):
            return XSD.string
        # Follow $ref
        if "$ref" in spec and spec["$ref"].startswith("#/components/schemas/"):
            ref_name = spec["$ref"].split("/")[-1]
            ref_def = self._get_schemas().get(ref_name)
            if ref_def:
                return self._get_datatype_from_spec(ref_def)
        spec_type = spec.get('type', 'string')
        if spec_type == 'string':
            return xsd_for_string_format(spec.get('format'))
        if spec_type == 'integer': return XSD.integer
        if spec_type == 'number': return XSD.float if spec.get('format') == 'float' else XSD.double
        if spec_type == 'boolean': return XSD.boolean
        if spec_type == 'array': return self._get_datatype_from_spec(spec.get('items', {}))
        # anyOf/oneOf — use first option
        for key in ('anyOf', 'oneOf'):
            if key in spec and spec[key]:
                return self._get_datatype_from_spec(spec[key][0])
        return XSD.string

    def _resolve_reference(self, ref):
        """Resolve a $ref reference to an RDF URI.

        A filesystem-relative string serialises to a file:/// IRI that leaks
        the build directory and breaks cross-machine reproducibility, so an
        unmatched ref is a parser gap, not vocabulary data (snm-api-native
        determination S4).
        """
        # Internal reference
        if ref.startswith("#/components/schemas/"):
            ref_name = ref.split("/")[-1]
            # Check that the referenced schema actually exists
            schemas = self._get_schemas()
            if ref_name not in schemas:
                # Schema doesn't exist — track as unresolved
                self.unresolved_references.append(ref)
                return None, None
            # Through `_class_iri`, the single place a class IRI is minted: a per-schema namespace
            # override and the transport namespace both apply here, so an inheritance edge or a
            # property class-shape names a class exactly as its declaration does.
            return self._class_iri(ref_name), None

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

            # Injective local name, for the same reason as `_class_iri`: an external class IRI is
            # still a class IRI, and folding `-` to `_` collapses two schema names onto one.
            return self.prefixes[ext_prefix][format_local_name(ref_name)], None

        # Fragment-less $ref like "Money.yaml" — legal OAS 3.1 but not yet
        # supported. Raise naming the spec section rather than silently
        # mis-resolving. Zero occurrences in the current corpora, but this
        # shape is legal and will eventually appear.
        if ref.endswith(".yaml") or ref.endswith(".yml") or ref.endswith(".json"):
            raise ValueError(
                f"Fragment-less $ref '{ref}' is not supported. "
                f"OAS 3.1 §4.8.24 permits this form, but it requires resolving "
                f"the entire document as a schema, which this converter does not "
                f"yet implement. Use an explicit fragment: '{ref}#/components/schemas/SchemaName'"
            )

        # Unresolvable reference — track it rather than inventing a placeholder.
        self.unresolved_references.append(ref)
        return None, None

    def _load_external_schema(self, ref):
        """Load a schema definition from an external YAML file reference."""
        if ".yaml#" not in ref:
            return None
        filename, remainder = ref.split("#/components/schemas/")
        schema_name = remainder
        # Resolve relative to current file's directory
        yaml_dir = os.path.dirname(self.yaml_file)
        ext_path = os.path.join(yaml_dir, filename)
        if not os.path.exists(ext_path):
            return None
        if not hasattr(self, '_ext_schema_cache'):
            self._ext_schema_cache = {}
        if ext_path not in self._ext_schema_cache:
            try:
                with open(ext_path, "r", encoding="utf-8") as f:
                    self._ext_schema_cache[ext_path] = yaml.safe_load(f).get("components", {}).get("schemas", {})
            except Exception:
                self._ext_schema_cache[ext_path] = {}
        return self._ext_schema_cache[ext_path].get(schema_name)

    def _is_object_type_from_ref(self, ref):
        """Determine if a reference points to an object type by looking up the schema.

        Derive from the declared type only. Guessing from spelling is how a
        rule starts inventing meaning (snm-api-native determination S3).
        """
        if ref is None:
            return False  # Conservative default: unresolved ref → unknown type
        if self._union_members(ref) is not None:
            # A `oneOf` union is not an object type here because no class is declared for it, so an
            # `rdfs:subClassOf` or `sh:class` naming it would be a dangling axiom. Callers that need
            # the union's content ask `_union_members` and constrain against the members.
            return False
        ref_name = ref.split("/")[-1]
        if ref.startswith("#/components/schemas/"):
            return not self._is_primitive_schema(ref_name)
        # For external refs, load the file and check
        ext_schema = self._load_external_schema(ref)
        if ext_schema is not None:
            schemas = self._ext_schema_cache.get(
                os.path.join(os.path.dirname(self.yaml_file), ref.split("#")[0]), {})
            return not self._is_primitive_def(ext_schema, schemas)
        # Could not determine type from declared schema — return False rather
        # than guessing from the name.
        return False

    def _create_bnode(self):
        """Create a new blank node."""
        return BNode()

    def _create_shacl_list_path(self):
        """
        Create SHACL sequence path for list validation: ( [ sh:zeroOrMorePath rdf:rest ] rdf:first )
        
        This creates an RDF list representing the SHACL sequence path where:
        - First element: a blank node with sh:zeroOrMorePath pointing to rdf:rest
        - Second element: rdf:first
        
        Uses rdflib.Collection to ensure proper RDF list structure that GraphDB can validate.
        
        Returns the head of the RDF list.
        """
        # Create blank node for zeroOrMorePath
        zero_or_more_node = self._create_bnode()
        self.shacl_graph.add((zero_or_more_node, self.SH.zeroOrMorePath, RDF.rest))
        
        # Use rdflib.Collection to create a proper RDF list with two elements:
        # [sh:zeroOrMorePath rdf:rest] and rdf:first
        # This ensures GraphDB-compatible list structure where every list node has both rdf:first and rdf:rest
        path_list = self._create_bnode()
        Collection(self.shacl_graph, path_list, [zero_or_more_node, RDF.first])
        
        return path_list

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
        """
        Create an RDF list from Python list.
        
        For sh:class and sh:datatype, items should be URIRef objects (IRIs), not Literals.
        For sh:in (enum values), items can be Literals.
        """
        if not items:
            return RDF.nil
        
        # Always create a proper RDF list structure, even for single items
        # GraphDB requires sh:in to be an RDF list (Resource), not a Literal
        list_node = self._create_bnode()
        current_node = list_node
        
        for i, item in enumerate(items):
            # Convert item to string representation for checking
            item_str = str(item) if item is not None else ""
            
            # First, check if item is already a proper RDF term (URIRef, BNode, etc.)
            if isinstance(item, URIRef):
                # Already a URIRef (including XSD types which are URIRefs) - use as-is
                processed_item = item
            elif isinstance(item, (BNode, Namespace)):
                # Already an RDF term - use as-is
                processed_item = item
            elif item_str.startswith("http://") or item_str.startswith("https://") or item_str.startswith("urn:"):
                # It's a URI string - convert to URIRef
                # This ensures sh:class and sh:datatype use URIRefs, not Literals
                processed_item = URIRef(item_str)
            elif item_str.startswith("xsd:") or "XMLSchema" in item_str:
                # XSD datatype reference - convert to URIRef.
                # Only exact matches or full URI forms; no substring guessing.
                # Guessing from spelling is how a rule starts inventing meaning
                # (snm-api-native determination S3).
                if item_str == "xsd:string" or item_str.endswith("#string"):
                    processed_item = XSD.string
                elif item_str == "xsd:integer" or item_str.endswith("#integer"):
                    processed_item = XSD.integer
                elif item_str == "xsd:double" or item_str.endswith("#double"):
                    processed_item = XSD.double
                elif item_str == "xsd:boolean" or item_str.endswith("#boolean"):
                    processed_item = XSD.boolean
                elif item_str == "xsd:float" or item_str.endswith("#float"):
                    processed_item = XSD.float
                else:
                    # Parse XSD URI
                    xsd_uri = item_str.replace("xsd:", "http://www.w3.org/2001/XMLSchema#")
                    processed_item = URIRef(xsd_uri)
            elif isinstance(item, (int, float, bool)):
                # Numeric/boolean values - keep as Literal
                processed_item = Literal(item)
            elif isinstance(item, str):
                # Regular string - use xsd:string typed literal for SHACL sh:in compatibility
                processed_item = Literal(item, datatype=XSD.string)
            else:
                # Other types - use as-is
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
            if format_val not in STRING_FORMAT_DATATYPES:
                comments.append(f"Note: OpenAPI format '{format_val}' constraint not directly expressible in RDF/SHACL")
        
        return comments

    def format_name(self, name):
        """Fold a FILE name into a Turtle prefix: strip the extension, ``-`` to ``_``.

        **Only for prefixes derived from filenames.** Class and property local names go through
        :func:`openapi_to_rdf.property_uri.format_local_name`, which is injective; this folding is
        lossy and would collapse two schema names onto one IRI. A prefix is a serialisation
        convenience with no identity, so folding it is safe; an IRI is an identity, so it is not.
        """
        name = os.path.splitext(name)[0]
        return name.replace("-", "_")

    def save_rdf(self):
        """Serialize both RDF and SHACL graphs as separate Turtle files in subdirectories."""
        base_filename = os.path.basename(self.yaml_file).replace(".yaml", "")
        
        # Create separate subdirectories for RDF and SHACL files
        rdf_dir = os.path.join(self.output_dir, "rdf")
        shacl_dir = os.path.join(self.output_dir, "shacl")
        index_dir = os.path.join(self.output_dir, "index")
        
        try:
            os.makedirs(rdf_dir, exist_ok=True)
            os.makedirs(shacl_dir, exist_ok=True)
            os.makedirs(index_dir, exist_ok=True)
        except OSError as e:
            raise ValueError(f"Cannot create output directories: {e}")
        
        # Save RDF vocabulary file to rdf/ subdirectory
        rdf_filename = f"{base_filename}_rdf.ttl"
        rdf_path = os.path.join(rdf_dir, rdf_filename)
        try:
            self.rdf_graph.serialize(destination=rdf_path, format="turtle")
            print(f"✅ RDF vocabulary file saved: {rdf_path}")
        except Exception as e:
            raise ValueError(f"Failed to serialize RDF graph to {rdf_path}: {e}")
        
        # Save SHACL shapes file to shacl/ subdirectory
        shacl_filename = f"{base_filename}_shacl.ttl"
        shacl_path = os.path.join(shacl_dir, shacl_filename)
        try:
            self.shacl_graph.serialize(destination=shacl_path, format="turtle")
            print(f"✅ SHACL shapes file saved: {shacl_path}")
        except Exception as e:
            raise ValueError(f"Failed to serialize SHACL graph to {shacl_path}: {e}")

        # Save property index sidecar
        index_filename = f"{base_filename}_property_index.yaml"
        index_path = os.path.join(index_dir, index_filename)
        try:
            self.property_index.write(index_path)
            print(f"✅ Property index saved: {index_path}")
        except Exception as e:
            raise ValueError(f"Failed to write property index to {index_path}: {e}")

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