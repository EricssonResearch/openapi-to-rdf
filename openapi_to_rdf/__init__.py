"""OpenAPI to RDF/SHACL converter.

This package converts OpenAPI specifications to RDF and SHACL shapes. The public API consists
of two converter classes, the derived ``Mapping`` every artifact projects from, and a version
identifier. Names outside ``__all__`` are internal and carry no stability contract.

``build_mapping`` is the single derived fact set: the vocabulary, the shapes, an OpenAPI Overlay,
a JSON-LD context and an operation graph are all projections of it, so they cannot disagree about
what a class is called or which class declares a property. See :mod:`openapi_to_rdf.mapping`.
"""

from importlib.metadata import version

from openapi_to_rdf.mapping import (
    ClassFact,
    Mapping,
    OperationFact,
    PropertyFact,
    build_mapping,
)
from openapi_to_rdf.projections import (
    context_from_mapping,
    operations_from_mapping,
    overlay_from_mapping,
)
from openapi_to_rdf.rdf_converter import OpenAPIToRDFConverter
from openapi_to_rdf.shacl_converter import OpenAPIToSHACLConverter

__version__ = version("openapi-to-rdf")

__all__ = [
    "ClassFact",
    "Mapping",
    "OpenAPIToRDFConverter",
    "OpenAPIToSHACLConverter",
    "OperationFact",
    "PropertyFact",
    "build_mapping",
    "context_from_mapping",
    "operations_from_mapping",
    "overlay_from_mapping",
]
