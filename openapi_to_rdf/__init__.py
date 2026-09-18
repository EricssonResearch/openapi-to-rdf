"""OpenAPI to RDF/SHACL converter.

This package converts OpenAPI specifications to RDF and SHACL shapes. The public API consists
of two converter classes and a version identifier. Names outside ``__all__`` are internal and
carry no stability contract.
"""

from importlib.metadata import version

from openapi_to_rdf.rdf_converter import OpenAPIToRDFConverter
from openapi_to_rdf.shacl_converter import OpenAPIToSHACLConverter

__version__ = version("openapi-to-rdf")

__all__ = [
    "OpenAPIToRDFConverter",
    "OpenAPIToSHACLConverter",
]
