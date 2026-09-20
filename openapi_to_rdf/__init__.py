"""OpenAPI to RDF/SHACL converter.

This package converts OpenAPI specifications to RDF and SHACL shapes. The public API consists
of the derived ``Mapping`` every artifact projects from, the ``OpenAPIToSHACLConverter``, and a
version identifier. Names outside ``__all__`` are internal and carry no stability contract.

``build_mapping`` is the single derived fact set: the vocabulary, the shapes, an OpenAPI Overlay,
a JSON-LD context and an operation graph are all projections of it, so they cannot disagree about
what a class is called or which class declares a property. See :mod:`openapi_to_rdf.mapping`.

``OpenAPIToRDFConverter`` is legacy/internal and does NOT honour the determinations the SHACL
path enforces: it folds ``-`` → ``_`` on both class and property names, mints a class per
``oneOf`` union, and invents a class from ``info.title``. Its class names differ from the SHACL
path (and therefore from ``Mapping.classes``) on 37 of 38 3GPP specs. For new work, use
``build_mapping`` and project from it. The CLI still routes through ``OpenAPIToRDFConverter``
for backward compatibility with existing users.
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
    "OpenAPIToSHACLConverter",
    "OperationFact",
    "PropertyFact",
    "build_mapping",
    "context_from_mapping",
    "operations_from_mapping",
    "overlay_from_mapping",
]
