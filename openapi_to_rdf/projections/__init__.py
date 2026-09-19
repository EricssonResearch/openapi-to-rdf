"""Projections from the derived Mapping to various output formats.

Every artifact derives from a single ``Mapping`` object (:mod:`openapi_to_rdf.mapping`), so the
vocabulary, shapes, overlay and context cannot disagree about what a class is called or which
class declares a property. Each projection reads the decisions rather than re-deriving them.
"""

from openapi_to_rdf.projections.context import context_from_mapping
from openapi_to_rdf.projections.operations import operations_from_mapping
from openapi_to_rdf.projections.overlay import overlay_from_mapping

__all__ = ["context_from_mapping", "operations_from_mapping", "overlay_from_mapping"]
