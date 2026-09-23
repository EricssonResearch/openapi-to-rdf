"""Tests for the property URI helper module.

These tests pin down the URI-minting rules used by both the SHACL and OWL
converters for class-scoped properties.
"""

import pytest
from rdflib import Namespace, URIRef

from openapi_to_rdf.property_uri import (
    class_namespace,
    format_local_name,
    property_uri,
)


class TestFormatLocalName:
    """Preserves legal IRI characters, including dashes."""

    def test_plain_name_unchanged(self):
        assert format_local_name("startTime") == "startTime"

    def test_dashes_preserved(self):
        assert format_local_name("start-time") == "start-time"

    def test_multiple_dashes(self):
        assert format_local_name("a-b-c") == "a-b-c"

    def test_underscores_preserved(self):
        assert format_local_name("already_safe") == "already_safe"

    def test_empty_raises(self):
        with pytest.raises(ValueError):
            format_local_name("")

    def test_none_raises(self):
        with pytest.raises(ValueError):
            format_local_name(None)


class TestClassNamespace:
    """class_namespace turns <base># or <base>/ into <base>/<Class>/ — slash, see the function."""

    def test_base_ends_with_hash(self):
        got = class_namespace("http://x/TS28623/ComDefs#", "TimeWindow")
        assert got == "http://x/TS28623/ComDefs/TimeWindow/"

    def test_base_ends_with_slash(self):
        got = class_namespace("http://x/TS28623/ComDefs/", "TimeWindow")
        assert got == "http://x/TS28623/ComDefs/TimeWindow/"

    def test_base_has_neither(self):
        got = class_namespace("http://x/TS28623/ComDefs", "TimeWindow")
        assert got == "http://x/TS28623/ComDefs/TimeWindow/"

    def test_class_name_with_dash_is_preserved(self):
        got = class_namespace("http://x#", "Meta-Data")
        assert got == "http://x/Meta-Data/"

    def test_underscored_class_name_preserved(self):
        got = class_namespace("http://x#", "Snake_Case")
        assert got == "http://x/Snake_Case/"

    def test_empty_class_raises(self):
        with pytest.raises(ValueError):
            class_namespace("http://x#", "")

    def test_none_class_raises(self):
        with pytest.raises(ValueError):
            class_namespace("http://x#", None)

    def test_empty_base_raises(self):
        with pytest.raises(ValueError):
            class_namespace("", "TimeWindow")


class TestPropertyUri:
    """property_uri assembles a URIRef under a per-class namespace."""

    def test_basic(self):
        got = property_uri("http://x/TS28623/ComDefs#", "TimeWindow", "startTime")
        assert isinstance(got, URIRef)
        assert str(got) == "http://x/TS28623/ComDefs/TimeWindow/startTime"

    def test_property_name_with_dash(self):
        got = property_uri("http://x#", "A", "has-value")
        assert str(got) == "http://x/A/has-value"

    def test_interops_with_rdflib_namespace(self):
        cls_ns = Namespace(class_namespace("http://x#", "A"))
        direct = property_uri("http://x#", "A", "p")
        assert cls_ns["p"] == direct

    def test_different_classes_give_different_uris(self):
        a = property_uri("http://x#", "A", "status")
        b = property_uri("http://x#", "B", "status")
        assert a != b
        assert str(a) == "http://x/A/status"
        assert str(b) == "http://x/B/status"

    def test_empty_property_raises(self):
        with pytest.raises(ValueError):
            property_uri("http://x#", "A", "")

    def test_none_property_raises(self):
        with pytest.raises(ValueError):
            property_uri("http://x#", "A", None)


def test_namespace_for_document_derives_the_3gpp_shape():
    from openapi_to_rdf.property_uri import namespace_for_document

    prefix = "http://ericsson.com/models/3gpp/"
    assert (
        namespace_for_document("TS28623_ComDefs.yaml", prefix)
        == "http://ericsson.com/models/3gpp/TS28623/ComDefs#"
    )


def test_namespace_for_document_falls_back_for_a_non_3gpp_filename():
    from openapi_to_rdf.property_uri import namespace_for_document

    prefix = "http://ericsson.com/models/3gpp/"
    assert (
        namespace_for_document("common.yaml", prefix)
        == "http://ericsson.com/models/3gpp/rdf/common#"
    )


def test_the_converter_derives_its_own_namespace_through_the_shared_function(tmp_path):
    """The self path and the sibling path must not be able to drift apart again.

    This is D1 in one assertion: the two derivations were identical bodies, and identical
    bodies drift. See the spec's D1 section.
    """
    import yaml

    from openapi_to_rdf import OpenAPIToSHACLConverter
    from openapi_to_rdf.property_uri import namespace_for_document

    spec = tmp_path / "TS28623_ComDefs.yaml"
    spec.write_text(
        yaml.safe_dump(
            {
                "openapi": "3.0.0",
                "info": {"title": "ComDefs", "version": "1.0"},
                "components": {"schemas": {"Thing": {"type": "object",
                                                     "properties": {"id": {"type": "string"}}}}},
            }
        )
    )
    prefix = "http://example.test/models/"
    converter = OpenAPIToSHACLConverter(
        str(spec), output_dir=str(tmp_path / "out"), external_refs=[],
        base_namespace_prefix=prefix,
    )
    assert converter.base_namespace == namespace_for_document("TS28623_ComDefs.yaml", prefix)
