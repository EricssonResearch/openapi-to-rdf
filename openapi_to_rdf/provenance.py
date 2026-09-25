"""The contact and project identity that generated artifacts carry in their header.

ONE definition, because the header is emitted from two places -- `shacl_converter` writes the
vocabulary and shapes, `scripts/generate_test_cases.py` writes the 628 SHACL fixtures -- and two
copies of a contact address drift the moment one is updated.

Deliberately NOT read from package metadata. `pyproject.toml` declares `jean.martins@gmail.com`, a
personal address, and the address a reader of a generated artifact should write to is the work one.
Reading the metadata would silently put the wrong address in 704 files.
"""

from __future__ import annotations

#: Who to contact about a generated artifact. Not a copyright claim -- see `NOTICE` for the rights in
#: the source documents these artifacts are derived from.
CONTACT = "Jean Martins <jean.martins@ericsson.com>"

PROJECT_URL = "https://github.com/EricssonResearch/openapi-to-rdf"
