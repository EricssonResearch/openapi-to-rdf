"""The contact and project identity that generated artifacts carry in their header.

ONE definition, because the header is emitted from two places -- `shacl_converter` writes the
vocabulary and shapes, `scripts/generate_test_cases.py` writes the 628 SHACL fixtures -- and two
copies of a contact address drift the moment one is updated.

Deliberately NOT read from package metadata. `pyproject.toml` declares `jean.martins@gmail.com`, a
personal address, and the address a reader of a generated artifact should write to is the work one.
Reading the metadata would silently put the wrong address in 704 files.
"""

from __future__ import annotations

from rdflib import Graph, URIRef
from rdflib.namespace import DCTERMS, OWL, RDF

#: Who to contact about a generated artifact. Not a copyright claim -- see `NOTICE` for the rights in
#: the source documents these artifacts are derived from.
CONTACT = "Jean Martins <jean.martins@ericsson.com>"

PROJECT_URL = "https://github.com/EricssonResearch/openapi-to-rdf"


#: The predicate stating which tool produced a graph, and the class of the node carrying it.
#:
#: Added 2026-09-25, because the provenance was in a Turtle COMMENT and therefore invisible to every
#: consumer: `output/rdf/TS28623_ComDefs_rdf.ttl` had 5 distinct predicates, no `owl:Ontology`, and the
#: tool's name appeared nowhere in the graph. A comment survives `cat`; it does not survive `Graph.parse`,
#: a triple store, or a merge, which is where a consumer actually meets the data.
#:
#: The immediate consumer is `snm-api-native`'s S9 refusal, which had to infer authorship from the
#: presence of `isIriValued` markers -- and that inference is WRONG for a document with no IRI-valued
#: properties. Measured: `TS28104_MdaNrm.yaml`, converted by this toolchain, carries zero markers and
#: was refused as "a TBox it did not emit". An explicit statement replaces a guess.
#:
#: Vocabulary status, because it is part of the claim: `owl:versionInfo` is OWL 2 (W3C Recommendation),
#: `dcterms:source`/`creator`/`rights`/`publisher` are DCMI Metadata Terms (a DCMI Recommendation).
#: Nothing minted here -- a provenance statement is exactly the case where a standard term should be
#: reused.
#:
#: REAL TERMS, and complete, as of 2026-09-28. This was a tuple of four CURIE *strings*
#: (`"owl:versionInfo"`, …) with no consumer, while `_stamp_provenance` emitted SIX triples -- it also
#: writes `rdf:type owl:Ontology` and `dcterms:publisher`. So the constant was a copy of the emitter
#: that had already drifted, and being unread was the only reason nobody noticed. It is now the single
#: definition: the emitter writes these terms and `split_provenance` filters on them, so the two cannot
#: disagree, and `tests/test_generated_file_provenance.py` asserts the emitted set equals this one.
PROVENANCE_PREDICATES: tuple[URIRef, ...] = (
    RDF.type,
    OWL.versionInfo,
    DCTERMS.source,
    DCTERMS.creator,
    DCTERMS.rights,
    DCTERMS.publisher,
)


def split_provenance(graph: Graph, namespace: str) -> tuple[Graph, Graph]:
    """Partition `graph` into `(vocabulary, provenance)` on the namespace node's provenance terms.

    Needed because provenance is a statement about a FILE and a vocabulary can come from several. A
    document converted whole carries one `dcterms:source`; the same document converted as two and
    merged carries two, which is correct -- the vocabulary really did come from two files -- and it
    means the merged graph has one more triple than the whole one.

    That made `scripts/measure_split_isomorphism.py`'s `graphs_isomorphic` **unsatisfiable by
    construction**: every document failed, for a reason that is not the defect the gate exists to
    find. Measured 2026-09-28: 0 of 46 documents isomorphic with provenance included, **4 of 46**
    with it excluded. Those four -- `TS28531_NSProvMnS`, `TS28531_NSSProvMnS`, `TS28623_ComDefs`,
    `TS28623_FeatureNrm` -- are genuinely correct and were being reported as broken.

    Only triples whose SUBJECT is the namespace node are moved. A `dcterms:source` on some other
    subject would be a statement about something else and belongs to the vocabulary.
    """
    subject = URIRef(namespace)
    terms = set(PROVENANCE_PREDICATES)
    vocabulary, provenance = Graph(), Graph()
    for triple in graph:
        (vocabulary, provenance)[triple[0] == subject and triple[1] in terms].add(triple)
    return vocabulary, provenance

#: The disclaimer, in the GRAPH rather than only in a file header. Short on purpose: it has to survive
#: being read as a literal in a store.
DERIVED_DISCLAIMER = (
    "Derived by openapi-to-rdf from the source document named in dcterms:source. The publisher of that "
    "document did not produce, review or endorse this vocabulary and it must not be cited as their model."
)
