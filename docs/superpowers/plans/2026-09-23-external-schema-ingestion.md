# External Schema Ingestion Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make an external schema participate in class registration, ancestry and declaring-class attribution exactly as a local one does, and mint its IRI under the namespace of the document that declares it.

**Architecture:** Two defects, fixed in one pass. **D1:** a document's namespace is derived by two identical-but-desynchronised functions, only one of which `base_namespace` can override — collapsed onto a single `namespace_for_document` and a per-document namespace map. **D2:** `build_mapping` indexes local `components/schemas` only — external ancestors are now registered transitively into `classes`/`parents`/`declared_by`, marked with the document that declares them, and **referenced but never re-declared** by the TTL projection.

**Tech Stack:** Python ≥ 3.11, `uv` (never `make`), `pytest`, `rdflib`, `PyYAML`, hatchling/PEP 621.

**Spec:** `docs/superpowers/specs/2026-09-23-external-schema-ingestion-design.md` — read it before Task 1. It carries the measurements each acceptance criterion is checked against, and two corrections to `docs/REQUEST-external-schema-ingestion.md` that change how the tests must be written.

## Global Constraints

- **Run commands with `uv run`.** Never add a `make` target. For multiple processes, a `Procfile` + `honcho`.
- **Never fold `-` to `_`** in a class or property local name. `format_local_name` keeps the dash; folding is a silent collision.
- **Never pass an explicit `output_dir` default in a test.** `tests/conftest.py` fails any test that writes under `output/` (a published deliverable). Pass `output_dir=str(tmp_path)`.
- **Commit signed:** `git commit -S`. **No AI-attribution trailer** in any commit message — not `Co-Authored-By`, not "Generated with", not a robot emoji.
- **Run only the tests that cover the change** during a task. The full suite runs at the task boundary before a commit that claims completion. The suite is ~491s; a single module is seconds.
- **Every new gate must be observed failing** before it is trusted (AC-9). Where a step says "run it to verify it fails", the expected failure text is given — if it passes, or fails for a different reason, stop: that is a finding about the fixture, not a pass.
- **Pre-fix baselines, to be compared against, not retyped elsewhere:** 34 spurious `(class, property)` pairs on a split TMF620; 175 distinct dangling class targets on the committed 3GPP `output/` (34 `subClassOf` + 141 `range`); `rdfs:domain` dangles 0 of 889.
- **TM Forum documents are not redistributed.** They resolve from `/home/earejma/snm-api-native-src` via `scripts/_corpora.py`. A corpus that cannot be found must **skip with a stated reason**, never silently.

## File Structure

| File | Responsibility |
|---|---|
| `openapi_to_rdf/property_uri.py` | **Modify.** Gains `namespace_for_document` — the one copy of the filename→namespace derivation. Already the documented "single point of decision" for minted URIs, so it is where this belongs. |
| `openapi_to_rdf/mapping.py` | **Modify.** `_external_parents_of`, external registration in `build_mapping`, `ClassFact.declaring_document`, `Mapping.external_name_collisions`, basename keying in `_parents_of`. |
| `openapi_to_rdf/shacl_converter.py` | **Modify.** `document_namespaces` parameter, `_namespace_for_document` resolver, `_generate_namespace_for_file` deleted, `_resolve_reference` external branch, prefix binding. |
| `scripts/reconcile_projections.py` | **Modify.** Exclude external classes from `expected`, and assert how many were excluded. |
| `scripts/measure_corpora.py` | **Modify.** A corpus arm that loads siblings, plus the `dangling_class_targets` metric. |
| `scripts/measure_split_isomorphism.py` | **Create.** Promotes the ad-hoc whole-vs-split probe to a committed entry point (AC-1, AC-2). |
| `tests/test_external_schema_ingestion.py` | **Create.** The shared fixture and every Mapping-level test for D2. |
| `tests/test_cross_document_refs.py` | **Modify.** Repair three substring assertions into IRI assertions. |
| `HYPOTHESES.md` | **Modify.** D1/D2 to settled with evidence; D3/D4 recorded as deferred; the 175 → 0 expectation as an open hypothesis until measured. |

---

### Task 1: One copy of the filename→namespace derivation

`_generate_base_namespace` (`shacl_converter.py:146`) and `_generate_namespace_for_file` (line 328) hold the same regex derivation. Only the first is overridable, which is the whole of D1. This task makes them one function; Task 3 makes the override reach both.

**Files:**
- Modify: `openapi_to_rdf/property_uri.py`
- Modify: `openapi_to_rdf/shacl_converter.py:146-158`, `:328-337`
- Test: `tests/test_property_uri.py`

**Interfaces:**
- Consumes: nothing.
- Produces: `namespace_for_document(filename: str, base_namespace_prefix: str) -> str`, importable from `openapi_to_rdf.property_uri`.

- [ ] **Step 1: Write the failing test**

Append to `tests/test_property_uri.py`:

```python
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
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
cd /home/earejma/er-github/openapi-to-rdf
uv run pytest tests/test_property_uri.py -v
```

Expected: the three new tests FAIL with `ImportError: cannot import name 'namespace_for_document'`. Read the real exit status — do not pipe through `grep`/`tail`.

- [ ] **Step 3: Add the function**

Append to `openapi_to_rdf/property_uri.py`:

```python
def namespace_for_document(filename: str, base_namespace_prefix: str) -> str:
    """The namespace the classes of one OpenAPI *document* are minted under.

    ``TS28623_ComDefs.yaml`` → ``<prefix>TS28623/ComDefs#``; anything else →
    ``<prefix>rdf/<stem>#``.

    **The single copy of this derivation**, and the reason it is a module-level function rather
    than a converter method. The emitter previously held it twice — once for the document being
    converted, where an explicit ``base_namespace`` could override it, and once for every sibling
    document, where nothing could. Supplying ``base_namespace`` therefore desynchronised a
    document from everything that referred to it, and the referring document emitted an IRI no
    document declared: **175 distinct dangling class targets** in the committed 3GPP ``output/``
    tree (34 of 46 ``rdfs:subClassOf`` targets, 141 of 797 ``rdfs:range``, against 0 of 889
    ``rdfs:domain`` as the control). Recorded as D1 in
    ``docs/superpowers/specs/2026-09-23-external-schema-ingestion-design.md``.

    A class's namespace is a property of the document that DECLARES it. Which namespace a given
    document gets is the caller's to state (``document_namespaces``); this function is only the
    fallback when the caller has not stated one.
    """
    stem = filename.rsplit("/", 1)[-1]
    for extension in (".yaml", ".yml"):
        if stem.endswith(extension):
            stem = stem[: -len(extension)]
            break
    match = re.match(r"(?P<num>TS\d*)_(?P<name>.*)", stem)
    if match:
        return f"{base_namespace_prefix}{match.group('num')}/{match.group('name')}#"
    return f"{base_namespace_prefix}rdf/{stem}#"
```

Add `import re` to the top of `property_uri.py` (it currently imports only `rdflib.URIRef`).

- [ ] **Step 4: Point both converter methods at it**

In `openapi_to_rdf/shacl_converter.py`, replace the body of `_generate_base_namespace` (lines 146-158) with a delegation, keeping its docstring's first line:

```python
    def _generate_base_namespace(self):
        """This document's own namespace, derived from its filename.

        Delegates to :func:`openapi_to_rdf.property_uri.namespace_for_document` so the self path
        and the sibling path cannot drift. See that function for the 175-dangling-target
        measurement that earned the collapse.
        """
        return namespace_for_document(
            os.path.basename(self.yaml_file), self.base_namespace_prefix
        )
```

Delete `_generate_namespace_for_file` (lines 328-337) entirely. Update its two call sites to the shared function for now — Task 3 replaces them with the resolver:

- line 251: `ext_ns_uri = namespace_for_document(ext_filename, self.base_namespace_prefix)`
- line 1681: `ext_ns_uri = namespace_for_document(doc_part, self.base_namespace_prefix)`

Add `namespace_for_document` to the existing `from openapi_to_rdf.property_uri import (...)` block in `shacl_converter.py`.

- [ ] **Step 5: Run the covering tests**

```bash
uv run pytest tests/test_property_uri.py tests/test_cross_document_refs.py tests/test_schema_namespaces.py -v
```

Expected: PASS. This task is a pure refactor — no emitted IRI changes, because the two bodies were identical. If any cross-document test changes its result here, stop: the bodies were **not** identical and the spec's D1 analysis needs revisiting.

- [ ] **Step 6: Commit**

```bash
git add openapi_to_rdf/property_uri.py openapi_to_rdf/shacl_converter.py tests/test_property_uri.py
git commit -S -m "refactor: one copy of the filename->namespace derivation

_generate_base_namespace and _generate_namespace_for_file held the same
regex body, and only the first could be overridden by base_namespace. That
asymmetry is D1: supplying base_namespace desynchronised a document from
everything referring to it. Collapsed onto property_uri.namespace_for_document.

Pure refactor: the bodies were identical, so no emitted IRI moves. The
override reaching both paths is the next task."
```

---

### Task 2: Register external ancestors in the Mapping

The core of D2. `build_mapping` indexes local `components/schemas` only (`mapping.py:904`, `922`, `923`), so an external ancestor has no entry in `parents`, `classes` or `declared_by` and the ancestry walk in `_resolve_declaring_class` terminates at the document boundary.

**The fixture matters more than the assertion here.** It must **restate an inherited property** in the child. A fixture that does not cannot reach this defect at all — the minimal two-document case used during diagnosis attributed its one property perfectly correctly, because nothing was restated. TM Forum restates `href`/`id`/`@type` constantly, which is why the real corpus shows 34 misattributions.

**Files:**
- Create: `tests/test_external_schema_ingestion.py`
- Modify: `openapi_to_rdf/mapping.py`
- Test: `tests/test_external_schema_ingestion.py`, `tests/test_mapping.py`

**Interfaces:**
- Consumes: `namespace_for_document` from Task 1 (not called here; `build_mapping` receives resolved namespaces).
- Produces:
  - `ClassFact.declaring_document: str | None` (None = the document being converted) and `ClassFact.is_external -> bool`.
  - `build_mapping(..., external_namespaces: dict[str, str] | None = None)` — `{document_basename: namespace}`.
  - `Mapping.external_name_collisions: tuple[tuple[str, str], ...]` — `(schema_name, document)` skipped because the name was already registered with a different body.
  - `mapping._external_parents_of(schema_def, schemas, external_schemas) -> list[tuple[str, str]]`.

- [ ] **Step 1: Write the failing test**

Create `tests/test_external_schema_ingestion.py`:

```python
"""External schemas participate in attribution exactly as local ones do.

Spec: docs/superpowers/specs/2026-09-23-external-schema-ingestion-design.md (D1, D2).

**Read this before adding a fixture here.** The request that prompted this work shipped a
reproduction that popped schemas out of `components/schemas` without rewriting the `$ref`
strings, so the refs stayed *internal* and dangling and `external_schemas` was never consulted.
It fails today for the wrong reason. Every fixture below writes the ref in its external form,
and `split_with_restatement` additionally **restates** an inherited property, which is the only
shape that can reach the attribution half of the defect.
"""

from __future__ import annotations

import pytest

from openapi_to_rdf import build_mapping

#: The document name external refs in these fixtures point at.
COMMON = "common.yaml"


@pytest.fixture
def split_with_restatement() -> dict:
    """An API document whose class inherits across a document boundary AND restates two fields.

    `Addressable` declares `href` and `id`. `PolicyRef` composes it by external `$ref` and
    restates both, exactly as TM Forum does. Whole-document conversion attributes `href`/`id` to
    `Addressable`; without this fix the split attributes them to `PolicyRef`.
    """
    common_schemas = {
        "Extensible": {
            "type": "object",
            "properties": {"@baseType": {"type": "string"}},
        },
        "Addressable": {
            "allOf": [
                {"$ref": "#/components/schemas/Extensible"},
                {
                    "type": "object",
                    "properties": {
                        "href": {"type": "string"},
                        "id": {"type": "string"},
                    },
                },
            ]
        },
    }
    api_document = {
        "openapi": "3.0.0",
        "info": {"title": "Api", "version": "1.0"},
        "components": {
            "schemas": {
                "PolicyRef": {
                    "allOf": [
                        {"$ref": f"{COMMON}#/components/schemas/Addressable"},
                        {
                            "type": "object",
                            "properties": {
                                # Restated from Addressable. This is the load-bearing part.
                                "href": {"type": "string"},
                                "id": {"type": "string"},
                                "@type": {"type": "string"},
                            },
                        },
                    ]
                }
            }
        },
    }
    return {"api": api_document, "external": {COMMON: common_schemas}}


def test_an_external_ancestor_is_registered_as_a_class(split_with_restatement):
    mapping = build_mapping(
        split_with_restatement["api"],
        namespace="https://tmforum.org/ontology/",
        external_schemas=split_with_restatement["external"],
    )
    assert "Addressable" in mapping.classes
    assert mapping.classes["Addressable"].declaring_document == COMMON
    assert mapping.classes["Addressable"].is_external is True
    assert mapping.classes["PolicyRef"].is_external is False


def test_a_restated_inherited_property_is_attributed_across_the_boundary(
    split_with_restatement,
):
    """The 34-misattribution defect, in one assertion."""
    mapping = build_mapping(
        split_with_restatement["api"],
        namespace="https://tmforum.org/ontology/",
        external_schemas=split_with_restatement["external"],
    )
    assert mapping.declaring_class("PolicyRef", "href") == "Addressable"
    assert mapping.declaring_class("PolicyRef", "id") == "Addressable"
    # Genuinely the leaf's own field.
    assert mapping.declaring_class("PolicyRef", "@type") == "PolicyRef"
    # And it is NOT also attributed to the leaf, which is what minted the second IRI.
    assert ("PolicyRef", "href") not in mapping.properties_by_class


def test_ancestry_crosses_the_boundary_transitively(split_with_restatement):
    """A in doc 1 -> B in doc 2 -> C in doc 2. `Extensible` is reached only through `Addressable`."""
    mapping = build_mapping(
        split_with_restatement["api"],
        namespace="https://tmforum.org/ontology/",
        external_schemas=split_with_restatement["external"],
    )
    assert mapping.classes["PolicyRef"].parents == ("Addressable",)
    assert mapping.classes["Addressable"].parents == ("Extensible",)
    assert "Extensible" in mapping.classes
    assert mapping.declaring_class("PolicyRef", "@baseType") == "Extensible"


def test_an_external_class_is_minted_under_its_own_documents_namespace(
    split_with_restatement,
):
    """A class's namespace is a property of the document that declares it (D1)."""
    mapping = build_mapping(
        split_with_restatement["api"],
        namespace="https://tmforum.org/ontology/",
        external_schemas=split_with_restatement["external"],
        external_namespaces={COMMON: "https://common.example/v5/"},
    )
    assert mapping.classes["Addressable"].iri == "https://common.example/v5/Addressable"
    assert mapping.classes["PolicyRef"].iri == "https://tmforum.org/ontology/PolicyRef"


def test_without_an_external_namespace_the_mappings_own_namespace_is_used(
    split_with_restatement,
):
    """The split model's default: one vocabulary spread over documents, zero configuration."""
    mapping = build_mapping(
        split_with_restatement["api"],
        namespace="https://tmforum.org/ontology/",
        external_schemas=split_with_restatement["external"],
    )
    assert mapping.classes["Addressable"].iri == "https://tmforum.org/ontology/Addressable"
```

- [ ] **Step 2: Run the tests to verify they fail, and read *how***

```bash
uv run pytest tests/test_external_schema_ingestion.py -v
```

Expected: all six FAIL. The first fails on `assert "Addressable" in mapping.classes`; `test_a_restated_inherited_property_is_attributed_across_the_boundary` fails on `assert mapping.declaring_class("PolicyRef", "href") == "Addressable"` returning `'PolicyRef'`. **If that second one passes, stop** — the fixture is not restating and cannot reach the defect.

- [ ] **Step 3: Add the provenance field to `ClassFact`**

In `openapi_to_rdf/mapping.py`, extend `ClassFact` (line 184) with a field and a derived predicate, and document why provenance-as-a-field does not contradict the settled determination:

```python
    #: The document that DECLARES this class, or None for the document being converted. Provenance,
    #: never identity: `HYPOTHESES.md` settles that the source document is recorded as a fact and
    #: never as an IRI segment, and this field is that fact. What it *does* decide is which
    #: namespace the class is minted under, because a class's namespace is a property of the
    #: document that declares it — see the spec's D1.
    declaring_document: str | None = None

    @property
    def is_external(self) -> bool:
        """True when another document in the OAD declares this class.

        Projections that DECLARE terms must skip these; projections that merely REFERENCE them
        must not. The declaring document already emits the declaration, which is what makes a
        split conversion's union equal to the whole-document conversion — the acceptance
        criterion. Re-declaring would also publish two contradictory definitions under one IRI
        wherever two documents declare the same name differently: 44 such names on 3GPP.
        """
        return self.declaring_document is not None
```

- [ ] **Step 4: Add `_external_parents_of` and fix the basename keying (D3)**

In `openapi_to_rdf/mapping.py`, add near `_parents_of`:

```python
def _ref_document_key(document: str) -> str:
    """The key an external document is indexed under: its basename.

    ``_external_schemas_map`` is keyed by basename and ``_resolve_reference`` basenames before
    looking up, but ``_parents_of`` used the parsed prefix verbatim — so a ref carrying a
    directory component (``sub/common.yaml#/...``) resolved for range purposes and failed for
    inheritance. Latent rather than live: **0 of 2,313** external refs in the 3GPP corpus carry a
    directory component. Keyed here so the two paths agree by construction rather than by corpus
    accident (spec D3). ``posixpath``, not ``os.path``: a ``$ref`` is a URI and its separator is
    ``/`` on every platform.
    """
    return posixpath.basename(document)


def _external_parents_of(
    schema_def: Any,
    schemas: dict[str, Any],
    external_schemas: dict[str, dict[str, Any]],
) -> list[tuple[str, str]]:
    """``(document, schema_name)`` for each top-level ``allOf`` external ``$ref`` that resolves.

    The document qualifier is what :func:`_parents_of` throws away, and it is load-bearing twice:
    it decides the namespace the parent is minted under, and it says which document's schemas an
    internal ``#/...`` ref inside that parent resolves against.

    Applies the same exclusions as :func:`_parents_of` — a ``$ref`` to a primitive is a datatype
    constraint and a ``$ref`` to a ``oneOf`` union names no class, so neither yields a parent.
    """
    if not isinstance(schema_def, dict):
        return []
    found: list[tuple[str, str]] = []
    for member in schema_def.get("allOf") or []:
        if _ref_name(member) is not None:
            continue  # An internal ref, resolved by `_parents_of`.
        ref = member.get("$ref") if isinstance(member, dict) else None
        parsed = _parse_external_ref(ref) if isinstance(ref, str) else None
        if parsed is None:
            continue
        document, schema_name = parsed
        document = _ref_document_key(document)
        document_schemas = external_schemas.get(document)
        if not document_schemas or schema_name not in document_schemas:
            continue
        external_def = document_schemas[schema_name]
        if is_primitive_def(external_def, document_schemas) or is_json_only_union(external_def):
            continue
        if (document, schema_name) not in found:
            found.append((document, schema_name))
    return found
```

Add `import posixpath` to the imports at the top of `mapping.py`.

In `_parents_of` (line 663), apply the same keying so the two agree:

```python
                doc, schema_name = parsed
                doc = _ref_document_key(doc)
                if doc in ext_schemas and schema_name in ext_schemas[doc]:
```

- [ ] **Step 5: Register external ancestors in `build_mapping`**

In `openapi_to_rdf/mapping.py`, add the `external_namespaces` parameter to `build_mapping`'s signature (after `external_schemas`):

```python
    external_namespaces: dict[str, str] | None = None,
```

and to its docstring's Args:

```
        external_namespaces: Optional ``{document_name: namespace_uri}`` for the documents in
            ``external_schemas``. A class's namespace is a property of the document that DECLARES
            it, so an external class is minted under its own document's namespace — which is what
            lets 3GPP keep one vocabulary per document while TM Forum's split model keeps one
            vocabulary across documents. A document with no entry falls back to ``namespace``:
            that is this parameter's documented default for every class in the mapping, and it is
            exactly right for the split model, so the zero-configuration case is the split one.
```

Delete the `namespace_of` helper (lines 897-898). It has exactly two call sites, lines 913 and 947, and both are replaced below by `class_namespace_of`, which additionally knows the declaring document. Leaving it would be a second namespace decision in the module that answers only for the local case.

Replace the local-only class loop (lines 900-923) with a version that records each class's definition and document, then registers external ancestors:

```python
    # --- Classes. Every named schema that is not a primitive/datatype alias, and not a
    # --- JSON-only `oneOf` union (which is not a kind of thing — determination S2). -----------
    ext_schemas = external_schemas or {}
    ext_namespaces = external_namespaces or {}
    classes: dict[str, ClassFact] = {}
    parents: dict[str, tuple[str, ...]] = {}
    #: Each registered class's defining schema, so `declared_by` does not have to assume the
    #: definition came from the local document.
    definitions: dict[str, Any] = {}
    collisions: list[tuple[str, str]] = []

    def namespace_of_document(document: str | None) -> str:
        return namespace if document is None else ext_namespaces.get(document, namespace)

    def class_namespace_of(schema_name: str, document: str | None, transport: bool) -> str:
        # Transport envelopes are minted under their own namespace so plumbing is distinguishable
        # by IRI alone; everything else under its DECLARING document's (possibly overridden)
        # namespace. One function, so a class and the properties it declares cannot land in two.
        if transport:
            return transport_namespace
        return namespace_for_schema(schema_name, namespace_of_document(document), overrides)

    def register(schema_name: str, schema_def: Any, document: str | None) -> bool:
        """Register one named schema as a class. False when it is not a class, or already known."""
        if not isinstance(schema_def, dict):
            return False
        document_schemas = schemas if document is None else ext_schemas.get(document, {})
        if is_primitive_def(schema_def, document_schemas) or is_json_only_union(schema_def):
            return False
        if schema_name in classes:
            # Local always wins, and so does the first external registration. A name declared in
            # two documents with two different bodies is two classes, and this index holds one:
            # 49 of 1,741 3GPP schema names are declared in more than one of the 38 documents and
            # 44 of those differ. Report the skip; never merge silently.
            if document is not None and definitions.get(schema_name) != schema_def:
                collisions.append((schema_name, document))
            return False
        transport = is_transport(schema_name)
        parents[schema_name] = _parents_of(schema_def, document_schemas, ext_schemas)
        definitions[schema_name] = schema_def
        classes[schema_name] = ClassFact(
            iri=class_namespace_of(schema_name, document, transport)
            + format_local_name(schema_name),
            parents=parents[schema_name],
            is_transport=transport,
            referent=referent_name(schema_name),
            declaring_document=document,
        )
        return True

    for schema_name, schema_def in schemas.items():
        register(schema_name, schema_def, None)

    # --- External ancestors participate in attribution exactly as local ones do. --------------
    # Without this, `_resolve_declaring_class` walks `parents`, hits a name with no entry, and
    # stops at the document boundary — so a restated inherited property is attributed to the leaf
    # and mints a second IRI for one field. Measured: 34 such pairs on a split TMF620.
    # Spec: docs/superpowers/specs/2026-09-23-external-schema-ingestion-design.md (D2).
    queue: list[tuple[str, str]] = []
    for schema_name in list(classes):
        if classes[schema_name].is_external:
            continue
        queue.extend(_external_parents_of(definitions[schema_name], schemas, ext_schemas))
    visited: set[tuple[str, str]] = set()
    while queue:
        document, schema_name = queue.pop(0)
        if (document, schema_name) in visited:
            continue
        visited.add((document, schema_name))
        document_schemas = ext_schemas.get(document, {})
        external_def = document_schemas.get(schema_name)
        if not register(schema_name, external_def, document):
            continue
        # Its own ancestors are external from THIS document's point of view too, whether they sit
        # in the same external document or a third one. A parent in neither is a dangling ref: the
        # walk stops and the emitter reports it, which is the existing no-guessing behaviour.
        for parent in parents[schema_name]:
            if parent in document_schemas:
                queue.append((document, parent))
        queue.extend(_external_parents_of(external_def, document_schemas, ext_schemas))
```

Replace the `declared_by`/`required_by` lines (922-923) so they read each class's own definition:

```python
    declared_by = {name: flattened_properties(definitions[name]) for name in classes}
    required_by = {name: flattened_required(definitions[name]) for name in classes}
```

In the property loop, replace the `declaring_ns` block (lines 944-948) with the shared helper, so a property cannot land in a different namespace from the class that declares it:

```python
            declaring_fact = classes[declaring]
            declaring_ns = class_namespace_of(
                declaring, declaring_fact.declaring_document, declaring_fact.is_transport
            )
```

and gate the by-name index on locality (line 961):

```python
            properties_by_class[(declaring, property_name)] = fact
            # First LOCAL declaration in document order owns the by-name index; an external entry
            # could shadow a local one, and this index is what a context/overlay keys on. Every
            # fact is kept in properties_by_class. See Mapping's docstring.
            if not classes[declaring].is_external:
                properties.setdefault(property_name, fact)
```

- [ ] **Step 6: Carry the collision report on the `Mapping`**

Add to `Mapping` (after `api_version`, line 285):

```python
    #: ``(schema_name, document)`` for each external schema NOT registered because the name was
    #: already taken by a different body. Reported rather than merged: deciding that two
    #: same-named schemas are one class is an opinionated modelling step this project refuses to
    #: take silently, and 44 of the 49 names declared in two 3GPP documents genuinely differ.
    external_name_collisions: tuple[tuple[str, str], ...] = ()
```

and pass it in the `return Mapping(...)` (line 1017):

```python
        external_name_collisions=tuple(collisions),
```

- [ ] **Step 7: Run the covering tests**

```bash
uv run pytest tests/test_external_schema_ingestion.py tests/test_mapping.py tests/test_cross_document_refs.py tests/test_allof_subclass.py -v
```

Expected: the six new tests PASS. `tests/test_mapping.py` must stay green — in particular `test_both_declaring_class_implementations_agree`, which reconciles `Mapping.declaring_class` against `_resolve_declaring_class` over a different data path.

- [ ] **Step 8: Add the collision test**

Append to `tests/test_external_schema_ingestion.py`:

```python
def test_a_name_declared_in_two_documents_registers_once_and_reports_the_skip():
    """49 of 1,741 3GPP schema names are declared in more than one document; 44 bodies differ."""
    api_document = {
        "openapi": "3.0.0",
        "info": {"title": "Api", "version": "1.0"},
        "components": {
            "schemas": {
                # Local TimeWindow, with its own body.
                "TimeWindow": {"type": "object", "properties": {"start": {"type": "string"}}},
                "Job": {
                    "allOf": [
                        {"$ref": f"{COMMON}#/components/schemas/TimeWindow"},
                        {"type": "object", "properties": {"name": {"type": "string"}}},
                    ]
                },
            }
        },
    }
    external = {
        COMMON: {
            # Same name, DIFFERENT body — genuinely a different class.
            "TimeWindow": {"type": "object", "properties": {"duration": {"type": "integer"}}}
        }
    }
    mapping = build_mapping(
        api_document, namespace="https://example.org/", external_schemas=external
    )
    # The local declaration wins, and keeps the local namespace.
    assert mapping.classes["TimeWindow"].is_external is False
    assert "start" in {p for _c, p in mapping.properties_by_class}
    assert "duration" not in {p for _c, p in mapping.properties_by_class}
    # And the skip is reported rather than silent.
    assert ("TimeWindow", COMMON) in mapping.external_name_collisions


def test_a_cyclic_allof_across_documents_terminates():
    """Two documents whose classes compose each other must not hang the registration walk."""
    api_document = {
        "openapi": "3.0.0",
        "info": {"title": "Api", "version": "1.0"},
        "components": {
            "schemas": {
                "Leaf": {"allOf": [{"$ref": f"{COMMON}#/components/schemas/Loop"}]},
            }
        },
    }
    external = {
        COMMON: {
            "Loop": {"allOf": [{"$ref": "#/components/schemas/Other"}]},
            "Other": {
                "allOf": [
                    {"$ref": "#/components/schemas/Loop"},
                    {"type": "object", "properties": {"x": {"type": "string"}}},
                ]
            },
        }
    }
    mapping = build_mapping(
        api_document, namespace="https://example.org/", external_schemas=external
    )
    assert {"Leaf", "Loop", "Other"} <= set(mapping.classes)


def test_an_external_ref_with_a_directory_component_resolves_for_inheritance():
    """D3: `_parents_of` keyed on the raw prefix while `_resolve_reference` basenamed it.

    Latent, not live — 0 of 2,313 external refs in the 3GPP corpus carry a directory component —
    so this is the only input that reaches it.
    """
    api_document = {
        "openapi": "3.0.0",
        "info": {"title": "Api", "version": "1.0"},
        "components": {
            "schemas": {
                "Child": {"allOf": [{"$ref": "sub/dir/common.yaml#/components/schemas/Parent"}]}
            }
        },
    }
    external = {COMMON: {"Parent": {"type": "object",
                                    "properties": {"id": {"type": "string"}}}}}
    mapping = build_mapping(
        api_document, namespace="https://example.org/", external_schemas=external
    )
    assert mapping.classes["Child"].parents == ("Parent",)
    assert mapping.classes["Parent"].is_external is True
```

- [ ] **Step 9: Run them, then invert the collision guard**

```bash
uv run pytest tests/test_external_schema_ingestion.py -v
```

Expected: PASS. Then **observe each new guard failing** (AC-9), restoring after each:

1. In `register`, change `if document is not None and definitions.get(schema_name) != schema_def:` to `if False:` — `test_a_name_declared_in_two_documents_registers_once_and_reports_the_skip` must fail on the `external_name_collisions` assertion. Restore.
2. In `_ref_document_key`, `return document` instead of the basename — `test_an_external_ref_with_a_directory_component_resolves_for_inheritance` must fail. Restore.
3. Delete the `queue.extend(_external_parents_of(external_def, document_schemas, ext_schemas))` line and the `for parent in parents[schema_name]` loop — `test_ancestry_crosses_the_boundary_transitively` must fail on `"Extensible" in mapping.classes`. Restore.

If any inversion does **not** fail, that is a finding about the sample: another defence is already covering the case. Find which, and say so, before continuing.

- [ ] **Step 10: Commit**

```bash
git add openapi_to_rdf/mapping.py tests/test_external_schema_ingestion.py
git commit -S -m "feat: external schemas are registered as classes, so attribution crosses documents

build_mapping indexed local components/schemas only, so an external ancestor
had no entry in classes, parents or declared_by and _resolve_declaring_class
terminated at the document boundary. A restated inherited property was then
attributed to the leaf and minted a second IRI for one field: 34 such pairs
on a correctly split TMF620.

Contrary to the request's evidence block, _parents_of already resolved the
external parent NAME — PolicyRef.parents == ('EntityRef',), not (). The break
was one level up, in the three indexes.

External ancestors are now registered transitively, each minted under its own
declaring document's namespace (external_namespaces), because a class's
namespace is a property of the document that declares it. ClassFact gains
declaring_document as provenance; the settled determination that file
structure is never an IRI *segment* is unaffected.

Name collisions are reported, not merged: 49 of 1,741 3GPP schema names are
declared in more than one of the 38 documents and 44 of those differ, so the
bare-name index genuinely cannot hold both. Local always wins.

Also fixes D3: _parents_of keyed an external document on the raw ref prefix
while _resolve_reference basenamed it, so a ref with a directory component
resolved for range and failed for inheritance. Latent — 0 of 2,313 3GPP
external refs carry one — and covered by a regression test.

Each new guard was observed failing with its mechanism removed."
```

---

### Task 3: Wire the per-document namespace through the converter

Task 2 taught the `Mapping` to mint an external class correctly; this task makes the emitter agree, and closes D1 at the emitter.

**Files:**
- Modify: `openapi_to_rdf/shacl_converter.py:58-145` (signature, `__init__`), `:245-255` (prefix binding), `:1665-1690` (`_resolve_reference` external branch)
- Test: `tests/test_external_schema_ingestion.py`, `tests/test_cross_document_refs.py`

**Interfaces:**
- Consumes: `namespace_for_document` (Task 1); `build_mapping(external_namespaces=...)`, `ClassFact.is_external` (Task 2).
- Produces: `OpenAPIToSHACLConverter(..., document_namespaces: dict[str, str] | None = None)` and `self._namespace_for_document(filename: str) -> str`.

- [ ] **Step 1: Write the failing test**

Append to `tests/test_external_schema_ingestion.py`:

```python
def _write(directory, name, document):
    import yaml

    path = directory / name
    path.write_text(yaml.safe_dump(document))
    return path


@pytest.fixture
def split_files(tmp_path, split_with_restatement):
    """`split_with_restatement`, on disk, as common.yaml + api.yaml."""
    common = {
        "openapi": "3.0.0",
        "info": {"title": "Common", "version": "1.0"},
        "components": {"schemas": split_with_restatement["external"][COMMON]},
    }
    return {
        "common": _write(tmp_path, COMMON, common),
        "api": _write(tmp_path, "api.yaml", split_with_restatement["api"]),
        "dir": tmp_path,
    }


def test_a_referenced_external_class_gets_its_declaring_documents_iri(split_files, tmp_path):
    """D1: the referring document used to mint this from the FILENAME and a hardcoded prefix.

    Pre-fix this emits <http://ericsson.com/models/3gpp/rdf/common#Addressable> — a 3GPP IRI
    inside a TM Forum vocabulary, and an IRI no document declares.
    """
    from rdflib import RDFS, URIRef

    from openapi_to_rdf import OpenAPIToSHACLConverter

    shared = "https://tmforum.org/ontology/"
    converter = OpenAPIToSHACLConverter(
        str(split_files["api"]),
        base_namespace=shared,
        output_dir=str(tmp_path / "out"),
        external_refs=[str(split_files["common"])],
    )
    converter.convert()
    assert (
        URIRef(f"{shared}PolicyRef"),
        RDFS.subClassOf,
        URIRef(f"{shared}Addressable"),
    ) in converter.rdf_graph


def test_document_namespaces_overrides_per_document(split_files, tmp_path):
    """3GPP's convention: one vocabulary per document, stated by the caller."""
    from rdflib import RDFS, URIRef

    from openapi_to_rdf import OpenAPIToSHACLConverter

    converter = OpenAPIToSHACLConverter(
        str(split_files["api"]),
        base_namespace="https://api.example/",
        output_dir=str(tmp_path / "out"),
        external_refs=[str(split_files["common"])],
        document_namespaces={COMMON: "https://common.example/v5/"},
    )
    converter.convert()
    assert (
        URIRef("https://api.example/PolicyRef"),
        RDFS.subClassOf,
        URIRef("https://common.example/v5/Addressable"),
    ) in converter.rdf_graph


def test_an_external_class_is_referenced_but_never_declared(split_files, tmp_path):
    """AC-7. The declaring document emits the declaration; re-declaring would duplicate it."""
    from rdflib import RDF, RDFS, URIRef

    from openapi_to_rdf import OpenAPIToSHACLConverter

    shared = "https://tmforum.org/ontology/"
    converter = OpenAPIToSHACLConverter(
        str(split_files["api"]),
        base_namespace=shared,
        output_dir=str(tmp_path / "out"),
        external_refs=[str(split_files["common"])],
    )
    converter.convert()
    external = URIRef(f"{shared}Addressable")
    assert (external, RDF.type, RDFS.Class) not in converter.rdf_graph
    assert not list(converter.rdf_graph.triples((None, RDFS.domain, external)))
    assert not list(converter.shacl_graph.triples((None, None, external)))
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
uv run pytest tests/test_external_schema_ingestion.py -v -k "declaring_documents_iri or document_namespaces_overrides or never_declared"
```

Expected: the first two FAIL, showing the object as `http://ericsson.com/models/3gpp/rdf/common#Addressable`. `test_an_external_class_is_referenced_but_never_declared` is expected to **pass already** — `_parse_schemas` iterates local schemas only, so suppression holds by construction. It is written to pin that, and it must be observed failing in Step 6.

- [ ] **Step 3: Add the parameter and the resolver**

In `openapi_to_rdf/shacl_converter.py`, add `document_namespaces=None` to `__init__`'s signature (after `schema_namespaces`), and to the docstring's Args:

```
            document_namespaces: Optional ``{document_filename: namespace_uri}`` map, keyed by
                basename. A class's namespace is a property of the document that DECLARES it, so
                this is how a referring document learns the namespace a sibling was converted
                under. Without it, a sibling's namespace could only be re-derived from its
                filename, which silently disagreed with any explicitly-supplied
                ``base_namespace``: 175 distinct dangling class targets in the committed 3GPP
                ``output/`` tree. See the spec's D1.
```

In `__init__`, beside `self.schema_namespaces` (line 101):

```python
        # Keyed by basename, matching `_external_schemas_map`.
        self.document_namespaces = {
            os.path.basename(name): uri for name, uri in (document_namespaces or {}).items()
        }
```

Add the resolver beside `_namespace_for_schema` (line 275):

```python
    def _namespace_for_document(self, filename):
        """The namespace the classes of one document in this OAD are minted under.

        The four-step resolution, in order: an explicit ``document_namespaces`` entry; this
        document's own ``base_namespace`` when ``filename`` IS this document; otherwise the
        filename derivation. Step two is what closes D1 — ``base_namespace`` used to be a value
        only the self path could see, so supplying it desynchronised this document from every
        document referring to it.

        Per-class ``schema_namespaces`` overrides are applied on top of this, by
        :func:`openapi_to_rdf.property_uri.namespace_for_schema`, and still win.
        """
        base = os.path.basename(filename)
        if base in self.document_namespaces:
            return self.document_namespaces[base]
        if base == os.path.basename(self.yaml_file):
            return self.base_namespace
        return namespace_for_document(base, self.base_namespace_prefix)
```

- [ ] **Step 4: Pass the external namespaces into `build_mapping`**

In `__init__`, replace the `build_mapping` call (lines 137-142):

```python
        self.mapping = build_mapping(
            self.data if isinstance(self.data, dict) else {},
            namespace=self.base_namespace,
            schema_namespaces=self.schema_namespaces,
            transport_namespace=self.transport_namespace,
            external_schemas=getattr(self, "_external_schemas_map", None),
            # The converter is the component that knows filenames, so it resolves every external
            # document's namespace here rather than leaving `build_mapping` to re-derive one.
            external_namespaces={
                document: self._namespace_for_document(document)
                for document in getattr(self, "_external_schemas_map", {})
            },
        )
```

- [ ] **Step 5: Use the resolver at both remaining call sites**

Prefix binding (line 251, from Task 1):

```python
            ext_ns_uri = self._namespace_for_document(ext_filename)
```

`_resolve_reference`'s external branch — replace the whole `if ref_name in self.schema_namespaces: … else: …` block (lines ~1665-1690) with:

```python
                    # Resolved. Through `_class_iri` where the Mapping holds the class, which is
                    # the single place a class IRI is minted and now carries the external class
                    # under its own declaring document's namespace. A resolved external schema
                    # that is NOT a class — a primitive alias or a `oneOf` union — has no Mapping
                    # entry, so its namespace is resolved directly rather than falling through to
                    # this document's.
                    if self.mapping is not None and ref_name in self.mapping.classes:
                        return self._class_iri(ref_name), None
                    ext_ns_uri = self._namespace_for_document(doc_name)
                    ext_prefix = self.format_name(os.path.splitext(doc_name)[0])
                    if ext_prefix not in self.prefixes:
                        ext_ns = Namespace(ext_ns_uri)
                        self.prefixes[ext_prefix] = ext_ns
                        self.rdf_graph.bind(ext_prefix, ext_ns)
                        self.shacl_graph.bind(ext_prefix, ext_ns)
                    # Injective local name, for the same reason as `_class_iri`: folding `-` to
                    # `_` collapses two schema names onto one IRI.
                    return Namespace(ext_ns_uri)[format_local_name(ref_name)], None
```

- [ ] **Step 6: Run the covering tests, then invert the suppression guard**

```bash
uv run pytest tests/test_external_schema_ingestion.py tests/test_cross_document_refs.py tests/test_schema_namespaces.py tests/test_ttl_projection.py -v
```

Expected: PASS. Then invert `test_an_external_class_is_referenced_but_never_declared` (AC-9), since it passed before the fix and is therefore unproven: in `_parse_schemas`, add `for name, fact in self.mapping.classes.items():` emitting `self.rdf_graph.add((self._class_iri(name), RDF.type, RDFS.Class))` for external classes, confirm the test fails, then **revert the edit**.

- [ ] **Step 7: Commit**

```bash
git add openapi_to_rdf/shacl_converter.py tests/test_external_schema_ingestion.py
git commit -S -m "fix: an external class's IRI comes from the document that declares it

base_namespace was a value only the self path could see: a referring document
re-derived a sibling's namespace from its filename and could not know the
sibling had been handed one explicitly. The referring document then emitted an
IRI nothing declared. On a TM Forum split that means a 3GPP-prefixed IRI
inside a TM Forum vocabulary.

Adds document_namespaces (keyed by basename, as _external_schemas_map is) and
_namespace_for_document, whose four-step resolution puts base_namespace in
reach of the sibling path. _resolve_reference's external branch now goes
through _class_iri where the Mapping holds the class, so there is once again
one place a class IRI is minted.

An external class is referenced and never re-declared: _parse_schemas iterates
local schemas only, and a test pins that, observed failing with a declaration
loop added."
```

---

### Task 4: Teach the reconciliation gate about external classes

`scripts/reconcile_projections.py:93` builds `expected` from every entry of `mapping.classes`. Task 2 puts external classes in there and Task 3 deliberately keeps them out of the TTL, so AC-4's gate breaks unless it is told — and an exclusion with no assertion on its size is how a gate goes vacuous after a rename.

**Files:**
- Modify: `scripts/reconcile_projections.py:88-100`, `:196-205`
- Test: `tests/test_reconciliation.py`

**Interfaces:**
- Consumes: `ClassFact.is_external` (Task 2).
- Produces: `reconcile(...)` result gains `"external_classes_excluded": int`.

- [ ] **Step 1: Write the failing test**

Append to `tests/test_reconciliation.py`:

```python
def test_reconciliation_excludes_external_classes_and_counts_them(tmp_path):
    """An external class is deliberately absent from the TTL, so it must be out of `expected`.

    The count is asserted, not just the exclusion: a gate that quietly excludes an unbounded set
    reports clean while covering less and less.
    """
    import yaml

    from openapi_to_rdf import OpenAPIToSHACLConverter
    from scripts.reconcile_projections import reconcile

    common = tmp_path / "common.yaml"
    common.write_text(
        yaml.safe_dump(
            {
                "openapi": "3.0.0",
                "info": {"title": "Common", "version": "1.0"},
                "components": {
                    "schemas": {
                        "Addressable": {
                            "type": "object",
                            "properties": {"href": {"type": "string"}},
                        }
                    }
                },
            }
        )
    )
    api = tmp_path / "api.yaml"
    api.write_text(
        yaml.safe_dump(
            {
                "openapi": "3.0.0",
                "info": {"title": "Api", "version": "1.0"},
                "components": {
                    "schemas": {
                        "PolicyRef": {
                            "allOf": [
                                {"$ref": "common.yaml#/components/schemas/Addressable"},
                                {"type": "object",
                                 "properties": {"@type": {"type": "string"}}},
                            ]
                        }
                    }
                },
            }
        )
    )
    converter = OpenAPIToSHACLConverter(
        str(api),
        base_namespace="https://example.org/",
        output_dir=str(tmp_path / "out"),
        external_refs=[str(common)],
    )
    converter.convert()

    with open(api) as handle:
        document = yaml.safe_load(handle)
    result = reconcile(converter.mapping, doc=document, spec_path=api)

    assert result["external_classes_excluded"] == 1
    assert "Addressable" in converter.mapping.classes
    assert not result["only_in"]["ttl_only"]
```

- [ ] **Step 2: Run it to verify it fails**

```bash
uv run pytest tests/test_reconciliation.py -v -k external_classes
```

Expected: FAIL with `KeyError: 'external_classes_excluded'`.

- [ ] **Step 3: Exclude and count**

In `scripts/reconcile_projections.py`, replace the `expected` construction (line 93):

```python
    # An external class is declared by ITS OWN document and deliberately referenced-only here, so
    # it is not a term this document's TTL is expected to declare. The count is reported so a
    # rename that quietly widens this exclusion fails loudly instead of narrowing the gate.
    external_classes = [name for name, fact in mapping.classes.items() if fact.is_external]
    expected = {
        name: fact.iri for name, fact in mapping.classes.items() if not fact.is_external
    }
```

Restrict the two `for name, fact in mapping.classes.items()` loops that populate `ttl_classes` (line 76) and the transport pass (line 88) with `if fact.is_external: continue`, and add to the returned dict (near line 202):

```python
        "external_classes_excluded": len(external_classes),
        "external_classes": sorted(external_classes),
```

In `main()`, print it beside the TTL class count (line 222):

```python
    print(f"External classes excluded: {result.get('external_classes_excluded', 0)}")
```

- [ ] **Step 4: Run the covering tests**

```bash
uv run pytest tests/test_reconciliation.py -v
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add scripts/reconcile_projections.py tests/test_reconciliation.py
git commit -S -m "fix: the reconciliation gate excludes external classes, and says how many

Registering external ancestors puts them in mapping.classes while the TTL
deliberately does not declare them, so the AC-4 gate would report every one as
a missing term. Excluded, with the count asserted rather than the exclusion
alone: a gate that quietly excludes an unbounded set stays green while
covering less."
```

---

### Task 5: Repair the three decorative assertions

All three compare IRI **substrings**, so each passes today against the wrong IRI. Spec D5.

**Files:**
- Modify: `tests/test_cross_document_refs.py:100-128`, `:155-226`
- Test: the same file.

**Interfaces:**
- Consumes: Task 3's corrected emitter.
- Produces: nothing.

- [ ] **Step 1: Rewrite the assertions**

In `tests/test_cross_document_refs.py`, replace the assertion in `test_external_ref_as_property_target_resolves` (lines 116-128):

```python
    from rdflib import RDFS, URIRef

    # An IRI, not a substring. The previous form — any("CommonType" in str(r) …) — passed while
    # the IRI was http://ericsson.com/models/3gpp/rdf/base#CommonType, which is the exact wrong
    # answer it existed to catch. Its own comment conceded it ("the exact IRI depends on the
    # namespace assigned to external schemas"); the point of the fix is that it no longer does.
    assert URIRef("https://example.org/CommonType") in set(graph.objects(None, RDFS.range)), (
        f"range must be the shared-namespace IRI; got {sorted(str(r) for r in graph.objects(None, RDFS.range))}"
    )
```

and in `test_split_model_common_class_iri_is_stable` replace lines 221-226:

```python
    # The IRI, not a substring of it. This test's NAME claims IRI stability; the previous
    # assertion was any("TimePeriod" in parent …), which held while the IRI was
    # http://ericsson.com/models/3gpp/rdf/common#TimePeriod.
    from rdflib import URIRef

    assert (
        URIRef(f"{shared_namespace}Order"),
        RDFS.subClassOf,
        URIRef(f"{shared_namespace}TimePeriod"),
    ) in converter.rdf_graph, (
        "Order must be subClassOf the shared-namespace TimePeriod; got "
        f"{[(str(s), str(o)) for s, _p, o in converter.rdf_graph.triples((None, RDFS.subClassOf, None))]}"
    )
```

- [ ] **Step 2: Verify they now fail against the pre-fix code**

```bash
git stash push openapi_to_rdf/mapping.py openapi_to_rdf/shacl_converter.py openapi_to_rdf/property_uri.py
uv run pytest tests/test_cross_document_refs.py -v
```

Expected: both repaired tests FAIL, each showing an `http://ericsson.com/models/3gpp/rdf/...` IRI. This is the AC-9 evidence for Task 3. Then restore:

```bash
git stash pop
```

- [ ] **Step 3: Run them against the fix**

```bash
uv run pytest tests/test_cross_document_refs.py -v
```

Expected: PASS, all five tests in the module.

- [ ] **Step 4: Commit**

```bash
git add tests/test_cross_document_refs.py
git commit -S -m "test: compare cross-document IRIs, not substrings of them

Three assertions in this module tested `\"CommonType\" in str(iri)` and
`\"TimePeriod\" in parent`, so each passed while the IRI was the
filename-derived http://ericsson.com/models/3gpp/rdf/<doc># form — the exact
wrong answer they existed to catch. test_split_model_common_class_iri_is_stable
asserted less than its own name claimed.

Both were observed failing against the pre-fix converter, which is the
observed-failure evidence for the namespace fix."
```

---

### Task 6: `scripts/measure_split_isomorphism.py` — the whole-vs-split gate

AC-1 and AC-2. The comparison was written ad hoc twice during diagnosis; committing it is what makes this spec's figures re-derivable by someone who is not their author.

**Files:**
- Create: `scripts/measure_split_isomorphism.py`
- Test: `tests/test_external_schema_ingestion.py`

**Interfaces:**
- Consumes: `build_mapping`, `OpenAPIToSHACLConverter`, `scripts._corpora`.
- Produces: `split_document(document, fraction=0.5) -> tuple[dict, dict]` and `compare(path, namespace) -> dict` with keys `pairs_only_in_split`, `pairs_only_in_whole`, `graphs_isomorphic`, `whole_triples`, `split_triples`.

- [ ] **Step 1: Write the failing test**

Append to `tests/test_external_schema_ingestion.py`:

```python
def test_splitting_a_document_rewrites_its_refs():
    """The request's repro did not, which is why it proved nothing.

    Popping schemas out of components/schemas leaves `#/components/schemas/X` behind — an
    internal ref to an absent schema. external_schemas is never consulted, so the result is
    indistinguishable from an absent ancestor. The splitter must rewrite.
    """
    from scripts.measure_split_isomorphism import split_document

    document = {
        "openapi": "3.0.0",
        "info": {"title": "Api", "version": "1.0"},
        "components": {
            "schemas": {
                "Addressable": {"type": "object", "properties": {"id": {"type": "string"}}},
                "PolicyRef": {"allOf": [{"$ref": "#/components/schemas/Addressable"}]},
            }
        },
    }
    api, common = split_document(document, moved={"Addressable"})
    assert "Addressable" not in api["components"]["schemas"]
    assert api["components"]["schemas"]["PolicyRef"]["allOf"][0]["$ref"] == (
        "common.yaml#/components/schemas/Addressable"
    )
    assert "Addressable" in common["components"]["schemas"]


def test_a_split_document_attributes_exactly_as_the_whole_one_does(split_with_restatement):
    """AC-1, on the fixture that restates. Pre-fix this reports two extra pairs."""
    from scripts.measure_split_isomorphism import attribution_pairs

    whole = {
        "openapi": "3.0.0",
        "info": {"title": "Api", "version": "1.0"},
        "components": {
            "schemas": {
                **split_with_restatement["external"][COMMON],
                "PolicyRef": {
                    "allOf": [
                        {"$ref": "#/components/schemas/Addressable"},
                        split_with_restatement["api"]["components"]["schemas"]["PolicyRef"][
                            "allOf"
                        ][1],
                    ]
                },
            }
        },
    }
    namespace = "https://tmforum.org/ontology/"
    whole_pairs = attribution_pairs(
        build_mapping(whole, namespace=namespace)
    )
    split_pairs = attribution_pairs(
        build_mapping(
            split_with_restatement["api"],
            namespace=namespace,
            external_schemas=split_with_restatement["external"],
        )
    )
    assert not (split_pairs - whole_pairs), (
        f"split invented attributions absent from the whole document: {sorted(split_pairs - whole_pairs)}"
    )
```

- [ ] **Step 2: Run it to verify it fails**

```bash
uv run pytest tests/test_external_schema_ingestion.py -v -k "rewrites_its_refs or attributes_exactly"
```

Expected: FAIL with `ModuleNotFoundError: No module named 'scripts.measure_split_isomorphism'`.

- [ ] **Step 3: Write the script**

Create `scripts/measure_split_isomorphism.py`:

```python
"""Does converting one document whole agree with converting it split across two?

The acceptance gate for external schema ingestion. A description split into (common + api) must
yield the same vocabulary as the same description in one file — that is the premise of TM Forum's
split model ("one class, one IRI, referenced by N APIs") and it is what
`docs/superpowers/specs/2026-09-23-external-schema-ingestion-design.md` exists to restore.

**Why a committed script rather than a probe.** The comparison was written ad hoc twice during
diagnosis, and the first version did not rewrite `$ref` strings when it split — so it measured an
absent ancestor rather than an external one, and produced a number that looked like evidence. See
`split_document`: the rewrite is the load-bearing part.

Two measurements, because they fail independently:

* **Attribution** (`attribution_pairs`): the `(declaring_class, property)` pairs. A split that
  invents a pair the whole document does not have has attributed an inherited property to the
  leaf, minting a second IRI for one field.
* **Graph isomorphism** (`compare`): the RDF vocabulary of the whole document against the UNION of
  the split conversions. Isomorphism rather than byte or triple-count equality, for the reason
  `tests/test_output_freshness.py` records — rdflib's blank-node ordering is not stable across
  runs, so bytes are the wrong equivalence relation for a graph.

Usage:
    uv run python scripts/measure_split_isomorphism.py --tmforum-dir /path/to/tmf
    uv run python scripts/measure_split_isomorphism.py --corpus mine=path/to/one.yaml --json out.json
"""

from __future__ import annotations

import argparse
import copy
import json
import sys
from pathlib import Path
from typing import Any

import yaml
from rdflib import Graph

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from openapi_to_rdf import OpenAPIToSHACLConverter, build_mapping  # noqa: E402
from openapi_to_rdf.mapping import Mapping  # noqa: E402
from scripts import _corpora  # noqa: E402

#: The filename the moved half is written to, and the document qualifier refs are rewritten to.
COMMON_NAME = "common.yaml"


def attribution_pairs(mapping: Mapping) -> set[tuple[str, str]]:
    """``(declaring_class, property)`` for every LOCAL class in the mapping.

    External classes are excluded because their declaring document owns them: they appear in the
    split's mapping and not in the whole document's, and counting them would report a difference
    where the two agree.
    """
    return {
        (declaring, prop)
        for (declaring, prop) in mapping.properties_by_class
        if declaring in mapping.classes and not mapping.classes[declaring].is_external
    }


def _rewrite_refs(node: Any, moved: set[str], document_name: str) -> Any:
    """Rewrite every internal ``$ref`` naming a moved schema into its external form.

    **This is the step the diagnosis probe skipped.** Without it, a popped schema leaves
    `#/components/schemas/X` behind — an internal ref to something absent — so `external_schemas`
    is never consulted and the measurement cannot distinguish a cross-document reference from a
    missing ancestor. The request's own reproduction has this defect.
    """
    if isinstance(node, dict):
        rewritten = {}
        for key, value in node.items():
            if (
                key == "$ref"
                and isinstance(value, str)
                and value.startswith("#/components/schemas/")
                and value.rsplit("/", 1)[-1] in moved
            ):
                rewritten[key] = f"{document_name}#/components/schemas/{value.rsplit('/', 1)[-1]}"
            else:
                rewritten[key] = _rewrite_refs(value, moved, document_name)
        return rewritten
    if isinstance(node, list):
        return [_rewrite_refs(item, moved, document_name) for item in node]
    return node


def split_document(
    document: dict, *, moved: set[str] | None = None, fraction: float = 0.5
) -> tuple[dict, dict]:
    """Split one document into (api, common), rewriting refs across the new boundary.

    ``moved`` names the schemas that go to ``common``; by default the first ``fraction`` of the
    names in sorted order. Refs are rewritten in BOTH halves: a schema left in ``api`` may refer
    to a moved one, and a moved one may refer back to a schema that stayed.
    """
    schemas = (document.get("components") or {}).get("schemas") or {}
    if moved is None:
        names = sorted(schemas)
        moved = set(names[: int(len(names) * fraction)])

    api = copy.deepcopy(document)
    api_schemas = {n: d for n, d in schemas.items() if n not in moved}
    common_schemas = {n: copy.deepcopy(d) for n, d in schemas.items() if n in moved}

    api["components"]["schemas"] = _rewrite_refs(api_schemas, moved, COMMON_NAME)
    common = {
        "openapi": document.get("openapi", "3.0.0"),
        "info": {"title": "Common", "version": (document.get("info") or {}).get("version", "1.0")},
        # A ref from common to a schema that STAYED is external in the other direction. The api
        # document keeps the document's own name, so that is what those refs must point at.
        "components": {"schemas": common_schemas},
    }
    return api, common


def compare(path: Path, namespace: str, work_dir: Path) -> dict:
    """Convert ``path`` whole and split, and report whether the two agree."""
    with open(path, encoding="utf-8") as handle:
        document = yaml.safe_load(handle)

    whole_mapping = build_mapping(document, namespace=namespace)
    api, common = split_document(document)

    api_path = work_dir / "api.yaml"
    common_path = work_dir / COMMON_NAME
    api_path.write_text(yaml.safe_dump(api), encoding="utf-8")
    common_path.write_text(yaml.safe_dump(common), encoding="utf-8")

    split_mapping = build_mapping(
        api,
        namespace=namespace,
        external_schemas={COMMON_NAME: (common["components"]["schemas"])},
    )

    def convert(spec: Path, siblings: list[Path]) -> Graph:
        converter = OpenAPIToSHACLConverter(
            str(spec),
            base_namespace=namespace,
            output_dir=str(work_dir / "out"),
            external_refs=[str(s) for s in siblings],
        )
        converter.convert()
        return converter.rdf_graph

    whole_graph = convert(path, [])
    split_graph = convert(api_path, [common_path]) + convert(common_path, [api_path])

    whole_pairs = attribution_pairs(whole_mapping)
    split_pairs = attribution_pairs(split_mapping)
    return {
        "spec": path.name,
        "moved_schemas": len(common["components"]["schemas"]),
        "whole_triples": len(whole_graph),
        "split_triples": len(split_graph),
        "pairs_only_in_split": sorted(split_pairs - whole_pairs),
        "pairs_only_in_whole": sorted(whole_pairs - split_pairs),
        "graphs_isomorphic": whole_graph.isomorphic(split_graph),
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    _corpora.add_arguments(parser)
    parser.add_argument(
        "--namespace",
        default="https://tmforum.org/ontology/",
        help="the shared namespace both halves are converted under",
    )
    parser.add_argument(
        "--work-dir",
        type=Path,
        default=None,
        help="where the split halves are written (default: a temporary directory)",
    )
    args = parser.parse_args()

    import tempfile

    results: list[dict] = []
    failed = False
    with tempfile.TemporaryDirectory() as temporary:
        work_dir = args.work_dir or Path(temporary)
        work_dir.mkdir(parents=True, exist_ok=True)
        for corpus in _corpora.resolve(args):
            if not corpus:
                # Skip WITH the reason. A summary that quietly covers one corpus instead of two
                # is worse than one that fails, because its number still looks like a number.
                print(f"\n=== {corpus.label}: SKIPPED — {corpus.skip_reason} ===")
                continue
            print(f"\n=== {corpus.label}: {len(corpus.paths)} documents ===")
            for path in corpus.paths:
                result = compare(path, args.namespace, work_dir)
                result["corpus"] = corpus.label
                results.append(result)
                invented = len(result["pairs_only_in_split"])
                verdict = "OK" if invented == 0 and result["graphs_isomorphic"] else "DIVERGENT"
                failed = failed or verdict == "DIVERGENT"
                print(
                    f"  {path.name:55s} moved={result['moved_schemas']:4d} "
                    f"whole={result['whole_triples']:6d} split={result['split_triples']:6d} "
                    f"invented_pairs={invented:4d} isomorphic={result['graphs_isomorphic']!s:5s} "
                    f"{verdict}"
                )
                for pair in result["pairs_only_in_split"][:10]:
                    print(f"      invented: {pair[0]}.{pair[1]}")

    if not results:
        print("\nNo corpus was measured. Nothing is asserted by this run.", file=sys.stderr)
        return 2
    if args.json:
        args.json.write_text(json.dumps(results, indent=2), encoding="utf-8")
        print(f"\nWrote {args.json}")
    print(f"\n{'FAIL' if failed else 'PASS'}: {len(results)} documents compared")
    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
```

- [ ] **Step 4: Run the tests and the script's own entry point**

```bash
uv run pytest tests/test_external_schema_ingestion.py -v
uv run python scripts/measure_split_isomorphism.py --help
uv run python scripts/measure_split_isomorphism.py --corpus tmf620=/home/earejma/snm-api-native-src/tmf620-product-catalog-management-v5.yaml
```

Expected: tests PASS; `--help` prints usage; the run prints a table and exits 0. **Confirm the run emitted a table.** A checker that printed nothing has not passed — it has not run. If `main()` exits 2 with "No corpus was measured", the corpus resolution is wrong, not the converter.

- [ ] **Step 5: Commit**

```bash
git add scripts/measure_split_isomorphism.py tests/test_external_schema_ingestion.py
git commit -S -m "feat: scripts/measure_split_isomorphism.py — the whole-vs-split gate

Promotes the diagnosis probe to a committed entry point, so this work's
figures can be re-derived by someone who is not their author.

The load-bearing part is that split_document REWRITES \$ref strings across the
new boundary. The probe that did not — and the reproduction in the request
document — leaves #/components/schemas/X behind, an internal ref to an absent
schema, so external_schemas is never consulted and the measurement cannot tell
a cross-document reference from a missing ancestor.

Two independent measurements: invented (declaring_class, property) pairs, and
graph isomorphism of the whole document against the union of the split halves.
Isomorphism rather than bytes, for the reason test_output_freshness records."
```

---

### Task 7: The dangling-target metric, on a corpus that loads siblings

AC-3 and AC-8. `census_one` converts with `external_refs=[]` (`measure_corpora.py:75`), so **no metric it reports can move** in response to any of this work. That is the gate the request names as "the real gate".

**Files:**
- Modify: `scripts/measure_corpora.py`
- Test: `tests/test_tmforum_corpus.py`

**Interfaces:**
- Consumes: `ClassFact.is_external` (Task 2), the corrected emitter (Task 3).
- Produces: `census_one(path, siblings=())`; corpus result key `dangling_class_targets`; `--load-siblings` flag.

- [ ] **Step 1: Write the failing test**

Append to `tests/test_tmforum_corpus.py`:

```python
def test_the_corpus_census_can_load_siblings(tmp_path):
    """The gate must be able to reach cross-document resolution at all.

    Pre-fix, census_one hardcoded external_refs=[], so every cross-document ref was unresolved
    and no metric it reported could move no matter what the converter did. This asserts the
    instrument can produce a positive.
    """
    import yaml

    from scripts.measure_corpora import census_one

    common = tmp_path / "common.yaml"
    common.write_text(
        yaml.safe_dump(
            {
                "openapi": "3.0.0",
                "info": {"title": "Common", "version": "1.0"},
                "components": {
                    "schemas": {
                        "Addressable": {"type": "object",
                                        "properties": {"href": {"type": "string"}}}
                    }
                },
            }
        )
    )
    api = tmp_path / "api.yaml"
    api.write_text(
        yaml.safe_dump(
            {
                "openapi": "3.0.0",
                "info": {"title": "Api", "version": "1.0"},
                "components": {
                    "schemas": {
                        "PolicyRef": {
                            "allOf": [
                                {"$ref": "common.yaml#/components/schemas/Addressable"},
                                {"type": "object",
                                 "properties": {"@type": {"type": "string"}}},
                            ]
                        }
                    }
                },
            }
        )
    )

    without = census_one(api)
    with_sibling = census_one(api, siblings=(common,))
    assert without["unresolved_refs"] > 0, "the no-siblings arm must show the ref unresolved"
    assert with_sibling["unresolved_refs"] == 0, (
        f"the sibling arm must resolve it; got {with_sibling['_unresolved']}"
    )
    assert with_sibling["dangling_class_targets"] == 0
```

- [ ] **Step 2: Run it to verify it fails**

```bash
uv run pytest tests/test_tmforum_corpus.py -v -k can_load_siblings
```

Expected: FAIL with `TypeError: census_one() got an unexpected keyword argument 'siblings'`.

- [ ] **Step 3: Add the sibling arm and the metric**

In `scripts/measure_corpora.py`, add the metric name to the `METRICS` tuple (line 51-65), after `unresolved_refs_distinct`:

```python
    "dangling_class_targets",
```

Replace `census_one`'s signature and the converter construction (lines 73-76):

```python
def census_one(path: Path, siblings: tuple[Path, ...] = ()) -> dict:
    """Convert one document in memory and count everything the summary reports.

    ``siblings`` are the other documents of the OAD, loaded so cross-document ``$ref`` resolves.
    It defaults to empty, and for six months that default was the only behaviour — which meant no
    metric in this file could move in response to a cross-document defect, in either direction.
    `dangling_class_targets` in particular is identically 0 without siblings, because an
    unresolved ref emits no triple at all and a triple that is never emitted cannot dangle.
    """
    converter = OpenAPIToSHACLConverter(
        str(path), external_refs=[str(sibling) for sibling in siblings]
    )
    converter.convert()
```

Add the metric alongside the other counts, before the `return` (near line 117):

```python
    # A class target no document DECLARES. This is what D1 produced: the declaring document minted
    # a class under one namespace while every referring document re-derived another from the
    # filename, so the arrow pointed at nothing. Counted per document against that document's own
    # declarations; `dangling_class_targets_corpuswide` in `census` is the number that matters,
    # because a target declared by a SIBLING is correct and only the union can see that.
    declared_here = set(rdf.subjects())
    local_dangling = {
        o
        for predicate in (RDFS.subClassOf, RDFS.range)
        for o in rdf.objects(None, predicate)
        if isinstance(o, URIRef)
        and not str(o).startswith(str(XSD))
        and not str(o).startswith("http://www.w3.org/")
        and o not in declared_here
    }
```

and in the returned dict:

```python
        "dangling_class_targets": len(local_dangling),
        "_dangling": sorted(str(o) for o in local_dangling),
        "_declared": sorted(str(s) for s in declared_here),
```

In `census` (line 143), thread siblings through and compute the corpus-wide figure, which is the one AC-3 asserts:

```python
def census(corpus: _corpora.Corpus, load_siblings: bool = True) -> dict:
```

and inside its loop over `corpus.paths`, replace the `census_one(path)` call with:

```python
            siblings = tuple(p for p in corpus.paths if p != path) if load_siblings else ()
            row = census_one(path, siblings=siblings)
```

After the loop, beside the other corpus-wide aggregates (near line 171):

```python
    # Corpus-wide, because a target declared by a SIBLING is correct: only the union of the
    # corpus's graphs can tell a cross-document reference from a dangling one. Pre-fix this read
    # 175 on 3GPP (34 subClassOf + 141 range) against 0 rdfs:domain, which was the control.
    all_declared: set[str] = set()
    for row in rows:
        all_declared |= set(row["_declared"])
    dangling_corpuswide = {
        target for row in rows for target in row["_dangling"] if target not in all_declared
    }
```

and in its returned dict:

```python
        "dangling_class_targets_corpuswide": len(dangling_corpuswide),
        "dangling_class_targets_examples": sorted(dangling_corpuswide)[:20],
```

Print it in `print_corpus` beside the other corpus-wide keys (line 191-197 list) by adding `"dangling_class_targets_corpuswide"` to that tuple, and add a verdict line after the declared-terms verdict (line 205):

```python
    dangling = result["dangling_class_targets_corpuswide"]
    print(
        f"  -> dangling class targets (no document in the corpus declares them): {dangling} "
        + ("OK" if dangling == 0 else "FAIL")
    )
    for example in result["dangling_class_targets_examples"]:
        print(f"       {example}")
```

Add `--no-load-siblings` to `main()`'s parser so the old behaviour stays reachable for comparison, defaulting to loading them:

```python
    parser.add_argument(
        "--no-load-siblings",
        action="store_true",
        help=(
            "convert each document alone, as this script did before 2026-09-23. Every "
            "cross-document $ref is then unresolved and dangling_class_targets is identically 0 "
            "— useful only for reproducing the old figures."
        ),
    )
```

and pass `load_siblings=not args.no_load_siblings` into `census`. Make the script's exit status non-zero when `dangling_class_targets_corpuswide` is non-zero for any measured corpus, so it is a gate and not a report.

Add `URIRef` to the `rdflib` import at the top if it is not already there.

- [ ] **Step 4: Run the covering tests, then invert the metric**

```bash
uv run pytest tests/test_tmforum_corpus.py -v
```

Expected: PASS. Then invert (AC-9): in `census_one`, change `o not in declared_here` to `False` and confirm `test_the_corpus_census_can_load_siblings` fails on `dangling_class_targets == 0`. Restore.

- [ ] **Step 5: Commit**

```bash
git add scripts/measure_corpora.py tests/test_tmforum_corpus.py
git commit -S -m "feat: measure_corpora loads siblings, and counts dangling class targets

census_one converted with external_refs=[], so no metric in this file could
move in response to a cross-document defect — in either direction. The request
names this script as the real gate for external schema ingestion; it could not
observe it at all.

Adds the sibling arm (default on, --no-load-siblings to reproduce the old
figures) and dangling_class_targets: a subClassOf/range object that no
document in the corpus DECLARES. Corpus-wide rather than per-document, because
a target declared by a sibling is correct and only the union can tell.

Non-zero now exits non-zero, so it is a gate and not a report. The metric was
observed failing with its declaration check disabled."
```

---

### Task 8: Measure both corpora, and record what the numbers mean

AC-3 and AC-8, and the point at which the plan's predictions become results. **Nothing in this task is a code change.** The expectation is stated before the run: 3GPP `dangling_class_targets_corpuswide` **0**, against the pre-fix **175**.

**Files:**
- Create: `artifacts/external-schema-ingestion.json`
- Modify: `HYPOTHESES.md`

**Interfaces:**
- Consumes: Tasks 6 and 7's entry points.
- Produces: the committed artifact both figures are read from afterwards.

- [ ] **Step 1: Run the 3GPP corpus with siblings loaded**

This converts 38 documents each loading 37 siblings, so it is **minutes, not seconds** — say so before starting it and do not poll it with `sleep`.

```bash
uv run python scripts/measure_corpora.py --json artifacts/external-schema-ingestion.json
```

Read the **real exit status, unpiped**. Record the printed `dangling_class_targets_corpuswide` for both corpora.

- [ ] **Step 2: Compare against the stated expectation**

| corpus | metric | pre-fix | expectation | measured |
|---|---|---|---|---|
| 3GPP | `dangling_class_targets_corpuswide` | 175 | 0 | _fill in_ |
| 3GPP | `properties_multi_range` | 0 | 0 | _fill in_ |
| TM Forum | `dangling_class_targets_corpuswide` | not measured | 0 | _fill in_ |

If 3GPP is not 0, **do not weaken the metric.** Read the remaining targets: the 9 non-filename-family `rdfs:range` danglers identified in the spec are `ClassFact.referent` names minted by convention, which `mapping.py`'s module docstring documents as legitimately naming a class the document does not declare. If the residue is exactly those, that is a finding about the metric's definition and it should exclude referents explicitly, with the count asserted — not lowered to whatever the run produced.

- [ ] **Step 3: Run the isomorphism gate on both corpora**

```bash
uv run python scripts/measure_split_isomorphism.py --json artifacts/split-isomorphism.json
```

Expected: exit 0, `invented_pairs=0` on every document. On TMF620 the pre-fix figure is 34.

- [ ] **Step 4: Run the reconciliation gate**

```bash
uv run python scripts/reconcile_projections.py
```

Expected: clean, and it prints a non-zero "External classes excluded" for any document with external refs. Zero everywhere would mean the exclusion is not being exercised — check before accepting it.

- [ ] **Step 5: Update `HYPOTHESES.md`**

Move D1 and D2 into **Settled** with their evidence and the script that re-derives each; add D3 and D4 under a "Recorded, deferred" note with the grounds for deferring D4 (a separate emitter, not reached by any code this change touches); and remove the AC-8 open question's claim that "TM Forum is asserted, not measured" if this run measured it. Every figure must name the script that produced it — do not retype a number that a committed script owns.

- [ ] **Step 6: Commit**

```bash
git add artifacts/external-schema-ingestion.json artifacts/split-isomorphism.json HYPOTHESES.md
git commit -S -m "measure: both corpora after external schema ingestion

Stated before the run: 3GPP dangling_class_targets_corpuswide 175 -> 0, and
invented (declaring_class, property) pairs 34 -> 0 on TMF620. Measured figures
in artifacts/, re-derivable by scripts/measure_corpora.py and
scripts/measure_split_isomorphism.py.

HYPOTHESES.md: D1 and D2 settled with evidence; D3 and D4 recorded as
deferred, D4 on the verified grounds that rdf_converter.py is a separate
emitter no code in this change reaches."
```

---

### Task 9: Regenerate the published `output/` tree

Every cross-document IRI in the deliverable moves, so `test_output_freshness` fails until this runs. Decided with the user: same change, not a follow-up.

**Files:**
- Modify: `output/rdf/*.ttl`, `output/shacl/*.ttl`, `output/index/*.yaml`
- Modify: `scripts/regenerate_output.py`
- Test: `tests/test_output_freshness.py`

**Interfaces:**
- Consumes: Task 3's `document_namespaces`.
- Produces: a deliverable tree whose cross-document arrows resolve.

- [ ] **Step 1: Make the regeneration state its own namespace table**

`scripts/regenerate_output.py:62` passes `base_namespace=f"https://example.org/{spec_path.stem}/"` per document, which is exactly the argument no sibling could see. Now that siblings can be told, state it once and pass it to every conversion:

```python
    # One table, shared by every conversion, so a document and its referrers agree on where each
    # class lives. Passing base_namespace per document while leaving siblings to re-derive from
    # the filename is what left 175 dangling class targets in this tree — see
    # docs/superpowers/specs/2026-09-23-external-schema-ingestion-design.md (D1).
    document_namespaces = {
        path.name: f"https://example.org/{path.stem}/" for path in specs
    }

    for spec_path in specs:
        print(f"  {spec_path.name}")
        external_refs = [p.name for p in gpp_dir.glob("*.yaml") if p != spec_path]
        converter = OpenAPIToSHACLConverter(
            str(spec_path),
            base_namespace=document_namespaces[spec_path.name],
            output_dir=str(output_dir),
            external_refs=external_refs,
            document_namespaces=document_namespaces,
        )
        converter.run()
```

- [ ] **Step 2: Regenerate**

Minutes, not seconds — announce it.

```bash
uv run python scripts/regenerate_output.py
```

- [ ] **Step 3: Confirm the tree improved, with a number**

```bash
uv run python - <<'PY'
from pathlib import Path
from rdflib import Graph, RDFS, URIRef
g = Graph()
files = sorted(Path("output/rdf").glob("*.ttl"))
for f in files:
    g.parse(f, format="turtle")
declared = set(g.subjects())
for predicate, label in ((RDFS.subClassOf, "subClassOf"), (RDFS.range, "range"), (RDFS.domain, "domain")):
    targets = {o for o in g.objects(None, predicate)
               if isinstance(o, URIRef) and not str(o).startswith("http://www.w3.org/")}
    dangling = {o for o in targets if o not in declared}
    print(f"{label:11s} {len(dangling):4d} of {len(targets):4d} distinct targets dangle")
PY
```

Expected, stated before running: `subClassOf` **0 of 46** (was 34 of 46), `range` **0 or 9 of 797** (was 141 of 797 — the 9 are convention-minted referents), `domain` **0 of 889** unchanged as the control. If `range` is neither 0 nor 9, read the residue before accepting it.

- [ ] **Step 4: Check the file count and run the freshness gate**

```bash
git status --short output/ | head -20
ls output/rdf output/shacl output/index | wc -l
uv run pytest tests/test_output_freshness.py -v
```

Expected: PASS. The tracked file count stays at **114** — a *new* file under `output/` means a test wrote into the deliverable, which `tests/conftest.py` is supposed to prevent. `.ttl` files are compared by graph isomorphism and everything else by bytes, so a serialisation-only diff is not a failure.

- [ ] **Step 5: Run the full suite**

This is the task boundary that claims completion, so the whole suite runs — ~491s.

```bash
uv run pytest -q
```

Expected: the 49 pre-existing `test_3gpp_shacl_coverage.py::test_bad_instance_rejected` failures are **still there and still 49** — `HYPOTHESES.md` H1–H3 record them as a defect in the test data, not the converter, and they are not a blocker for this work. **Any other failure, and any change in that count, is this change's.** Report the count against 791 passed / 49 failed / 1 xfailed as the pre-existing baseline.

- [ ] **Step 6: Commit**

```bash
git add output/ scripts/regenerate_output.py
git commit -S -m "chore: regenerate output/ — cross-document arrows now resolve

Every cross-document class IRI in the deliverable moves, because the referring
document no longer re-derives a sibling's namespace from its filename.
regenerate_output.py now states one document->namespace table and passes it to
every conversion, instead of handing each document a base_namespace that no
sibling could see.

Measured over the regenerated tree: rdfs:subClassOf dangling targets 34 of 46
-> 0 of 46, rdfs:range 141 of 797 -> 0, with rdfs:domain unchanged at 0 of 889
as the control. The 38 documents join up for the first time.

Suite unchanged apart from this: the 49 test_bad_instance_rejected failures
are the pre-existing test-data defect recorded as H1-H3 in HYPOTHESES.md."
```

---

## Self-Review

**Spec coverage.** D1 → Tasks 1, 3, 9. D2 → Task 2. D3 → Task 2 (Steps 4, 8, 9) and AC-10. D4 → recorded only, by decision, and re-recorded in Task 8 Step 5. D5 → Tasks 5 and 7. Part 1 (namespace resolver) → Tasks 1 and 3. Part 2 (registration, reference-only) → Tasks 2 and 3, with the projection table's TTL row pinned by `test_an_external_class_is_referenced_but_never_declared` and its reconcile row by Task 4. Part 3 items 1–6 → Tasks 7, 6, 5, 9, 8 respectively. AC-1/AC-2 → Task 6. AC-3 → Tasks 7, 8. AC-4 → Task 2. AC-5 → Task 2 Step 8. AC-6 → Task 4. AC-7 → Task 3. AC-8 → Task 8. AC-9 → inversion steps in Tasks 2, 3, 5, 7. AC-10 → Task 2 Step 8.

**Gaps accepted deliberately.** The spec's projection table says the JSON-LD `@context` should carry inherited external terms; no task adds a test for it, because `projections/context.py:75-91` already walks `ClassFact.parents` and the behaviour follows from Task 2 with no code change. If Task 2's review finds the walk does not reach external ancestors, that is a new task, not a silent omission.

**Type consistency.** `declaring_document: str | None` and the derived `is_external` are used identically in Tasks 2, 3, 4 and 6. `external_namespaces` is `build_mapping`'s parameter and `document_namespaces` is the converter's — deliberately different names for different scopes (the converter's map includes itself; the mapping's does not), stated in both docstrings. `census_one(path, siblings=())` matches its call in `census`. `attribution_pairs` and `split_document(document, *, moved=None, fraction=0.5)` match their uses in Task 6's tests.
