# Changelog

All notable changes to this project will be documented in this file.

The format is loosely based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html)
as much as possible for a pre-1.0 project.

## [0.2.0]

### Breaking changes

- **Property URIs are now class-scoped.** Previously, a property named
  `status` declared on two different OpenAPI schemas produced a single RDF
  property `<base>#status`. That silently conflated two semantically
  distinct fields and, in the OWL output, created unintended
  `rdfs:domain` / `rdfs:range` intersections. Each property now lives
  under a per-class namespace, e.g.:

      <base>/<ClassName>#<propertyName>

  so `TimeWindow.startTime` becomes
  `.../TS28623/ComDefs/TimeWindow#startTime`, distinct from
  `PerfMetricJob.startTime`. By construction, every property now has
  exactly one `rdfs:domain` and one `rdfs:range`.

  Any downstream consumer that references property URIs built under the
  previous flat scheme needs to update. The new URIs are deterministic
  and can be reconstructed from `(base_namespace, class_name, property_name)`
  via `openapi_to_rdf.property_uri.property_uri(...)`.

### Added

- **Property index sidecar (`*_property_index.yaml`).** For every
  converted input file, a YAML manifest is now written alongside the
  TTL output (under `<output_dir>/index/`) listing every
  `(local_name, uri, owner_class, range, description)` tuple produced
  and flagging any same-named properties that disagree on range or
  description as collisions. The file is the intended input for a
  future, opinion-driven `merge` subcommand (Stage 2). See
  `CONVERSION_DOC.md` section "Property identity and merging" for the
  schema.

- New module `openapi_to_rdf.property_uri` exposing
  `class_namespace(base, class)` and
  `property_uri(base, class, prop)` — used by both converters and
  importable by downstream tools that need to reconstruct URIs.

- New module `openapi_to_rdf.property_index` with the `PropertyIndex`
  collector/writer.

- New TDD test suites:
  - `tests/test_property_uri.py` (helper unit tests)
  - `tests/test_rdf_converter_tdd.py` (OWL-path collision fixture)
  - `tests/test_property_index.py` (sidecar unit + integration tests)
  - collision-fixture tests in `tests/test_conversion_tdd.py`

### Fixed

- **Per-class enum constraints on same-named properties no longer
  collapse.** Under the old flat URI scheme, when two OpenAPI schemas
  declared a property with the same name but different `enum` value
  sets (e.g. `DelayTolerance.support` with `[SUPPORTED, NOT_SUPPORTED]`
  and `UserMgmtOpen.support` with `[YES, NO]`), the converter attached
  `sh:in` to only one of the two generated property shapes. Validation
  of instances of the "loser" class silently accepted any string. With
  class-scoped URIs each class's property shape now carries its own
  `sh:in`, and invalid enum values are correctly rejected. This
  resolves 22 previously-failing cases in
  `tests/test_3gpp_shacl_coverage.py` covering the 3GPP
  `TS28541_SliceNrm` schemas.

### Changed

- `shacl_converter._process_property` no longer contains the
  range-conflict branch or the "remove prior domain" logic: with
  class-scoped URIs, every property is unique to its owning class.
- `rdf_converter._process_property` (OWL path) similarly mints
  class-scoped URIs with a single `rdfs:domain` / `rdfs:range` each.
- The OpenAPI document-header path (`_convert_openapi_header`) now
  scopes its `version`, `description`, and `url` properties under the
  metadata class namespace.

### Deferred

- **Stage 2 (merging step)** is not included in this release. The
  property-index YAML is designed to be consumed by a future
  `openapi-to-rdf merge` subcommand that applies user-edited merge
  groups and emits `owl:unionOf` domain/range or
  `owl:equivalentProperty` as appropriate.
