# TM Forum Open API v5 documents — where these came from

Two of TM Forum's Open API v5 descriptions, redistributed here under Apache-2.0. They are the
second corpus this project measures against, and the reason a second corpus exists at all: several
determinations were implemented and validated against 3GPP and turned out to be **invisible** there.
`tests/test_tmforum_corpus.py` records the measurements.

## Provenance, verified 2026-09-25

| file | upstream | verification |
|---|---|---|
| `TMF620-Product_Catalog_Management-v5.0.0.oas.yaml` | [`tmforum-apis/TMF620_ProductCatalog`](https://github.com/tmforum-apis/TMF620_ProductCatalog) at commit `24e69f3026f2b514baa27d7206540fc7b55bcb45` | fetched and compared byte-for-byte against the working copy this project had been using; SHA-256 `a84bc2e456b611c5f2fe3cb82a7f5d6b68e9589a43bcdbe8b5c0a0c3a7429351` |
| `TMF622-ProductOrdering-v5.0.0.oas.yaml` | [`tmforum-apis/TMF622_ProductOrder`](https://github.com/tmforum-apis/TMF622_ProductOrder) at commit `a3825f95248eb04fb1c7e2321305bc99631168a4` | same; SHA-256 `833d15f23c6e79c43594e68a236821669f1257426dedf51111e98fccdf30a3ba` |

`LICENSE` in this directory is the Apache-2.0 text as published in those repositories. The GitHub
organisation describes itself as *"TM Forum Open APIs under Apache 2.0 licence mode"*, and both
repositories carry `Apache-2.0` as their declared licence. **TM Forum has not produced, reviewed or
endorsed anything this project derives from these documents.**

The digests above are recorded as provenance, not as a gate: **nothing in the test suite asserts
them.** They say what was fetched and when. If they are ever turned into an assertion, that
assertion has to read them from one place rather than copying them again.

## What is NOT here, and why

**TMF641 Service Ordering v5 is absent.** The project has been measuring against a
`tmf641-service-ordering-v5.yaml`, and its origin could not be established:

* `tmforum-apis/TMF641_ServiceOrder` — the only Service Ordering repository in the Apache-2.0
  organisation — holds **no v5 document**. One branch, tags stopping at `v4.2.0`, and Swagger 2.0
  JSON only, despite being pushed as recently as 2026-08-15.
* `tmforum-rand` (TM Forum's other, non-Apache licence mode) has TMF641 repositories, but they are
  R16.5–R18 era, last pushed 2021, and declare no licence.
* `tmforum.org` itself sits behind a Cloudflare bot challenge — verified as the "Just a moment…"
  interstitial with a browser user-agent, so it is bot protection rather than a login wall, and not
  scriptable either way.

So that document's licence is **unverified**, and this repository is public. It is deliberately not
redistributed here on that basis. Anyone measuring the TMF641 figures quoted elsewhere in this
repository needs their own copy, and `scripts/_corpora.py` resolves it from a path.

## Why these are vendored when the 3GPP corpus is not

`assets/MnS-Rel-19-OpenAPI/` is **fetched** (`scripts/fetch_corpus.py`, pinned and digest-verified)
and gitignored, and this directory is the opposite. That is a deliberate asymmetry, not an oversight:

* **3GPP** is 38+ documents, openly published on a git forge with a resolvable ref, and the committed
  snapshot turned out to be re-derivable from **no** ref — so a fetcher earned its keep.
* **TM Forum** is two fetchable documents. A fetcher for two files is more machinery than the
  problem: there is no v5 *tag* to pin (tags stop at v4.2.0), so a pin would have to name a commit on
  a moving branch, and the whole apparatus would exist to avoid copying 1.1 MB that Apache-2.0
  explicitly permits copying.

If TM Forum publishes more v5 documents to GitHub, that trade changes and a fetcher becomes worth
revisiting.
