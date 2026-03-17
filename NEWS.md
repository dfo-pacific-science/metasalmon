metasalmon 0.0.14
----------------

- Simplified the package-creation surface so `create_sdp()` is the clear one-shot entrypoint, `write_salmon_datapackage()` is the advanced/manual writer, and the older create-from-data helper was removed.
- Reworked `create_sdp()` output into a cleaner review layout with `metadata/` and `data/` subdirectories, package-root `README-review.txt`, package-root `semantic_suggestions.csv` (when present), and root `datapackage.json`.
- Rewrote `README-review.txt` as a step-by-step checklist that explains the canonical Salmon Data Package, how to share the full package folder (or zip), and how to return to R for validation.
- Tightened default semantic seeding so code-level semantic suggestions run only for factor/categorical source columns by default, while keeping column-level and table-level seeding available.
- Added optional update notifications inside `create_sdp()` via `check_updates`, using the explicit `check_for_updates()` helper rather than package-attach network checks.
- Refreshed README, vignettes, reference pages, generated documentation, tests, and pkgdown outputs to match the new workflow and layout.

metasalmon 0.0.13
----------------

- Made `edh_build_iso19139_xml()` default to the richer North American Profile / HNAP-aware EDH export while keeping `profile = "iso19139"` available as an explicit fallback.
- Expanded EDH export support for bilingual locale scaffolding, deterministic identifiers, legal constraints, maintenance/status, reference systems, bounding boxes, and distribution metadata, with regression coverage against the confirmed EDH sample shape.
- Added `apply_semantic_suggestions()` for explicit opt-in merges of `suggest_semantics()` results into dictionaries.
- Restored canonical Salmon Data Package CSVs (`dataset.csv`, `tables.csv`, `column_dictionary.csv`, optional `codes.csv`) as the source of truth in `read_salmon_datapackage()`, treating `datapackage.json` as derived/interoperability metadata.
- Refreshed README, vignettes, pkgdown reference metadata, and GPT collaboration guidance to match the EDH default/export semantics and explicit dictionary-application workflow.
- Rebuilt package documentation, tests, source tarball, and pkgdown site for the 0.0.13 release.

metasalmon 0.0.12
----------------

- Added a GCDFO-backed `find_terms()` search backend that queries the DFO Salmon Ontology first via content negotiation against `https://w3id.org/gcdfo/salmon`.
- For salmon-domain roles, `find_terms()` now prioritizes GCDFO results and only falls back to OLS/NVS when GCDFO returns no good label hit.
- Updated `suggest_semantics()`, `infer_dictionary(seed_semantics = TRUE)`, man pages, and vignettes to reflect the new GCDFO-first search behavior.
- Rebuilt package documentation, tests, source tarball, and pkgdown site for the 0.0.12 release.

metasalmon 0.0.11
----------------

- Added optional semantic seeding to `infer_dictionary()` via
  `seed_semantics = TRUE`, with optional source/max-per-role controls
  (`semantic_sources`, `semantic_max_per_role`).
  - This returns dictionary suggestions via
    `attr(dict, "semantic_suggestions")` without changing existing defaults.
- Added guidance at the package README quick example that keeps the home-page flow
  short and links to 5-minute Quickstart + dedicated deep-dive articles.
- Marked related vignettes as workflow-specific to avoid duplicating the Quickstart
  path; `data-dictionary-publication` and `reusing-standards-salmon-data-terms`
  now orient users to post-Quickstart use.

metasalmon 0.0.10
----------------

- Changed `validate_dictionary()` and `validate_semantics()` non-strict semantics:
  - missing `term_iri`, `property_iri`, `entity_iri`, and `unit_iri` on
    `column_role == "measurement"` no longer block package creation by default;
  - missing fields now trigger a strong warning that calls out next steps and points to `suggest_semantics()` plus the standards guide.
- Preserved strict validation when `require_iris = TRUE` so CI/high-assurance flows can still enforce full semantic coverage.
- Updated `README`, man pages, and tests to document and verify the new behavior.
- Added `metasalmon` package release metadata for version 0.0.10.

metasalmon 0.0.9
----------------

- Added `edh_build_iso19139_xml()` to generate starter ISO 19139 metadata XML for DFO Enterprise Data Hub / GeoNetwork upload workflows.
- Added tests and reference documentation for the EDH XML export helper.
- Updated dataset metadata examples/templates to better support EDH workflows:
  - Expanded `inst/extdata/dataset.csv` with `contact_org`, `contact_position`, `update_frequency`, `topic_categories`, `keywords`, and `security_classification`.
  - Updated `inst/extdata/custom-gpt-prompt.md` to distinguish controlled `topic_categories` from free-text `keywords` and to note XML export support.
  - Refreshed README and vignette examples to include EDH-ready optional metadata and XML export guidance.

metasalmon 0.0.8
----------------

- Added and documented NuSEDS method crosswalk helpers:
  - `nuseds_enumeration_method_crosswalk()`
  - `nuseds_estimate_method_crosswalk()`
- Added reference documentation pages for both crosswalk helpers.
- Refreshed README feature list to include the new NuSEDS crosswalk utilities.

metasalmon 0.0.6
----------------

- Added `read_github_csv_dir()` to read all CSV files from a GitHub directory into a named list, similar to using `dir()` with `lapply()` for local files.
- Supports pattern matching, version pinning, and passes options to `read_csv()` for all files.
- Added comprehensive test coverage for the new function.

metasalmon 0.0.5
----------------

- Renamed the GitHub CSV helpers to generic names: `github_raw_url()` and `read_github_csv()`. `repo` is now required unless you provide a full URL.

metasalmon 0.0.4
----------------

- Added `ms_setup_github()` to guide one-time PAT setup (git check, browser token creation, git credential storage) and verify access to the private Qualark data repository.
- Added `qualark_raw_url()` and `read_qualark_csv()` to build stable raw GitHub URLs and read Qualark CSVs using the stored PAT (with SSO-aware error messages and retry logic).
- New tests cover URL construction, blob/raw URL normalization, and an opt-in Qualark fetch when a token is configured.

metasalmon 0.0.3
----------------

- Added `find_terms()` function for searching candidate terms across external vocabularies (OLS, NVS, BioPortal).
- `find_terms()` now ranks results deterministically using I-ADOPT role hints from `inst/extdata/iadopt-terminologies.csv` (preferred vocabularies boosted; ties stable).
- `suggest_semantics()` now returns best-effort suggestions (stored in `attr(,'semantic_suggestions')`) instead of a placeholder message.
- Added I-ADOPT component fields (`property_iri`, `entity_iri`, `constraint_iri`, `method_iri`) to dictionary schema and package creation/reading.
- Enhanced validation: measurement columns now require I-ADOPT components (`term_iri`, `property_iri`, `entity_iri`, `unit_iri`).
- Updated table metadata: renamed `entity_type`/`entity_iri` to `observation_unit`/`observation_unit_iri` for clarity.
- Added `httr` package dependency for vocabulary search functionality.
- Dictionary validation now normalizes optional semantic columns and returns the normalized dictionary.
- Vignettes now show end-to-end semantic enrichment (I-ADOPT-aware suggestions) and how to align with `smn-gpt`.

metasalmon 0.0.2
----------------

- Unified semantic fields to `term_iri` + `term_type` and reserved `concept_scheme_iri` for code lists only.
- Updated GPT collaboration guidance, schemas, and pkgdown outputs to match the new fields.
- Refreshed vignettes, tests, and reference docs; bumped package version.

metasalmon 0.0.1
----------------

- Initial development snapshot.
