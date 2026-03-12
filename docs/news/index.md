# Changelog

## metasalmon 0.0.11

- Added optional semantic seeding to
  [`infer_dictionary()`](https://dfo-pacific-science.github.io/metasalmon/reference/infer_dictionary.md)
  via `seed_semantics = TRUE`, with optional source/max-per-role
  controls (`semantic_sources`, `semantic_max_per_role`).
  - This returns dictionary suggestions via
    `attr(dict, "semantic_suggestions")` without changing existing
    defaults.
- Added guidance at the package README quick example that keeps the
  home-page flow short and links to 5-minute Quickstart + dedicated
  deep-dive articles.
- Marked related vignettes as workflow-specific to avoid duplicating the
  Quickstart path; `data-dictionary-publication` and
  `reusing-standards-salmon-data-terms` now orient users to
  post-Quickstart use.

## metasalmon 0.0.10

- Changed
  [`validate_dictionary()`](https://dfo-pacific-science.github.io/metasalmon/reference/validate_dictionary.md)
  and
  [`validate_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/validate_semantics.md)
  non-strict semantics:
  - missing `term_iri`, `property_iri`, `entity_iri`, and `unit_iri` on
    `column_role == "measurement"` no longer block package creation by
    default;
  - missing fields now trigger a strong warning that calls out next
    steps and points to
    [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md)
    plus the standards guide.
- Preserved strict validation when `require_iris = TRUE` so
  CI/high-assurance flows can still enforce full semantic coverage.
- Updated `README`, man pages, and tests to document and verify the new
  behavior.
- Added `metasalmon` package release metadata for version 0.0.10.

## metasalmon 0.0.9

- Added
  [`edh_build_iso19139_xml()`](https://dfo-pacific-science.github.io/metasalmon/reference/edh_build_iso19139_xml.md)
  to generate starter ISO 19139 metadata XML for DFO Enterprise Data Hub
  / GeoNetwork upload workflows.
- Added tests and reference documentation for the EDH XML export helper.
- Updated dataset metadata examples/templates to better support EDH
  workflows:
  - Expanded `inst/extdata/dataset.csv` with `contact_org`,
    `contact_position`, `update_frequency`, `topic_categories`,
    `keywords`, and `security_classification`.
  - Updated `inst/extdata/custom-gpt-prompt.md` to distinguish
    controlled `topic_categories` from free-text `keywords` and to note
    XML export support.
  - Refreshed README and vignette examples to include EDH-ready optional
    metadata and XML export guidance.

## metasalmon 0.0.8

- Added and documented NuSEDS method crosswalk helpers:
  - [`nuseds_enumeration_method_crosswalk()`](https://dfo-pacific-science.github.io/metasalmon/reference/nuseds_enumeration_method_crosswalk.md)
  - [`nuseds_estimate_method_crosswalk()`](https://dfo-pacific-science.github.io/metasalmon/reference/nuseds_estimate_method_crosswalk.md)
- Added reference documentation pages for both crosswalk helpers.
- Refreshed README feature list to include the new NuSEDS crosswalk
  utilities.

## metasalmon 0.0.6

- Added
  [`read_github_csv_dir()`](https://dfo-pacific-science.github.io/metasalmon/reference/read_github_csv_dir.md)
  to read all CSV files from a GitHub directory into a named list,
  similar to using [`dir()`](https://rdrr.io/r/base/list.files.html)
  with [`lapply()`](https://rdrr.io/r/base/lapply.html) for local files.
- Supports pattern matching, version pinning, and passes options to
  [`read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)
  for all files.
- Added comprehensive test coverage for the new function.

## metasalmon 0.0.5

- Renamed the GitHub CSV helpers to generic names:
  [`github_raw_url()`](https://dfo-pacific-science.github.io/metasalmon/reference/github_raw_url.md)
  and
  [`read_github_csv()`](https://dfo-pacific-science.github.io/metasalmon/reference/read_github_csv.md).
  `repo` is now required unless you provide a full URL.

## metasalmon 0.0.4

- Added
  [`ms_setup_github()`](https://dfo-pacific-science.github.io/metasalmon/reference/ms_setup_github.md)
  to guide one-time PAT setup (git check, browser token creation, git
  credential storage) and verify access to the private Qualark data
  repository.
- Added `qualark_raw_url()` and `read_qualark_csv()` to build stable raw
  GitHub URLs and read Qualark CSVs using the stored PAT (with SSO-aware
  error messages and retry logic).
- New tests cover URL construction, blob/raw URL normalization, and an
  opt-in Qualark fetch when a token is configured.

## metasalmon 0.0.3

- Added
  [`find_terms()`](https://dfo-pacific-science.github.io/metasalmon/reference/find_terms.md)
  function for searching candidate terms across external vocabularies
  (OLS, NVS, BioPortal).
- [`find_terms()`](https://dfo-pacific-science.github.io/metasalmon/reference/find_terms.md)
  now ranks results deterministically using I-ADOPT role hints from
  `inst/extdata/iadopt-terminologies.csv` (preferred vocabularies
  boosted; ties stable).
- [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md)
  now returns best-effort suggestions (stored in
  `attr(,'semantic_suggestions')`) instead of a placeholder message.
- Added I-ADOPT component fields (`property_iri`, `entity_iri`,
  `constraint_iri`, `method_iri`) to dictionary schema and package
  creation/reading.
- Enhanced validation: measurement columns now require I-ADOPT
  components (`term_iri`, `property_iri`, `entity_iri`, `unit_iri`).
- Updated table metadata: renamed `entity_type`/`entity_iri` to
  `observation_unit`/`observation_unit_iri` for clarity.
- Added `httr` package dependency for vocabulary search functionality.
- Dictionary validation now normalizes optional semantic columns and
  returns the normalized dictionary.
- Vignettes now show end-to-end semantic enrichment (I-ADOPT-aware
  suggestions) and how to align with `smn-gpt`.

## metasalmon 0.0.2

- Unified semantic fields to `term_iri` + `term_type` and reserved
  `concept_scheme_iri` for code lists only.
- Updated GPT collaboration guidance, schemas, and pkgdown outputs to
  match the new fields.
- Refreshed vignettes, tests, and reference docs; bumped package
  version.

## metasalmon 0.0.1

- Initial development snapshot.
