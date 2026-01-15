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
