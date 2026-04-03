# Changelog

## metasalmon 0.1.2

- Fixed the seeded semantic-context warning path so
  `seed_semantics = TRUE` no longer crashes when mixed or previously
  unsupported `llm_context_files` trigger `cli` interpolation in package
  creation/review flows.
- Expanded `llm_context_files` handling so HTML/HTM, DOCX, `.R`, `.Rmd`,
  and `.qmd` inputs are read or normalized cleanly during LLM review
  instead of failing on unsupported-file warnings.
- Added Excel workbook context-file support for package-native LLM
  review, including `.xls`, `.xlsx`, and `.xlsm` inputs via the optional
  `readxl` package.
- Hardened LLM assessment parsing so malformed `accept` responses
  without a selected candidate degrade to `review`, and falsey
  `missing_context` placeholders no longer pollute outputs.
- Expanded LLM regression coverage with mixed-context bundle tests for
  the exact `chapi` + `ollama2.mistral:7b` configuration, including
  markdown, CSV, Excel, PDF, HTML, DOCX, and notebook/source context
  bundles across `dataset.csv`, `tables.csv`, `column_dictionary.csv`,
  and `codes.csv` targets.
- Finished the `scripts/llm-sanity-check.R` harness into a richer
  end-to-end smoke tool: it now generates per-case context bundles,
  records context formats in the summaries, rebuilds EDH XML after a
  simulated review pass, and writes stable CSV outputs under
  `artifacts/`.
- Added and linked a dedicated LLM review getting-started guide from the
  quickstart/setup docs so the package-native workflow is easier to
  discover.

## metasalmon 0.1.1

- Added a first-class `chapi` LLM provider preset for DFO’s internal
  Open WebUI endpoint. It defaults to `ollama2.mistral:7b`, uses
  `https://chapi-dev.intra.azure.cloud.dfo-mpo.gc.ca/api`, reads
  provider-specific overrides from `CHAPI_API_KEY`, `CHAPI_MODEL`, and
  `CHAPI_BASE_URL`, and now gives slower `gpt-oss` responses a longer
  effective timeout plus one retry.
- Updated the quickstart/home-page docs so internal DFO users can opt
  into `chapi` directly from `create_sdp(..., llm_assess = TRUE)`, while
  external users get parallel OpenRouter-free and OpenAI-credit setup
  paths.
- Promoted
  [`create_sdp()`](https://dfo-pacific-science.github.io/metasalmon/reference/create_sdp.md)
  and the Salmon Data Package workflow into a coherent release shape:
  single-table and multi-table package creation, semantic review
  artifacts, and post-review EDH rebuild are now aligned and documented
  as the primary path.
- Hardened final-review behavior:
  `validate_salmon_datapackage(..., require_iris = TRUE)` now fails on
  unresolved metadata placeholders, blank table observation-unit IRIs,
  and lingering review sentinels so strict validation actually means
  review is finished.
- Hardened table-level semantic review writes and EDH rebuilds:
  LLM-selected table suggestions now write back into
  `metadata/tables.csv`, and
  [`write_edh_xml_from_sdp()`](https://dfo-pacific-science.github.io/metasalmon/reference/write_edh_xml_from_sdp.md)
  now refuses to rebuild from obviously unreviewed packages.
- Improved package-native LLM review ergonomics: one-shot shortlist
  preservation now respects `llm_top_n`, shared `llm_context_files` are
  reused across targets, and non-interactive profile-scoped term
  requests now fail clearly instead of silently emitting junk defaults.
- Fixed multi-table semantic seeding so later tables use their own
  context instead of borrowing semantic context from table 1.
- Cleaned the release docs surface: refreshed the package description,
  fixed broken source-view links and vignette anchors, removed stale
  GPT-era remnants and orphaned assets, hid leaked internal helper pages
  from the public site, and rebuilt pkgdown from the integrated source.
- Bundled a matching Fraser Coho 2023–2024 starter dictionary plus
  provenance link so the installed package has a realistic context-file
  demo for the package-native LLM workflow.

## metasalmon 0.0.27

- Fixed a deterministic semantic-query bug for spawner-style measurement
  columns: the property-slot query no longer hard-codes `count` for
  columns like `natural_adult_spawners`, and now prefers
  `spawner abundance` so the shortlist is more semantically sensible
  before LLM review.
- Added one bounded LLM exploration round for weak semantic shortlists:
  when the first LLM pass comes back as review/propose-new-term or
  low-confidence, `suggest_semantics(..., llm_assess = TRUE)` may
  request 1–2 alternate plain-text search queries, rerun deterministic
  retrieval, merge/de-dupe candidates, and reassess once without letting
  the model mint raw IRIs.

## metasalmon 0.0.26

- Further tuned the OpenRouter free path for practicality:
  `openrouter/free` now uses smaller pair-sized batches and a smaller
  effective candidate shortlist per target so free-router prompts stay
  lighter on larger quickstart-style runs.

## metasalmon 0.0.25

- Made the OpenRouter free path more practical for full semantic review
  runs: live `openrouter/free` requests are now serially batched in
  pairs and use a smaller effective shortlist per target when using the
  built-in HTTP client, which trims request overhead without adding
  flaky parallel fan-out.
- Added batch fallback safety: if a batched OpenRouter response is
  malformed or incomplete, `metasalmon` now falls back to per-target
  assessment instead of poisoning the whole run.
- Retained the 0.0.24 hardening: longer effective timeout, one retry for
  transient failures, lighter context payloads, and downgrade-to-review
  handling for out-of-range candidate indexes.

## metasalmon 0.0.24

- Hardened package-native LLM review for flaky free-router behavior:
  OpenRouter free models now get a longer effective timeout, one
  automatic retry for transient HTTP/network failures, and fewer context
  chunks per request so prompts stay lighter.
- Hardened invalid LLM candidate-index handling: out-of-range
  `selected_candidate_index` values no longer poison the whole target;
  they are downgraded to `review` with no auto-selection instead of
  surfacing as a hard LLM error.

## metasalmon 0.0.23

- Added package-native LLM semantic review on top of deterministic
  retrieval: `suggest_semantics(..., llm_assess = TRUE)` can now assess
  shortlisted candidates with OpenAI-compatible providers, attach
  `llm_*` review columns to `semantic_suggestions`, and expose
  target-level results via `attr(dict, "semantic_llm_assessments")`.
- Added local context-file support for LLM semantic review, including
  README/markdown/text-style files and optional PDF extraction via
  `pdftools`, with bounded chunking so reports are trimmed before
  prompting.
- Added OpenRouter support for package-native LLM review, including
  pass-through model ids (so OpenRouter models ending in `:free` work
  without special branching).
- Extended
  [`infer_dictionary()`](https://dfo-pacific-science.github.io/metasalmon/reference/infer_dictionary.md),
  [`infer_salmon_datapackage_artifacts()`](https://dfo-pacific-science.github.io/metasalmon/reference/infer_salmon_datapackage_artifacts.md),
  and
  [`create_sdp()`](https://dfo-pacific-science.github.io/metasalmon/reference/create_sdp.md)
  to thread the optional LLM semantic review arguments through the
  start-here workflow.
- Extended
  [`apply_semantic_suggestions()`](https://dfo-pacific-science.github.io/metasalmon/reference/apply_semantic_suggestions.md)
  with `strategy = "llm"` and `min_llm_confidence` for explicit
  application of LLM-reviewed matches.
- Updated README, GPT-collaboration vignette, entrypoint docs, tests,
  and generated documentation for the 0.0.23 feature release.

## metasalmon 0.0.22

- Simplified EDH XML support down to the single DFO Enterprise Data Hub
  HNAP export we actually use:
  [`edh_build_hnap_xml()`](https://dfo-pacific-science.github.io/metasalmon/reference/edh_build_hnap_xml.md)
  is now the canonical helper, while
  [`edh_build_iso19139_xml()`](https://dfo-pacific-science.github.io/metasalmon/reference/edh_build_iso19139_xml.md)
  remains only as a deprecated compatibility alias.
- Simplified
  [`create_sdp()`](https://dfo-pacific-science.github.io/metasalmon/reference/create_sdp.md)
  EDH export behavior: `include_edh_xml = TRUE` now always writes
  `metadata/metadata-edh-hnap.xml`; legacy `edh_profile` / `EDH_Profile`
  / `EDH_profile` inputs are still accepted as deprecated compatibility
  shims, while `edh_xml_path` is deprecated and ignored.
- Rebuilt reference docs, tests, package artifacts, and pkgdown site for
  the 0.0.22 patch release.

## metasalmon 0.0.20

- Hardened GitHub helper security: GitHub readers now reject non-GitHub
  remote URLs and avoid attaching GitHub auth headers to non-GitHub
  hosts; improved public/private auth behavior and related tests.
- Hardened package writing + export reliability:
  [`create_sdp()`](https://dfo-pacific-science.github.io/metasalmon/reference/create_sdp.md)
  now fails fast with an explicit `overwrite = TRUE` message when the
  target directory already exists, fixed DwC validator execution path,
  and improved ontology fetch robustness with explicit timeout handling
  and cache fallback behavior.
- Surfaced clearer warning messages when online vocabulary API lookups
  time out, so empty
  [`find_terms()`](https://dfo-pacific-science.github.io/metasalmon/reference/find_terms.md)
  results are less opaque during semantic seeding.
- Fixed
  [`submit_term_request_issues()`](https://dfo-pacific-science.github.io/metasalmon/reference/submit_term_request_issues.md)
  batch routing so per-row `ontology_repo` values are honored instead of
  posting all rows to the first repo.
- Clarified
  [`validate_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/validate_semantics.md)
  API by explicitly deprecating ignored legacy arguments
  (`entity_defaults`, `vocab_priority`) with coverage for warning
  behavior.
- Improved release/test hygiene: dependency bootstrap script hardening,
  tighter warning assertions in brittle tests, and refreshed package
  description wording.

## metasalmon 0.0.19

- Hardened table observation-unit auto-apply in
  [`create_sdp()`](https://dfo-pacific-science.github.io/metasalmon/reference/create_sdp.md):
  table-level observation-unit suggestions are now ignored when driven
  by placeholder review text and only auto-applied when lexical
  compatibility checks pass against non-placeholder table metadata.
- Improved non-measurement `term_iri` auto-apply quality without
  disabling the feature: incompatible candidates are now filtered using
  role-hint mismatch checks, match-type/score guards, and token-level
  lexical compatibility with the target column context.
- Strengthened `infer_column_role()` heuristics for NuSEDS-like fields:
  year-like columns are now classified as temporal more reliably, and
  `NATURAL_ADULT_SPAWNERS`-style quantity columns are inferred as
  measurement.
- Tightened default code-level seeding gates to reduce free-text noise
  while preserving useful low-cardinality categorical/attribute
  suggestions: text-like field names and non-code-like all-unique short
  character values are excluded from the default factor-scope code
  seeding path.
- Added regression coverage for the above hardening paths, including
  placeholder-driven table seeding prevention, bad non-measurement
  suggestion filtering, improved role inference for fuller examples, and
  free-text seeding guardrails.
- Rebuilt reference docs, tests, package artifacts, and pkgdown site for
  the 0.0.19 patch release.

## metasalmon 0.0.18

- Reworked review placeholders so missing descriptions/metadata are
  labeled explicitly (`MISSING DESCRIPTION:` / `MISSING METADATA:`)
  instead of the more ambiguous generic review wording.
- [`create_sdp()`](https://dfo-pacific-science.github.io/metasalmon/reference/create_sdp.md)
  and related inference paths now seed table-level observation-unit
  review content and auto-apply the top table semantic suggestion into
  `tables.csv`, including `observation_unit_iri` and a backfilled
  `observation_unit` label when needed.
- Broadened default semantic suggestion coverage beyond measurement
  columns in a conservative way: categorical and controlled
  low-cardinality attribute columns can now receive lighter `term_iri`
  suggestions, while identifier and temporal columns remain excluded
  from default non-measurement suggestion seeding.
- Broadened default code-level semantic seeding so ordinary
  low-cardinality character columns from typical CSV imports are
  considered, rather than relying on R factor inputs.
- Made inferred `required` flags less misleading by marking obvious
  identifier columns as required and leaving other columns unknown
  (`NA`) until reviewed, instead of defaulting everything to `FALSE`.
- Improved auto-filled `term_type` values when `term_iri` suggestions
  are applied and kept the `target_description` vs `column_description`
  distinction explicit in suggestion outputs.
- Added a second bundled official NuSEDS example dataset:
  `nuseds-fraser-coho-2023-2024.csv` (173 rows across 2023–2024), while
  keeping the existing 30-row demo sample intact.
- Added reproducible provenance for bundled NuSEDS examples via
  `data-raw/nuseds_fraser_coho_examples.R` and documented the upstream
  Open Government Canada record/resource and licensing.
- Updated README, vignettes, reference docs, and tests to reflect the
  broader semantic seeding behavior, required-flag review stance,
  observation-unit handling, and the tiny-vs-fuller example-data
  workflow.

## metasalmon 0.0.17

- Improved measurement semantic query shaping for count-like fields:
  - split variable/property query behavior so `NATURAL_SPAWNERS_TOTAL`
    no longer defaults both roles to the same abundance concept,
  - added a count-like unit fallback query (`count`) for measurement
    columns that clearly represent totals/counts/abundance.
- Added/updated regression tests for role-aware query behavior,
  count-like unit fallback, and unit-label backfill when applying unit
  suggestions.

## metasalmon 0.0.16

- Rewrote `README-review.txt` intro and checklist to be shorter, more
  first-time friendly, and more action-oriented.
- [`create_sdp()`](https://dfo-pacific-science.github.io/metasalmon/reference/create_sdp.md)
  now prints an explicit up-front note that semantic seeding may take a
  few minutes.
- Improved column-level semantic query construction for measurement
  fields so placeholder text is not used as the query source.
- Added role-aware query shaping that improves built-in sample
  suggestions for `NATURAL_SPAWNERS_TOTAL` (e.g., variable/property
  `SpawnerAbundance`, entity `Population`, constraint `NaturalOrigin`)
  and avoids the previous exploitation/mortality-rate mismatches.
- Unit suggestions are now skipped when no unit context exists, and
  applying a unit suggestion now backfills `unit_label` when missing.

## metasalmon 0.0.15

- [`create_sdp()`](https://dfo-pacific-science.github.io/metasalmon/reference/create_sdp.md)
  now tells users up front when online semantic seeding may take a few
  minutes and points to `seed_semantics = FALSE` for the fastest first
  pass.
- Simplified `README-review.txt` into a shorter 7-step checklist so the
  review flow is easier to follow.

## metasalmon 0.0.14

- Simplified the package-creation surface so
  [`create_sdp()`](https://dfo-pacific-science.github.io/metasalmon/reference/create_sdp.md)
  is the clear one-shot entrypoint,
  [`write_salmon_datapackage()`](https://dfo-pacific-science.github.io/metasalmon/reference/write_salmon_datapackage.md)
  is the advanced/manual writer, and the older create-from-data helper
  was removed.
- Reworked
  [`create_sdp()`](https://dfo-pacific-science.github.io/metasalmon/reference/create_sdp.md)
  output into a cleaner review layout with `metadata/` and `data/`
  subdirectories, package-root `README-review.txt`, package-root
  `semantic_suggestions.csv` (when present), and root
  `datapackage.json`.
- Rewrote `README-review.txt` as a step-by-step checklist that explains
  the canonical Salmon Data Package, how to share the full package
  folder (or zip), and how to return to R for validation.
- Tightened default semantic seeding so code-level semantic suggestions
  run only for factor/categorical source columns by default, while
  keeping column-level and table-level seeding available.
- Added optional update notifications inside
  [`create_sdp()`](https://dfo-pacific-science.github.io/metasalmon/reference/create_sdp.md)
  via `check_updates`, using the explicit
  [`check_for_updates()`](https://dfo-pacific-science.github.io/metasalmon/reference/check_for_updates.md)
  helper rather than package-attach network checks.
- Refreshed README, vignettes, reference pages, generated documentation,
  tests, and pkgdown outputs to match the new workflow and layout.

## metasalmon 0.0.13

- Made
  [`edh_build_iso19139_xml()`](https://dfo-pacific-science.github.io/metasalmon/reference/edh_build_iso19139_xml.md)
  default to the richer North American Profile / HNAP-aware EDH export
  while keeping `profile = "iso19139"` available as an explicit
  fallback.
- Expanded EDH export support for bilingual locale scaffolding,
  deterministic identifiers, legal constraints, maintenance/status,
  reference systems, bounding boxes, and distribution metadata, with
  regression coverage against the confirmed EDH sample shape.
- Added
  [`apply_semantic_suggestions()`](https://dfo-pacific-science.github.io/metasalmon/reference/apply_semantic_suggestions.md)
  for explicit opt-in merges of
  [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md)
  results into dictionaries.
- Restored canonical Salmon Data Package CSVs (`dataset.csv`,
  `tables.csv`, `column_dictionary.csv`, optional `codes.csv`) as the
  source of truth in
  [`read_salmon_datapackage()`](https://dfo-pacific-science.github.io/metasalmon/reference/read_salmon_datapackage.md),
  treating `datapackage.json` as derived/interoperability metadata.
- Refreshed README, vignettes, pkgdown reference metadata, and GPT
  collaboration guidance to match the EDH default/export semantics and
  explicit dictionary-application workflow.
- Rebuilt package documentation, tests, source tarball, and pkgdown site
  for the 0.0.13 release.

## metasalmon 0.0.12

- Added a GCDFO-backed
  [`find_terms()`](https://dfo-pacific-science.github.io/metasalmon/reference/find_terms.md)
  search backend that queries the DFO Salmon Ontology first via content
  negotiation against `https://w3id.org/gcdfo/salmon`.
- For salmon-domain roles,
  [`find_terms()`](https://dfo-pacific-science.github.io/metasalmon/reference/find_terms.md)
  now prioritizes GCDFO results and only falls back to OLS/NVS when
  GCDFO returns no good label hit.
- Updated
  [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md),
  `infer_dictionary(seed_semantics = TRUE)`, man pages, and vignettes to
  reflect the new GCDFO-first search behavior.
- Rebuilt package documentation, tests, source tarball, and pkgdown site
  for the 0.0.12 release.

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
