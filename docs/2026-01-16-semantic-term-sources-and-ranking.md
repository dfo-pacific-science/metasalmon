# ExecPlan: Expand semantic term sources + ranking roadmap (metasalmon/salmonpy)

## Purpose / Big Picture

Improve `metasalmon::find_terms()` / `metasalmon::suggest_semantics()` (and the mirrored salmonpy functions) so they:

- **Return useful candidates reliably** (no silent empty results when a provider changes),
- **Prefer salmon-appropriate vocabularies** (per `dfo-salmon-ontology` conventions),
- Set up a path toward **better matching** (role-aware queries + modern reranking).

This plan is intentionally **cross-repo**:

- `../metasalmon` (R package)
- `../salmonpy` (Python mirror)

## Progress

- [x] 2026-01-16: Create ExecPlan (this document).
- [x] 2026-01-16: Phase 1 — provider reliability (NVS fix), add ZOOMA + ICES (code lists), verify real (non-empty) results.
  - NVS: switched to SPARQL endpoint with simplified query (removed slow altLabel matching, increased timeout to 60s)
  - ZOOMA: implemented with OLS term resolution, 60s timeout
  - ICES: implemented code type and code list helpers with filtering
  - Python: added curl fallback for environments with broken urllib SSL, configurable timeout, debug mode via `SALMONPY_DEBUG_FETCH=1`
  - R: updated `.safe_json` with configurable timeout, simplified NVS SPARQL query
  - All unit tests pass in both repos
- [x] 2026-01-17: Phase 2 — encode ontology preferences by role (salmon-first, QUDT units, Darwin Core scaffold, Wikidata alignment-only).
  - Created `inst/extdata/ontology-preferences.csv` with ranked allowlist per I-ADOPT role
  - Added QUDT source (`.search_qudt()`) using SPARQL endpoint for unit lookups
  - Added GBIF source (`.search_gbif()`) for taxon backbone entity resolution
  - Added WoRMS source (`.search_worms()`) for marine species entity resolution
  - Added `sources_for_role()` helper to get role-optimized source sets
  - Updated `.score_and_rank_terms()` with role-based ontology preference scoring:
    - Priority-based boost: Priority 1 = +2.0, Priority 2 = +1.5, etc.
    - QUDT preferred for unit role (priority 1)
    - gcdfo preferred for entity role (priority 1), then ODO, fishtraits, ncbitaxon, WoRMS, GBIF
    - STATO/OBA preferred for property role
    - gcdfo: SKOS + SOSA/PROV patterns preferred for method role (AGROVOC as broad fallback)
  - Implemented Wikidata alignment-only handling:
    - `alignment_only` column added to results
    - Wikidata IRIs penalized (-0.5 score) rather than boosted
  - Updated `find_terms()` to support new sources and return `alignment_only` flag
  - Added comprehensive tests for Phase 2 features
  - salmonpy mirroring completed (Phase 2 sources + role preferences)
  - Phase 2 tests run: `devtools::test('.', filter = 'term-search')` (pass)
- [x] 2026-01-16: Phase 3 — Darwin Core: prefer DwC Conceptual Model + DwC Data Package (DwC-DP), not legacy "DwC terms" search.
  - Added `suggest_dwc_mappings()` helper to propose DwC-DP table/field mappings (no generic DwC term search).
  - Added DwC-DP field cache (`inst/extdata/dwc-dp-fields.csv`) sourced from core table schemas (event/occurrence/location/taxon/organism).
  - Expanded DwC-DP cache with identification, material, and material-assertion (MeasurementOrFact) fields.
  - Python mirror added (`salmonpy.dwc_dp.suggest_dwc_mappings`) with shared field cache.
  - Tests added for DwC-DP mapping suggestions in both repos.
- [x] 2026-01-17: Phase 4 prework — ZOOMA confidence model review + OAK feasibility check (embedding rerank + A/B harness deferred).
- [x] 2026-01-29: Phase 4 — matching quality: role-aware query expansion, cross-source agreement boosts, optional embedding rerank.
  - Added cross-source agreement detection (`.apply_cross_source_agreement()`):
    - IRI agreement: +0.5 boost per additional source returning same IRI
    - Label-only agreement: +0.2 boost per additional source returning same label
    - New `agreement_sources` column in results for explainability
  - Added role-aware query expansion (`.expand_query()`):
    - Unit role: expands abbreviations (kg → kilogram), adds "unit" suffix
    - Method role: adds "method" suffix
    - Entity role: extracts genus from binomial species names
    - Property role: adds "measurement" suffix
    - New `expand_query = TRUE/FALSE` parameter in `find_terms()`
  - Added per-source diagnostic reporting:
    - Results have `diagnostics` attribute with source/query/status/elapsed_secs/error
    - Errors captured with tryCatch, sources that fail return empty + diagnostic entry
  - Added embedding rerank placeholder infrastructure:
    - `.apply_embedding_rerank()` and `.embedding_rerank_enabled()` functions
    - Enable via `METASALMON_EMBEDDING_RERANK=1` environment variable
    - Placeholder logs message in debug mode; full implementation deferred
  - Added `score` column to output for explainability
  - Phase 4 tests run: `devtools::test('.', filter = 'term-search')` (88 pass)

## Surprises & Discoveries

- NVS `search_nvs/*?format=json` endpoints are currently returning HTML, causing silent empties unless detected.
- NVS SPARQL queries with OPTIONAL + REGEX on altLabel are very slow (>30s); simplified to prefLabel-only REGEX.
- Python urllib on some macOS configurations fails with DNS errors (`Errno 8` or `Errno 9`); curl fallback was essential.
- `dfo-salmon-ontology` conventions explicitly treat Darwin Core mostly as **properties** and as an **interoperability scaffold**, and recommend Wikidata **alignment-only**.
- DwC "the new way" is the **DwC Conceptual Model + DwC Data Package guide** (DwC-DP), hosted at `https://gbif.github.io/dwc-dp/` (schemas in `gbif/dwc-dp`).
- **Phase 2**: QUDT SPARQL endpoint (`https://www.qudt.org/fuseki/qudt/sparql`) works well for unit lookups; returns JSON with `Accept: application/sparql-results+json`.
- **Phase 2**: GBIF species match API (`/v1/species/match`) returns single best match; fallback to `/v1/species/search` for broader results.
- **Phase 2**: WoRMS API returns data frames directly; `AphiaRecordsByMatchNames` endpoint useful for fuzzy matching.
- **Phase 4**: Cross-source agreement is a strong signal—terms appearing in multiple sources with the same IRI are highly reliable candidates.
- **Phase 4**: Query expansion helps unit searches significantly (abbreviation expansion), but needs tuning to avoid noise in other roles.
- **Phase 4**: Per-source diagnostics (elapsed time, error messages) are valuable for debugging slow or failing searches; exposed via `attr(result, "diagnostics")`.

## Decision Log

- 2026-01-16: Fix NVS by switching to NVS SPARQL (`https://vocab.nerc.ac.uk/sparql/`) restricted to P01/P06 rather than relying on `search_nvs`.
- 2026-01-16: Add ZOOMA as a candidate generator (`source = "zooma"`) that resolves returned `semanticTags` to OLS term metadata.
- 2026-01-16: Add ICES Vocab API as a **code-list** integration (separate from ontology term search), starting with code-type + code retrieval helpers.
- 2026-01-17: Add QUDT as preferred source for unit role; implemented via SPARQL endpoint with 60s timeout.
- 2026-01-17: Add GBIF/WoRMS as taxon resolvers for entity role; GBIF uses species match/search APIs, WoRMS uses AphiaRecordsByName.
- 2026-01-17: Create `ontology-preferences.csv` to encode ranked allowlists per I-ADOPT role with priority rankings.
- 2026-01-17: Implement priority-based scoring: Priority 1 = +2.0 boost, Priority 2 = +1.5, etc.; Wikidata penalized as alignment-only.
- 2026-01-17: Add `sources_for_role()` helper function to simplify role-aware source selection.
- 2026-01-17: Add `alignment_only` column to results to flag Wikidata terms for downstream filtering.
- 2026-01-29: Implement cross-source agreement boosting: IRI agreement +0.5, label-only +0.2 per additional source.
- 2026-01-29: Implement role-aware query expansion with unit abbreviations, method/measurement suffixes, genus extraction.
- 2026-01-29: Add per-source diagnostics attribute to track success/failure/timing for each source query.
- 2026-01-29: Add embedding rerank placeholder (deferred full implementation); enable via METASALMON_EMBEDDING_RERANK=1.
- 2026-01-29: Expose `score` column in results for ranking explainability.

## Context and Orientation

### Definitions (terms of art)

- **IRI**: a globally unique identifier used in RDF/OWL (often URL-shaped).
- **OLS**: EBI Ontology Lookup Service (cross-ontology search + term metadata).
- **NVS**: NERC Vocabulary Server (SKOS vocabularies; P01 observables, P06 units).
- **SPARQL**: RDF query language used by many vocabulary endpoints.
- **ZOOMA**: EBI service for text → ontology term annotations.
- **DwC-DP**: Darwin Core Data Package guide (Frictionless Data Package implementation of the Darwin Core Conceptual Model).

### Conventions to honor (from `../dfo-salmon-ontology/docs/CONVENTIONS.md`)

- Prefer `gcdfo:` terms first when they exist; use external vocabularies for cross-domain concepts.
- QUDT is the preferred source for units (often stored as IRIs).
- Darwin Core is an interoperability scaffold; many terms are RDF properties (align like properties, not `skos:*Match`).
- Wikidata is alignment-only (useful for reconciliation/crosswalks, not canonical modeling).

## Phases (roadmap)

### Phase 1 — Provider Reliability + Source Expansion (this execution)

**Goal:** Make term lookup robust and broaden candidate generation without changing higher-level ranking strategy.

- NVS: switch to SPARQL (P01/P06 restriction) so “nvs” doesn’t silently go empty.
- Add “zooma” source to `find_terms()` (text annotations → OLS term details).
- Add ICES vocab helpers (code lists): fetch code types, fetch codes for a code type, and basic local filtering utilities.
- Add focused tests (mocked) + smoke tests (real network) proving non-empty, meaningful outputs.

### Phase 2 — Ontology Preferences by Role

**Goal:** Improve match quality by preferring the right vocabularies for the right role.

- Encode a ranked allowlist per role:
  - `unit`: QUDT + NVS P06
  - `method`: `gcdfo:` SKOS methods + SOSA/PROV patterns + AGROVOC fallback
  - `entity`: gcdfo + NCEAS Salmon (ODO) + taxa resolvers (GBIF/WoRMS) for species-like entities
  - `property`: measurement/statistics ontologies (e.g., STATO/OBA) where appropriate
- Keep Wikidata alignment-only.

### Phase 3 — Darwin Core (DwC-CM + DwC-DP)

**Goal:** Stop treating “Darwin Core terms” as a generic term-search backend.

- Add a dedicated mapping helper that uses DwC Conceptual Model + DwC-DP schemas to suggest **table/field** mappings and (only then) the appropriate `dwc:`/`dwciri:` properties.

### Phase 4 — Matching Quality (forward-looking)

**Goal:** Reduce false positives and increase relevance.

- Role-aware query expansion (use `unit_label` for unit, method hints for method, etc.).
- Cross-source agreement boosts (same IRI/label across providers).
- Optional embedding rerank (local model) over the top lexical candidates.

## Validation and Acceptance

### Phase 1 acceptance criteria

- `find_terms(..., sources="nvs")` returns real results (non-empty label + IRI) for reasonable queries (e.g., "count", "fish").
- `find_terms(..., sources="zooma")` returns non-empty results for a phrase query (e.g., "spawner count"), with resolved labels/definitions (not just raw IRIs).
- ICES helpers return non-empty code types and codes (e.g., `Gear`) and allow basic filtering.
- Tests pass in both repos:
  - R: `devtools::test(..., filter = 'term-search|dictionary-helpers')`
  - Python: `python3 -m unittest salmonpy.tests.test_term_search salmonpy.tests.test_semantics`

### Phase 2 acceptance criteria

- `sources_for_role("unit")` returns `c("qudt", "nvs", "ols")` (QUDT first).
- `sources_for_role("entity")` returns `c("gbif", "worms", "bioportal", "ols")` (taxon resolvers first; gcdfo/ODO preference handled via scoring).
- `find_terms("kilogram", role="unit", sources="qudt")` returns QUDT unit IRIs.
- `find_terms("Oncorhynchus kisutch", role="entity", sources="gbif")` returns GBIF taxon backbone results.
- `find_terms("Oncorhynchus kisutch", role="entity", sources="worms")` returns WoRMS marine species results.
- Results from QUDT/GBIF/WoRMS rank higher than generic OLS results for their respective roles.
- Wikidata results have `alignment_only = TRUE` and score lower than domain-specific ontology matches.
- `ontology-preferences.csv` correctly encodes:
  - Unit: QUDT (priority 1), NVS P06 (priority 2)
  - Entity: gcdfo (priority 1), odo (priority 2), fishtraits (priority 3), ncbitaxon (priority 4), worms (priority 5), gbif (priority 6)
  - Property: STATO (priority 1), OBA (priority 2), PATO (priority 3)
  - Method: gcdfo (priority 1), sosa (priority 2), prov (priority 3), obi (priority 4), ices (priority 5), agrovoc (priority 6)
- All Phase 2 tests pass: `devtools::test(..., filter = 'term-search')`

### Phase 4 acceptance criteria

- [x] `find_terms()` returns `score` column for ranking explainability.
- [x] `find_terms()` returns `agreement_sources` column showing cross-source agreement count.
- [x] Cross-source agreement boosting: IRI agreement +0.5, label-only +0.2 per additional source.
- [x] Role-aware query expansion via `.expand_query()`:
  - Unit role: abbreviation expansion (kg → kilogram), "unit" suffix
  - Method role: "method" suffix
  - Entity role: genus extraction from binomial
  - Property role: "measurement" suffix
- [x] `expand_query = TRUE/FALSE` parameter controls query expansion.
- [x] Per-source diagnostics via `attr(result, "diagnostics")` with source/query/status/elapsed_secs/error.
- [x] Embedding rerank placeholder infrastructure (`.apply_embedding_rerank()`, `METASALMON_EMBEDDING_RERANK=1`).
- [x] All Phase 4 tests pass: `devtools::test('.', filter = 'term-search')` (88 tests)

### Phase 3 acceptance criteria

- `suggest_dwc_mappings()` returns non-empty suggestions for common DwC fields:
  - `Event Date` → `event.eventDate`
  - `Decimal Latitude` → `location.decimalLatitude`
  - `Scientific Name` → `occurrence.scientificName` or `taxon.scientificName`
- Suggestions include the DwC property IRI (`term_iri`) for review (no auto-filling).
- `find_terms()` does not treat DwC as a generic search source (DwC suggestions are separate).
- Tests pass in both repos:
  - R: `devtools::test(..., filter = 'dwc-dp')`
  - Python: `python -m unittest salmonpy.tests.test_dwc_dp`

### DwC-DP validation strategy (export view)

- Use DwC-DP as an export/interoperability layer (not canonical semantics); default DwC mapping is OFF, opt-in via `include_dwc` in `suggest_semantics()`.
- For DwC-DP export, validate with generic Frictionless checks + profile conformance:
  - Ensure `datapackage.json` includes the DwC-DP profile URL and reserved resource/table names.
  - Validate resources against canonical DwC-DP table schemas (fields/types/PK/FK) from `rs.tdwg.org` / `dwc-dp/table-schemas`.
  - Run Frictionless table/dialect checks (RFC 4180 CSV, encoding, required fields).
  - If available, run FrictionlessDarwinCore conversion/validation; GBIF DwC-Archive validator does not yet validate DwC-DP packages.
  - Keep SDP as the canonical package; DwC-DP is a derived/export view that must not violate SDP spec (`dataset.csv`, `tables.csv`, `column_dictionary.csv`, `codes.csv` stay authoritative).

## pkgdown/vignette doc updates needed (plan)

- [x] `vignettes/data-dictionary-publication.Rmd`: add the `include_dwc` toggle example (ontology suggestions vs DwC export mappings side-by-side) and clarify DwC is export-only, default OFF.
  - **Done 2026-01-29**: Added `suggest_dwc_mappings()` direct usage example, "Semantic suggestions with role-aware sources" section explaining role-aware ranking and alignment_only filtering.
- [x] `vignettes/reusing-standards-salmon-data-terms.Rmd`: update source list (OLS/NVS/ZOOMA/QUDT/GBIF/WoRMS), role-aware ranking, `alignment_only`, and mention DwC mappings as a separate export view.
  - **Done 2026-01-29**: Added "Available sources by role" table, QUDT/GBIF/WoRMS examples, "Interpreting results" section (score, alignment_only, agreement_sources), diagnostics debugging section.
- [x] `vignettes/gpt-collaboration.Rmd`: note the optional DwC mapping layer for biodiversity exports and how GPT prompts should keep ontology suggestions primary.
  - **Done 2026-01-29**: Added "Advanced: Phase 4 matching quality features" section covering query expansion, cross-source agreement, diagnostics, and embedding rerank options.
- [x] `vignettes/glossary.Rmd`: add short entries for DwC-DP, Assertion (DwC MeasurementOrFact pattern), NVS SPARQL, and the new `include_dwc` flag.
  - **Done 2026-01-29**: Added QUDT, GBIF, WoRMS, query expansion, cross-source agreement, diagnostics attribute, alignment_only, include_dwc to quick reference table.
- `vignettes/metasalmon.Rmd` (landing/overview): brief note that DwC-DP export mappings are available via `suggest_semantics(..., include_dwc = TRUE)`; remind default is OFF.
  - **Previously done**: Already mentions `include_dwc = FALSE` default at lines 126-130.
- [x] pkgdown home/index YAML (if present): ensure new functions (`suggest_dwc_mappings`, `include_dwc` flag) surface in reference/articles.
  - **Done 2026-01-29**: Added `sources_for_role` to "Semantic Helpers" group, created new "Darwin Core (DwC-DP)" reference group with `suggest_dwc_mappings` and `dwc_dp_build_descriptor`.
- [x] README.md: mention role-aware vocabulary search features.
  - **Done 2026-01-29**: Added role-aware vocabulary search bullet points to "For data stewards" section (QUDT, GBIF/WoRMS, STATO/OBA, cross-source agreement).

## Plan: frictionless-based DwC-DP export/validation prototype

- Goal: provide an opt-in prototype to emit a DwC-DP-flavored `datapackage.json` that references canonical DwC-DP table schemas and (optionally) runs Frictionless validation when the user supplies DwC-ready CSVs.
- Approach:
  - Add a helper (Python) to build a DwC-DP descriptor from a user-supplied list of resources (name, path, schema name). Point each resource’s `schema` to the canonical DwC-DP table schema URL (e.g., `https://raw.githubusercontent.com/gbif/dwc-dp/master/dwc-dp/table-schemas/{schema}.json`).
  - Set the Data Package `profile` to the DwC-DP profile URL.
  - Optionally run `frictionless validate` (if installed) on the constructed descriptor; otherwise warn that validation is skipped.
  - Keep this export prototype separate from SDP (SDP remains canonical). Users opt-in to produce a DwC-DP view when they already have DwC-shaped CSVs or have mapped SDP columns to DwC tables.
- Implemented:
  - Python: `salmonpy.dwc_dp_export` (`build_dwc_dp_descriptor`, `save_descriptor`, `validate_descriptor`).
  - R: `dwc_dp_build_descriptor(resources, ..., validate = FALSE, python = 'python3')` builds a descriptor, can call frictionless validation if available.
  - Tests added in both stacks; validation is best-effort (warns if frictionless missing).
- Validation baseline:
  - Descriptor must include DwC-DP profile URL.
  - Resource schemas must match the canonical DwC-DP table schemas (field names/types/PK/FK).
  - Frictionless should check CSV dialect (RFC 4180), encoding, required fields; fail fast on missing files.
- Spec touchpoints (for later):
  - We should *not* change `SPECIFICATION.md` to require DwC-DP; keep SDP as canonical. At most, document that an optional DwC-DP export MAY be produced alongside SDP deliverables.

## Concrete Steps (Phase 1)

Run from the appropriate repo directories:

- R package smoke:
  - `Rscript -e "devtools::load_all('../metasalmon'); metasalmon::find_terms('spawner count') %>% head()"`
  - `Rscript -e "devtools::load_all('../metasalmon'); metasalmon::find_terms('count', sources='nvs') %>% head()"`
  - `Rscript -e "devtools::load_all('../metasalmon'); metasalmon::find_terms('spawner count', sources='zooma') %>% head()"`
- Python package smoke:
  - `python3 -c "import sys; sys.path.insert(0,'../'); import salmonpy; print(salmonpy.find_terms('spawner count').head())"`

## Concrete Steps (Phase 2)

Run from the metasalmon repo directory to validate Phase 2 changes:

- Role-optimized source selection:
  - `Rscript -e "devtools::load_all('.'); sources_for_role('unit')"`
  - `Rscript -e "devtools::load_all('.'); sources_for_role('entity')"`

- QUDT unit search:
  - `Rscript -e "devtools::load_all('.'); find_terms('kilogram', role='unit', sources=sources_for_role('unit')) %>% head()"`

- GBIF taxon resolution:
  - `Rscript -e "devtools::load_all('.'); find_terms('Oncorhynchus kisutch', role='entity', sources='gbif') %>% head()"`

- WoRMS marine species:
  - `Rscript -e "devtools::load_all('.'); find_terms('Oncorhynchus kisutch', role='entity', sources='worms') %>% head()"`

- Verify Wikidata alignment-only:
  - `Rscript -e "devtools::load_all('.'); res <- find_terms('salmon'); res[res$alignment_only == TRUE, ]"`

- Run Phase 2 tests:
  - `Rscript -e "devtools::test('.', filter = 'term-search')"`

## Concrete Steps (Phase 3)

Run from the metasalmon repo directory:

- DwC-DP mapping suggestions:
  - `Rscript -e "devtools::load_all('.'); dict <- tibble::tibble(column_name='event_date', column_label='Event Date', column_description='Date the event occurred'); dict <- suggest_dwc_mappings(dict); attr(dict, 'dwc_mappings')"`
- Run Phase 3 tests:
  - `Rscript -e "devtools::test('.', filter = 'dwc-dp')"`
  - `python -m unittest salmonpy.tests.test_dwc_dp`

## SDP ↔ DwC-DP Synergies (both Frictionless Data Package based)

**Goal:** strengthen interoperability (interoperability means different systems can exchange and use data without custom one-off conversions) between Salmon Data Package (SDP) and Darwin Core Data Package (DwC-DP) using shared Frictionless patterns.

- **Shared core structure:** both use `datapackage.json` with `resources` and `schema.fields`, so SDP tables can be surfaced as DwC-DP tables (and vice‑versa) without changing the container format.
- **Schema alignment layer:** `column_dictionary.csv` can map to `resources[].schema.fields` by syncing `column_name`, `column_label`, `value_type`, and `required` → this is a direct bridge for DwC-DP field descriptors.
- **Semantic bridge:** SDP’s `term_iri`/I‑ADOPT fields can populate DwC `dcterms:isVersionOf` / property IRIs when a DwC field mapping exists; otherwise retain SDP semantics as the primary model.
- **Table mapping as metadata, not renaming:** use `suggest_dwc_mappings()` to recommend DwC table/field targets while keeping native SDP column names intact unless the user opts into a DwC view.
- **Controlled vocab reuse:** SDP `codes.csv` can be reused to populate DwC enumerations (e.g., `sex`, `lifeStage`, `occurrenceStatus`) where they align; otherwise keep as SDP‑specific vocab.
- **Round‑trip intent:** maintain a mapping file (future) that records `table_id → DwC table` and `column_name → DwC field` so exports can toggle between SDP and DwC‑DP without losing SDP‑specific metadata.

## Documentation updates (vignettes)

Vignette (a long-form R package tutorial) updates to cover Phase 1–3 changes:

- `vignettes/reusing-standards-salmon-data-terms.Rmd`: add a section on `find_terms()` sources (OLS/NVS/ZOOMA/QUDT/GBIF/WoRMS), role-aware ranking, and the `alignment_only` flag.
- `vignettes/data-dictionary-publication.Rmd`: add a short workflow block showing `suggest_semantics()` plus `suggest_dwc_mappings()` for DwC‑DP table/field suggestions.
- `vignettes/gpt-collaboration.Rmd`: note that DwC‑DP mappings are available as a parallel review layer for biodiversity‑style datasets.
- `vignettes/glossary.Rmd`: add brief entries for DwC‑DP, ZOOMA, NVS SPARQL, and Assertion (an Assertion in DwC‑DP is the MeasurementOrFact pattern for numeric or categorical facts).
- Keep DwC mapping UX simple: default OFF; one toggle to include DwC mappings in `suggest_semantics()` output, labeled as “export mapping (DwC‑DP)” so biologists aren’t overwhelmed.
  - Implemented as `include_dwc = TRUE/FALSE` in `suggest_semantics()` (R and salmonpy); when TRUE, attaches `dwc_mappings` alongside `semantic_suggestions`.

### Vignette snippets (ready to paste)

`vignettes/reusing-standards-salmon-data-terms.Rmd`

```r
library(metasalmon)
devtools::load_all(".")
find_terms("spawner count",
           role = "property",
           sources = sources_for_role("property")) |>
  dplyr::select(label, source, ontology, score, alignment_only) |>
  head()
```

`vignettes/data-dictionary-publication.Rmd`

```r
dict <- readr::read_csv("inst/extdata/column_dictionary.csv")
sem <- suggest_semantics(dict, include_dwc = TRUE)
attr(sem, "dwc_mappings") |>
  dplyr::filter(dwc_table %in% c("event", "occurrence")) |>
  dplyr::select(column_name, dwc_table, dwc_field, term_iri)
```

`vignettes/gpt-collaboration.Rmd`

```
Prompt helper: "Use metasalmon as the canonical semantic source. Propose ontology IRIs first. If asked for DwC export hints, include the DwC-DP mappings from suggest_semantics(..., include_dwc = TRUE); do not replace the ontology suggestions."
```

`vignettes/glossary.Rmd`

- DwC-DP: Darwin Core Data Package; a Frictionless-style profile for DwC tables.
- Assertion: DwC-DP MeasurementOrFact pattern (assertionType/value/unit).
- ZOOMA: EBI text-to-ontology annotation service; returns a confidence tier.
- NVS SPARQL: NERC Vocabulary Server SPARQL endpoint used for P01/P06.

`vignettes/metasalmon.Rmd`

```r
sem <- suggest_semantics(dict)                  # default: DwC export off
sem_with_dwc <- suggest_semantics(dict, include_dwc = TRUE)
```

## DwC‑DP cache expansion (measurement/material/identification)

- DwC‑DP uses **Assertion** tables (Assertion is the MeasurementOrFact pattern in DwC‑DP) rather than a single `measurement` table. Capture `material-assertion` fields (assertionType/value/unit) as the measurement path in the cache.
- Keep `material` and `identification` tables in the cache to support specimen/sample workflows; include core identifiers and provenance fields (e.g., `materialEntityID`, `catalogNumber`, `identifiedBy`, `dateIdentified`).
- Align with SDP spec: ensure DwC export hints can be expressed via `column_dictionary.csv` + `tables.csv` without violating SDP rules (no schema drift; SDP remains canonical, DwC is a derived/export view).
- Validation pointers: document how to run DwC‑DP validators/profile checks when generating DwC exports (look for GBIF/OH DWC-DP validators; if none, use frictionless datapackage validation + schema alignment checks).

## DwC terms vs I‑ADOPT in `suggest_semantics()`

I‑ADOPT (a framework for describing observed variables) integration thoughts:

- If a DwC‑DP mapping exists, use the DwC field IRI to inform the **property** role (e.g., DwC `lifeStage` or `sex`) while keeping I‑ADOPT components as primary for measurement columns.
- For Assertion tables, map `assertionType` → I‑ADOPT property, `assertionValueNumeric` → value, and `assertionUnit`/`assertionUnitIRI` → unit; treat `assertionBy`/`assertionProtocolID` as method hints.
- Keep DwC field suggestions separate from `find_terms()` results; expose them as a parallel suggestion set rather than mixing search sources.
- There is no `suggest_terms()` function today; if one is introduced, align it with `suggest_semantics()` output so DwC‑DP mappings and ontology candidates share the same suggestion schema.
- **Positioning DwC:** DwC/DwC‑DP remain an interoperability/export layer (table/field mappings) per dfo-salmon-ontology conventions; canonical semantics and ranking stay with domain/preferred ontologies (gcdfo, STATO/OBA, QUDT, etc.). If a DwC field is the best available fit, surface it with an explicit “export/alignment” flag so users opt in consciously.

## Phase 4 direction (matching quality, embeddings, A/B)

- Add optional embedding rerank (local sentence embeddings) applied over the top lexical candidates from `find_terms()`; guard with a flag and cache embeddings to avoid heavy dependencies by default. **Status:** planned.
- Run lightweight A/B checks: compare MRR / top‑k hit rates on a small curated query→expected IRI set (include salmon + DwC/DwC‑DP examples) with and without rerank/field‑aware boosts. **Status:** planned.
- Investigate ZOOMA confidence model to reuse evidence tiers instead of custom heuristics. **Status:** planned.
- Consider using Ontology Access Kit (OAK) or similar to avoid re‑implementing multi‑backend search if it provides measurable quality or maintenance wins. **Status:** planned.

### Phase 4 prework findings (2026-01-17)

**ZOOMA confidence model review**
- The annotate endpoint returns `confidence` tiers (`HIGH`, `GOOD`, `MEDIUM`, `LOW`) plus `semanticTags` and an `annotator` field that signals evidence (curated vs automatic).
- Recommended scoring crosswalk for `.score_and_rank_terms()`:
  - curated + `HIGH` or `GOOD`: keep and add a modest boost (about +0.75) on top of lexical scoring.
  - curated + `MEDIUM`: keep with a smaller boost (about +0.35); surface `confidence` in the result for explainability.
  - automatic + `LOW`: either drop or add a small penalty (about -0.25) to avoid noisy matches.
- Implementation guardrails: keep raw `confidence`, `annotator`, and `annotatedProperty` fields in results; resolve labels/definitions through OLS as today; treat missing `confidence` as neutral (no boost or penalty).

**Ontology Access Kit (OAK) feasibility check**
- Strengths: provides a unified search API with OLS, BioPortal, pronto, and SPARQL backends; has caching and CLI support.
- Gaps for us: no coverage for NVS SPARQL, ICES code lists, GBIF/WoRMS taxon APIs, or DwC-DP mapping; would be Python-only and add another dependency layer for R via `reticulate`.
- Cost/benefit: adds setup and dependency weight without reducing our custom clients for salmon-specific sources.
- Decision: defer adoption. Keep current per-source adapters; if we later want a unified search layer for OLS/BioPortal, pilot OAK as an optional backend in salmonpy first and only if it measurably simplifies maintenance.

## Alignment / open questions

- Could not locate `smn-gpt/CONVENTIONS.md`; defaulted to `dfo-salmon-ontology` conventions (DwC mostly properties; Wikidata alignment‑only; salmon ontologies preferred). Please point me to the smn-gpt conventions if different.
- Confirm whether DwC “pending” IRIs (e.g., `example.com/term-pending`) should be included or filtered; currently retained in the cache for completeness.

## Notes:

Next-step considerations (Phase 4+)

- **String distance limits:** current matching uses simple string distance (edit distance, the minimum number of single‑character edits to transform one string into another); consider whether this is sufficient for noisy column labels and synonyms.
- **Embedding rerank:** evaluate sentence embeddings (vector embeddings are numeric representations of text meaning) for reranking DwC‑DP and ontology candidates, not just lexical matches; keep this optional to avoid heavy dependencies.
- **ZOOMA behavior:** investigate how ZOOMA scores annotations (curated vs automated, evidence types) to see if we can reuse or approximate its confidence model rather than reinventing it.
- **Prebuilt search frameworks:** assess whether Ontology Access Kit (OAK) or other ontology search libraries already provide multi‑backend search + ranking so we can replace parts of our custom stack instead of expanding it.
- **Cross‑repo reuse:** if embeddings or NLP (NLP is natural language processing) are added, design a shared search module usable by both `metasalmon` and `salmonpy` to avoid divergence.

EBI OLS4 search has lots of knobs + LLM similarity endpoints
Search API supports ontology=, type=, queryFields=, exact=, local=, childrenOf=/allChildrenOf=, and rows= via https://www.ebi.ac.uk/ols4/api/search.
OpenAPI spec at https://www.ebi.ac.uk/ols4/v3/api-docs shows “LLM” endpoints (embeddings + “similar” lookups), e.g. /api/v2/ontologies/{onto}/classes/{class}/llm_similar and /api/v2/classes/llm_embedding (vector-in, not text-in).
EBI ZOOMA provides text→term annotation
https://www.ebi.ac.uk/spot/zooma/v2/api/services/annotate?propertyValue=... returns semanticTags (IRIs) plus a confidence and OLS links. This is a strong “candidate generator” to complement plain search.
NVS “search_nvs” JSON appears broken (returns HTML), but NVS SPARQL works
The URLs you’re using (https://vocab.nerc.ac.uk/search_nvs/P01/?q=...&format=json) are returning HTML now, which explains why sources="nvs" currently yields nothing.
The SPARQL endpoint works and can return JSON: https://vocab.nerc.ac.uk/sparql/ with Accept: application/sparql-results+json.
Biodiversity/fisheries vocab sources you can leverage
Darwin Core terms as RDF: https://rs.tdwg.org/dwc/terms.rdf (useful for common ecological “counts”, “lifeStage”, “sex”, etc.).
DwC-DP table schemas (core): https://github.com/gbif/dwc-dp/tree/0.1/dwc-dp/table-schemas
CF Standard Name Table (environment variables): https://cfconventions.org/Data/cf-standard-names/current/src/cf-standard-name-table.xml.
Taxon resolvers (great for entity_iri when entity is a species):
GBIF: https://api.gbif.org/v1/species/match?name=Oncorhynchus%20kisutch
WoRMS: https://www.marinespecies.org/rest/AphiaRecordsByName/Oncorhynchus%20kisutch?like=true&marine_only=false&offset=1
Similar “pluggable ontology access” design
The Ontology Access Kit (OAK) docs (https://incatools.github.io/ontology-access-kit/) show a mature pattern: multiple backends (OLS, SPARQL endpoints, etc.) behind a consistent “search” API.
Other sources worth adding (pragmatic, high value)

ZOOMA as a new "zooma" source: great for short phrases and for getting “computed annotation” style matches + confidence.
NVS via SPARQL as a fixed "nvs" source (or a new "nvs_sparql"): lets you search skos:prefLabel, skos:altLabel, skos:definition, and restrict to P01/P06 (or broader) via URI prefixes.
Darwin Core as a lightweight local index: parse terms.rdf once (at build-time or cached download) into a data frame and search it with lexical + fuzzy matching; this will often give better “field name” matches than biomedical ontologies.
GBIF/WoRMS as a separate “entity resolver” path: when the entity is a taxon, these APIs are more reliable than ontology-wide keyword search.
Cutting-edge (and realistic) matching strategy

Decompose the query into I-ADOPT-ish parts (even heuristically):
Detect/normalize tokens from column_name, column_label, column_description (underscores, camelCase, abbreviations).
Extract likely property (“count”, “abundance”, “length”), entity (“spawner”, “juvenile”), constraints (“natural”, “female”), method (“sonar”, “visual survey”), unit (“fish”, “kg”).
Role-aware candidate generation
For each role, query different sources and/or restrict ontologies:
unit: NVS P06 (SPARQL) + OLS restricted to unit ontologies.
property: STATO/OBA/other measurement ontologies; NVS P01 can help when it works.
entity: taxa services + organism/environment ontologies; local salmon ontology should be boosted hard.
method: OBI and friends.
Add ZOOMA annotate in parallel (it often “snaps” short phrases to a canonical term).
Modern reranking
Keep your current lexical scoring, but add:
Field-aware boosts: label match > synonym match > definition match.
Cross-source agreement boost (same IRI or same label from multiple sources).
Domain boost: ontologies/vocabs you trust for salmon (DFO salmon TTL, Darwin Core, NVS, OBA/STATO/UO) outrank generic biomedical hits.
Embedding rerank (optional but big payoff):
Locally embed (query) and (candidate label + definition) with a strong sentence embedding model and cosine-rerank the top ~50 lexical candidates.
This avoids requiring OLS’s vector endpoint (since OLS expects vectors in its own embedding space).
Explainability + diagnostics
Always return why something ranked high: matched tokens, source, ontology preference, confidence (ZOOMA), etc.
Capture per-source failures (timeouts, HTML instead of JSON, 429s) so “empty results” can be explained instead of mysterious.
Most important “similar issue” to fix next

Your "nvs" source is effectively non-functional right now because the endpoint is returning HTML. Switching NVS lookup to SPARQL (or detecting HTML and warning) will prevent silent empties that look like “no matches exist”.
If you want, I can implement the next concrete step: add a "zooma" source and replace "nvs" with an NVS SPARQL adapter (in both metasalmon and salmonpy), plus a small diagnostics attribute so empty results report which sources failed.

Fix the NVS thing for sure. Then read ../dfo-salmon-ontology/docs/CONVENTIONS.md to learn a bit more about ranked preferences regarding using specific ontologies which may slightly tweak your plan (but honestly your plan is pretty much on point already, no major reworks required). Just keep in mind Darwin Core terms are mostly RDF properties AND I want to rely on the new Darwin Core Data Package and Conceptual Model rather than the older DwC terms. Otherwise, let's create the plan and then do a final web search for salmon specific data standards or services that might already be out there that we should leverage.

NVS Fix

Replaced the broken search_nvs/\*?format=json calls (now returning HTML) with NVS SPARQL queries restricted to P01 (observables) + P06 (units) in metasalmon and salmonpy (term_search.R (line 111), term_search.py (line 105)).
Updated tests to assert the SPARQL endpoint + Accept header usage (test-term-search.R (line 1), test_term_search.py (line 1)).
Verification

R: Rscript -e "devtools::test('/Users/brettjohnson/code/metasalmon', filter = 'term-search')" and Rscript -e "devtools::test('/Users/brettjohnson/code/metasalmon', filter = 'dictionary-helpers')" (pass)
Python: python3 -m unittest salmonpy.tests.test_term_search salmonpy.tests.test_semantics (pass; run from /Users/brettjohnson/code so salmonpy imports correctly)
Updated Roadmap (aligned to dfo-salmon-ontology conventions + your DwC-DP preference)

Phase 1 (sources + guardrails): keep OLS + NVS(SPARQL) + BioPortal; add ZOOMA as a candidate generator; add ICES Vocab API as a code-list/controlled-vocab source (separate from ontology term search).
Phase 2 (ontology preferences by role): encode a ranked allowlist per I-ADOPT role (e.g., units prefer QUDT + NVS P06; methods prefer gcdfo: SKOS + SOSA/PROV patterns; keep Wikidata alignment-only; AGROVOC OK for generic indexing/alignment).
Phase 3 (Darwin Core done “the new way”): don’t index old DwC term IRIs as “terms”; instead add a dedicated mapping helper that uses DwC Conceptual Model + DwC Data Package artifacts (schemas/profile) to suggest DwC-DP table/field mappings and only then the appropriate dwc/dwciri properties where relevant.
Phase 4 (matching quality): role-aware query expansion + per-source diagnostics + optional embedding rerank over top lexical candidates (local model; keep ontology service calls as candidate generation).
Salmon/Fisheries Standards & Services Found (web scan)

gbif/dwc-dp (DwC Conceptual Model + DwC Data Package guide + Frictionless schemas): https://gbif.github.io/dwc-dp/ and https://github.com/gbif/dwc-dp
Salmon standards effort + ontology modules: https://github.com/salmon-data-mobiliszation/salmon-data-standards-workshop and https://github.com/salmon-data-mobiliszation/salmon-domain-ontology
ICES controlled vocab service (REST + Swagger): https://vocab.ices.dk/services/api/swagger/index.html
Broader fisheries ontologies to evaluate for alignment-only reuse: https://github.com/WorldFishCenter/fish-ontology, https://github.com/AileenUKennedy/FOD-ontology-v1.0
