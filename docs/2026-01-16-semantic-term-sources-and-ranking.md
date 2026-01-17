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
- [ ] Phase 3 — Darwin Core: prefer DwC Conceptual Model + DwC Data Package (DwC-DP), not legacy "DwC terms" search.
- [ ] Phase 4 — matching quality: role-aware query expansion, cross-source agreement boosts, optional embedding rerank.

## Surprises & Discoveries

- NVS `search_nvs/*?format=json` endpoints are currently returning HTML, causing silent empties unless detected.
- NVS SPARQL queries with OPTIONAL + REGEX on altLabel are very slow (>30s); simplified to prefLabel-only REGEX.
- Python urllib on some macOS configurations fails with DNS errors (`Errno 8` or `Errno 9`); curl fallback was essential.
- `dfo-salmon-ontology` conventions explicitly treat Darwin Core mostly as **properties** and as an **interoperability scaffold**, and recommend Wikidata **alignment-only**.
- DwC "the new way" is the **DwC Conceptual Model + DwC Data Package guide** (DwC-DP), hosted at `https://gbif.github.io/dwc-dp/` (schemas in `gbif/dwc-dp`).
- **Phase 2**: QUDT SPARQL endpoint (`https://www.qudt.org/fuseki/qudt/sparql`) works well for unit lookups; returns JSON with `Accept: application/sparql-results+json`.
- **Phase 2**: GBIF species match API (`/v1/species/match`) returns single best match; fallback to `/v1/species/search` for broader results.
- **Phase 2**: WoRMS API returns data frames directly; `AphiaRecordsByMatchNames` endpoint useful for fuzzy matching.

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

Notes:
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
