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
- [ ] Phase 2 — encode ontology preferences by role (salmon-first, QUDT units, Darwin Core scaffold, Wikidata alignment-only).
- [ ] Phase 3 — Darwin Core: prefer DwC Conceptual Model + DwC Data Package (DwC-DP), not legacy "DwC terms" search.
- [ ] Phase 4 — matching quality: role-aware query expansion, cross-source agreement boosts, optional embedding rerank.

## Surprises & Discoveries

- NVS `search_nvs/*?format=json` endpoints are currently returning HTML, causing silent empties unless detected.
- NVS SPARQL queries with OPTIONAL + REGEX on altLabel are very slow (>30s); simplified to prefLabel-only REGEX.
- Python urllib on some macOS configurations fails with DNS errors (`Errno 8` or `Errno 9`); curl fallback was essential.
- `dfo-salmon-ontology` conventions explicitly treat Darwin Core mostly as **properties** and as an **interoperability scaffold**, and recommend Wikidata **alignment-only**.
- DwC “the new way” is the **DwC Conceptual Model + DwC Data Package guide** (DwC-DP), hosted at `https://gbif.github.io/dwc-dp/` (schemas in `gbif/dwc-dp`).

## Decision Log

- 2026-01-16: Fix NVS by switching to NVS SPARQL (`https://vocab.nerc.ac.uk/sparql/`) restricted to P01/P06 rather than relying on `search_nvs`.
- 2026-01-16: Add ZOOMA as a candidate generator (`source = "zooma"`) that resolves returned `semanticTags` to OLS term metadata.
- 2026-01-16: Add ICES Vocab API as a **code-list** integration (separate from ontology term search), starting with code-type + code retrieval helpers.

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
  - `method`: `gcdfo:` SKOS methods + SOSA/PROV patterns
  - `entity`: salmon domain terms + taxa resolvers (GBIF/WoRMS) for species-like entities
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

- `find_terms(..., sources="nvs")` returns real results (non-empty label + IRI) for reasonable queries (e.g., “count”, “fish”).
- `find_terms(..., sources="zooma")` returns non-empty results for a phrase query (e.g., “spawner count”), with resolved labels/definitions (not just raw IRIs).
- ICES helpers return non-empty code types and codes (e.g., `Gear`) and allow basic filtering.
- Tests pass in both repos:
  - R: `devtools::test(..., filter = 'term-search|dictionary-helpers')`
  - Python: `python3 -m unittest salmonpy.tests.test_term_search salmonpy.tests.test_semantics`

## Concrete Steps (Phase 1)

Run from the appropriate repo directories:

- R package smoke:
  - `Rscript -e "devtools::load_all('../metasalmon'); metasalmon::find_terms('spawner count') %>% head()"`
  - `Rscript -e "devtools::load_all('../metasalmon'); metasalmon::find_terms('count', sources='nvs') %>% head()"`
  - `Rscript -e "devtools::load_all('../metasalmon'); metasalmon::find_terms('spawner count', sources='zooma') %>% head()"`
- Python package smoke:
  - `python3 -c "import sys; sys.path.insert(0,'../'); import salmonpy; print(salmonpy.find_terms('spawner count').head())"`

