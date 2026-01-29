# Find candidate terms across external vocabularies

Lightweight meta-search helper for IRIs. Uses public APIs when
available. Implements role-aware ontology preferences per
dfo-salmon-ontology CONVENTIONS.

## Usage

``` r
find_terms(
  query,
  role = NA_character_,
  sources = c("ols", "nvs"),
  expand_query = TRUE
)
```

## Arguments

- query:

  Character search string (e.g., `"spawner count"`, `"temperature"`).

- role:

  Optional I-ADOPT role hint for ranking and source selection. One of:
  `"variable"` (compound term), `"property"` (characteristic),
  `"entity"` (thing measured), `"constraint"` (qualifier), `"method"`,
  or `"unit"`. When specified, sources are optimized for the role and
  results are ranked higher when they match preferred ontologies for
  that role.

- sources:

  Character vector of vocabulary sources to query. Options: `"ols"`,
  `"nvs"`, `"zooma"`, `"qudt"`, `"gbif"`, `"worms"`, `"bioportal"`.
  Default is `c("ols", "nvs")`. Use
  [`sources_for_role()`](https://dfo-pacific-science.github.io/metasalmon/reference/sources_for_role.md)
  to get role-optimized sources.

- expand_query:

  Logical. If `TRUE` (default), applies role-aware query expansion
  (Phase 4) to generate additional query variants based on the role
  context. For example, unit queries get abbreviation expansions, method
  queries get "method" suffix added. Set to `FALSE` to search only the
  exact query.

## Value

Tibble with columns: `label`, `iri`, `source`, `ontology`, `role`,
`match_type`, `definition`, `score`, `alignment_only`,
`agreement_sources`, `zooma_confidence`, `zooma_annotator`. The `score`
column shows the computed ranking score. The `alignment_only` column
indicates terms from Wikidata (useful for crosswalks but not canonical
modeling). The `agreement_sources` column indicates how many sources
returned the same IRI or label (Phase 4 cross-source agreement). Returns
empty tibble if no matches found.

The result has a `"diagnostics"` attribute (access via
`attr(result, "diagnostics")`) containing per-source/query diagnostic
information: source, query, status (success/error), count, elapsed_secs,
and error message if applicable. This helps explain empty results or
slow queries.

## Details

**Supported sources:**

- **OLS** (Ontology Lookup Service): Broad cross-ontology search, no API
  key needed

- **NVS** (NERC Vocabulary Server): Marine and oceanographic terms
  (P01/P06)

- **ZOOMA** (EBI text-to-term annotations): Resolves to OLS term
  metadata

- **QUDT** (Quantities, Units, Dimensions and Types): Preferred for unit
  role

- **GBIF** (Global Biodiversity Information Facility): Taxon backbone
  for entity role

- **WoRMS** (World Register of Marine Species): Marine taxa for entity
  role

- **BioPortal**: Requires API key via `BIOPORTAL_APIKEY` environment
  variable

**Role-based ontology preferences (Phase 2):**

- `unit`: QUDT preferred, then NVS P06

- `property`: STATO/OBA measurement ontologies, NVS P01

- `entity`: gcdfo + NCEAS Salmon (ODO), GBIF/WoRMS for taxa

- `method`: gcdfo: SKOS + SOSA/PROV patterns, plus AGROVOC

- Wikidata is alignment-only (lower ranking for
  crosswalks/reconciliation)

Results are scored using I-ADOPT vocabulary hints and role-based
ontology preferences, then ranked by relevance. Network calls are
best-effort and return an empty tibble on failure.

## See also

[`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md)
for automated suggestions based on your dictionary.

[`sources_for_role()`](https://dfo-pacific-science.github.io/metasalmon/reference/sources_for_role.md)
for role-optimized source selection.

## Examples

``` r
if (FALSE) { # \dontrun{
# Search for terms matching "spawner count"
results <- find_terms("spawner count")
head(results)

# Search specifically for property terms
property_terms <- find_terms("temperature", role = "property")

# Search for units with QUDT preference
unit_terms <- find_terms("kilogram", role = "unit", sources = sources_for_role("unit"))

# Search for taxa using taxon resolvers
taxa <- find_terms("Oncorhynchus kisutch", role = "entity", sources = c("gbif", "worms"))

# Search a specific source
ols_results <- find_terms("salmon", sources = "ols")

# Search multiple sources
all_results <- find_terms("escapement", sources = c("ols", "nvs"))
} # }
```
