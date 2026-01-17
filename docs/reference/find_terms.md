# Find candidate terms across external vocabularies

Lightweight meta-search helper for IRIs. Uses public APIs when
available:

- OLS (no key): broad cross-ontology search

- NERC NVS P01/P06 (via SPARQL endpoint)

- ZOOMA (no key): EBI text-to-term annotations (resolves to OLS term
  metadata)

- BioPortal (optional; requires API key via env `BIOPORTAL_APIKEY`)

## Usage

``` r
find_terms(query, role = NA_character_, sources = c("ols", "nvs"))
```

## Arguments

- query:

  Character search string (e.g., `"spawner count"`, `"temperature"`).

- role:

  Optional I-ADOPT role hint for ranking results. One of: `"variable"`
  (compound term), `"property"` (characteristic), `"entity"` (thing
  measured), `"constraint"` (qualifier), or `"unit"`. Results matching
  the specified role are ranked higher.

- sources:

  Character vector of sources to query (`"ols"`, `"nvs"`, `"zooma"`,
  `"bioportal"`).

## Value

Tibble with columns: `label`, `iri`, `source`, `ontology`, `role`,
`match_type`, `definition`. Returns empty tibble if no matches found.

## Details

**Supported sources:**

- **OLS** (Ontology Lookup Service): Broad cross-ontology search, no API
  key needed

- **NVS** (NERC Vocabulary Server): Marine and oceanographic terms
  (P01/P06)

- **BioPortal**: Requires API key via `BIOPORTAL_APIKEY` environment
  variable

Results are scored using I-ADOPT vocabulary hints and ranked by
relevance. Network calls are best-effort and return an empty tibble on
failure.

## See also

[`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md)
for automated suggestions based on your dictionary.

## Examples

``` r
if (FALSE) { # \dontrun{
# Search for terms matching "spawner count"
results <- find_terms("spawner count")
head(results)

# Search specifically for property terms
property_terms <- find_terms("temperature", role = "property")

# Search a specific source
ols_results <- find_terms("salmon", sources = "ols")

# Search multiple sources
all_results <- find_terms("escapement", sources = c("ols", "nvs"))
} # }
```
