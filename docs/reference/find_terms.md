# Find candidate terms across external vocabularies

Lightweight meta-search helper for IRIs. Uses public APIs when
available:

- OLS (no key): broad cross-ontology search

- NERC NVS P01/P06 (best-effort; returns empty on failure)

- BioPortal (optional; requires API key via env `BIOPORTAL_APIKEY`)

## Usage

``` r
find_terms(query, role = NA_character_, sources = c("ols", "nvs"))
```

## Arguments

- query:

  Character search string.

- role:

  Optional I-ADOPT role hint (`"variable"`, `"property"`, `"entity"`,
  `"constraint"`, `"method"`, `"unit"`); used only for metadata tagging.

- sources:

  Character vector of sources to query (`"ols"`, `"nvs"`,
  `"bioportal"`).

## Value

Tibble with columns `label`, `iri`, `source`, `ontology`, `role`,
`match_type`, `definition`.

## Details

Results are scored using role-specific I-ADOPT vocabulary hints (from
`inst/extdata/iadopt-terminologies.csv`) and deterministic tie-breakers
so the ordering is stable.

Network calls are best-effort and will return an empty tibble on
failure.
