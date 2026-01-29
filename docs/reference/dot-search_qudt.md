# Search QUDT for unit terms

Preferred source for unit role (per dfo-salmon-ontology CONVENTIONS).
Uses the QUDT SPARQL endpoint to find matching unit terms.

## Usage

``` r
.search_qudt(query, role)
```

## Arguments

- query:

  Search query string

- role:

  I-ADOPT role (typically "unit")

## Value

Tibble of matching terms
