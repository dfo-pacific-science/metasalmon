# Expand query based on role context (Phase 4)

Generates additional query variants based on role-specific patterns. For
example, unit queries get "unit" suffix, entity queries get species name
variants.

## Usage

``` r
.expand_query(query, role)
```

## Arguments

- query:

  Original query string

- role:

  I-ADOPT role hint

## Value

Character vector of query variants (original always first)
