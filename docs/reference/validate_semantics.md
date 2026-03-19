# Validate semantics with graceful gap reporting

Ensures structural requirements, adds a `required` column if missing,
runs
[`validate_dictionary()`](https://dfo-pacific-science.github.io/metasalmon/reference/validate_dictionary.md),
and reports measurement rows missing `term_iri`. Also flags
non-canonical Salmon ontology IRIs so source boundaries stay explicit
(`smn` under `https://w3id.org/smn/`, `gcdfo` under
`https://w3id.org/gcdfo/salmon#`). In non-strict mode
(`require_iris = FALSE`), semantic gaps emit warnings but do not fail
the overall call.

## Usage

``` r
validate_semantics(
  dict,
  require_iris = FALSE,
  entity_defaults = NULL,
  vocab_priority = NULL
)
```

## Arguments

- dict:

  Dictionary tibble/data frame.

- require_iris:

  Logical; if TRUE, require non-empty semantic fields (`term_iri`,
  `property_iri`, `entity_iri`, `unit_iri`) for measurement rows.

- entity_defaults:

  Deprecated and ignored. Previously reserved for future default entity
  mapping.

- vocab_priority:

  Deprecated and ignored. Previously reserved for future vocabulary
  ordering.

## Value

A list with elements:

- `dict`: normalized dictionary with `required` column.

- `issues`: tibble of structural issues (empty if none).

- `missing_terms`: tibble of measurement rows missing `term_iri`.
