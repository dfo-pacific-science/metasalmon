# Validate semantics with graceful gap reporting

Ensures structural requirements, adds a `required` column if missing,
runs
[`validate_dictionary()`](https://dfo-pacific-science.github.io/metasalmon/reference/validate_dictionary.md),
and reports missing `term_iri` for measurement columns without aborting
the entire run.

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

  Logical; if TRUE, require IRIs in all semantic fields.

- entity_defaults:

  Optional data frame with `table_prefix` and `entity_iri` (not applied
  automatically here but reserved for future use).

- vocab_priority:

  Optional character vector of vocab sources (reserved).

## Value

A list with elements:

- `dict`: normalized dictionary with `required` column.

- `issues`: tibble of structural issues (empty if none).

- `missing_terms`: tibble of measurement rows missing `term_iri`.
