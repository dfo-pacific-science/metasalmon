# Infer a starter dictionary from a data frame

Proposes a starter dictionary (column dictionary schema) from raw data
by guessing column types, roles, and basic metadata.

## Usage

``` r
infer_dictionary(
  df,
  guess_types = TRUE,
  dataset_id = "dataset-1",
  table_id = "table-1",
  seed_semantics = FALSE,
  semantic_sources = c("smn", "gcdfo", "ols", "nvs"),
  semantic_max_per_role = 1,
  seed_verbose = TRUE
)
```

## Arguments

- df:

  A data frame or tibble to analyze.

- guess_types:

  Logical; if `TRUE` (default), infer value types from data.

- dataset_id:

  Character; dataset identifier (default: "dataset-1").

- table_id:

  Character; table identifier (default: "table-1").

- seed_semantics:

  Logical; if `TRUE`, run
  [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md)
  and attach the resulting `semantic_suggestions` attribute to the
  returned dictionary.

- semantic_sources:

  Character vector of vocabulary sources passed to
  [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md)
  when `seed_semantics = TRUE`. Default:
  `c("smn", "gcdfo", "ols", "nvs")`.

- semantic_max_per_role:

  Maximum number of suggestions retained per I-ADOPT role when seeding
  suggestions. Default: `1`.

- seed_verbose:

  Logical; if TRUE, print a short progress message while seeding
  semantic suggestions.

## Value

A tibble with dictionary schema columns: `dataset_id`, `table_id`,
`column_name`, `column_label`, `column_description`, `column_role`,
`value_type`, `unit_label`, `unit_iri`, `term_iri`, `term_type`,
`required`, and I-ADOPT component fields (`property_iri`, `entity_iri`,
`constraint_iri`, `method_iri`).

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  species = c("Coho", "Chinook"),
  count = c(100, 200),
  date = as.Date(c("2024-01-01", "2024-01-02"))
)
dict <- infer_dictionary(df)

# Optional: seed semantic suggestions from vocabulary services
# (SMN is queried first; GCDFO is a distinct DFO-specific source)
dict <- infer_dictionary(
  df,
  seed_semantics = TRUE,
  semantic_sources = c("smn", "gcdfo", "ols", "nvs")
)
suggestions <- attr(dict, "semantic_suggestions")
} # }
```
