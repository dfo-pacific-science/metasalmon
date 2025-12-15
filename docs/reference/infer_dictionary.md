# Infer a starter dictionary from a data frame

Proposes a starter dictionary (column dictionary schema) from raw data
by guessing column types, roles, and basic metadata. IRIs and semantic
fields are left blank for manual or GPT-assisted completion.

## Usage

``` r
infer_dictionary(
  df,
  guess_types = TRUE,
  dataset_id = "dataset-1",
  table_id = "table-1"
)
```

## Arguments

- df:

  A data frame or tibble to analyze

- guess_types:

  Logical; if `TRUE` (default), infer value types from data

- dataset_id:

  Character; dataset identifier (default: "dataset-1")

- table_id:

  Character; table identifier (default: "table-1")

## Value

A tibble with dictionary schema columns: `dataset_id`, `table_id`,
`column_name`, `column_label`, `column_description`, `column_role`,
`value_type`, `unit_label`, `unit_iri`, `term_iri`, `term_type`,
`required`

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  species = c("Coho", "Chinook"),
  count = c(100, 200),
  date = as.Date(c("2024-01-01", "2024-01-02"))
)
dict <- infer_dictionary(df)
} # }
```
