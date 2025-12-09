# Apply a salmon dictionary to a data frame

Renames columns, coerces types, applies factor levels from codes, and
reports mismatches. Returns a transformed tibble ready for analysis or
packaging.

## Usage

``` r
apply_salmon_dictionary(df, dict, codes = NULL, strict = TRUE)
```

## Arguments

- df:

  A data frame or tibble to transform

- dict:

  A validated dictionary tibble

- codes:

  Optional tibble with code lists (columns: `dataset_id`, `table_id`,
  `column_name`, `code_value`, `code_label`, etc.)

- strict:

  Logical; if `TRUE` (default), errors on type coercion failures; if
  `FALSE`, warns and coerces to character

## Value

A tibble with renamed columns, coerced types, and factor levels applied

## Examples

``` r
if (FALSE) { # \dontrun{
dict <- infer_dictionary(mtcars)
validate_dictionary(dict)
applied <- apply_salmon_dictionary(mtcars, dict)
} # }
```
