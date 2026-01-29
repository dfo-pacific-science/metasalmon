# Suggest Darwin Core Data Package mappings for dictionary columns

Uses DwC Conceptual Model + DwC-DP table schemas (cached locally) to
suggest likely table/field mappings for column dictionary entries, and
returns the associated Darwin Core property IRIs for review.

## Usage

``` r
suggest_dwc_mappings(dict, max_per_column = 3)
```

## Arguments

- dict:

  A dictionary tibble with `column_name`, and optionally `column_label`
  and `column_description`.

- max_per_column:

  Maximum number of mapping suggestions per column.

## Value

The dictionary tibble with a `dwc_mappings` attribute containing
suggestions (columns: `column_name`, `table_id`, `field_name`,
`field_label`, `term_iri`, `match_score`, `match_basis`).

## Examples

``` r
if (FALSE) { # \dontrun{
dict <- infer_dictionary(mtcars)
dict <- suggest_dwc_mappings(dict)
attr(dict, "dwc_mappings") %>% head()
} # }
```
