# Apply semantic suggestions into a dictionary

Copies selected IRIs from a `semantic_suggestions` tibble into the
matching dictionary fields. Suggestions remain separate by default; this
helper gives you an explicit merge step when you decide the top
candidates are good enough.

## Usage

``` r
apply_semantic_suggestions(
  dict,
  suggestions = attr(dict, "semantic_suggestions"),
  strategy = c("top", "llm"),
  columns = NULL,
  roles = NULL,
  min_score = NULL,
  min_llm_confidence = NULL,
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- dict:

  A dictionary tibble, typically returned by
  [`infer_dictionary()`](https://dfo-pacific-science.github.io/metasalmon/reference/infer_dictionary.md)
  or
  [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md).

- suggestions:

  A suggestions tibble, usually `attr(dict, "semantic_suggestions")`. If
  omitted, the function reads that attribute from `dict`.

- strategy:

  Selection strategy per column-role pair. `"top"` keeps the original
  lexical ranking; `"llm"` applies only candidates marked with
  `llm_selected = TRUE` by `suggest_semantics(..., llm_assess = TRUE)`.

- columns:

  Optional character vector limiting application to specific
  `column_name` values.

- roles:

  Optional character vector limiting application to specific suggestion
  roles: `"variable"`, `"property"`, `"entity"`, `"unit"`,
  `"constraint"`, `"method"`.

- min_score:

  Optional numeric threshold. Only available when `suggestions` includes
  a `score` column; otherwise the function errors.

- min_llm_confidence:

  Optional numeric threshold for `strategy = "llm"`. Requires
  `llm_confidence` in `suggestions`.

- overwrite:

  Logical; if `FALSE` (default), only missing fields are filled. Set
  `TRUE` to intentionally replace existing IRIs.

- verbose:

  Logical; if `TRUE` (default), print a short summary.

## Value

The dictionary tibble with selected semantic IRI fields filled in.

## Details

Matching is done by both `column_name` and `dictionary_role`. When the
suggestions tibble also includes `dataset_id` and `table_id`, those keys
are honored too. Suggestions that target non-column destinations (for
example `codes.csv`, `tables.csv`, or `dataset.csv`) are ignored by this
helper and remain review-only.

## Examples

``` r
if (FALSE) { # \dontrun{
dict <- infer_dictionary(my_data, dataset_id = "example", table_id = "main")
dict <- suggest_semantics(my_data, dict)

# Fill only the missing semantic fields for one measurement column
dict <- apply_semantic_suggestions(dict, columns = "SPAWNER_COUNT")

# Require stronger lexical matches when score is available
dict <- apply_semantic_suggestions(dict, min_score = 2)
} # }
```
