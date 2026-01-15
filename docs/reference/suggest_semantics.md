# Suggest semantic annotations for a dictionary

Placeholder function for future GPT/LLM or heuristic-based suggestions
of IRIs, concept schemes, and semantic annotations for dictionary
fields.

## Usage

``` r
suggest_semantics(
  df,
  dict,
  sources = c("ols", "nvs"),
  max_per_role = 3,
  search_fn = find_terms
)
```

## Arguments

- df:

  A data frame or tibble

- dict:

  A dictionary tibble (may be incomplete)

- sources:

  Search sources to use for
  [`find_terms()`](https://dfo-pacific-science.github.io/metasalmon/reference/find_terms.md);
  default is OLS + NVS.

- max_per_role:

  Maximum suggestions to keep per role/column.

- search_fn:

  Function used to search terms (defaults to `find_terms`); useful for
  testing or custom search strategies.

## Value

The dictionary tibble (unchanged) with a `semantic_suggestions`
attribute containing suggested IRIs for missing fields.

## Examples

``` r
if (FALSE) { # \dontrun{
dict <- infer_dictionary(mtcars)
suggested <- suggest_semantics(mtcars, dict, sources = "ols")
} # }
```
