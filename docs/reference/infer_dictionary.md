# Infer a starter dictionary from a data frame

Proposes a starter dictionary (column dictionary schema) from raw data
by guessing column types, roles, and basic metadata.

## Usage

``` r
infer_dictionary(
  df,
  guess_types = TRUE,
  dataset_id = "dataset-1",
  table_id = "table_1",
  seed_semantics = FALSE,
  semantic_sources = c("smn", "gcdfo", "ols", "nvs"),
  semantic_max_per_role = 1,
  seed_verbose = TRUE,
  seed_codes = NULL,
  seed_table_meta = NULL,
  seed_dataset_meta = NULL,
  llm_assess = FALSE,
  llm_provider = c("openai", "openrouter", "openai_compatible", "chapi"),
  llm_model = NULL,
  llm_api_key = NULL,
  llm_base_url = NULL,
  llm_top_n = 5L,
  llm_context_files = NULL,
  llm_context_text = NULL,
  llm_timeout_seconds = 60,
  llm_request_fn = NULL
)
```

## Arguments

- df:

  A data frame or tibble to analyze. Or, when provided as a named list
  of data frames, `infer_dictionary()` infers each table and returns a
  combined dictionary.

- guess_types:

  Logical; if `TRUE` (default), infer value types from data.

- dataset_id:

  Character; dataset identifier (default: "dataset-1").

- table_id:

  Character; table identifier (default: "table_1").

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

- seed_codes:

  Optional `codes.csv`-style tibble forwarded to
  [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md)
  when `seed_semantics = TRUE`.

- seed_table_meta:

  Optional `tables.csv`-style tibble forwarded to
  [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md)
  when `seed_semantics = TRUE`.

- seed_dataset_meta:

  Optional `dataset.csv`-style tibble forwarded to
  [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md)
  when `seed_semantics = TRUE`.

- llm_assess:

  Logical; if `TRUE`, run the optional LLM shortlist assessment inside
  [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md).

- llm_provider:

  LLM provider preset forwarded to
  [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md).

- llm_model:

  Optional LLM model identifier forwarded to
  [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md).

- llm_api_key:

  Optional API key override forwarded to
  [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md).

- llm_base_url:

  Optional OpenAI-compatible base URL forwarded to
  [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md).

- llm_top_n:

  Maximum number of retrieved candidates sent to the LLM per target.

- llm_context_files:

  Optional local context files forwarded to
  [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md).

- llm_context_text:

  Optional inline context snippets forwarded to
  [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md).

- llm_timeout_seconds:

  Timeout for each LLM request in seconds.

- llm_request_fn:

  Advanced/test hook overriding the low-level OpenAI-compatible request
  function.

## Value

A tibble with dictionary schema columns in canonical Salmon Data Package
order: `dataset_id`, `table_id`, `column_name`, `column_label`,
`column_description`, `term_iri`, `property_iri`, `entity_iri`,
`constraint_iri`, `method_iri`, `unit_label`, `unit_iri`, `term_type`,
`value_type`, `column_role`, `required`.

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
