# Infer Salmon Data Package artifacts from resource tables

Infers column dictionaries, table metadata, candidate code lists, and
dataset-level metadata in a single step from one or more raw data
tables.

## Usage

``` r
infer_salmon_datapackage_artifacts(
  resources,
  dataset_id = "dataset-1",
  table_id = "table_1",
  guess_types = TRUE,
  seed_semantics = TRUE,
  semantic_sources = c("smn", "gcdfo", "ols", "nvs"),
  semantic_max_per_role = 1,
  seed_verbose = TRUE,
  seed_codes = NULL,
  seed_table_meta = TRUE,
  seed_dataset_meta = NULL,
  semantic_code_scope = c("factor", "all", "none"),
  llm_assess = FALSE,
  llm_provider = c("openai", "openrouter", "openai_compatible"),
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

- resources:

  Either a named list of data frames (one per resource table) or a
  single data frame (converted internally to a one-table list).

- dataset_id:

  Dataset identifier applied to all inferred metadata.

- table_id:

  Name used when `resources` is a single data frame.

- guess_types:

  Logical; if `TRUE` (default), infer `value_type` for each dictionary
  column.

- seed_semantics:

  Logical; if `TRUE`, run
  [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md)
  and attach semantic suggestions to the returned dictionary.

- semantic_sources:

  Vector of vocabulary sources passed to
  [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md).

- semantic_max_per_role:

  Maximum number of suggestions retained per I-ADOPT role.

- seed_verbose:

  Logical; if TRUE, emit progress messages while seeding semantic
  suggestions.

- seed_codes:

  Optional `codes.csv`-style seed metadata.

- seed_table_meta:

  Optional `tables.csv`-style seed metadata.

- seed_dataset_meta:

  Optional `dataset.csv`-style seed metadata.

- semantic_code_scope:

  Character string controlling which `codes.csv` rows are sent through
  [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md)
  during one-shot seeding. `"factor"` (default) analyzes codes sourced
  from factor columns and low-cardinality character columns in the
  original data frame(s); `"all"` analyzes all inferred or supplied code
  rows; `"none"` skips code-level semantic suggestions.

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

A named list with the following components:

- `resources`: Named list of input tables

- `dict`: Inferred dictionary tibble

- `table_meta`: Inferred table metadata tibble

- `codes`: Inferred candidate codes tibble

- `dataset_meta`: Inferred dataset metadata one-row tibble

- `semantic_suggestions`: Semantic suggestion tibble (or `NULL`)

- `semantic_llm_assessments`: Target-level LLM review summary tibble (or
  `NULL`)

## Details

This is a convenience helper for biologists who want to get from raw
data frames to package-ready metadata artifacts with one call.

## Examples

``` r
if (FALSE) { # \dontrun{
resources <- list(
  catches = data.frame(
    station_id = c("A", "B"),
    species = c("Coho", "Chinook"),
    count = c(10L, 20L),
    sample_date = as.Date(c("2024-01-01", "2024-01-02"))
  ),
  stations = data.frame(
    station_id = c("A", "B"),
    latitude = c(49.8, 49.9),
    longitude = c(-124.4, -124.5)
  )
)

artifacts <- infer_salmon_datapackage_artifacts(
  resources,
  dataset_id = "demo-1",
  seed_semantics = TRUE,
  seed_verbose = TRUE
)

dict <- artifacts$dict
table_meta <- artifacts$table_meta
codes <- artifacts$codes
dataset_meta <- artifacts$dataset_meta
} # }
```
