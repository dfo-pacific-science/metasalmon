# Suggest semantic annotations for a dictionary

Searches external vocabularies to suggest IRIs for semantic gaps in the
dictionary and package metadata. Measurement columns keep full I-ADOPT
decomposition (`term_iri`, `property_iri`, `entity_iri`, `unit_iri`,
`constraint_iri`), while selected non-measurement columns can receive
lighter `term_iri` coverage when they are categorical or controlled
low-cardinality attributes.

## Usage

``` r
suggest_semantics(
  df,
  dict,
  sources = c("smn", "gcdfo", "ols", "nvs"),
  include_dwc = FALSE,
  max_per_role = 3,
  search_fn = find_terms,
  codes = NULL,
  table_meta = NULL,
  dataset_meta = NULL,
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

- df:

  A data frame or tibble containing the data being documented, or a
  named list of data frames for multi-table workflows. When a named list
  is supplied, `suggest_semantics()` matches each dictionary row to the
  correct table via `dict$table_id` and uses that table's data as
  context.

- dict:

  A dictionary tibble created by
  [`infer_dictionary()`](https://dfo-pacific-science.github.io/metasalmon/reference/infer_dictionary.md)
  (may have incomplete semantic fields).

- sources:

  Character vector of vocabulary sources to search. Options are `"smn"`
  (Salmon Domain Ontology via content negotiation), `"gcdfo"`
  (DFO-specific source), `"ols"` (Ontology Lookup Service), `"nvs"`
  (NERC Vocabulary Server), and `"bioportal"` (requires
  `BIOPORTAL_APIKEY` environment variable). Default is
  `c("smn", "gcdfo", "ols", "nvs")`.

- include_dwc:

  Logical; if `TRUE`, also attach DwC-DP export mappings (via
  [`suggest_dwc_mappings()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_dwc_mappings.md))
  as a parallel attribute `dwc_mappings`. Default is `FALSE` to keep the
  UI simple for non-DwC users.

- max_per_role:

  Maximum number of suggestions to keep per I-ADOPT role (variable,
  property, entity, unit, constraint) per column. Default is 3.

- search_fn:

  Function used to search terms. Defaults to
  [`find_terms()`](https://dfo-pacific-science.github.io/metasalmon/reference/find_terms.md).
  Can be replaced for testing or custom search strategies.

- codes:

  Optional `codes.csv`-like tibble. When provided, suggestions are also
  generated for missing `codes.csv$term_iri` targets.

- table_meta:

  Optional `tables.csv`-like tibble. When provided, suggestions are
  generated for missing `tables.csv$observation_unit_iri`.

- dataset_meta:

  Optional `dataset.csv`-like tibble. When provided, suggestions are
  generated for missing `dataset.csv$keywords` as candidate semantic
  keywords (IRIs intended for keyword curation).

- llm_assess:

  Logical; if `TRUE`, assess the top semantic candidates per target with
  an LLM after deterministic retrieval. When the first shortlist looks
  weak, the LLM may request at most one bounded alternate-query pass
  (1–2 plain-text search phrases) before a single reassessment. Default
  is `FALSE`.

- llm_provider:

  LLM provider preset. One of `"openai"`, `"openrouter"`, or
  `"openai_compatible"`.

- llm_model:

  Character model identifier. Required when `llm_assess = TRUE` unless
  supplied via `METASALMON_LLM_MODEL`. When
  `llm_provider = "openrouter"` and no model is supplied, the package
  defaults to `"openrouter/free"`.

- llm_api_key:

  Optional API key override. If omitted, provider-specific environment
  variables are used (`OPENAI_API_KEY`, `OPENROUTER_API_KEY`, or
  `METASALMON_LLM_API_KEY`).

- llm_base_url:

  Optional base URL override for the OpenAI-compatible chat endpoint.
  Required for `llm_provider = "openai_compatible"` when not set via
  `METASALMON_LLM_BASE_URL`.

- llm_top_n:

  Maximum number of retrieved candidates to send to the LLM per target
  for each assessment round. Default is `5`.

- llm_context_files:

  Optional character vector of local context files (for example README
  files, markdown notes, or PDF reports) used to provide extra domain
  context to the LLM.

- llm_context_text:

  Optional character vector of extra inline context snippets passed
  alongside `llm_context_files`.

- llm_timeout_seconds:

  Timeout for each LLM request in seconds.

- llm_request_fn:

  Advanced/test hook overriding the low-level OpenAI-compatible request
  function.

## Value

The dictionary tibble (unchanged) with a `semantic_suggestions`
attribute containing a tibble of suggested IRIs. The suggestions tibble
starts with `column_name`, `dictionary_role`, `table_id`, and
`dataset_id` so the original dictionary term is visible before the
candidate match. It also includes `target_scope`, `target_sdp_file`, and
`target_sdp_field` so users can see exactly where each accepted
suggestion would land in the Salmon Data Package. Additional columns
include `search_query`, `target_query_basis`, `target_query_context`,
`column_label`, `column_description`, `label`, `iri`, `source`,
`ontology`, `definition`, `retrieval_query`, and `retrieval_pass`. If
the underlying search results include a `score` column, it is preserved
for downstream filtering. For non-column targets, the tibble also
includes explicit destination context (`target_row_key`, `target_label`,
`target_description`, `code_value`, `code_label`, `code_description`) so
table-, dataset-, and code-level rows are inspectable without extra
joins. When `llm_assess = TRUE`, the suggestions also include `llm_*`
review columns such as `llm_decision`, `llm_confidence`, `llm_selected`,
`llm_candidate_rank`, and bounded exploration metadata, and the
dictionary gains a parallel `semantic_llm_assessments` attribute with
one row per assessed target.

## Details

The function uses the column's label or description as the search query
and returns suggestions as an attribute on the dictionary tibble. This
allows you to review candidates before accepting them into your
dictionary.

Column targets keep full I-ADOPT behavior for
`column_role == "measurement"` rows. Non-measurement coverage is
lighter: only missing `term_iri` values are considered, focused on
categorical rows and controlled low-cardinality attribute rows inferred
through `codes.csv`. Identifier and temporal columns are skipped by
default. When `codes`, `table_meta`, or `dataset_meta` are supplied,
additional target rows are generated for `codes.csv`, `tables.csv`, and
`dataset.csv` respectively. Table-level observation-unit queries ignore
review placeholders such as `MISSING METADATA:` and fall back to real
table metadata context instead.

When `llm_assess = TRUE`, the LLM only judges deterministically
retrieved candidates; it does not mint new IRIs. If the first shortlist
looks weak, the model may suggest at most one bounded alternate-query
round (1–2 plain-text queries), the package reruns deterministic
retrieval, de-dupes the merged shortlist, and reassesses once. Local
context files are read on disk, chunked, and lexically trimmed down
before prompt assembly so large README/report files do not get dumped
wholesale into the model call.

A term can legitimately appear more than once with different
`dictionary_role` values (for example as both a variable and a
property). In that case, `match_type` still describes lexical match
quality, while `target_sdp_field` tells you where that suggestion would
be written in the package. The output adds `role_collision` and
`role_collision_note` so variable-vs-property collisions stay explicit
and destination-aware.

After calling this function, access suggestions with:

    suggestions <- attr(result, "semantic_suggestions")

Suggestions stay separate by default. Review them first, then use
[`apply_semantic_suggestions()`](https://dfo-pacific-science.github.io/metasalmon/reference/apply_semantic_suggestions.md)
for an explicit opt-in merge, or copy values manually when you need
finer control.

## See also

[`find_terms()`](https://dfo-pacific-science.github.io/metasalmon/reference/find_terms.md)
for direct vocabulary searches,
[`infer_dictionary()`](https://dfo-pacific-science.github.io/metasalmon/reference/infer_dictionary.md)
for creating starter dictionaries,
[`apply_semantic_suggestions()`](https://dfo-pacific-science.github.io/metasalmon/reference/apply_semantic_suggestions.md)
for explicitly filling selected IRI fields,
[`validate_dictionary()`](https://dfo-pacific-science.github.io/metasalmon/reference/validate_dictionary.md)
for checking dictionary completeness.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a starter dictionary
dict <- infer_dictionary(my_data, dataset_id = "example", table_id = "main")

# Get semantic suggestions for measurement columns
dict_with_suggestions <- suggest_semantics(my_data, dict)

# View the suggestions
suggestions <- attr(dict_with_suggestions, "semantic_suggestions")
print(suggestions)

# Filter suggestions for a specific column
spawner_suggestions <- suggestions[suggestions$column_name == "SPAWNER_COUNT", ]

# Explicitly apply the top suggestion for one column without overwriting
# any existing IRIs in the dictionary
dict <- apply_semantic_suggestions(dict_with_suggestions, columns = "SPAWNER_COUNT")
} # }
```
