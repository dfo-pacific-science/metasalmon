# Detect missing semantic terms that are not covered by SMN

Given semantic suggestions (typically attached to a dictionary as
`semantic_suggestions`), this function summarizes candidate fields that
appear to need ontology support but do not have a direct `smn` match.

## Usage

``` r
detect_semantic_term_gaps(
  dict = NULL,
  suggestions = NULL,
  include_target_scopes = c("column", "code", "table", "dataset"),
  include_dictionary_roles = NULL,
  min_score = NA_real_
)
```

## Arguments

- dict:

  A dictionary tibble. Used only when `suggestions` is `NULL`.

- suggestions:

  Optional semantic suggestion table. If omitted, this function uses
  `attr(dict, "semantic_suggestions")`.

- include_target_scopes:

  Target scopes to inspect. Defaults to all supported scopes.

- include_dictionary_roles:

  Optional vector of dictionary roles to restrict the gap scan (for
  example `c("variable", "property", "entity")`).

- min_score:

  Optional minimum score filter. Rows with score below this value are
  ignored when score is available.

## Value

A tibble with one row per target that has no SMN match. Key columns:

- `dataset_id`, `table_id`, `column_name`, `target_scope`,
  `target_sdp_file`, `target_sdp_field`, `target_row_key`,
  `dictionary_role`;

- `search_query` text used for lookup;

- `top_non_smn_source`, `top_non_smn_label`, `top_non_smn_iri`,
  `top_non_smn_score`;

- `non_smn_sources`, `candidate_count`, `placement_recommendation`,
  `placement_confidence`, `placement_rationale`.

## Details

It is designed to support a practical workflow:

1.  generate semantic suggestions with
    [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md);

2.  detect unresolved gaps with `detect_semantic_term_gaps()`;

3.  render request payloads with
    [`render_ontology_term_request()`](https://dfo-pacific-science.github.io/metasalmon/reference/render_ontology_term_request.md);

4.  optionally submit issues with
    [`submit_term_request_issues()`](https://dfo-pacific-science.github.io/metasalmon/reference/submit_term_request_issues.md).

## See also

[`render_ontology_term_request()`](https://dfo-pacific-science.github.io/metasalmon/reference/render_ontology_term_request.md),
[`submit_term_request_issues()`](https://dfo-pacific-science.github.io/metasalmon/reference/submit_term_request_issues.md),
[`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md)

## Examples

``` r
suggestions <- tibble::tibble(
  dataset_id = c("d1", "d1"),
  table_id = c("t1", "t1"),
  column_name = c("run_id", "run_id"),
  code_value = NA_character_,
  column_label = c("Run ID", "Run ID"),
  column_description = "Run identifier from local monitoring pipeline",
  dictionary_role = c("variable", "variable"),
  target_scope = c("column", "column"),
  target_sdp_file = c("column_dictionary.csv", "column_dictionary.csv"),
  target_sdp_field = c("term_iri", "term_iri"),
  target_row_key = c("run_id", "run_id"),
  search_query = c("run_id", "run_id"),
  label = c("Run ID", "Run ID"),
  iri = c(NA_character_, NA_character_),
  source = c("gbif", "worms"),
  ontology = c("gbif", "worms"),
  match_type = c("label", "label"),
  definition = NA_character_,
  score = c(0.9, 0.85)
)
gaps <- detect_semantic_term_gaps(
  suggestions = suggestions,
  include_dictionary_roles = "variable"
)
#> Error in detect_semantic_term_gaps(suggestions = suggestions, include_dictionary_roles = "variable"): could not find function "detect_semantic_term_gaps"
gaps
#> Error: object 'gaps' not found

```
