# Render GitHub-ready ontology term request payloads

Convert gap candidates into request payload rows (title/body) suitable
for creating GitHub issues against the DFO Salmon Ontology repository.

## Usage

``` r
render_ontology_term_request(
  gaps,
  scope = c("auto", "smn", "profile"),
  ask = interactive(),
  profile_name = NULL,
  scope_overrides = NULL,
  issue_labels = NULL,
  term_request_template =
    "https://github.com/dfo-pacific-science/dfo-salmon-ontology/blob/main/.github/ISSUE_TEMPLATE/new-term-request.md",
  ontology_repo = "dfo-pacific-science/dfo-salmon-ontology"
)
```

## Arguments

- gaps:

  Output from
  [`detect_semantic_term_gaps()`](https://dfo-pacific-science.github.io/metasalmon/reference/detect_semantic_term_gaps.md).

- scope:

  One of `"auto"`, `"smn"`, or `"profile"`.

  - `"auto"`: honor `placement_recommendation` and ask for uncertainty

  - `"smn"`: route all requests to shared SMN

  - `"profile"`: route all requests to a profile

- ask:

  If `TRUE`, unresolved rows are asked interactively.

- profile_name:

  If routing to profiles, provide a default profile name.

- scope_overrides:

  Optional per-row scope overrides (`"smn"`, `"profile"`, `"skip"`).
  Useful in non-interactive pipelines.

- issue_labels:

  Optional labels to include on created GitHub issues.

- term_request_template:

  URL for the target issue template.

- ontology_repo:

  Repository slug to target when submitting issues.

## Value

A tibble with one row per rendered request payload. Rows with
`request_scope == "skip"` are retained and can be filtered before
submission.

## Details

For interactive workflows this function can prompt users row-by-row for
whether a gap should be requested as a shared SMN term, a
profile-specific term, or skipped.

## See also

[`detect_semantic_term_gaps()`](https://dfo-pacific-science.github.io/metasalmon/reference/detect_semantic_term_gaps.md),
[`submit_term_request_issues()`](https://dfo-pacific-science.github.io/metasalmon/reference/submit_term_request_issues.md),
[`validate_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/validate_semantics.md)

## Examples

``` r
gap <- dplyr::tibble(
  dataset_id = "d1",
  table_id = "t1",
  column_name = "run_id",
  code_value = NA_character_,
  target_scope = "column",
  target_sdp_file = "column_dictionary.csv",
  target_sdp_field = "term_iri",
  target_row_key = "run_id",
  dictionary_role = "variable",
  search_query = "run id",
  column_label = "Run ID",
  column_description = "Dataset-specific run identifier",
  top_non_smn_source = "gbif",
  top_non_smn_label = "Run event id",
  top_non_smn_iri = NA_character_,
  top_non_smn_ontology = NA_character_,
  top_non_smn_match_type = "label",
  top_non_smn_score = 0.9,
  candidate_count = 2,
  non_smn_sources = "gbif, worms",
  placement_recommendation = "profile",
  placement_confidence = 0.82,
  placement_rationale = "Contains internal identifier patterns."
)

render_ontology_term_request(gap, scope = "auto", ask = FALSE, profile_name = "pacific-monitoring")
#> # A tibble: 1 × 29
#>   dataset_id table_id column_name code_value target_scope target_sdp_file      
#>   <chr>      <chr>    <chr>       <chr>      <chr>        <chr>                
#> 1 d1         t1       run_id      NA         column       column_dictionary.csv
#> # ℹ 23 more variables: target_sdp_field <chr>, target_row_key <chr>,
#> #   dictionary_role <chr>, search_query <chr>, column_label <chr>,
#> #   column_description <chr>, top_non_smn_source <chr>,
#> #   top_non_smn_label <chr>, top_non_smn_iri <chr>, top_non_smn_ontology <chr>,
#> #   top_non_smn_match_type <chr>, top_non_smn_score <dbl>,
#> #   candidate_count <dbl>, non_smn_sources <chr>,
#> #   placement_recommendation <chr>, placement_confidence <dbl>, …
```
