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
  dataset_meta = NULL
)
```

## Arguments

- df:

  A data frame or tibble containing the data being documented.

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
`ontology`, and `definition`. If the underlying search results include a
`score` column, it is preserved for downstream filtering. For non-column
targets, the tibble also includes explicit destination context
(`target_row_key`, `target_label`, `target_description`, `code_value`,
`code_label`, `code_description`) so table-, dataset-, and code-level
rows are inspectable without extra joins.

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
