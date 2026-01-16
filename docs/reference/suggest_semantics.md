# Suggest semantic annotations for a dictionary

Searches external vocabularies to suggest IRIs for measurement columns
that are missing semantic annotations. For each measurement column with
missing I-ADOPT component fields (`term_iri`, `property_iri`,
`entity_iri`, `unit_iri`, `constraint_iri`), this function queries
vocabulary services and ranks results by relevance.

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

  A data frame or tibble containing the data being documented.

- dict:

  A dictionary tibble created by
  [`infer_dictionary()`](https://dfo-pacific-science.github.io/metasalmon/reference/infer_dictionary.md)
  (may have incomplete semantic fields).

- sources:

  Character vector of vocabulary sources to search. Options are `"ols"`
  (Ontology Lookup Service), `"nvs"` (NERC Vocabulary Server), and
  `"bioportal"` (requires `BIOPORTAL_APIKEY` environment variable).
  Default is `c("ols", "nvs")`.

- max_per_role:

  Maximum number of suggestions to keep per I-ADOPT role (variable,
  property, entity, unit, constraint) per column. Default is 3.

- search_fn:

  Function used to search terms. Defaults to
  [`find_terms()`](https://dfo-pacific-science.github.io/metasalmon/reference/find_terms.md).
  Can be replaced for testing or custom search strategies.

## Value

The dictionary tibble (unchanged) with a `semantic_suggestions`
attribute containing a tibble of suggested IRIs. The suggestions tibble
includes columns: `column_name`, `dictionary_role` (which IRI field the
suggestion is for), `label`, `iri`, `source`, `ontology`, and
`definition`.

## Details

The function uses the column's label or description as the search query
and returns suggestions as an attribute on the dictionary tibble. This
allows you to review candidates before accepting them into your
dictionary.

Only columns with \`column_role == "measurement" are processed, since
I-ADOPT components are primarily relevant for measurement metadata.
Columns with existing IRIs in a field are skipped for that field.

After calling this function, access suggestions with:

    suggestions <- attr(result, "semantic_suggestions")

Then manually review and copy desired IRIs into your dictionary.

## See also

[`find_terms()`](https://dfo-pacific-science.github.io/metasalmon/reference/find_terms.md)
for direct vocabulary searches,
[`infer_dictionary()`](https://dfo-pacific-science.github.io/metasalmon/reference/infer_dictionary.md)
for creating starter dictionaries,
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

# Accept a suggestion by copying the IRI into your dictionary
dict$term_iri[dict$column_name == "SPAWNER_COUNT"] <- spawner_suggestions$iri[1]
} # }
```
