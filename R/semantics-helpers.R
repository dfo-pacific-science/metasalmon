#' Suggest semantic annotations for a dictionary
#'
#' Searches external vocabularies to suggest IRIs for measurement columns that
#' are missing semantic annotations. For each measurement column with missing
#' I-ADOPT component fields (`term_iri`, `property_iri`, `entity_iri`, `unit_iri`,
#' `constraint_iri`), this function queries vocabulary services and ranks
#' results by relevance.
#'
#' The function uses the column's label or description as the search query and
#' returns suggestions as an attribute on the dictionary tibble. This allows
#' you to review candidates before accepting them into your dictionary.
#'
#' @param df A data frame or tibble containing the data being documented.
#' @param dict A dictionary tibble created by `infer_dictionary()` (may have
#'   incomplete semantic fields).
#' @param sources Character vector of vocabulary sources to search. Options are
#'   `"ols"` (Ontology Lookup Service), `"nvs"` (NERC Vocabulary Server), and
#'   `"bioportal"` (requires `BIOPORTAL_APIKEY` environment variable).
#'   Default is `c("ols", "nvs")`.
#' @param max_per_role Maximum number of suggestions to keep per I-ADOPT role
#'   (variable, property, entity, unit, constraint) per column. Default is 3.
#' @param search_fn Function used to search terms. Defaults to `find_terms()`.
#'   Can be replaced for testing or custom search strategies.
#'
#' @return The dictionary tibble (unchanged) with a `semantic_suggestions`
#'   attribute containing a tibble of suggested IRIs. The suggestions tibble

#'   includes columns: `column_name`, `dictionary_role` (which IRI field the
#'   suggestion is for), `label`, `iri`, `source`, `ontology`, and `definition`.
#'
#' @details
#' Only columns with `column_role == "measurement"
#' are processed, since I-ADOPT components are primarily relevant for
#' measurement metadata. Columns with existing IRIs in a field are skipped
#' for that field.
#'
#' After calling this function, access suggestions with:
#' ```
#' suggestions <- attr(result, "semantic_suggestions")
#' ```
#'
#' Then manually review and copy desired IRIs into your dictionary.
#'
#' @seealso [find_terms()] for direct vocabulary searches, [infer_dictionary()]
#'   for creating starter dictionaries, [validate_dictionary()] for checking
#'   dictionary completeness.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a starter dictionary
#' dict <- infer_dictionary(my_data, dataset_id = "example", table_id = "main")
#'
#' # Get semantic suggestions for measurement columns
#' dict_with_suggestions <- suggest_semantics(my_data, dict)
#'
#' # View the suggestions
#' suggestions <- attr(dict_with_suggestions, "semantic_suggestions")
#' print(suggestions)
#'
#' # Filter suggestions for a specific column
#' spawner_suggestions <- suggestions[suggestions$column_name == "SPAWNER_COUNT", ]
#'
#' # Accept a suggestion by copying the IRI into your dictionary
#' dict$term_iri[dict$column_name == "SPAWNER_COUNT"] <- spawner_suggestions$iri[1]
#' }
suggest_semantics <- function(df,
                              dict,
                              sources = c("ols", "nvs"),
                              max_per_role = 3,
                              search_fn = find_terms) {
  if (nrow(dict) == 0) {
    attr(dict, "semantic_suggestions") <- tibble::tibble()
    return(dict)
  }

  roles <- c(
    term_iri = "variable",
    property_iri = "property",
    entity_iri = "entity",
    unit_iri = "unit",
    constraint_iri = "constraint",
    method_iri = "method"
  )

  is_missing <- function(x) is.null(x) || is.na(x) || x == ""

  suggestions <- purrr::map_dfr(seq_len(nrow(dict)), function(i) {
    row <- dict[i, , drop = TRUE]
    if (!identical(row$column_role, "measurement")) return(tibble::tibble())

    query <- row$column_label %||% row$column_description %||% row$column_name
    purrr::imap_dfr(roles, function(role_name, col_name) {
      if (!col_name %in% names(row)) return(tibble::tibble())
      if (!is_missing(row[[col_name]])) return(tibble::tibble())
      res <- search_fn(query, role = role_name, sources = sources)
      if (nrow(res) == 0) return(tibble::tibble())
      res <- utils::head(res, max_per_role)
      res$column_name <- row$column_name
      res$dictionary_role <- role_name
      res
    })
  })

  attr(dict, "semantic_suggestions") <- suggestions

  if (nrow(suggestions) > 0) {
    cli::cli_inform("Semantic suggestions added (attr 'semantic_suggestions'); no fields were auto-filled.")
  } else {
    cli::cli_inform("No semantic suggestions found for missing measurement metadata.")
  }

  dict
}
