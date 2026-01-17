#' Suggest semantic annotations for a dictionary
#'
#' Placeholder function for future GPT/LLM or heuristic-based suggestions
#' of IRIs, concept schemes, and semantic annotations for dictionary fields.
#'
#' @param df A data frame or tibble
#' @param dict A dictionary tibble (may be incomplete)
#' @param sources Search sources to use for `find_terms()`; default is OLS + NVS.
#' @param max_per_role Maximum suggestions to keep per role/column.
#' @param search_fn Function used to search terms (defaults to `find_terms`);
#'   useful for testing or custom search strategies.
#'
#' @return The dictionary tibble (unchanged) with a `semantic_suggestions`
#'   attribute containing suggested IRIs for missing fields.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dict <- infer_dictionary(mtcars)
#' suggested <- suggest_semantics(mtcars, dict, sources = "ols")
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
  first_non_empty <- function(values) {
    values <- values[!vapply(values, is_missing, logical(1))]
    if (length(values) == 0) "" else values[[1]]
  }
  clean_query <- function(x) {
    x <- gsub("[._]+", " ", x)
    x <- gsub("\\s+", " ", x)
    trimws(x)
  }

  suggestions <- purrr::map_dfr(seq_len(nrow(dict)), function(i) {
    row <- dict[i, , drop = TRUE]
    if (!identical(row$column_role, "measurement")) return(tibble::tibble())

    query <- first_non_empty(list(row$column_description, row$column_label, row$column_name))
    query <- clean_query(query)
    purrr::imap_dfr(roles, function(role_name, col_name) {
      if (!col_name %in% names(row)) return(tibble::tibble())
      if (!is_missing(row[[col_name]])) return(tibble::tibble())
      role_query <- query
      if (role_name == "unit") {
        role_query <- first_non_empty(list(row$unit_label, query))
      }
      if (!nzchar(role_query)) return(tibble::tibble())
      res <- search_fn(role_query, role = role_name, sources = sources)
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
