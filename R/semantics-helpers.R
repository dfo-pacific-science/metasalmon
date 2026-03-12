#' Suggest semantic annotations for a dictionary
#'
#' Searches external vocabularies to suggest IRIs for measurement columns that
#' are missing semantic annotations. For each measurement column with missing
#' I-ADOPT component fields (`term_iri`, `property_iri`, `entity_iri`, `unit_iri`,
#' `constraint_iri`), this function queries vocabulary services and ranks
#' results by relevance, with GCDFO queried first for salmon-domain roles.
#'
#' The function uses the column's label or description as the search query and
#' returns suggestions as an attribute on the dictionary tibble. This allows
#' you to review candidates before accepting them into your dictionary.
#'
#' @param df A data frame or tibble containing the data being documented.
#' @param dict A dictionary tibble created by `infer_dictionary()` (may have
#'   incomplete semantic fields).
#' @param sources Character vector of vocabulary sources to search. Options are
#'   `"gcdfo"` (DFO Salmon Ontology via content negotiation), `"ols"` (Ontology Lookup Service), `"nvs"` (NERC Vocabulary Server), and
#'   `"bioportal"` (requires `BIOPORTAL_APIKEY` environment variable).
#'   Default is `c("gcdfo", "ols", "nvs")`.
#' @param include_dwc Logical; if `TRUE`, also attach DwC-DP export mappings
#'   (via `suggest_dwc_mappings()`) as a parallel attribute `dwc_mappings`.
#'   Default is `FALSE` to keep the UI simple for non-DwC users.
#' @param max_per_role Maximum number of suggestions to keep per I-ADOPT role
#'   (variable, property, entity, unit, constraint) per column. Default is 3.
#' @param search_fn Function used to search terms. Defaults to `find_terms()`.
#'   Can be replaced for testing or custom search strategies.
#'
#' @return The dictionary tibble (unchanged) with a `semantic_suggestions`
#'   attribute containing a tibble of suggested IRIs. The suggestions tibble
#'   includes `dataset_id`, `table_id`, `column_name`, `dictionary_role`
#'   (which dictionary field the suggestion targets), `label`, `iri`, `source`,
#'   `ontology`, and `definition`. If the underlying search results include a
#'   `score` column, it is preserved for downstream filtering.
#'
#' @details
#' Only columns with `column_role == "measurement"` are processed, since
#' I-ADOPT components are primarily relevant for measurement metadata. Columns
#' with existing IRIs in a field are skipped for that field.
#'
#' After calling this function, access suggestions with:
#' ```
#' suggestions <- attr(result, "semantic_suggestions")
#' ```
#'
#' Suggestions stay separate by default. Review them first, then use
#' [apply_semantic_suggestions()] for an explicit opt-in merge, or copy values
#' manually when you need finer control.
#'
#' @seealso [find_terms()] for direct vocabulary searches, [infer_dictionary()]
#'   for creating starter dictionaries, [apply_semantic_suggestions()] for
#'   explicitly filling selected IRI fields, [validate_dictionary()] for
#'   checking dictionary completeness.
#' @importFrom metasalmon suggest_dwc_mappings
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
#' # Explicitly apply the top suggestion for one column without overwriting
#' # any existing IRIs in the dictionary
#' dict <- apply_semantic_suggestions(dict_with_suggestions, columns = "SPAWNER_COUNT")
#' }
suggest_semantics <- function(df,
                              dict,
                              sources = c("gcdfo", "ols", "nvs"),
                              include_dwc = FALSE,
                              max_per_role = 3,
                              search_fn = find_terms) {
  if (nrow(dict) == 0) {
    attr(dict, "semantic_suggestions") <- tibble::tibble()
    if (isTRUE(include_dwc)) {
      attr(dict, "dwc_mappings") <- tibble::tibble()
    }
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
      res$dataset_id <- row$dataset_id
      res$table_id <- row$table_id
      res$column_name <- row$column_name
      res$dictionary_role <- role_name
      res
    })
  })

  attr(dict, "semantic_suggestions") <- suggestions

  if (isTRUE(include_dwc)) {
    attr(dict, "dwc_mappings") <- metasalmon::suggest_dwc_mappings(dict) |> attr("dwc_mappings")
  }

  if (nrow(suggestions) > 0) {
    cli::cli_inform("Semantic suggestions added (attr 'semantic_suggestions'); no fields were auto-filled.")
  } else {
    cli::cli_inform("No semantic suggestions found for missing measurement metadata.")
  }

  dict
}

#' Apply semantic suggestions into a dictionary
#'
#' Copies selected IRIs from a `semantic_suggestions` tibble into the matching
#' dictionary fields. Suggestions remain separate by default; this helper gives
#' you an explicit merge step when you decide the top candidates are good enough.
#'
#' Matching is done by both `column_name` and `dictionary_role`. When the
#' suggestions tibble also includes `dataset_id` and `table_id`, those keys are
#' honored too.
#'
#' @param dict A dictionary tibble, typically returned by [infer_dictionary()] or
#'   [suggest_semantics()].
#' @param suggestions A suggestions tibble, usually
#'   `attr(dict, "semantic_suggestions")`. If omitted, the function reads that
#'   attribute from `dict`.
#' @param strategy Selection strategy per column-role pair. Currently only
#'   `"top"` is supported, which uses the first suggestion in each matched group.
#' @param columns Optional character vector limiting application to specific
#'   `column_name` values.
#' @param roles Optional character vector limiting application to specific
#'   suggestion roles: `"variable"`, `"property"`, `"entity"`, `"unit"`,
#'   `"constraint"`, `"method"`.
#' @param min_score Optional numeric threshold. Only available when
#'   `suggestions` includes a `score` column; otherwise the function errors.
#' @param overwrite Logical; if `FALSE` (default), only missing fields are
#'   filled. Set `TRUE` to intentionally replace existing IRIs.
#' @param verbose Logical; if `TRUE` (default), print a short summary.
#'
#' @return The dictionary tibble with selected semantic IRI fields filled in.
#' @export
#'
#' @examples
#' \dontrun{
#' dict <- infer_dictionary(my_data, dataset_id = "example", table_id = "main")
#' dict <- suggest_semantics(my_data, dict)
#'
#' # Fill only the missing semantic fields for one measurement column
#' dict <- apply_semantic_suggestions(dict, columns = "SPAWNER_COUNT")
#'
#' # Require stronger lexical matches when score is available
#' dict <- apply_semantic_suggestions(dict, min_score = 2)
#' }
apply_semantic_suggestions <- function(dict,
                                       suggestions = attr(dict, "semantic_suggestions"),
                                       strategy = "top",
                                       columns = NULL,
                                       roles = NULL,
                                       min_score = NULL,
                                       overwrite = FALSE,
                                       verbose = TRUE) {
  if (!inherits(dict, "data.frame")) {
    cli::cli_abort("{.arg dict} must be a data frame or tibble")
  }

  if (is.null(suggestions)) {
    cli::cli_abort(
      c(
        "No semantic suggestions supplied.",
        "i" = "Pass {.arg suggestions} explicitly or run {.fn suggest_semantics} first."
      )
    )
  }

  suggestions <- tibble::as_tibble(suggestions)
  if (nrow(suggestions) == 0) {
    if (isTRUE(verbose)) {
      cli::cli_inform("No semantic suggestions to apply.")
    }
    return(dict)
  }

  required_cols <- c("column_name", "dictionary_role", "iri")
  missing_cols <- setdiff(required_cols, names(suggestions))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Suggestions are missing required columns: {.field {missing_cols}}"
    )
  }

  role_to_field <- c(
    variable = "term_iri",
    property = "property_iri",
    entity = "entity_iri",
    unit = "unit_iri",
    constraint = "constraint_iri",
    method = "method_iri"
  )

  if (!is.null(roles)) {
    invalid_roles <- setdiff(roles, names(role_to_field))
    if (length(invalid_roles) > 0) {
      cli::cli_abort(
        "Unsupported {.arg roles}: {.val {invalid_roles}}. Valid roles: {.val {names(role_to_field)}}"
      )
    }
  }

  if (!is.null(min_score) && !"score" %in% names(suggestions)) {
    cli::cli_abort(
      c(
        "{.arg min_score} requires scored suggestions.",
        "i" = "Run {.fn suggest_semantics} with the default search results or pass a suggestions tibble that includes a {.field score} column."
      )
    )
  }

  if (!identical(strategy, "top")) {
    cli::cli_abort("Unsupported {.arg strategy}: {.val {strategy}}. Use {.val top}.")
  }

  out <- dict
  for (field in unique(unname(role_to_field))) {
    if (!field %in% names(out)) {
      out[[field]] <- NA_character_
    }
  }

  suggestions$.row_id <- seq_len(nrow(suggestions))
  suggestions <- suggestions[!is.na(suggestions$iri) & suggestions$iri != "", , drop = FALSE]

  if (!is.null(columns)) {
    suggestions <- suggestions[suggestions$column_name %in% columns, , drop = FALSE]
  }
  if (!is.null(roles)) {
    suggestions <- suggestions[suggestions$dictionary_role %in% roles, , drop = FALSE]
  }
  if (!is.null(min_score)) {
    suggestions <- suggestions[!is.na(suggestions$score) & suggestions$score >= min_score, , drop = FALSE]
  }

  unknown_roles <- unique(suggestions$dictionary_role[!is.na(suggestions$dictionary_role) &
    !suggestions$dictionary_role %in% names(role_to_field)])
  if (length(unknown_roles) > 0) {
    cli::cli_warn("Ignoring unsupported suggestion roles: {.val {unknown_roles}}")
  }
  suggestions <- suggestions[!is.na(suggestions$dictionary_role) & suggestions$dictionary_role %in% names(role_to_field), , drop = FALSE]

  if (nrow(suggestions) == 0) {
    if (isTRUE(verbose)) {
      cli::cli_inform("No semantic suggestions met the requested filters.")
    }
    return(out)
  }

  match_keys <- c(intersect(c("dataset_id", "table_id"), names(suggestions)), "column_name", "dictionary_role")
  selected <- suggestions[order(suggestions$.row_id), , drop = FALSE]
  group_id <- do.call(
    paste,
    c(
      lapply(selected[match_keys], function(x) ifelse(is.na(x), "<NA>", as.character(x))),
      sep = "\r"
    )
  )
  selected <- selected[!duplicated(group_id), , drop = FALSE]

  dict_match_keys <- intersect(c("dataset_id", "table_id"), names(out))
  applied <- 0L
  skipped_existing <- 0L
  unmatched <- 0L

  for (i in seq_len(nrow(selected))) {
    suggestion <- selected[i, , drop = FALSE]
    role <- suggestion$dictionary_role[[1]]
    field <- role_to_field[[role]]

    matches <- out$column_name == suggestion$column_name[[1]]
    for (key in dict_match_keys) {
      if (key %in% names(suggestion)) {
        key_value <- suggestion[[key]][[1]]
        if (!is.na(key_value) && key_value != "") {
          matches <- matches & out[[key]] == key_value
        }
      }
    }

    row_ids <- which(matches)
    if (length(row_ids) == 0) {
      unmatched <- unmatched + 1L
      next
    }

    if (isTRUE(overwrite)) {
      out[[field]][row_ids] <- suggestion$iri[[1]]
      applied <- applied + length(row_ids)
      next
    }

    missing_now <- is.na(out[[field]][row_ids]) | out[[field]][row_ids] == ""
    fill_rows <- row_ids[missing_now]
    if (length(fill_rows) > 0) {
      out[[field]][fill_rows] <- suggestion$iri[[1]]
      applied <- applied + length(fill_rows)
    }
    skipped_existing <- skipped_existing + sum(!missing_now)
  }

  if (isTRUE(verbose)) {
    msg <- c(
      "Applied {.val {applied}} semantic suggestion field{?s} using the {.val {strategy}} strategy."
    )
    if (!overwrite && skipped_existing > 0) {
      msg <- c(
        msg,
        "i" = paste0(
          skipped_existing,
          " field",
          if (skipped_existing == 1) " was" else "s were",
          " left alone because the dictionary already had an IRI. Use overwrite = TRUE to replace them."
        )
      )
    }
    if (unmatched > 0) {
      msg <- c(
        msg,
        "i" = paste0(
          unmatched,
          " suggestion",
          if (unmatched == 1) " did" else "s did",
          " not match any dictionary row."
        )
      )
    }
    cli::cli_inform(msg)
  }

  out
}
