#' Validate semantics with graceful gap reporting
#'
#' Ensures structural requirements, adds a `required` column if missing,
#' runs `validate_dictionary()`, and reports missing `term_iri` for
#' measurement columns without aborting the entire run.
#'
#' @param dict Dictionary tibble/data frame.
#' @param require_iris Logical; if TRUE, require IRIs in all semantic fields.
#' @param entity_defaults Optional data frame with `table_prefix` and `entity_iri`
#'   (not applied automatically here but reserved for future use).
#' @param vocab_priority Optional character vector of vocab sources (reserved).
#'
#' @return A list with elements:
#'   - `dict`: normalized dictionary with `required` column.
#'   - `issues`: tibble of structural issues (empty if none).
#'   - `missing_terms`: tibble of measurement rows missing `term_iri`.
#' @importFrom dplyr filter mutate select coalesce
#' @importFrom tools toTitleCase
#' @export
validate_semantics <- function(dict,
                               require_iris = FALSE,
                               entity_defaults = NULL,
                               vocab_priority = NULL) {
  if (!"required" %in% names(dict)) {
    dict$required <- FALSE
  }

  issues <- tibble::tibble()
  missing_terms <- tibble::tibble()

  val_result <- tryCatch({
    validate_dictionary(dict, require_iris = require_iris)
    NULL
  }, error = function(e) e)

  if (inherits(val_result, "error")) {
    issues <- tibble::tibble(message = val_result$message)
  }

  missing_terms <- dict %>%
    dplyr::filter(.data$column_role == "measurement",
                  is.na(.data$term_iri) | .data$term_iri == "") %>%
    dplyr::mutate(term_label = tools::toTitleCase(gsub("_", " ", .data$column_name)),
                  term_definition = dplyr::coalesce(.data$column_description, ""),
                  term_type = "skos_concept",
                  suggested_parent_iri = "https://w3id.org/gcdfo/salmon#TargetOrLimitRateOrAbundance",
                  notes = paste0("Derived from ", .data$column_name,
                                 " in ", .data$table_id,
                                 " (constraints: ", dplyr::coalesce(.data$constraint_iri, ""), ")")) %>%
    dplyr::select(term_label, term_definition, term_type, suggested_parent_iri, notes)

  list(
    dict = dict,
    issues = issues,
    missing_terms = missing_terms
  )
}
