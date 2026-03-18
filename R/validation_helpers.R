#' Validate semantics with graceful gap reporting
#'
#' Ensures structural requirements, adds a `required` column if missing,
#' runs `validate_dictionary()`, and reports measurement rows missing
#' `term_iri`. Also flags non-canonical Salmon ontology IRIs so source
#' boundaries stay explicit (`smn` under `https://w3id.org/smn/`, `gcdfo`
#' under `https://w3id.org/gcdfo/salmon#`). In non-strict mode
#' (`require_iris = FALSE`), semantic gaps emit warnings but do not fail
#' the overall call.
#'
#' @param dict Dictionary tibble/data frame.
#' @param require_iris Logical; if TRUE, require non-empty semantic fields
#'   (`term_iri`, `property_iri`, `entity_iri`, `unit_iri`) for measurement
#'   rows.
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
    dict$required <- rep(NA, nrow(dict))
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

  semantic_iri_cols <- intersect(
    c("term_iri", "property_iri", "entity_iri", "unit_iri", "constraint_iri", "method_iri"),
    names(dict)
  )
  if (length(semantic_iri_cols) > 0) {
    iri_rows <- purrr::map_dfr(semantic_iri_cols, function(col) {
      vals <- dict[[col]]
      keep <- which(!is.na(vals) & nzchar(trimws(vals)))
      if (length(keep) == 0) {
        return(tibble::tibble())
      }
      tibble::tibble(row = keep, field = col, iri = vals[keep])
    })

    if (nrow(iri_rows) > 0) {
      iri_rows$issue <- dplyr::case_when(
        grepl("^salmon:[^\\s]+$", iri_rows$iri) ~ "legacy_smn_curie",
        grepl("^smn:[^\\s]+$", iri_rows$iri) ~ "noncanonical_smn_curie",
        grepl("^gcdfo:[^\\s]+$", iri_rows$iri) ~ "noncanonical_gcdfo_curie",
        grepl("^https?://w3id\\.org/salmon/", iri_rows$iri) ~ "legacy_smn_namespace",
        grepl("^http://w3id\\.org/smn/", iri_rows$iri) ~ "noncanonical_smn_http",
        grepl("^https?://w3id\\.org/smn#", iri_rows$iri) ~ "noncanonical_smn_form",
        grepl("^http://w3id\\.org/gcdfo/salmon#", iri_rows$iri) ~ "noncanonical_gcdfo_http",
        grepl("^https?://w3id\\.org/gcdfo/salmon/", iri_rows$iri) ~ "noncanonical_gcdfo_form",
        TRUE ~ ""
      )
      iri_rows <- dplyr::filter(iri_rows, .data$issue != "")

      if (nrow(iri_rows) > 0) {
        iri_issues <- dplyr::mutate(
          iri_rows,
          message = dplyr::case_when(
            .data$issue == "legacy_smn_curie" ~
              sprintf("Row %s field %s uses legacy SMN CURIE form (%s); use https://w3id.org/smn/<Term>.", .data$row, .data$field, .data$iri),
            .data$issue == "noncanonical_smn_curie" ~
              sprintf("Row %s field %s uses non-canonical SMN CURIE form (%s); use https://w3id.org/smn/<Term>.", .data$row, .data$field, .data$iri),
            .data$issue == "noncanonical_gcdfo_curie" ~
              sprintf("Row %s field %s uses non-canonical GCDFO CURIE form (%s); use https://w3id.org/gcdfo/salmon#<Term>.", .data$row, .data$field, .data$iri),
            .data$issue == "legacy_smn_namespace" ~
              sprintf("Row %s field %s uses legacy SMN namespace (%s); use https://w3id.org/smn/<Term>.", .data$row, .data$field, .data$iri),
            .data$issue == "noncanonical_smn_http" ~
              sprintf("Row %s field %s uses non-canonical SMN HTTP IRI (%s); use https://w3id.org/smn/<Term>.", .data$row, .data$field, .data$iri),
            .data$issue == "noncanonical_smn_form" ~
              sprintf("Row %s field %s uses non-canonical SMN IRI form (%s); use https://w3id.org/smn/<Term>.", .data$row, .data$field, .data$iri),
            .data$issue == "noncanonical_gcdfo_http" ~
              sprintf("Row %s field %s uses non-canonical GCDFO HTTP IRI (%s); use https://w3id.org/gcdfo/salmon#<Term>.", .data$row, .data$field, .data$iri),
            .data$issue == "noncanonical_gcdfo_form" ~
              sprintf("Row %s field %s uses non-canonical GCDFO IRI form (%s); use https://w3id.org/gcdfo/salmon#<Term>.", .data$row, .data$field, .data$iri),
            TRUE ~ .data$issue
          )
        ) %>%
          dplyr::select("message")
        issues <- dplyr::bind_rows(issues, iri_issues)
      }
    }
  }

  missing_terms <- dict %>%
    dplyr::filter(.data$column_role == "measurement",
                  is.na(.data$term_iri) | .data$term_iri == "") %>%
    dplyr::mutate(term_label = tools::toTitleCase(gsub("_", " ", .data$column_name)),
                  term_definition = dplyr::coalesce(.data$column_description, ""),
                  term_type = "skos_concept",
                  suggested_parent_iri = "https://w3id.org/smn/TargetOrLimitRateOrAbundance",
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
