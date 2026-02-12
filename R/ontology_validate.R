#' Validate dictionary IRIs against the DFO Salmon Ontology
#'
#' Checks that IRI fields in a data dictionary reference valid terms from the
#' DFO Salmon Ontology (or other namespaces). Produces a structured validation
#' report with pass/warn/fail status for each IRI.
#'
#' The validation logic:
#' \itemize{
#'   \item **pass** – IRI is found in the ontology term list.
#'   \item **warn** – IRI is non-empty but uses a namespace outside the
#'     ontology (e.g. `dwc:`, `dcterms:`, `qudt:`) and therefore cannot be
#'     verified. Also raised when the IRI is empty for a non-measurement column.
#'   \item **fail** – IRI uses the `gcdfo:` namespace but is not present in the
#'     ontology, indicating a typo or stale reference.
#' }
#'
#' @param dict A dictionary tibble (as produced by [infer_dictionary()]).
#' @param ontology_terms A tibble of ontology terms as returned by
#'   [parse_ontology_terms()]. If `NULL`, the ontology is fetched and parsed
#'   automatically via [fetch_salmon_ontology()] + [parse_ontology_terms()].
#' @param fields Character vector of IRI column names to validate. Defaults to
#'   the standard semantic fields: `term_iri`, `property_iri`, `entity_iri`,
#'   `unit_iri`, `constraint_iri`, `method_iri`.
#' @param gcdfo_ns Character; the gcdfo namespace IRI prefix used to identify
#'   ontology-scoped terms. Default: `"https://w3id.org/gcdfo/salmon#"`.
#'
#' @return A list with class `"ms_validation_report"` containing:
#'   \describe{
#'     \item{status}{Overall status: `"pass"`, `"warn"`, or `"fail"` (character).}
#'     \item{summary}{A one-line summary message (character).}
#'     \item{results}{A tibble with one row per validated IRI, columns:
#'       `column_name`, `field`, `iri`, `status` (pass/warn/fail), `message`.}
#'     \item{ontology_version}{Version string from ontology header, or `NA`.}
#'     \item{n_pass}{Count of pass results.}
#'     \item{n_warn}{Count of warn results.}
#'     \item{n_fail}{Count of fail results.}
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' ttl_path <- fetch_salmon_ontology()
#' terms <- parse_ontology_terms(ttl_path)
#' dict <- infer_dictionary(my_data)
#' report <- validate_ontology_terms(dict, terms)
#' print(report)
#' }
validate_ontology_terms <- function(dict,
                                    ontology_terms = NULL,
                                    fields = c("term_iri", "property_iri",
                                               "entity_iri", "unit_iri",
                                               "constraint_iri", "method_iri"),
                                    gcdfo_ns = "https://w3id.org/gcdfo/salmon#") {
  if (!inherits(dict, "data.frame")) {
    cli::cli_abort("{.arg dict} must be a data frame or tibble.")
  }

  # Auto-fetch ontology if not provided
  if (is.null(ontology_terms)) {
    cli::cli_inform("Fetching and parsing DFO Salmon Ontology...")
    ttl_path <- fetch_salmon_ontology()
    ontology_terms <- parse_ontology_terms(ttl_path)
  }

  # Build lookup set of valid IRIs
  valid_iris <- unique(ontology_terms$iri)

  # Extract ontology version from the terms table if possible
  ont_version <- NA_character_
  ont_header <- ontology_terms[ontology_terms$iri == sub("#$", "", gcdfo_ns), ]
  if (nrow(ont_header) == 0) {
    # Try the full ontology IRI without trailing #
    ont_header <- ontology_terms[grepl("gcdfo/salmon$", ontology_terms$iri), ]
  }

  # Only validate fields that exist in the dictionary
  fields <- intersect(fields, names(dict))
  if (length(fields) == 0) {
    report <- list(
      status = "pass",
      summary = "No IRI fields found in dictionary to validate.",
      results = tibble::tibble(
        column_name = character(), field = character(),
        iri = character(), status = character(), message = character()
      ),
      ontology_version = ont_version,
      n_pass = 0L, n_warn = 0L, n_fail = 0L
    )
    class(report) <- "ms_validation_report"
    return(report)
  }

  # ── Validate each IRI in each field ──
  result_rows <- vector("list", nrow(dict) * length(fields))
  k <- 0L
  for (i in seq_len(nrow(dict))) {
    col_name <- dict$column_name[i]
    col_role <- if ("column_role" %in% names(dict)) dict$column_role[i] else NA_character_
    for (fld in fields) {
      k <- k + 1L
      iri_val <- dict[[fld]][i]

      if (is.null(iri_val) || is.na(iri_val) || !nzchar(trimws(iri_val))) {
        # Empty IRI: warn for measurement columns, skip silently otherwise
        if (!is.na(col_role) && col_role == "measurement" &&
            fld %in% c("term_iri", "property_iri", "entity_iri", "unit_iri")) {
          result_rows[[k]] <- list(
            column_name = col_name, field = fld, iri = "",
            status = "warn",
            message = paste0("Empty ", fld, " for measurement column '", col_name, "'.")
          )
        }
        next
      }

      iri_val <- trimws(iri_val)

      # Check if it's a gcdfo: namespace IRI
      is_gcdfo <- startsWith(iri_val, gcdfo_ns)

      if (is_gcdfo) {
        if (iri_val %in% valid_iris) {
          result_rows[[k]] <- list(
            column_name = col_name, field = fld, iri = iri_val,
            status = "pass",
            message = "Valid gcdfo: term."
          )
        } else {
          local_name <- sub(gcdfo_ns, "", iri_val, fixed = TRUE)
          result_rows[[k]] <- list(
            column_name = col_name, field = fld, iri = iri_val,
            status = "fail",
            message = paste0(
              "gcdfo:", local_name, " not found in ontology. ",
              "Check spelling or verify the term exists in the current release."
            )
          )
        }
      } else {
        # External namespace – cannot verify, warn
        result_rows[[k]] <- list(
          column_name = col_name, field = fld, iri = iri_val,
          status = "pass",
          message = "External namespace IRI (not verified against gcdfo ontology)."
        )
      }
    }
  }

  # Remove NULLs and build tibble
  result_rows <- result_rows[!vapply(result_rows, is.null, logical(1))]
  if (length(result_rows) == 0) {
    results_df <- tibble::tibble(
      column_name = character(), field = character(),
      iri = character(), status = character(), message = character()
    )
  } else {
    results_df <- tibble::as_tibble(
      do.call(rbind.data.frame, c(result_rows, stringsAsFactors = FALSE))
    )
  }

  n_pass <- sum(results_df$status == "pass")
  n_warn <- sum(results_df$status == "warn")
  n_fail <- sum(results_df$status == "fail")

  overall <- if (n_fail > 0) "fail" else if (n_warn > 0) "warn" else "pass"
  summary_msg <- sprintf(
    "Ontology validation: %d pass, %d warn, %d fail (%s)",
    n_pass, n_warn, n_fail, toupper(overall)
  )

  report <- list(
    status = overall,
    summary = summary_msg,
    results = results_df,
    ontology_version = ont_version,
    n_pass = n_pass,
    n_warn = n_warn,
    n_fail = n_fail
  )
  class(report) <- "ms_validation_report"
  report
}

#' Print method for ms_validation_report
#'
#' @param x An `ms_validation_report` object.
#' @param ... Additional arguments (ignored).
#' @export
print.ms_validation_report <- function(x, ...) {
  status_icon <- switch(x$status,
    pass = cli::col_green(cli::symbol$tick),
    warn = cli::col_yellow("!"),
    fail = cli::col_red(cli::symbol$cross)
  )
  cli::cli_h2("{status_icon} {x$summary}")

  if (nrow(x$results) == 0) {
    cli::cli_inform("No IRI fields were checked.")
    return(invisible(x))
  }

  # Show failures first
  fails <- x$results[x$results$status == "fail", ]
  if (nrow(fails) > 0) {
    cli::cli_h3(cli::col_red("Failures ({nrow(fails)})"))
    for (i in seq_len(nrow(fails))) {
      cli::cli_alert_danger(
        "{fails$column_name[i]}.{fails$field[i]}: {fails$message[i]}"
      )
    }
  }

  # Then warnings
  warns <- x$results[x$results$status == "warn", ]
  if (nrow(warns) > 0) {
    cli::cli_h3(cli::col_yellow("Warnings ({nrow(warns)})"))
    for (i in seq_len(min(nrow(warns), 10L))) {
      cli::cli_alert_warning(
        "{warns$column_name[i]}.{warns$field[i]}: {warns$message[i]}"
      )
    }
    if (nrow(warns) > 10L) {
      cli::cli_inform("... and {nrow(warns) - 10L} more warnings.")
    }
  }

  # Summary of passes
  if (x$n_pass > 0) {
    cli::cli_alert_success("{x$n_pass} IRI{?s} validated successfully.")
  }

  invisible(x)
}
