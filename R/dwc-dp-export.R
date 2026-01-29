#' Build a DwC-DP datapackage descriptor (export helper)
#'
#' This is an opt-in export helper for DwC-DP (Darwin Core Data Package).
#' SDP remains canonical; DwC-DP is a derived/interoperability view.
#'
#' @param resources Data frame with columns `name`, `path`, `schema`
#'   (schema is the DwC-DP table schema name, e.g., "occurrence", "event").
#' @param profile_version Git ref for DwC-DP schemas (default "master").
#' @param profile_url DwC-DP profile URL.
#' @param output_path Optional path to write the descriptor JSON.
#' @param validate If TRUE, attempt frictionless validation via python.
#' @param python Path to python executable (default "python3").
#'
#' @return A list representing the descriptor (invisible); writes to
#'   `output_path` when provided.
#' @export
dwc_dp_build_descriptor <- function(resources,
                                    profile_version = "master",
                                    profile_url = "http://rs.tdwg.org/dwc/dwc-dp",
                                    output_path = NULL,
                                    validate = FALSE,
                                    python = "python3") {
  if (!all(c("name", "path", "schema") %in% names(resources))) {
    cli::cli_abort("resources must include columns: name, path, schema")
  }

  res_list <- purrr::pmap(resources[, c("name", "path", "schema")], function(name, path, schema) {
    list(
      name = name,
      path = path,
      profile = "tabular-data-resource",
      schema = sprintf("https://raw.githubusercontent.com/gbif/dwc-dp/%s/dwc-dp/table-schemas/%s.json", profile_version, schema)
    )
  })

  descriptor <- list(
    profile = profile_url,
    name = "dwc-dp-export",
    resources = res_list
  )

  if (!is.null(output_path)) {
    jsonlite::write_json(descriptor, output_path, auto_unbox = TRUE, pretty = TRUE)
  }

  if (isTRUE(validate)) {
    .dwc_dp_validate_with_frictionless(descriptor, python = python)
  }

  invisible(descriptor)
}

.dwc_dp_validate_with_frictionless <- function(descriptor, python = "python3") {
  py <- Sys.which(python)
  if (py == "") {
    cli::cli_warn("Python not found; skip frictionless validation.")
    return(invisible(NULL))
  }

  tmp <- tempfile(fileext = ".json")
  jsonlite::write_json(descriptor, tmp, auto_unbox = TRUE, pretty = TRUE)

  cmd <- c(
    "- <<'PY'",
    "import json, sys",
    "try:",
    "    import frictionless",
    "except ImportError:",
    "    print('frictionless not installed; install with: pip install frictionless', file=sys.stderr)",
    "    sys.exit(1)",
    "",
    "pkg = frictionless.Package(sys.argv[1])",
    "report = pkg.validate()",
    "print(report.flatten(['taskName','valid','errors']))",
    "sys.exit(0 if report.valid else 1)",
    "PY",
    shQuote(tmp)
  )

  status <- system2(py, cmd, stdout = TRUE, stderr = TRUE)
  if (length(status) > 0) {
    cat(paste(status, collapse = "\n"), "\n")
  }
  invisible(status)
}
