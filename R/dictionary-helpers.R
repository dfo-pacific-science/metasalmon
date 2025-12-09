#' Infer a starter dictionary from a data frame
#'
#' Proposes a starter dictionary (column dictionary schema) from raw data by
#' guessing column types, roles, and basic metadata. IRIs and semantic fields
#' are left blank for manual or GPT-assisted completion.
#'
#' @param df A data frame or tibble to analyze
#' @param guess_types Logical; if `TRUE` (default), infer value types from data
#' @param dataset_id Character; dataset identifier (default: "dataset-1")
#' @param table_id Character; table identifier (default: "table-1")
#'
#' @return A tibble with dictionary schema columns: `dataset_id`, `table_id`,
#'   `column_name`, `column_label`, `column_description`, `column_role`,
#'   `value_type`, `unit_label`, `unit_iri`, `metric_iri`, `dimension_iri`,
#'   `concept_scheme_iri`, `concept_iri`, `required`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   species = c("Coho", "Chinook"),
#'   count = c(100, 200),
#'   date = as.Date(c("2024-01-01", "2024-01-02"))
#' )
#' dict <- infer_dictionary(df)
#' }
infer_dictionary <- function(df, guess_types = TRUE, dataset_id = "dataset-1", table_id = "table-1") {
  if (!inherits(df, "data.frame")) {
    cli::cli_abort("{.arg df} must be a data frame or tibble")
  }

  col_names <- names(df)
  n_cols <- length(col_names)

  # Initialize dictionary structure
  dict <- tibble::tibble(
    dataset_id = rep(dataset_id, n_cols),
    table_id = rep(table_id, n_cols),
    column_name = col_names,
    column_label = col_names,  # Default to column name
    column_description = rep(NA_character_, n_cols),
    column_role = rep(NA_character_, n_cols),
    value_type = rep(NA_character_, n_cols),
    unit_label = rep(NA_character_, n_cols),
    unit_iri = rep(NA_character_, n_cols),
    metric_iri = rep(NA_character_, n_cols),
    dimension_iri = rep(NA_character_, n_cols),
    concept_scheme_iri = rep(NA_character_, n_cols),
    concept_iri = rep(NA_character_, n_cols),
    required = rep(FALSE, n_cols)
  )

  if (guess_types) {
    # Infer value types from data
    for (i in seq_along(col_names)) {
      col <- df[[col_names[i]]]
      dict$value_type[i] <- infer_value_type(col)
      dict$column_role[i] <- infer_column_role(col_names[i], col)
    }
  }

  dict
}

#' Infer value type from a column
#'
#' @param col A column vector
#' @return Character string indicating the value type
#' @keywords internal
infer_value_type <- function(col) {
  if (inherits(col, "Date") || inherits(col, "POSIXt")) {
    return("date")
  }
  if (inherits(col, "logical")) {
    return("boolean")
  }
  if (inherits(col, "integer")) {
    return("integer")
  }
  if (inherits(col, "numeric")) {
    return("number")
  }
  if (inherits(col, "factor")) {
    return("string")
  }
  if (inherits(col, "character")) {
    return("string")
  }
  "string"  # Default fallback
}

#' Infer column role from name and data
#'
#' @param col_name Column name
#' @param col Column vector
#' @return Character string indicating the column role
#' @keywords internal
infer_column_role <- function(col_name, col) {
  name_lower <- tolower(col_name)

  # Check for common identifier patterns
  if (grepl("^id$|_id$|^id_", name_lower)) {
    return("identifier")
  }
  if (grepl("^key$|_key$|^key_", name_lower)) {
    return("identifier")
  }

  # Check for date/time patterns
  if (grepl("date|time|dtt|timestamp", name_lower) || inherits(col, "Date") || inherits(col, "POSIXt")) {
    return("temporal")
  }

  # Check for measurement/quantity patterns
  if (grepl("count|total|number|amount|quantity|measure", name_lower)) {
    return("measurement")
  }

  # Default to attribute
  "attribute"
}

#' Validate a salmon data dictionary
#'
#' Validates a dictionary tibble against the salmon data package schema.
#' Checks required columns, value types, required flags, and optionally
#' validates IRIs. Reports issues using `cli` messaging.
#'
#' @param dict A tibble or data.frame with dictionary schema columns
#' @param require_iris Logical; if `TRUE`, requires non-empty IRIs for semantic
#'   fields (default: `FALSE`)
#'
#' @return Invisibly returns the normalized dictionary if valid; otherwise
#'   raises errors with clear messages
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dict <- infer_dictionary(mtcars)
#' validate_dictionary(dict)
#' }
validate_dictionary <- function(dict, require_iris = FALSE) {
  if (!inherits(dict, "data.frame")) {
    cli::cli_abort("{.arg dict} must be a data frame or tibble")
  }

  # Required columns
  required_cols <- c(
    "dataset_id", "table_id", "column_name", "column_label",
    "column_description", "column_role", "value_type", "required"
  )

  missing_cols <- setdiff(required_cols, names(dict))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Dictionary missing required columns: {.field {missing_cols}}"
    )
  }

  # Optional semantic columns (should exist but can be empty)
  semantic_cols <- c(
    "unit_label", "unit_iri", "metric_iri", "dimension_iri",
    "concept_scheme_iri", "concept_iri"
  )

  # Validate value types
  valid_types <- c("string", "integer", "number", "boolean", "date", "datetime")
  invalid_types <- !dict$value_type %in% valid_types & !is.na(dict$value_type)
  if (any(invalid_types)) {
    bad_rows <- which(invalid_types)
    cli::cli_abort(
      "Invalid {.field value_type} in rows {bad_rows}: {dict$value_type[bad_rows]}. ",
      "Valid types: {.val {valid_types}}"
    )
  }

  # Validate column roles (optional but if present should be valid)
  valid_roles <- c("identifier", "attribute", "measurement", "temporal", "categorical")
  if ("column_role" %in% names(dict)) {
    invalid_roles <- !dict$column_role %in% valid_roles & !is.na(dict$column_role)
    if (any(invalid_roles)) {
      bad_rows <- which(invalid_roles)
      cli::cli_abort(
        "Invalid {.field column_role} in rows {bad_rows}: {dict$column_role[bad_rows]}. ",
        "Valid roles: {.val {valid_roles}}"
      )
    }
  }

  # Validate required flag is logical
  if (!is.logical(dict$required)) {
    cli::cli_abort("{.field required} must be logical (TRUE/FALSE)")
  }

  # Check for duplicate column names within same table
  dupes <- dict %>%
    dplyr::group_by(.data$dataset_id, .data$table_id, .data$column_name) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(.data$n > 1)

  if (nrow(dupes) > 0) {
    cli::cli_abort(
      "Duplicate column names found in dictionary: {.field {dupes$column_name}}"
    )
  }

  # Optionally require IRIs
  if (require_iris) {
    iri_cols <- c("concept_scheme_iri", "concept_iri")
    for (col in iri_cols) {
      if (col %in% names(dict)) {
        missing_iris <- is.na(dict[[col]]) | dict[[col]] == ""
        if (any(missing_iris)) {
          bad_rows <- which(missing_iris)
          cli::cli_abort(
            "Missing required {.field {col}} in rows {bad_rows}"
          )
        }
      }
    }
  }

  cli::cli_alert_success("Dictionary validation passed")
  invisible(dict)
}

#' Apply a salmon dictionary to a data frame
#'
#' Renames columns, coerces types, applies factor levels from codes, and
#' reports mismatches. Returns a transformed tibble ready for analysis or
#' packaging.
#'
#' @param df A data frame or tibble to transform
#' @param dict A validated dictionary tibble
#' @param codes Optional tibble with code lists (columns: `dataset_id`,
#'   `table_id`, `column_name`, `code_value`, `code_label`, etc.)
#' @param strict Logical; if `TRUE` (default), errors on type coercion
#'   failures; if `FALSE`, warns and coerces to character
#'
#' @return A tibble with renamed columns, coerced types, and factor levels
#'   applied
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dict <- infer_dictionary(mtcars)
#' validate_dictionary(dict)
#' applied <- apply_salmon_dictionary(mtcars, dict)
#' }
apply_salmon_dictionary <- function(df, dict, codes = NULL, strict = TRUE) {
  if (!inherits(df, "data.frame")) {
    cli::cli_abort("{.arg df} must be a data frame or tibble")
  }

  # Validate dictionary first
  validate_dictionary(dict, require_iris = FALSE)

  # Start with a copy
  result <- tibble::as_tibble(df)

  # Get unique table_id from dictionary (assume single table for now)
  table_ids <- unique(dict$table_id)
  if (length(table_ids) > 1) {
    cli::cli_warn(
      "Dictionary contains multiple tables; applying to first: {.val {table_ids[1]}}"
    )
  }
  table_id <- table_ids[1]

  # Filter dictionary for this table
  table_dict <- dict %>%
    dplyr::filter(.data$table_id == table_id)

  # Rename columns
  # Only rename columns that exist in df
  existing_cols <- intersect(names(result), table_dict$column_name)
  if (length(existing_cols) > 0) {
    # dplyr::rename expects new_name = old_name, so names are new, values are old
    rename_map <- stats::setNames(
      existing_cols,
      table_dict$column_label[match(existing_cols, table_dict$column_name)]
    )
    result <- dplyr::rename(result, !!!rename_map)
  }

  # Coerce types and apply codes
  for (i in seq_len(nrow(table_dict))) {
    col_name <- table_dict$column_name[i]
    new_name <- table_dict$column_label[i]
    value_type <- table_dict$value_type[i]

    # Skip if column doesn't exist
    if (!col_name %in% names(df)) {
      next
    }

    # Get column (use original name)
    col <- df[[col_name]]

    # Coerce type
    if (!is.na(value_type)) {
      tryCatch({
        if (value_type == "integer") {
          result[[new_name]] <- as.integer(col)
        } else if (value_type == "number") {
          result[[new_name]] <- as.numeric(col)
        } else if (value_type == "boolean") {
          result[[new_name]] <- as.logical(col)
        } else if (value_type == "date") {
          if (inherits(col, "Date")) {
            result[[new_name]] <- col
          } else {
            result[[new_name]] <- as.Date(col)
          }
        } else if (value_type == "datetime") {
          if (inherits(col, "POSIXt")) {
            result[[new_name]] <- col
          } else {
            result[[new_name]] <- as.POSIXct(col)
          }
        } else {
          # string - keep as is or convert to character
          result[[new_name]] <- as.character(col)
        }
      }, error = function(e) {
        if (strict) {
          cli::cli_abort(
            "Failed to coerce column {.field {col_name}} to {.val {value_type}}: {e$message}"
          )
        } else {
          cli::cli_warn(
            "Failed to coerce column {.field {col_name}} to {.val {value_type}}, keeping as character"
          )
          result[[new_name]] <<- as.character(col)
        }
      })
    }

    # Apply factor levels from codes if available
    if (!is.null(codes) && col_name %in% codes$column_name) {
      col_codes <- codes %>%
        dplyr::filter(
          .data$table_id == table_id,
          .data$column_name == col_name
        )

      if (nrow(col_codes) > 0) {
        code_values <- col_codes$code_value
        code_labels <- col_codes$code_label

        # Convert to factor with levels from codes
        if (inherits(result[[new_name]], "character") || inherits(result[[new_name]], "factor")) {
          result[[new_name]] <- factor(
            result[[new_name]],
            levels = code_values,
            labels = code_labels
          )
        }
      }
    }
  }

  # Report missing required columns
  required_cols <- table_dict %>%
    dplyr::filter(.data$required) %>%
    dplyr::pull(.data$column_name)

  missing_required <- setdiff(required_cols, names(df))
  if (length(missing_required) > 0) {
    cli::cli_warn(
      "Missing required columns in data: {.field {missing_required}}"
    )
  }

  result
}
