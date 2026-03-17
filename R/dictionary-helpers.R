#' Infer a starter dictionary from a data frame
#'
#' Proposes a starter dictionary (column dictionary schema) from raw data by
#' guessing column types, roles, and basic metadata.
#'
#' @param df A data frame or tibble to analyze. Or, when provided as a named list of data frames,
#'   `infer_dictionary()` infers each table and returns a combined dictionary.
#' @param guess_types Logical; if `TRUE` (default), infer value types from data.
#' @param dataset_id Character; dataset identifier (default: "dataset-1").
#' @param table_id Character; table identifier (default: "table_1").
#' @param seed_semantics Logical; if `TRUE`, run `suggest_semantics()` and attach
#'   the resulting `semantic_suggestions` attribute to the returned dictionary.
#' @param semantic_sources Character vector of vocabulary sources passed to
#'   `suggest_semantics()` when `seed_semantics = TRUE`. Default: `c("smn", "gcdfo", "ols", "nvs")`.
#' @param semantic_max_per_role Maximum number of suggestions retained per I-ADOPT
#'   role when seeding suggestions. Default: `1`.
#' @param seed_verbose Logical; if TRUE, print a short progress message while
#'   seeding semantic suggestions.
#' @param seed_codes Optional `codes.csv`-style tibble forwarded to
#'   `suggest_semantics()` when `seed_semantics = TRUE`.
#' @param seed_table_meta Optional `tables.csv`-style tibble forwarded to
#'   `suggest_semantics()` when `seed_semantics = TRUE`.
#' @param seed_dataset_meta Optional `dataset.csv`-style tibble forwarded to
#'   `suggest_semantics()` when `seed_semantics = TRUE`.
#'
#' @return A tibble with dictionary schema columns in canonical Salmon Data
#'   Package order: `dataset_id`, `table_id`, `column_name`, `column_label`,
#'   `column_description`, `term_iri`, `property_iri`, `entity_iri`,
#'   `constraint_iri`, `method_iri`, `unit_label`, `unit_iri`, `term_type`,
#'   `value_type`, `column_role`, `required`.
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
#'
#' # Optional: seed semantic suggestions from vocabulary services
#' # (SMN is queried first; GCDFO is a distinct DFO-specific source)
#' dict <- infer_dictionary(
#'   df,
#'   seed_semantics = TRUE,
#'   semantic_sources = c("smn", "gcdfo", "ols", "nvs")
#' )
#' suggestions <- attr(dict, "semantic_suggestions")
#' }
infer_dictionary <- function(df, guess_types = TRUE, dataset_id = "dataset-1", table_id = "table_1",
                            seed_semantics = FALSE, semantic_sources = c("smn", "gcdfo", "ols", "nvs"), semantic_max_per_role = 1,
                            seed_verbose = TRUE,
                            seed_codes = NULL,
                            seed_table_meta = NULL,
                            seed_dataset_meta = NULL) {
  if (is.list(df) && !inherits(df, "data.frame")) {
    resources <- df
    if (length(resources) == 0) {
      cli::cli_abort("{.arg df} must be a non-empty list of data frames or a single data frame")
    }

    if (is.null(names(resources)) || any(names(resources) == "")) {
      cli::cli_abort("{.arg df} list inputs must be named by table_id")
    }

    bad_resource <- vapply(resources, function(x) !inherits(x, "data.frame"), logical(1))
    if (any(bad_resource)) {
      bad <- which(bad_resource)
      cli::cli_abort("All items in {.arg df} must be data frames. Invalid entries at: {.val {bad}}")
    }

    if (anyDuplicated(names(resources)) > 0) {
      cli::cli_abort("{.arg df} table_id names must be unique")
    }

    dict_parts <- lapply(names(resources), function(tab_id) {
      infer_dictionary(
        df = resources[[tab_id]],
        guess_types = guess_types,
        dataset_id = dataset_id,
        table_id = tab_id,
        seed_semantics = FALSE,
        semantic_sources = semantic_sources,
        semantic_max_per_role = semantic_max_per_role,
        seed_verbose = seed_verbose,
        seed_codes = NULL,
        seed_table_meta = NULL,
        seed_dataset_meta = NULL
      )
    })
    dict <- dplyr::bind_rows(dict_parts)

    inferred_table_meta <- infer_table_metadata_from_resources(resources, dataset_id = dataset_id)
    inferred_codes <- infer_codes_from_resources(resources, dataset_id = dataset_id)
    inferred_dataset_meta <- infer_dataset_metadata_from_resources(resources, dataset_id = dataset_id)

    if (!is.null(seed_table_meta)) {
      table_meta <- seed_table_meta
    } else {
      table_meta <- inferred_table_meta
    }

    if (!is.null(seed_codes)) {
      codes <- seed_codes
    } else {
      codes <- inferred_codes
    }

    if (!is.null(seed_dataset_meta)) {
      dataset_meta <- seed_dataset_meta
    } else {
      dataset_meta <- inferred_dataset_meta
    }

    if (isTRUE(seed_semantics)) {
      if (seed_verbose) {
        cli::cli_alert_info("Seeding semantic suggestions during infer_dictionary().")
      }
      dict <- suggest_semantics(
        df = resources[[1]],
        dict = dict,
        sources = semantic_sources,
        max_per_role = semantic_max_per_role,
        codes = codes,
        table_meta = table_meta,
        dataset_meta = dataset_meta
      )
      attr(dict, "inferred_table_meta") <- table_meta
      attr(dict, "inferred_codes") <- codes
      attr(dict, "inferred_dataset_meta") <- dataset_meta
      attr(dict, "inferred_resources") <- names(resources)
    }

    dict <- .ms_fill_review_placeholders_dictionary(dict)
    dict
  } else {
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
      column_label = col_names,
      column_description = rep(NA_character_, n_cols),
      term_iri = rep(NA_character_, n_cols),
      property_iri = rep(NA_character_, n_cols),
      entity_iri = rep(NA_character_, n_cols),
      constraint_iri = rep(NA_character_, n_cols),
      method_iri = rep(NA_character_, n_cols),
      unit_label = rep(NA_character_, n_cols),
      unit_iri = rep(NA_character_, n_cols),
      term_type = rep(NA_character_, n_cols),
      value_type = rep(NA_character_, n_cols),
      column_role = rep(NA_character_, n_cols),
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

    if (isTRUE(seed_semantics)) {
      if (seed_verbose) {
        cli::cli_alert_info("Seeding semantic suggestions during infer_dictionary().")
      }
      dict <- suggest_semantics(
        df = df,
        dict = dict,
        sources = semantic_sources,
        max_per_role = semantic_max_per_role,
        codes = seed_codes,
        table_meta = seed_table_meta,
        dataset_meta = seed_dataset_meta
      )
      if (!is.null(seed_table_meta)) {
        attr(dict, "seed_table_meta") <- seed_table_meta
      }
      if (!is.null(seed_codes)) {
        attr(dict, "seed_codes") <- seed_codes
      }
      if (!is.null(seed_dataset_meta)) {
        attr(dict, "seed_dataset_meta") <- seed_dataset_meta
      }
    }

    dict <- .ms_fill_review_placeholders_dictionary(dict)
    dict
  }
}

.ms_humanize_identifier <- function(x) {
  x <- gsub("[-_]+", " ", as.character(x))
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

.ms_titleize_identifier <- function(x) {
  humanized <- .ms_humanize_identifier(x)
  ifelse(
    is.na(humanized) | humanized == "",
    humanized,
    tools::toTitleCase(humanized)
  )
}

.ms_fill_review_placeholders_dictionary <- function(dict) {
  dict <- tibble::as_tibble(dict)

  blank_desc <- is.na(dict$column_description) | trimws(dict$column_description) == ""
  if (any(blank_desc)) {
    dict$column_description[blank_desc] <- sprintf(
      "REVIEW REQUIRED: define what '%s' means in table '%s'.",
      dict$column_name[blank_desc],
      dict$table_id[blank_desc]
    )
  }

  dict
}

.ms_fill_review_placeholders_table_meta <- function(table_meta) {
  table_meta <- tibble::as_tibble(table_meta)

  blank_label <- is.na(table_meta$table_label) | trimws(table_meta$table_label) == ""
  if (any(blank_label)) {
    table_meta$table_label[blank_label] <- .ms_titleize_identifier(table_meta$table_id[blank_label])
  }

  blank_desc <- is.na(table_meta$description) | trimws(table_meta$description) == ""
  if (any(blank_desc)) {
    table_meta$description[blank_desc] <- sprintf(
      "REVIEW REQUIRED: describe what each row in table '%s' represents.",
      table_meta$table_id[blank_desc]
    )
  }

  table_meta
}

.ms_fill_review_placeholders_dataset_meta <- function(dataset_meta) {
  dataset_meta <- tibble::as_tibble(dataset_meta)

  blank_title <- is.na(dataset_meta$title) | trimws(dataset_meta$title) == ""
  if (any(blank_title)) {
    dataset_meta$title[blank_title] <- .ms_titleize_identifier(dataset_meta$dataset_id[blank_title])
  }

  blank_description <- is.na(dataset_meta$description) | trimws(dataset_meta$description) == ""
  if (any(blank_description)) {
    dataset_meta$description[blank_description] <- sprintf(
      "REVIEW REQUIRED: describe the contents and purpose of dataset '%s'.",
      dataset_meta$dataset_id[blank_description]
    )
  }

  blank_creator <- is.na(dataset_meta$creator) | trimws(dataset_meta$creator) == ""
  if (any(blank_creator)) {
    dataset_meta$creator[blank_creator] <- "REVIEW REQUIRED: add creator, team, or originating program."
  }

  blank_contact_name <- is.na(dataset_meta$contact_name) | trimws(dataset_meta$contact_name) == ""
  if (any(blank_contact_name)) {
    dataset_meta$contact_name[blank_contact_name] <- "REVIEW REQUIRED: add primary contact name or team."
  }

  blank_contact_email <- is.na(dataset_meta$contact_email) | trimws(dataset_meta$contact_email) == ""
  if (any(blank_contact_email)) {
    dataset_meta$contact_email[blank_contact_email] <- "REVIEW REQUIRED: add primary contact email."
  }

  blank_license <- is.na(dataset_meta$license) | trimws(dataset_meta$license) == ""
  if (any(blank_license)) {
    dataset_meta$license[blank_license] <- "REVIEW REQUIRED: add dataset license (for example, CC-BY-4.0)."
  }

  blank_spec_version <- is.na(dataset_meta$spec_version) | trimws(dataset_meta$spec_version) == ""
  if (any(blank_spec_version)) {
    dataset_meta$spec_version[blank_spec_version] <- "sdp-0.1.0"
  }

  dataset_meta
}

infer_table_metadata_from_resources <- function(resources, dataset_id = "dataset-1") {
  table_meta <- purrr::map_dfr(names(resources), function(tab_id) {
    df <- resources[[tab_id]]
    col_names <- names(df)

    id_col <- col_names[grepl("(^|_)id$|_id$|^id_", tolower(col_names))]
    primary_key <- if (length(id_col) > 0) id_col[[1]] else NA_character_

    tibble::tibble(
      dataset_id = dataset_id,
      table_id = tab_id,
      file_name = file.path("data", paste0(tab_id, ".csv")),
      table_label = .ms_titleize_identifier(tab_id),
      description = NA_character_,
      observation_unit = NA_character_,
      observation_unit_iri = NA_character_,
      primary_key = primary_key
    )
  })

  .ms_fill_review_placeholders_table_meta(.ms_normalize_table_meta(table_meta))
}

infer_codes_from_resources <- function(resources, dataset_id = "dataset-1") {
  code_limits <- 30L

  code_tables <- purrr::map_dfr(names(resources), function(tab_id) {
    df <- resources[[tab_id]]
    col_names <- names(df)

    cols <- col_names[vapply(df, function(v) {
      inherits(v, "factor") || inherits(v, "character")
    }, logical(1))]

    if (length(cols) == 0) {
      return(tibble::tibble())
    }

    codes <- purrr::map_dfr(cols, function(col_name) {
      vals <- unique(stats::na.omit(as.character(df[[col_name]])))
      if (length(vals) == 0 || length(vals) > code_limits) {
        return(tibble::tibble())
      }

      tibble::tibble(
        dataset_id = dataset_id,
        table_id = tab_id,
        column_name = col_name,
        code_value = vals,
        code_label = vals,
        code_description = NA_character_,
        vocabulary_iri = NA_character_,
        term_iri = NA_character_,
        term_type = NA_character_
      )
    })

    .ms_normalize_codes(codes)
  })

  .ms_normalize_codes(code_tables)
}

infer_dataset_metadata_from_resources <- function(resources, dataset_id = "dataset-1") {
  parse_date_values <- function(x) {
    try_parse <- function(values, format = NULL) {
      parsed <- tryCatch(
        suppressWarnings(as.Date(values, format = format)),
        error = function(e) as.Date(rep(NA_character_, length(values)))
      )
      parsed[!is.na(parsed)]
    }

    if (is.null(x) || length(x) == 0) {
      return(as.Date(character()))
    }
    if (inherits(x, "Date")) {
      return(x)
    }
    if (inherits(x, "POSIXt")) {
      return(as.Date(x))
    }

    vals <- suppressWarnings(as.character(stats::na.omit(x)))
    if (length(vals) == 0) {
      return(as.Date(character()))
    }

    parse_attempts <- list(
      try_parse(vals),
      try_parse(vals, "%m/%d/%Y"),
      try_parse(vals, "%Y/%m/%d"),
      try_parse(vals, "%d-%b-%y"),
      try_parse(vals, "%d-%b-%Y")
    )

    for (parsed in parse_attempts) {
      if (length(parsed) > 0) {
        return(parsed)
      }
    }

    as.Date(character())
  }

  date_candidates <- unlist(
    purrr::map(resources, function(df) {
      date_cols <- names(df)[
        grepl("date|time|timestamp|dtt|obsdate|survey|year", tolower(names(df)))
      ]
      dates <- unlist(
        lapply(
          date_cols,
          function(col) parse_date_values(df[[col]])
        )
      )
      dates
    }),
    recursive = TRUE
  )
  if (length(date_candidates) > 0) {
    date_candidates <- as.Date(date_candidates, origin = "1970-01-01")
  }
  date_candidates <- date_candidates[!is.na(date_candidates)]
  temporal_start <- if (length(date_candidates) > 0) min(date_candidates) else as.Date(NA)
  temporal_end <- if (length(date_candidates) > 0) max(date_candidates) else as.Date(NA)

  lat_cols <- c()
  lon_cols <- c()
  for (df in resources) {
    nms <- tolower(names(df))
    lat_cols <- c(lat_cols, names(df)[grepl("(^|_)lat(itude)?($|_)", nms)])
    lon_cols <- c(lon_cols, names(df)[grepl("(^|_)lon|(^|_)long(itude)?($|_)", nms)])
  }

  lats <- unlist(lapply(resources, function(df) {
    values <- df[intersect(lat_cols, names(df))]
    as.numeric(unlist(values))
  }))
  lons <- unlist(lapply(resources, function(df) {
    values <- df[intersect(lon_cols, names(df))]
    as.numeric(unlist(values))
  }))
  lats <- lats[!is.na(lats)]
  lons <- lons[!is.na(lons)]

  spatial_extent <- if (length(lats) > 0 && length(lons) > 0) {
    glue::glue("lon={min(lons)}..{max(lons)}, lat={min(lats)}..{max(lats)}")
  } else {
    NA_character_
  }

  keywords <- unique(
    unlist(
      lapply(resources, function(df) {
        trimws(gsub("_", " ", names(df)))
      }),
      recursive = TRUE
    )
  )

  dataset_meta <- tibble::tibble(
    dataset_id = dataset_id,
    title = NA_character_,
    description = NA_character_,
    creator = NA_character_,
    contact_name = NA_character_,
    contact_email = NA_character_,
    license = NA_character_,
    contact_org = NA_character_,
    contact_position = NA_character_,
    temporal_start = as.character(temporal_start),
    temporal_end = as.character(temporal_end),
    spatial_extent = spatial_extent,
    dataset_type = NA_character_,
    source_citation = NA_character_,
    update_frequency = NA_character_,
    topic_categories = NA_character_,
    keywords = paste(utils::head(keywords, 8L), collapse = "; "),
    security_classification = NA_character_,
    provenance_note = NA_character_,
    created = NA_character_,
    modified = NA_character_,
    spec_version = NA_character_
  )

  .ms_fill_review_placeholders_dataset_meta(dataset_meta)
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

  # Preserve explicit factor/categorical intent from the source data.
  if (inherits(col, "factor")) {
    return("categorical")
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
#' @param require_iris Logical; if `TRUE`, requires non-empty semantic IRIs for
#'   measurement columns (`term_iri`, `property_iri`, `entity_iri`, and `unit_iri`).
#'   With the default `FALSE`, those fields are optional; missing values emit a strong
#'   warning so validation stays unblocked while you finish semantic fill-in.
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

  # Ensure optional semantic columns exist (fill with NA if absent)
  semantic_cols <- c(
    "unit_label", "unit_iri", "term_iri", "term_type",
    "property_iri", "entity_iri", "constraint_iri", "method_iri"
  )
  for (col in semantic_cols) {
    if (!col %in% names(dict)) {
      dict[[col]] <- NA_character_
    }
  }

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

  # Measurement columns are allowed to proceed without I-ADOPT identifiers in non-strict
  # mode; still surface a high-signal warning because missing fields reduce package quality.
  measurement_rows <- !is.na(dict$column_role) & dict$column_role == "measurement"
  semantic_fields <- c("term_iri", "property_iri", "entity_iri", "unit_iri")

  if (!require_iris) {
    missing_fields <- lapply(semantic_fields, function(field) {
      measurement_rows & (is.na(dict[[field]]) | dict[[field]] == "")
    })
    names(missing_fields) <- semantic_fields

    any_missing <- Reduce(`|`, missing_fields)
    if (any(any_missing, na.rm = TRUE)) {
      missing_summary <- character(0)
      for (field in names(missing_fields)) {
        rows <- which(missing_fields[[field]])
        if (length(rows) == 0) {
          next
        }
        fields <- dict$column_name[rows]
        missing_summary <- c(
          missing_summary,
          sprintf("%s: %s", field, paste0(sprintf("%s (rows %s)", fields, rows), collapse = ", "))
        )
      }

      cli::cli_warn(c(
        "Hey, you definitely should fill those out before publishing.",
        "x" = "Missing semantic fields for measurement columns:",
        " " = paste("  ", missing_summary, collapse = "\n"),
        "i" = "Next step: run {.fn suggest_semantics} to generate semantic candidates, then set term_iri, property_iri, entity_iri, and unit_iri for your measurement fields.",
        "i" = "See {.url https://dfo-pacific-science.github.io/metasalmon/articles/reusing-standards-salmon-data-terms.html} for how to choose IRI values."
      ))
    }
  }

  required_measurement_fields <- semantic_fields
  if (require_iris) {
    for (field in required_measurement_fields) {
      missing_field <- measurement_rows & (is.na(dict[[field]]) | dict[[field]] == "")
      if (any(missing_field, na.rm = TRUE)) {
        bad_rows <- which(missing_field)
        cli::cli_abort(
          "Measurement columns require {.field {field}}; missing in rows {bad_rows}."
        )
      }
    }
  }

  # Check for duplicate column names within same table
  dupes <- dict %>%
    dplyr::group_by(dataset_id, table_id, column_name) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(.data$n > 1)

  if (nrow(dupes) > 0) {
    cli::cli_abort(
      "Duplicate column names found in dictionary: {.field {dupes$column_name}}"
    )
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

  # Validate dictionary first (also normalizes optional columns)
  dict <- validate_dictionary(dict, require_iris = FALSE)

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
