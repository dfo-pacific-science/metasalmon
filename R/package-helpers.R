#' Create a Salmon Data Package
#'
#' Writes the canonical Salmon Data Package (SDP) CSV metadata files
#' (`dataset.csv`, `tables.csv`, `column_dictionary.csv`, and optional
#' `codes.csv`) plus the data resource files themselves. For interoperability
#' with Frictionless-style tooling, the function also emits a derived
#' `datapackage.json` descriptor.
#'
#' The SDP CSV files remain the canonical package metadata. `datapackage.json`
#' is a convenience export, not the source of truth.
#'
#' @param resources Named list of data frames/tibbles (one per resource)
#' @param dataset_meta Tibble with dataset-level metadata (one row)
#' @param table_meta Tibble with table-level metadata (one row per table)
#' @param dict Dictionary tibble with column definitions
#' @param codes Optional tibble with code lists
#' @param path Character; directory path where package will be written
#' @param format Character; resource format: `"csv"` (default, only format supported)
#' @param overwrite Logical; if `FALSE` (default), errors if path exists
#'
#' @return Invisibly returns the path to the created package
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a simple package
#' resources <- list(main_table = mtcars)
#' dataset_meta <- tibble::tibble(
#'   dataset_id = "test-1",
#'   title = "Test Dataset",
#'   description = "A test dataset"
#' )
#' table_meta <- tibble::tibble(
#'   dataset_id = "test-1",
#'   table_id = "main_table",
#'   file_name = "main_table.csv",
#'   table_label = "Main Table"
#' )
#' dict <- infer_dictionary(mtcars, dataset_id = "test-1", table_id = "main_table")
#' create_salmon_datapackage(
#'   resources, dataset_meta, table_meta, dict,
#'   path = tempdir()
#' )
#' }
create_salmon_datapackage <- function(
    resources,
    dataset_meta,
    table_meta,
    dict,
    codes = NULL,
    path,
    format = "csv",
    overwrite = FALSE
) {
  if (!identical(format, "csv")) {
    cli::cli_abort("Only CSV format is supported. Use {.code format = 'csv'}")
  }

  # Validate inputs
  if (!inherits(dataset_meta, "data.frame") || nrow(dataset_meta) != 1) {
    cli::cli_abort("{.arg dataset_meta} must be a single-row tibble")
  }

  if (!inherits(table_meta, "data.frame") || nrow(table_meta) == 0) {
    cli::cli_abort("{.arg table_meta} must be a non-empty tibble")
  }

  if (!is.list(resources) || length(resources) == 0) {
    cli::cli_abort("{.arg resources} must be a named list of data frames")
  }

  if (is.null(names(resources)) || any(!nzchar(names(resources)))) {
    cli::cli_abort("{.arg resources} must be a named list")
  }

  # Validate dictionary (also normalizes optional columns)
  dict <- validate_dictionary(dict, require_iris = FALSE)

  dataset_meta <- .ms_normalize_dataset_meta(dataset_meta)
  table_meta <- .ms_normalize_table_meta(table_meta)
  dict <- .ms_normalize_dictionary(dict)
  codes <- .ms_normalize_codes(codes)

  dataset_id <- dataset_meta$dataset_id[1]

  # Check if path exists
  if (dir.exists(path) && !overwrite) {
    cli::cli_abort(
      "Directory {.path {path}} already exists. Set {.code overwrite = TRUE} to replace."
    )
  }

  # Create directory
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  } else if (overwrite) {
    existing_files <- list.files(path, full.names = TRUE)
    unlink(existing_files, recursive = TRUE)
  }

  # Write resources and build derived datapackage descriptor.
  resource_list <- list()
  for (resource_name in names(resources)) {
    resource_df <- resources[[resource_name]]

    table_info <- table_meta %>%
      dplyr::filter(.data$table_id == resource_name)

    if (nrow(table_info) == 0) {
      cli::cli_warn(
        "No table metadata found for resource {.val {resource_name}}, skipping"
      )
      next
    }

    file_name <- table_info$file_name[1]
    if (is.na(file_name) || file_name == "") {
      file_name <- paste0(resource_name, ".", format)
      table_meta$file_name[table_meta$table_id == resource_name] <- file_name
    }

    file_path <- file.path(path, file_name)
    readr::write_csv(resource_df, file_path)

    table_dict <- dict %>%
      dplyr::filter(
        .data$dataset_id == dataset_id,
        .data$table_id == resource_name
      )

    fields <- purrr::map(seq_len(nrow(table_dict)), function(i) {
      field <- list(
        name = table_dict$column_name[i],
        type = table_dict$value_type[i],
        description = table_dict$column_description[i]
      )

      if (!is.na(table_dict$column_label[i]) && table_dict$column_label[i] != "" &&
          !identical(table_dict$column_label[i], table_dict$column_name[i])) {
        field$title <- table_dict$column_label[i]
      }

      if (!is.na(table_dict$required[i])) {
        field$constraints <- list(required = isTRUE(table_dict$required[i]))
      }

      if (!is.na(table_dict$unit_iri[i]) && table_dict$unit_iri[i] != "") {
        field$unit_iri <- table_dict$unit_iri[i]
      }
      if (!is.na(table_dict$term_iri[i]) && table_dict$term_iri[i] != "") {
        field$term_iri <- table_dict$term_iri[i]
      }
      if (!is.na(table_dict$term_type[i]) && table_dict$term_type[i] != "") {
        field$term_type <- table_dict$term_type[i]
      }
      if (!is.na(table_dict$property_iri[i]) && table_dict$property_iri[i] != "") {
        field$property_iri <- table_dict$property_iri[i]
      }
      if (!is.na(table_dict$entity_iri[i]) && table_dict$entity_iri[i] != "") {
        field$entity_iri <- table_dict$entity_iri[i]
      }
      if (!is.na(table_dict$constraint_iri[i]) && table_dict$constraint_iri[i] != "") {
        field$constraint_iri <- table_dict$constraint_iri[i]
      }
      if (!is.na(table_dict$method_iri[i]) && table_dict$method_iri[i] != "") {
        field$method_iri <- table_dict$method_iri[i]
      }

      field[!purrr::map_lgl(field, is.null)]
    })

    resource_entry <- list(
      name = resource_name,
      path = file_name,
      profile = "data-resource",
      schema = list(fields = fields)
    )

    if (!is.na(table_info$table_label[1]) && table_info$table_label[1] != "") {
      resource_entry$title <- table_info$table_label[1]
    }
    if (!is.na(table_info$description[1]) && table_info$description[1] != "") {
      resource_entry$description <- table_info$description[1]
    }
    if (!is.na(table_info$primary_key[1]) && table_info$primary_key[1] != "") {
      resource_entry$schema$primaryKey <- trimws(unlist(strsplit(table_info$primary_key[1], ",")))
    }

    resource_list[[length(resource_list) + 1]] <- resource_entry
  }

  datapackage <- list(
    profile = "data-package",
    name = dataset_id,
    title = dataset_meta$title[1],
    description = dataset_meta$description[1],
    resources = resource_list
  )

  if (!is.na(dataset_meta$creator[1]) && dataset_meta$creator[1] != "") {
    datapackage$creator <- dataset_meta$creator[1]
  }
  if (!is.na(dataset_meta$license[1]) && dataset_meta$license[1] != "") {
    datapackage$license <- dataset_meta$license[1]
  }
  if (!is.na(dataset_meta$temporal_start[1]) && dataset_meta$temporal_start[1] != "") {
    datapackage$temporal <- list(start = dataset_meta$temporal_start[1])
    if (!is.na(dataset_meta$temporal_end[1]) && dataset_meta$temporal_end[1] != "") {
      datapackage$temporal$end <- dataset_meta$temporal_end[1]
    }
  }

  # Write canonical SDP metadata after any file_name defaults were resolved.
  readr::write_csv(dataset_meta, file.path(path, "dataset.csv"), na = "")
  readr::write_csv(table_meta, file.path(path, "tables.csv"), na = "")
  readr::write_csv(dict, file.path(path, "column_dictionary.csv"), na = "")
  if (!is.null(codes)) {
    readr::write_csv(codes, file.path(path, "codes.csv"), na = "")
  }

  jsonlite::write_json(
    datapackage,
    file.path(path, "datapackage.json"),
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null"
  )

  cli::cli_alert_success("Created Salmon Data Package at {.path {path}}")
  invisible(path)
}

#' Read a Salmon Data Package
#'
#' Loads a Salmon Data Package from disk. When canonical SDP CSV metadata files
#' are present, those are treated as the source of truth. If they are missing,
#' the function falls back to reconstructing metadata from `datapackage.json`
#' for backwards compatibility with older `metasalmon` outputs.
#'
#' @param path Character; path to directory containing Salmon Data Package files
#'
#' @return A list with components:
#'   - `dataset`: Dataset metadata tibble
#'   - `tables`: Table metadata tibble
#'   - `dictionary`: Dictionary tibble
#'   - `codes`: Codes tibble (if available)
#'   - `resources`: Named list of data tibbles
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read a package
#' pkg <- read_salmon_datapackage("path/to/package")
#' pkg$resources$main_table
#' }
read_salmon_datapackage <- function(path) {
  if (!dir.exists(path)) {
    cli::cli_abort("Directory {.path {path}} does not exist")
  }

  dataset_path <- file.path(path, "dataset.csv")
  tables_path <- file.path(path, "tables.csv")
  dict_path <- file.path(path, "column_dictionary.csv")
  codes_path <- file.path(path, "codes.csv")
  json_path <- file.path(path, "datapackage.json")

  has_canonical <- all(file.exists(c(dataset_path, tables_path, dict_path)))

  if (has_canonical) {
    dataset_meta <- .ms_read_metadata_csv(dataset_path)
    table_meta <- .ms_read_metadata_csv(tables_path)
    dictionary <- .ms_read_metadata_csv(dict_path)
    dictionary <- .ms_normalize_dictionary(dictionary)
    if ("required" %in% names(dictionary)) {
      dictionary$required <- .ms_parse_logical(dictionary$required)
    }
  } else {
    if (!file.exists(json_path)) {
      cli::cli_abort(
        "No Salmon Data Package metadata found in {.path {path}} (expected canonical CSV metadata or {.file datapackage.json})."
      )
    }

    datapackage <- jsonlite::read_json(json_path, simplifyVector = FALSE)

    dataset_meta <- tibble::tibble(
      dataset_id = if (is.null(datapackage$name)) NA_character_ else datapackage$name,
      title = if (is.null(datapackage$title)) NA_character_ else datapackage$title,
      description = if (is.null(datapackage$description)) NA_character_ else datapackage$description,
      creator = if (is.null(datapackage$creator)) NA_character_ else datapackage$creator,
      contact_name = NA_character_,
      contact_email = NA_character_,
      license = if (is.null(datapackage$license)) NA_character_ else datapackage$license,
      temporal_start = if (is.null(datapackage$temporal) || is.null(datapackage$temporal$start)) NA_character_ else datapackage$temporal$start,
      temporal_end = if (is.null(datapackage$temporal) || is.null(datapackage$temporal$end)) NA_character_ else datapackage$temporal$end
    )
    dataset_meta <- .ms_normalize_dataset_meta(dataset_meta)

    table_meta_rows <- list()
    dict_rows <- list()

    resources_json <- datapackage$resources %||% list()
    for (resource in resources_json) {
      resource_name <- resource$name
      file_name <- resource$path %||% NA_character_

      table_meta_rows[[length(table_meta_rows) + 1]] <- list(
        dataset_id = datapackage$name %||% NA_character_,
        table_id = resource_name,
        file_name = file_name,
        table_label = resource$title %||% resource_name,
        description = resource$description %||% NA_character_,
        observation_unit = NA_character_,
        observation_unit_iri = NA_character_,
        primary_key = if (!is.null(resource$schema$primaryKey)) paste(unlist(resource$schema$primaryKey), collapse = ",") else NA_character_
      )

      if (!is.null(resource$schema) && !is.null(resource$schema$fields)) {
        for (field in resource$schema$fields) {
          required <- NA
          if (!is.null(field$constraints) && !is.null(field$constraints$required)) {
            required <- isTRUE(field$constraints$required)
          }

          dict_rows[[length(dict_rows) + 1]] <- list(
            dataset_id = datapackage$name %||% NA_character_,
            table_id = resource_name,
            column_name = field$name %||% NA_character_,
            column_label = field$title %||% field$name %||% NA_character_,
            column_description = field$description %||% NA_character_,
            column_role = NA_character_,
            value_type = field$type %||% "string",
            unit_label = NA_character_,
            unit_iri = field$unit_iri %||% NA_character_,
            term_iri = field$term_iri %||% NA_character_,
            term_type = field$term_type %||% NA_character_,
            required = required,
            property_iri = field$property_iri %||% NA_character_,
            entity_iri = field$entity_iri %||% NA_character_,
            constraint_iri = field$constraint_iri %||% NA_character_,
            method_iri = field$method_iri %||% NA_character_
          )
        }
      }
    }

    table_meta <- dplyr::bind_rows(table_meta_rows)
    table_meta <- .ms_normalize_table_meta(table_meta)
    dictionary <- dplyr::bind_rows(dict_rows)
    dictionary <- .ms_normalize_dictionary(dictionary)
    if ("required" %in% names(dictionary)) {
      dictionary$required <- .ms_parse_logical(dictionary$required)
    }
  }

  codes <- NULL
  if (file.exists(codes_path)) {
    codes <- .ms_read_metadata_csv(codes_path)
    codes <- .ms_normalize_codes(codes)
  }

  resources <- list()
  if (nrow(table_meta) > 0) {
    for (i in seq_len(nrow(table_meta))) {
      resource_name <- table_meta$table_id[i]
      file_name <- table_meta$file_name[i]
      if (is.na(file_name) || file_name == "") {
        next
      }

      file_path <- file.path(path, file_name)
      if (!file.exists(file_path)) {
        cli::cli_warn("Resource file {.path {file_path}} not found, skipping")
        next
      }

      resources[[resource_name]] <- readr::read_csv(file_path, show_col_types = FALSE)
    }
  }

  result <- list(
    dataset = dataset_meta,
    tables = table_meta,
    dictionary = dictionary,
    codes = codes,
    resources = resources
  )

  cli::cli_alert_success("Loaded Salmon Data Package from {.path {path}}")
  result
}

.ms_dataset_meta_cols <- function() {
  c(
    "dataset_id", "title", "description", "creator", "contact_name", "contact_email", "license",
    "contact_org", "contact_position", "temporal_start", "temporal_end", "spatial_extent",
    "dataset_type", "source_citation", "update_frequency", "topic_categories", "keywords",
    "security_classification", "provenance_note", "created", "modified", "spec_version"
  )
}

.ms_table_meta_cols <- function() {
  c(
    "dataset_id", "table_id", "file_name", "table_label", "description",
    "observation_unit", "observation_unit_iri", "primary_key"
  )
}

.ms_dictionary_cols <- function() {
  c(
    "dataset_id", "table_id", "column_name", "column_label", "column_description",
    "column_role", "value_type", "required", "unit_label", "unit_iri", "term_iri",
    "term_type", "property_iri", "entity_iri", "constraint_iri", "method_iri"
  )
}

.ms_codes_cols <- function() {
  c(
    "dataset_id", "table_id", "column_name", "code_value", "code_label",
    "code_description", "vocabulary_iri", "term_iri", "term_type"
  )
}

.ms_align_cols <- function(df, cols) {
  if (is.null(df)) {
    return(NULL)
  }

  df <- tibble::as_tibble(df)
  missing_cols <- setdiff(cols, names(df))
  for (col in missing_cols) {
    df[[col]] <- NA_character_
  }

  ordered_cols <- c(cols, setdiff(names(df), cols))
  df[, ordered_cols, drop = FALSE]
}

.ms_normalize_dataset_meta <- function(dataset_meta) {
  .ms_align_cols(dataset_meta, .ms_dataset_meta_cols())
}

.ms_normalize_table_meta <- function(table_meta) {
  .ms_align_cols(table_meta, .ms_table_meta_cols())
}

.ms_normalize_dictionary <- function(dict) {
  dict <- .ms_align_cols(dict, .ms_dictionary_cols())
  if (!"required" %in% names(dict)) {
    dict$required <- NA
  }
  dict
}

.ms_normalize_codes <- function(codes) {
  if (is.null(codes)) {
    return(NULL)
  }
  .ms_align_cols(codes, .ms_codes_cols())
}

.ms_parse_logical <- function(x) {
  if (is.logical(x)) {
    return(x)
  }

  values <- trimws(as.character(x))
  out <- rep(NA, length(values))
  out[toupper(values) == "TRUE"] <- TRUE
  out[toupper(values) == "FALSE"] <- FALSE
  as.logical(out)
}

.ms_validate_dataset_id_alignment <- function(dataset_meta, table_meta, dict, codes = NULL) {
  dataset_id <- dataset_meta$dataset_id[1]

  check_ids <- function(values, source_name) {
    values <- unique(values[!is.na(values) & values != ""])
    if (length(values) == 0) {
      return(invisible(NULL))
    }
    if (!all(values == dataset_id)) {
      cli::cli_abort(
        "{.arg {source_name}} contains dataset_id values that do not match {.field dataset_meta$dataset_id}."
      )
    }
    invisible(NULL)
  }

  check_ids(table_meta$dataset_id, "table_meta")
  check_ids(dict$dataset_id, "dict")
  if (!is.null(codes)) {
    check_ids(codes$dataset_id, "codes")
  }

  invisible(NULL)
}

.ms_read_metadata_csv <- function(path) {
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )
}
