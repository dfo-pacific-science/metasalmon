#' Create a Salmon Data Package
#'
#' Assembles a Frictionless Data Package with salmon-specific semantic fields
#' (IRIs, concept schemes, etc.) and writes resources plus `datapackage.json`
#' to disk.
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

  if (is.null(names(resources))) {
    cli::cli_abort("{.arg resources} must be a named list")
  }

  # Validate dictionary (also normalizes optional columns)
  dict <- validate_dictionary(dict, require_iris = FALSE)

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
    # Clean existing files
    existing_files <- list.files(path, full.names = TRUE)
    unlink(existing_files, recursive = TRUE)
  }

  dataset_id <- dataset_meta$dataset_id[1]

  # Write resources
  resource_list <- list()
  for (resource_name in names(resources)) {
    resource_df <- resources[[resource_name]]

    # Get table metadata for this resource
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
    }

    file_path <- file.path(path, file_name)

    # Write resource file (CSV only)
    readr::write_csv(resource_df, file_path)

    # Build resource schema from dictionary
    table_dict <- dict %>%
      dplyr::filter(
        .data$dataset_id == dataset_id,
        .data$table_id == resource_name
      )

    fields <- purrr::map(seq_len(nrow(table_dict)), function(i) {
      field <- list(
        name = table_dict$column_label[i],
        type = table_dict$value_type[i],
        description = table_dict$column_description[i]
      )

      # Add semantic extensions
      if (!is.na(table_dict$unit_iri[i]) && table_dict$unit_iri[i] != "") {
        field$unit_iri <- table_dict$unit_iri[i]
      }
      if ("term_iri" %in% names(table_dict) && !is.na(table_dict$term_iri[i]) && table_dict$term_iri[i] != "") {
        field$term_iri <- table_dict$term_iri[i]
      }
      if ("term_type" %in% names(table_dict) && !is.na(table_dict$term_type[i]) && table_dict$term_type[i] != "") {
        field$term_type <- table_dict$term_type[i]
      }
      if ("property_iri" %in% names(table_dict) && !is.na(table_dict$property_iri[i]) && table_dict$property_iri[i] != "") {
        field$property_iri <- table_dict$property_iri[i]
      }
      if ("entity_iri" %in% names(table_dict) && !is.na(table_dict$entity_iri[i]) && table_dict$entity_iri[i] != "") {
        field$entity_iri <- table_dict$entity_iri[i]
      }
      if ("constraint_iri" %in% names(table_dict) && !is.na(table_dict$constraint_iri[i]) && table_dict$constraint_iri[i] != "") {
        field$constraint_iri <- table_dict$constraint_iri[i]
      }
      if ("method_iri" %in% names(table_dict) && !is.na(table_dict$method_iri[i]) && table_dict$method_iri[i] != "") {
        field$method_iri <- table_dict$method_iri[i]
      }

      # Remove NULL values
      field <- field[!purrr::map_lgl(field, is.null)]
      field
    })

    # Build resource entry
    resource_entry <- list(
      name = resource_name,
      path = file_name,
      profile = "data-resource",
      schema = list(
        fields = fields
      )
    )

    resource_list[[length(resource_list) + 1]] <- resource_entry
  }

  # Build datapackage.json
  datapackage <- list(
    profile = "data-package",
    name = dataset_id,
    title = dataset_meta$title[1],
    description = dataset_meta$description[1],
    resources = resource_list
  )

  # Add optional metadata
  if ("creator" %in% names(dataset_meta) && !is.na(dataset_meta$creator[1])) {
    datapackage$creator <- dataset_meta$creator[1]
  }
  if ("license" %in% names(dataset_meta) && !is.na(dataset_meta$license[1])) {
    datapackage$license <- dataset_meta$license[1]
  }
  if ("temporal_start" %in% names(dataset_meta) && !is.na(dataset_meta$temporal_start[1])) {
    datapackage$temporal <- list(
      start = dataset_meta$temporal_start[1]
    )
    if ("temporal_end" %in% names(dataset_meta) && !is.na(dataset_meta$temporal_end[1])) {
      datapackage$temporal$end <- dataset_meta$temporal_end[1]
    }
  }

  # Write datapackage.json
  json_path <- file.path(path, "datapackage.json")
  jsonlite::write_json(
    datapackage,
    json_path,
    pretty = TRUE,
    auto_unbox = TRUE
  )

  cli::cli_alert_success("Created Salmon Data Package at {.path {path}}")
  invisible(path)
}

#' Read a Salmon Data Package
#'
#' Loads a Salmon Data Package from disk, reading `datapackage.json` and
#' associated resource files, returning tibbles and metadata for analysis.
#'
#' @param path Character; path to directory containing `datapackage.json`
#'
#' @return A list with components:
#'   - `dataset`: Dataset metadata tibble
#'   - `tables`: Table metadata tibble
#'   - `dictionary`: Dictionary tibble (reconstructed from schema)
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

  json_path <- file.path(path, "datapackage.json")
  if (!file.exists(json_path)) {
    cli::cli_abort(
      "datapackage.json not found in {.path {path}}"
    )
  }

  # Read JSON (don't simplify to preserve list structure for resources)
  datapackage <- jsonlite::read_json(json_path, simplifyVector = FALSE)

  # Extract dataset metadata
  dataset_meta <- tibble::tibble(
    dataset_id = if (is.null(datapackage$name)) NA_character_ else datapackage$name,
    title = if (is.null(datapackage$title)) NA_character_ else datapackage$title,
    description = if (is.null(datapackage$description)) NA_character_ else datapackage$description,
    creator = if (is.null(datapackage$creator)) NA_character_ else datapackage$creator,
    license = if (is.null(datapackage$license)) NA_character_ else datapackage$license,
    temporal_start = if (is.null(datapackage$temporal) || is.null(datapackage$temporal$start)) NA_character_ else datapackage$temporal$start,
    temporal_end = if (is.null(datapackage$temporal) || is.null(datapackage$temporal$end)) NA_character_ else datapackage$temporal$end
  )

  # Extract table metadata and resources
  resources <- list()
  table_meta_rows <- list()
  dict_rows <- list()

  for (resource in datapackage$resources) {
    resource_name <- resource$name
    file_path <- file.path(path, resource$path)

    if (!file.exists(file_path)) {
      cli::cli_warn(
        "Resource file {.path {file_path}} not found, skipping"
      )
      next
    }

    # Read resource data
    resource_df <- readr::read_csv(file_path, show_col_types = FALSE)
    resources[[resource_name]] <- resource_df

    # Build table metadata (observation_unit = what each row is about / unit of observation)
    table_meta_rows[[length(table_meta_rows) + 1]] <- list(
      dataset_id = datapackage$name,
      table_id = resource_name,
      file_name = resource$path,
      table_label = resource_name,
      description = NA_character_,
      observation_unit = NA_character_,
      observation_unit_iri = NA_character_,
      primary_key = NA_character_
    )

    # Reconstruct dictionary from schema
    if (!is.null(resource$schema) && !is.null(resource$schema$fields)) {
      for (field in resource$schema$fields) {
        dict_rows[[length(dict_rows) + 1]] <- list(
          dataset_id = datapackage$name,
          table_id = resource_name,
          column_name = field$name,
          column_label = field$name,
          column_description = if (is.null(field$description)) NA_character_ else field$description,
          column_role = NA_character_,
          value_type = if (is.null(field$type)) "string" else field$type,
          unit_label = NA_character_,
          unit_iri = if (is.null(field$unit_iri)) NA_character_ else field$unit_iri,
          term_iri = if (is.null(field$term_iri)) NA_character_ else field$term_iri,
          term_type = if (is.null(field$term_type)) NA_character_ else field$term_type,
          required = FALSE,
          property_iri = if (is.null(field$property_iri)) NA_character_ else field$property_iri,
          entity_iri = if (is.null(field$entity_iri)) NA_character_ else field$entity_iri,
          constraint_iri = if (is.null(field$constraint_iri)) NA_character_ else field$constraint_iri,
          method_iri = if (is.null(field$method_iri)) NA_character_ else field$method_iri
        )
      }
    }
  }

  table_meta <- dplyr::bind_rows(table_meta_rows)
  dictionary <- dplyr::bind_rows(dict_rows)

  # Codes are not stored in datapackage.json, so return empty if not found
  codes <- NULL
  codes_path <- file.path(path, "codes.csv")
  if (file.exists(codes_path)) {
    codes <- readr::read_csv(codes_path, show_col_types = FALSE)
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
