#' Write a Salmon Data Package from preassembled metadata
#'
#' Advanced/manual writer for cases where you already have the canonical Salmon
#' Data Package (SDP) metadata tables assembled. It writes the SDP CSV metadata
#' files under `metadata/` (`dataset.csv`, `tables.csv`,
#' `column_dictionary.csv`, and optional `codes.csv`) plus the data resource
#' files themselves under `data/`. For interoperability with Frictionless-style
#' tooling, the function also emits a derived `datapackage.json` descriptor at
#' the package root.
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
#' @param overwrite Logical; if `FALSE` (default), errors if path exists. If
#'   `TRUE`, replacement is only allowed for empty directories or directories
#'   previously written by `metasalmon`.
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
#'   file_name = "data/main_table.csv",
#'   table_label = "Main Table"
#' )
#' dict <- infer_dictionary(mtcars, dataset_id = "test-1", table_id = "main_table")
#' write_salmon_datapackage(
#'   resources, dataset_meta, table_meta, dict,
#'   path = tempdir()
#' )
#' }
write_salmon_datapackage <- function(
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

  dataset_meta <- .ms_fill_review_placeholders_dataset_meta(.ms_normalize_dataset_meta(dataset_meta))
  table_meta <- .ms_fill_review_placeholders_table_meta(.ms_normalize_table_meta(table_meta))
  dict <- .ms_fill_review_placeholders_dictionary(.ms_normalize_dictionary(dict))
  codes <- .ms_normalize_codes(codes)

  dataset_id <- dataset_meta$dataset_id[1]

  .ms_prepare_package_write_dir(path, overwrite = overwrite)

  dir.create(.ms_metadata_dir(path), recursive = TRUE, showWarnings = FALSE)

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
      file_name <- file.path("data", paste0(resource_name, ".", format))
    }
    file_name <- .ms_normalize_resource_file_name(file_name)
    file_name <- .ms_force_data_subdir(file_name)
    table_meta$file_name[table_meta$table_id == resource_name] <- file_name

    file_path <- file.path(path, file_name)
    dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
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
  readr::write_csv(dataset_meta, .ms_metadata_path(path, "dataset.csv"), na = "")
  readr::write_csv(table_meta, .ms_metadata_path(path, "tables.csv"), na = "")
  readr::write_csv(dict, .ms_metadata_path(path, "column_dictionary.csv"), na = "")
  if (!is.null(codes)) {
    readr::write_csv(codes, .ms_metadata_path(path, "codes.csv"), na = "")
  }

  jsonlite::write_json(
    datapackage,
    file.path(path, "datapackage.json"),
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null"
  )
  .ms_mark_package_ownership(path)

  cli::cli_alert_success("Created Salmon Data Package at {.path {path}}")
  invisible(path)
}

.ms_package_sentinel_file <- function(path) {
  file.path(path, ".metasalmon-package")
}

.ms_mark_package_ownership <- function(path) {
  writeLines("metasalmon-owned", .ms_package_sentinel_file(path), useBytes = TRUE)
  invisible(path)
}

.ms_dir_entries <- function(path) {
  list.files(path, all.files = TRUE, no.. = TRUE, full.names = TRUE)
}

.ms_is_metasalmon_package_dir <- function(path) {
  if (!dir.exists(path)) {
    return(FALSE)
  }

  if (file.exists(.ms_package_sentinel_file(path))) {
    return(TRUE)
  }

  has_sdp_csvs <- all(file.exists(c(
    .ms_metadata_path(path, "dataset.csv"),
    .ms_metadata_path(path, "tables.csv"),
    .ms_metadata_path(path, "column_dictionary.csv")
  )))
  if (has_sdp_csvs) {
    return(TRUE)
  }

  file.exists(file.path(path, "datapackage.json")) &&
    dir.exists(file.path(path, "data")) &&
    dir.exists(.ms_metadata_dir(path))
}

.ms_prepare_package_write_dir <- function(path, overwrite = FALSE) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    return(invisible(path))
  }

  if (!isTRUE(overwrite)) {
    cli::cli_abort(
      "Directory {.path {path}} already exists. Set {.code overwrite = TRUE} to replace."
    )
  }

  existing_files <- .ms_dir_entries(path)
  if (length(existing_files) == 0) {
    return(invisible(path))
  }

  if (!.ms_is_metasalmon_package_dir(path)) {
    cli::cli_abort(c(
      "Refusing to overwrite non-metasalmon directory {.path {path}}.",
      "i" = "Use a new/empty directory, or manually clean this directory first."
    ))
  }

  unlink(existing_files, recursive = TRUE, force = TRUE)
  invisible(path)
}

#' Infer Salmon Data Package artifacts from resource tables
#'
#' Infers column dictionaries, table metadata, candidate code lists, and
#' dataset-level metadata in a single step from one or more raw data tables.
#'
#' This is a convenience helper for biologists who want to get from raw
#' data frames to package-ready metadata artifacts with one call.
#'
#' @param resources Either a named list of data frames (one per resource table)
#'   or a single data frame (converted internally to a one-table list).
#' @param dataset_id Dataset identifier applied to all inferred metadata.
#' @param table_id Name used when `resources` is a single data frame.
#' @param guess_types Logical; if `TRUE` (default), infer `value_type` for each
#'   dictionary column.
#' @param seed_semantics Logical; if `TRUE`, run
#'   `suggest_semantics()` and attach semantic suggestions to the returned
#'   dictionary.
#' @param semantic_sources Vector of vocabulary sources passed to
#'   `suggest_semantics()`.
#' @param semantic_max_per_role Maximum number of suggestions retained per I-ADOPT
#'   role.
#' @param seed_verbose Logical; if TRUE, emit progress messages while seeding
#'   semantic suggestions.
#' @param seed_codes Optional `codes.csv`-style seed metadata.
#' @param seed_table_meta Optional `tables.csv`-style seed metadata.
#' @param seed_dataset_meta Optional `dataset.csv`-style seed metadata.
#' @param semantic_code_scope Character string controlling which `codes.csv`
#'   rows are sent through `suggest_semantics()` during one-shot seeding.
#'   `"factor"` (default) analyzes codes sourced from factor columns and
#'   low-cardinality character columns in the original data frame(s); `"all"`
#'   analyzes all inferred or supplied code rows; `"none"` skips code-level
#'   semantic suggestions.
#'
#' @return A named list with the following components:
#'   - `resources`: Named list of input tables
#'   - `dict`: Inferred dictionary tibble
#'   - `table_meta`: Inferred table metadata tibble
#'   - `codes`: Inferred candidate codes tibble
#'   - `dataset_meta`: Inferred dataset metadata one-row tibble
#'   - `semantic_suggestions`: Semantic suggestion tibble (or `NULL`)
#' @export
#'
#' @examples
#' \dontrun{
#' resources <- list(
#'   catches = data.frame(
#'     station_id = c("A", "B"),
#'     species = c("Coho", "Chinook"),
#'     count = c(10L, 20L),
#'     sample_date = as.Date(c("2024-01-01", "2024-01-02"))
#'   ),
#'   stations = data.frame(
#'     station_id = c("A", "B"),
#'     latitude = c(49.8, 49.9),
#'     longitude = c(-124.4, -124.5)
#'   )
#' )
#'
#' artifacts <- infer_salmon_datapackage_artifacts(
#'   resources,
#'   dataset_id = "demo-1",
#'   seed_semantics = TRUE,
#'   seed_verbose = TRUE
#' )
#'
#' dict <- artifacts$dict
#' table_meta <- artifacts$table_meta
#' codes <- artifacts$codes
#' dataset_meta <- artifacts$dataset_meta
#' }
infer_salmon_datapackage_artifacts <- function(
    resources,
    dataset_id = "dataset-1",
    table_id = "table_1",
    guess_types = TRUE,
    seed_semantics = TRUE,
    semantic_sources = c("smn", "gcdfo", "ols", "nvs"),
    semantic_max_per_role = 1,
    seed_verbose = TRUE,
    seed_codes = NULL,
    seed_table_meta = TRUE,
    seed_dataset_meta = NULL,
    semantic_code_scope = c("factor", "all", "none")
) {
  if (inherits(resources, "data.frame")) {
    resources <- list(resources)
    names(resources) <- table_id
  }

  if (!is.list(resources) || is.null(names(resources)) || any(!nzchar(names(resources)))) {
    cli::cli_abort("{.arg resources} must be a data frame or a named list of data frames")
  }
  if (anyDuplicated(names(resources)) > 0) {
    cli::cli_abort("{.arg resources} names must be unique")
  }
  if (length(resources) == 0) {
    cli::cli_abort("{.arg resources} cannot be empty")
  }

  bad_rows <- which(vapply(resources, function(x) !inherits(x, "data.frame"), logical(1L)))
  if (length(bad_rows) > 0) {
    cli::cli_abort("All entries in {.arg resources} must be data frames. Invalid entries at: {.val {bad_rows}}")
  }

  dict <- infer_dictionary(
    df = resources,
    guess_types = guess_types,
    dataset_id = dataset_id,
    table_id = table_id,
    seed_semantics = FALSE,
    semantic_sources = semantic_sources,
    semantic_max_per_role = semantic_max_per_role,
    seed_verbose = seed_verbose,
    seed_codes = NULL,
    seed_table_meta = NULL,
    seed_dataset_meta = NULL
  )

  table_meta <- if (is.null(seed_table_meta) || isTRUE(seed_table_meta)) {
    infer_table_metadata_from_resources(resources, dataset_id = dataset_id)
  } else {
    .ms_normalize_table_meta(seed_table_meta)
  }

  codes <- if (is.null(seed_codes)) {
    infer_codes_from_resources(resources, dataset_id = dataset_id)
  } else {
    .ms_normalize_codes(seed_codes)
  }

  dataset_meta <- if (is.null(seed_dataset_meta)) {
    infer_dataset_metadata_from_resources(resources, dataset_id = dataset_id)
  } else {
    .ms_normalize_dataset_meta(seed_dataset_meta)
  }

  semantic_code_scope <- match.arg(semantic_code_scope)
  semantic_codes <- .ms_select_semantic_seed_codes(
    codes = codes,
    resources = resources,
    scope = semantic_code_scope
  )

  semantic_suggestions <- NULL
  if (isTRUE(seed_semantics)) {
    if (seed_verbose) {
      cli::cli_alert_info("Seeding semantic suggestions during infer_salmon_datapackage_artifacts().")
    }

    dict <- suggest_semantics(
      df = resources[[1L]],
      dict = dict,
      sources = semantic_sources,
      max_per_role = semantic_max_per_role,
      include_dwc = FALSE,
      codes = semantic_codes,
      table_meta = table_meta,
      dataset_meta = dataset_meta
    )

    semantic_suggestions <- attr(dict, "semantic_suggestions", exact = TRUE)
  }

  dict <- .ms_fill_review_placeholders_dictionary(dict)
  table_meta <- .ms_fill_review_placeholders_table_meta(table_meta)
  dataset_meta <- .ms_fill_review_placeholders_dataset_meta(dataset_meta)

  list(
    resources = resources,
    dataset_id = dataset_id,
    dict = dict,
    table_meta = table_meta,
    codes = codes,
    dataset_meta = dataset_meta,
    semantic_suggestions = semantic_suggestions
  )
}

#' Create a Salmon Data Package directly from raw tables
#'
#' Primary one-shot wrapper: infer dictionary/table metadata/codes/dataset
#' metadata from raw data tables and immediately write a review-ready Salmon
#' Data Package.
#'
#' @param resources Either a named list of data frames (one per resource table)
#'   or a single data frame (converted internally to a one-table list).
#' @param path Character; directory path where package will be written. If
#'   omitted, defaults to `file.path(getwd(), paste0(<dataset_id>-sdp))` using
#'   a filesystem-safe dataset id slug.
#' @param dataset_id Dataset identifier applied to all inferred metadata rows.
#' @param table_id Fallback table identifier when `resources` is a single data frame.
#' @param guess_types Logical; if `TRUE` (default), infer `value_type` for each
#'   dictionary column.
#' @param seed_semantics Logical; if `TRUE` (default), seed semantic suggestions
#'   during inference.
#' @param semantic_sources Vector of vocabulary sources passed to
#'   `suggest_semantics()`.
#' @param semantic_max_per_role Maximum number of suggestions retained per
#'   I-ADOPT role.
#' @param seed_verbose Logical; if TRUE, emit progress messages while seeding
#'   semantic suggestions.
#' @param seed_codes Optional `codes.csv`-style seed metadata.
#' @param seed_table_meta Optional `tables.csv`-style seed metadata.
#' @param seed_dataset_meta Optional `dataset.csv`-style seed metadata.
#' @param semantic_code_scope Character string controlling which `codes.csv`
#'   rows are sent through `suggest_semantics()` during one-shot seeding.
#'   `"factor"` (default) analyzes codes sourced from factor columns and
#'   low-cardinality character columns in the original data frame(s); `"all"`
#'   analyzes all inferred or supplied code rows; `"none"` skips code-level
#'   semantic suggestions.
#' @param check_updates Logical; if `TRUE`, run a short, non-fatal
#'   [check_for_updates()] call after writing the package and mention newer
#'   releases only when one is available. Defaults to `interactive()`.
#' @param format Character; resource format: `"csv"` (default, only format supported)
#' @param overwrite Logical; if `FALSE` (default), errors if path exists. If
#'   `TRUE`, replacement is only allowed for empty directories or directories
#'   previously written by `metasalmon`.
#' @param include_edh_xml Logical; when `TRUE`, writes an HNAP-aware EDH XML
#'   metadata file to `metadata/metadata-edh-hnap.xml` using
#'   `edh_build_iso19139_xml()`. The default is `FALSE`.
#' @param ... Deprecated legacy EDH arguments accepted for backwards
#'   compatibility: `edh_profile`, `EDH_Profile`, and `EDH_profile` all enable
#'   EDH XML export and must be `"dfo_edh_hnap"` when supplied.
#'   `edh_xml_path` is ignored with a warning because XML now always writes to
#'   the default metadata path. Any other extra arguments error.
#'
#' @return Invisibly returns the package path.
#'
#' @details This one-shot helper creates a review-ready package by default:
#' semantic suggestions are seeded and the top-ranked column-level suggestions
#' are auto-applied only into missing dictionary IRI fields. Table-level
#' observation-unit suggestions stay enabled, but `create_sdp()` only
#' auto-applies them into missing `tables.csv$observation_unit_iri` values
#' when they are backed by non-placeholder table metadata and still look
#' lexically compatible with that context; compatible suggestions can also
#' backfill `tables.csv$observation_unit` labels when missing. To reduce review
#' noise conservatively, code-level suggestions default to factor and
#' low-cardinality character source columns only; set
#' `semantic_code_scope = "all"` to broaden that or `"none"` to disable it.
#' The package root contains `README-review.txt`,
#' `semantic_suggestions.csv` (when available), `datapackage.json`,
#' `metadata/`, and `data/`. To keep review files usable,
#' `semantic_suggestions.csv` trims code-level suggestions that do not have
#' enough human-readable context to review safely. Required-field review
#' placeholders are also inserted into the inferred metadata files. In
#' interactive use, `create_sdp()` can also mention an available package update;
#' set `check_updates = FALSE` to skip that network check. The package bundles
#' two Fraser coho examples: `nuseds-fraser-coho-sample.csv` (30 rows across
#' 1996-2024) for the quickest demo, and `nuseds-fraser-coho-2023-2024.csv`
#' (173 rows from the official Open Government Canada Fraser and BC Interior
#' workbook) for a fuller multi-year example. The bundled
#' `system.file("extdata", "example-data-README.md", package = "metasalmon")`
#' note points to the upstream record/resource URLs, licensing, and the
#' repository `data-raw/` script used to derive the fuller example.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data_path <- system.file("extdata", "nuseds-fraser-coho-sample.csv", package = "metasalmon")
#' fraser_coho <- readr::read_csv(data_path, show_col_types = FALSE)
#'
#' pkg <- create_sdp(
#'   fraser_coho,
#'   dataset_id = "fraser-coho-2024",
#'   table_id = "escapement",
#'   overwrite = FALSE
#' )
#' }
create_sdp <- function(
    resources,
    path = NULL,
    dataset_id = "dataset-1",
    table_id = "table_1",
    guess_types = TRUE,
    seed_semantics = TRUE,
    semantic_sources = c("smn", "gcdfo", "ols", "nvs"),
    semantic_max_per_role = 1,
    seed_verbose = TRUE,
    seed_codes = NULL,
    seed_table_meta = TRUE,
    seed_dataset_meta = NULL,
    semantic_code_scope = c("factor", "all", "none"),
    check_updates = interactive(),
    format = "csv",
    overwrite = FALSE,
    include_edh_xml = FALSE,
    ...
) {
  dots <- list(...)
  dot_names <- names(dots)
  if (is.null(dot_names)) {
    dot_names <- rep("", length(dots))
  }

  legacy_profile_names <- intersect(c("edh_profile", "EDH_Profile", "EDH_profile"), dot_names)
  legacy_path_names <- intersect("edh_xml_path", dot_names)
  legacy_requested_xml <- FALSE

  if (length(legacy_profile_names) > 1L) {
    cli::cli_abort(
      "Use only one legacy EDH profile argument; choose {.code edh_profile}, {.code EDH_Profile}, or {.code EDH_profile}."
    )
  }

  if (length(legacy_profile_names) == 1L) {
    legacy_name <- legacy_profile_names[[1]]
    legacy_value <- dots[[legacy_name]]
    legacy_value_chr <- trimws(as.character(legacy_value[[1]]))

    if (length(legacy_value) != 1L || is.na(legacy_value_chr) || !nzchar(legacy_value_chr)) {
      cli::cli_abort(
        "Legacy argument {.code {legacy_name}} must be a single non-empty string."
      )
    }
    if (!identical(legacy_value_chr, "dfo_edh_hnap")) {
      cli::cli_abort(
        "Only {.code \"dfo_edh_hnap\"} is supported for legacy argument {.code {legacy_name}}."
      )
    }

    legacy_requested_xml <- TRUE
    cli::cli_warn(c(
      "Argument {.code {legacy_name}} is deprecated.",
      "i" = "Use {.code include_edh_xml = TRUE}; the DFO EDH HNAP XML is now the only supported export."
    ))
  }

  if (length(legacy_path_names) > 0L) {
    legacy_requested_xml <- TRUE
    cli::cli_warn(c(
      "Argument {.code edh_xml_path} is deprecated and ignored.",
      "i" = "EDH XML now always writes to {.file metadata/metadata-edh-hnap.xml}."
    ))
  }

  unused_named <- setdiff(
    dot_names[nzchar(dot_names)],
    c("edh_profile", "EDH_Profile", "EDH_profile", "edh_xml_path")
  )
  unnamed_count <- sum(!nzchar(dot_names))
  if (length(unused_named) > 0L || unnamed_count > 0L) {
    pieces <- c(unused_named, rep("<unnamed>", unnamed_count))
    cli::cli_abort(
      "Unused argument{?s}: {.code {pieces}}"
    )
  }

  if (!isTRUE(include_edh_xml) && legacy_requested_xml) {
    include_edh_xml <- TRUE
    cli::cli_inform(c(
      "Assuming {.code include_edh_xml = TRUE} because a legacy EDH argument was supplied.",
      "i" = "EDH XML now always writes {.file metadata/metadata-edh-hnap.xml} using the DFO EDH HNAP profile."
    ))
  }

  if (is.null(path) || !nzchar(trimws(path))) {
    path <- file.path(getwd(), paste0(.ms_safe_path_slug(dataset_id), "-sdp"))
  }

  if (!isTRUE(overwrite) && dir.exists(path)) {
    cli::cli_abort(
      "Directory {.path {path}} already exists. Set {.code overwrite = TRUE} to replace."
    )
  }

  semantic_code_scope <- match.arg(semantic_code_scope)

  seed_note <- .ms_create_sdp_seed_note(
    seed_semantics = seed_semantics,
    seed_verbose = seed_verbose,
    semantic_code_scope = semantic_code_scope
  )
  if (!is.null(seed_note)) {
    cli::cli_alert_info(seed_note)
  }

  artifacts <- infer_salmon_datapackage_artifacts(
    resources = resources,
    dataset_id = dataset_id,
    table_id = table_id,
    guess_types = guess_types,
    seed_semantics = seed_semantics,
    semantic_sources = semantic_sources,
    semantic_max_per_role = semantic_max_per_role,
    seed_verbose = seed_verbose,
    seed_codes = seed_codes,
    seed_table_meta = seed_table_meta,
    seed_dataset_meta = seed_dataset_meta,
    semantic_code_scope = semantic_code_scope
  )

  suggestions <- artifacts$semantic_suggestions
  if (is.null(suggestions)) {
    suggestions <- attr(artifacts$dict, "semantic_suggestions", exact = TRUE)
  }
  if (!is.null(suggestions) && nrow(suggestions) > 0) {
    auto_apply_suggestions <- .ms_filter_auto_apply_suggestions(artifacts$dict, suggestions)
    artifacts$dict <- apply_semantic_suggestions(
      artifacts$dict,
      suggestions = auto_apply_suggestions,
      strategy = "top",
      overwrite = FALSE,
      verbose = FALSE
    )
    artifacts$table_meta <- .ms_apply_table_semantic_suggestions(
      artifacts$table_meta,
      suggestions = suggestions,
      overwrite = FALSE
    )
  }

  pkg_path <- write_salmon_datapackage(
    resources = artifacts$resources,
    dataset_meta = artifacts$dataset_meta,
    table_meta = artifacts$table_meta,
    dict = artifacts$dict,
    codes = artifacts$codes,
    path = path,
    format = format,
    overwrite = overwrite
  )

  review_suggestions <- .ms_prepare_review_suggestions(suggestions)
  .ms_write_sdp_review_readme(
    pkg_path = pkg_path,
    dataset_id = dataset_id,
    has_suggestions = !is.null(review_suggestions) && nrow(review_suggestions) > 0
  )

  if (!is.null(review_suggestions) && nrow(review_suggestions) > 0) {
    readr::write_csv(review_suggestions, file.path(pkg_path, "semantic_suggestions.csv"), na = "")
  }

  if (isTRUE(include_edh_xml)) {
    edh_xml_path <- .ms_metadata_path(pkg_path, "metadata-edh-hnap.xml")

    edh_build_iso19139_xml(
      artifacts$dataset_meta,
      output_path = edh_xml_path
    )

    cli::cli_alert_success("Wrote EDH metadata XML at {.path {edh_xml_path}}")
  }

  review_targets <- if (!is.null(review_suggestions) && nrow(review_suggestions) > 0) {
    "Review and finalize in Excel: {.file README-review.txt} and {.file semantic_suggestions.csv}."
  } else {
    "Review and finalize in Excel: {.file README-review.txt}."
  }
  update_note <- .ms_create_sdp_update_note(check_updates = check_updates)

  info_lines <- c(
    "Created review-ready one-shot package with {.fn create_sdp}.",
    "i" = "Top column-level semantic suggestions were auto-applied only where target fields were blank; table observation-unit suggestions were auto-applied only when non-placeholder table metadata and lexical compatibility made the match look plausible.",
    "i" = review_targets
  )
  if (!is.null(update_note)) {
    info_lines <- c(info_lines, "i" = update_note)
  }

  cli::cli_alert_info(info_lines)

  invisible(pkg_path)
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

  dataset_path <- .ms_locate_metadata_file(path, "dataset.csv")
  tables_path <- .ms_locate_metadata_file(path, "tables.csv")
  dict_path <- .ms_locate_metadata_file(path, "column_dictionary.csv")
  codes_path <- .ms_locate_metadata_file(path, "codes.csv")
  json_path <- file.path(path, "datapackage.json")

  has_canonical <- all(!is.na(c(dataset_path, tables_path, dict_path)))

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
  if (!is.na(codes_path) && file.exists(codes_path)) {
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

#' Validate a Salmon Data Package end to end
#'
#' Reads a package from disk, checks that metadata/data files stay aligned,
#' verifies coded values against `codes.csv` when present, and then runs
#' [validate_dictionary()] plus [validate_semantics()]. This is the quickest
#' pre-flight check before sharing a package-first submission.
#'
#' @param path Character; directory containing the Salmon Data Package.
#' @param require_iris Logical; if `TRUE`, require non-empty semantic IRIs for
#'   measurement fields (`term_iri`, `property_iri`, `entity_iri`, and
#'   `unit_iri`).
#'
#' @return Invisibly returns a list with components:
#'   - `package`: loaded package list from [read_salmon_datapackage()].
#'   - `semantic_validation`: result from [validate_semantics()].
#'   - `issues`: package-structure issue tibble (empty when validation passes).
#' @export
#'
#' @examples
#' \dontrun{
#' pkg_path <- create_sdp(
#'   mtcars,
#'   dataset_id = "demo-1",
#'   table_id = "counts",
#'   overwrite = TRUE
#' )
#' validate_salmon_datapackage(pkg_path, require_iris = FALSE)
#' }
validate_salmon_datapackage <- function(path, require_iris = FALSE) {
  pkg <- read_salmon_datapackage(path)

  .ms_validate_dataset_id_alignment(
    pkg$dataset,
    pkg$tables,
    pkg$dictionary,
    pkg$codes
  )

  issues <- .ms_collect_package_validation_issues(pkg, path = path)
  if (nrow(issues) > 0) {
    .ms_abort_package_validation_issues(issues)
  }

  dict <- validate_dictionary(pkg$dictionary, require_iris = require_iris)
  semantic_validation <- validate_semantics(dict, require_iris = require_iris)

  if (nrow(semantic_validation$issues) > 0) {
    preview <- utils::head(unique(semantic_validation$issues$message), 3)
    warn_lines <- c(
      "Package structure is valid, but {.fn validate_semantics} reported issue{?s}.",
      stats::setNames(preview, rep("!", length(preview)))
    )
    if (nrow(semantic_validation$issues) > length(preview)) {
      warn_lines <- c(
        warn_lines,
        "i" = sprintf(
          "%d more semantic issue%s returned in the result.",
          nrow(semantic_validation$issues) - length(preview),
          ifelse(nrow(semantic_validation$issues) - length(preview) == 1, "", "s")
        )
      )
    }
    cli::cli_warn(warn_lines)
  }

  cli::cli_alert_success("Salmon Data Package validation passed")
  invisible(list(
    package = pkg,
    semantic_validation = semantic_validation,
    issues = issues
  ))
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
    "term_iri", "property_iri", "entity_iri", "constraint_iri", "method_iri",
    "unit_label", "unit_iri", "term_type",
    "value_type", "column_role", "required"
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

.ms_dictionary_from_input <- function(dict, normalize = TRUE) {
  if (inherits(dict, "data.frame")) {
    dict <- tibble::as_tibble(dict)
    if (isTRUE(normalize)) {
      dict <- .ms_normalize_dictionary(dict)
      if ("required" %in% names(dict)) {
        dict$required <- .ms_parse_logical(dict$required)
      }
    }
    return(dict)
  }

  if (is.character(dict) && length(dict) == 1 && !is.na(dict) && nzchar(trimws(dict))) {
    dict_path <- trimws(dict)

    if (dir.exists(dict_path)) {
      located <- .ms_locate_metadata_file(dict_path, "column_dictionary.csv")
      if (is.na(located)) {
        cli::cli_abort(
          "Directory {.path {dict_path}} does not contain {.file column_dictionary.csv}."
        )
      }
      dict_path <- located
    } else if (!file.exists(dict_path)) {
      cli::cli_abort(
        "{.arg dict} must be a data frame, package directory, or path to {.file column_dictionary.csv}."
      )
    }

    dict <- .ms_read_metadata_csv(dict_path)
    if (isTRUE(normalize)) {
      dict <- .ms_normalize_dictionary(dict)
      if ("required" %in% names(dict)) {
        dict$required <- .ms_parse_logical(dict$required)
      }
    }
    return(dict)
  }

  cli::cli_abort(
    "{.arg dict} must be a data frame, package directory, or path to {.file column_dictionary.csv}."
  )
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

.ms_collect_package_validation_issues <- function(pkg, path = NULL) {
  issues <- list()

  add_issue <- function(issue_type, message, table_id = NA_character_, column_name = NA_character_, value = NA_character_) {
    issues[[length(issues) + 1]] <<- tibble::tibble(
      issue_type = issue_type,
      table_id = table_id,
      column_name = column_name,
      value = value,
      message = message
    )
    invisible(NULL)
  }

  trimmed_unique <- function(x) {
    x <- trimws(as.character(x))
    x <- unique(x[!is.na(x) & nzchar(x)])
    x
  }

  if (nrow(pkg$dataset) != 1) {
    add_issue(
      "dataset",
      sprintf("dataset.csv should contain exactly one row; found %s.", nrow(pkg$dataset))
    )
  }
  if (nrow(pkg$tables) == 0) {
    add_issue("tables", "No rows found in tables.csv.")
  }
  if (nrow(pkg$dictionary) == 0) {
    add_issue("dictionary", "No rows found in column_dictionary.csv.")
  }

  dup_tables <- unique(pkg$tables$table_id[duplicated(pkg$tables$table_id)])
  dup_tables <- dup_tables[!is.na(dup_tables) & nzchar(trimws(dup_tables))]
  if (length(dup_tables) > 0) {
    add_issue(
      "tables",
      sprintf("Duplicate table_id values in tables.csv: %s.", paste(dup_tables, collapse = ", "))
    )
  }

  table_ids <- trimmed_unique(pkg$tables$table_id)
  dict_table_ids <- trimmed_unique(pkg$dictionary$table_id)
  extra_dict_tables <- setdiff(dict_table_ids, table_ids)
  if (length(extra_dict_tables) > 0) {
    add_issue(
      "dictionary",
      sprintf(
        "column_dictionary.csv references table_id values not present in tables.csv: %s.",
        paste(extra_dict_tables, collapse = ", ")
      )
    )
  }

  if (!is.null(pkg$codes) && nrow(pkg$codes) > 0) {
    code_table_ids <- trimmed_unique(pkg$codes$table_id)
    extra_code_tables <- setdiff(code_table_ids, table_ids)
    if (length(extra_code_tables) > 0) {
      add_issue(
        "codes",
        sprintf(
          "codes.csv references table_id values not present in tables.csv: %s.",
          paste(extra_code_tables, collapse = ", ")
        )
      )
    }
  }

  for (i in seq_len(nrow(pkg$tables))) {
    table_row <- pkg$tables[i, , drop = FALSE]
    table_id <- .ms_scalar_text(table_row$table_id)
    if (!nzchar(table_id)) {
      next
    }

    file_name <- .ms_scalar_text(table_row$file_name)
    if (!table_id %in% names(pkg$resources)) {
      add_issue(
        "resource",
        sprintf(
          "Table '%s' points to resource '%s', but that file could not be loaded.",
          table_id,
          file_name
        ),
        table_id = table_id
      )
      next
    }

    table_dict <- pkg$dictionary[pkg$dictionary$table_id == table_id, , drop = FALSE]
    dict_cols <- trimmed_unique(table_dict$column_name)
    if (length(dict_cols) == 0) {
      add_issue(
        "dictionary",
        sprintf("No dictionary rows found for table '%s'.", table_id),
        table_id = table_id
      )
      next
    }

    data_df <- pkg$resources[[table_id]]
    data_cols <- names(data_df)

    missing_in_data <- setdiff(dict_cols, data_cols)
    if (length(missing_in_data) > 0) {
      add_issue(
        "columns",
        sprintf(
          "Table '%s' is missing dictionary columns in data: %s.",
          table_id,
          paste(missing_in_data, collapse = ", ")
        ),
        table_id = table_id,
        column_name = paste(missing_in_data, collapse = ", ")
      )
    }

    extra_in_data <- setdiff(data_cols, dict_cols)
    if (length(extra_in_data) > 0) {
      add_issue(
        "columns",
        sprintf(
          "Table '%s' has data columns not listed in column_dictionary.csv: %s.",
          table_id,
          paste(extra_in_data, collapse = ", ")
        ),
        table_id = table_id,
        column_name = paste(extra_in_data, collapse = ", ")
      )
    }

    primary_key <- .ms_scalar_text(table_row$primary_key)
    if (nzchar(primary_key)) {
      pk_cols <- trimws(unlist(strsplit(primary_key, ",", fixed = TRUE)))
      pk_cols <- pk_cols[nzchar(pk_cols)]
      missing_pk <- setdiff(pk_cols, data_cols)
      if (length(missing_pk) > 0) {
        add_issue(
          "primary_key",
          sprintf(
            "Table '%s' primary_key references columns not present in data: %s.",
            table_id,
            paste(missing_pk, collapse = ", ")
          ),
          table_id = table_id,
          column_name = paste(missing_pk, collapse = ", ")
        )
      }
    }

    if (!is.null(pkg$codes) && nrow(pkg$codes) > 0) {
      table_codes <- pkg$codes[pkg$codes$table_id == table_id, , drop = FALSE]
      code_columns <- trimmed_unique(table_codes$column_name)

      for (column_name in code_columns) {
        if (!column_name %in% dict_cols) {
          add_issue(
            "codes",
            sprintf(
              "codes.csv references table '%s' column '%s', but that column is not in column_dictionary.csv.",
              table_id,
              column_name
            ),
            table_id = table_id,
            column_name = column_name
          )
        }

        if (!column_name %in% data_cols) {
          add_issue(
            "codes",
            sprintf(
              "codes.csv references table '%s' column '%s', but that column is not present in data.",
              table_id,
              column_name
            ),
            table_id = table_id,
            column_name = column_name
          )
          next
        }

        data_values <- trimmed_unique(data_df[[column_name]])
        code_values <- trimmed_unique(table_codes$code_value[table_codes$column_name == column_name])
        missing_code_values <- setdiff(data_values, code_values)
        if (length(missing_code_values) > 0) {
          add_issue(
            "codes",
            sprintf(
              "Table '%s' column '%s' has data values not listed in codes.csv: %s.",
              table_id,
              column_name,
              paste(missing_code_values, collapse = ", ")
            ),
            table_id = table_id,
            column_name = column_name,
            value = paste(missing_code_values, collapse = ", ")
          )
        }
      }
    }
  }

  composite_hints <- .ms_collect_composite_hint_values(
    dataset_meta = pkg$dataset,
    table_meta = pkg$tables,
    datapackage_path = if (!is.null(path)) file.path(path, "datapackage.json") else NA_character_,
    hint_fields = c("route", "route_key", "upload_route", "data_level"),
    optional_hint_fields = "source_name"
  )

  if (.ms_values_indicate_composite_intent(composite_hints$value)) {
    wsp_signal <- .ms_detect_wsp_composite_signal(pkg$resources)
    if (!wsp_signal$any_populated) {
      hint_fields_detected <- paste(unique(composite_hints$field), collapse = ", ")
      hint_values_detected <- paste(unique(composite_hints$value), collapse = ", ")
      add_issue(
        "composite_intent",
        sprintf(
          "Explicit composite route intent detected in %s (%s), but no populated WSP composite signal columns were found in cu_timeseries. Populate at least one of: %s.",
          hint_fields_detected,
          hint_values_detected,
          paste(wsp_signal$required_columns, collapse = ", ")
        ),
        table_id = "cu_timeseries",
        column_name = paste(wsp_signal$required_columns, collapse = ", "),
        value = hint_values_detected
      )
    }
  }

  if (length(issues) == 0) {
    return(tibble::tibble(
      issue_type = character(),
      table_id = character(),
      column_name = character(),
      value = character(),
      message = character()
    ))
  }

  dplyr::bind_rows(issues)
}

.ms_abort_package_validation_issues <- function(issues) {
  preview_n <- min(10, nrow(issues))
  messages <- issues$message[seq_len(preview_n)]
  cli_lines <- c(
    sprintf(
      "Salmon Data Package validation failed with %d structural issue%s.",
      nrow(issues),
      ifelse(nrow(issues) == 1, "", "s")
    ),
    stats::setNames(messages, rep("x", length(messages)))
  )

  if (nrow(issues) > preview_n) {
    cli_lines <- c(
      cli_lines,
      "i" = sprintf(
        "%d more issue%s not shown.",
        nrow(issues) - preview_n,
        ifelse(nrow(issues) - preview_n == 1, "", "s")
      )
    )
  }

  cli::cli_abort(cli_lines)
}

.ms_read_metadata_csv <- function(path) {
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )
}

.ms_safe_path_slug <- function(x) {
  slug <- as.character(x)[1]
  if (is.na(slug) || !nzchar(trimws(slug))) {
    slug <- "dataset"
  }
  slug <- tolower(trimws(slug))
  slug <- gsub("[^a-z0-9._-]+", "-", slug)
  slug <- gsub("(^[-._]+|[-._]+$)", "", slug)
  if (!nzchar(slug)) {
    slug <- "dataset"
  }
  slug
}

.ms_metadata_dir <- function(path) {
  file.path(path, "metadata")
}

.ms_metadata_path <- function(path, file_name) {
  file.path(.ms_metadata_dir(path), file_name)
}

.ms_locate_metadata_file <- function(path, file_name) {
  candidates <- c(.ms_metadata_path(path, file_name), file.path(path, file_name))
  hits <- candidates[file.exists(candidates)]
  if (length(hits) == 0) {
    return(NA_character_)
  }
  hits[[1]]
}

.ms_collect_composite_hint_values <- function(
    dataset_meta,
    table_meta,
    datapackage_path,
    hint_fields,
    optional_hint_fields = character()
) {
  all_fields <- unique(c(hint_fields, optional_hint_fields))

  collect_from_df <- function(df, source_label) {
    if (is.null(df) || nrow(df) == 0) {
      return(tibble::tibble(source = character(), field = character(), value = character()))
    }

    present <- intersect(names(df), all_fields)
    if (length(present) == 0) {
      return(tibble::tibble(source = character(), field = character(), value = character()))
    }

    purrr::map_dfr(present, function(field_name) {
      values <- .ms_nonempty_text_values(df[[field_name]])
      if (length(values) == 0) {
        return(tibble::tibble(source = character(), field = character(), value = character()))
      }

      tibble::tibble(
        source = source_label,
        field = field_name,
        value = values
      )
    })
  }

  collect_from_json <- function(path) {
    if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path) || !file.exists(path)) {
      return(tibble::tibble(source = character(), field = character(), value = character()))
    }

    datapackage <- tryCatch(
      jsonlite::read_json(path, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(datapackage)) {
      return(tibble::tibble(source = character(), field = character(), value = character()))
    }

    top_values <- purrr::map_dfr(all_fields, function(field_name) {
      if (is.null(datapackage[[field_name]])) {
        return(tibble::tibble(source = character(), field = character(), value = character()))
      }

      values <- .ms_nonempty_text_values(datapackage[[field_name]])
      if (length(values) == 0) {
        return(tibble::tibble(source = character(), field = character(), value = character()))
      }

      tibble::tibble(
        source = "datapackage",
        field = field_name,
        value = values
      )
    })

    resource_values <- purrr::map_dfr(datapackage$resources %||% list(), function(resource) {
      resource_name <- resource$name %||% "<unnamed_resource>"
      purrr::map_dfr(all_fields, function(field_name) {
        if (is.null(resource[[field_name]])) {
          return(tibble::tibble(source = character(), field = character(), value = character()))
        }

        values <- .ms_nonempty_text_values(resource[[field_name]])
        if (length(values) == 0) {
          return(tibble::tibble(source = character(), field = character(), value = character()))
        }

        tibble::tibble(
          source = paste0("datapackage_resource:", resource_name),
          field = field_name,
          value = values
        )
      })
    })

    dplyr::bind_rows(top_values, resource_values)
  }

  dplyr::bind_rows(
    collect_from_df(dataset_meta, "dataset.csv"),
    collect_from_df(table_meta, "tables.csv"),
    collect_from_json(datapackage_path)
  ) %>%
    dplyr::distinct()
}

.ms_nonempty_text_values <- function(x) {
  values <- as.character(unlist(x, use.names = FALSE))
  values <- trimws(values)
  values <- values[!is.na(values) & nzchar(values)]
  unique(values)
}

.ms_values_indicate_composite_intent <- function(values) {
  if (length(values) == 0) {
    return(FALSE)
  }

  any(grepl("composite", values, ignore.case = TRUE))
}

.ms_column_has_populated_values <- function(x) {
  values <- x[!is.na(x)]
  if (length(values) == 0) {
    return(FALSE)
  }

  if (inherits(values, "factor") || is.character(values)) {
    values <- trimws(as.character(values))
    return(any(nzchar(values)))
  }

  TRUE
}

.ms_detect_wsp_composite_signal <- function(resources) {
  required_columns <- c("SPN_ABD_WILD", "SPN_TREND_WILD", "RAPID_STATUS")
  resource_names <- names(resources %||% list())
  idx <- which(tolower(resource_names) == "cu_timeseries")

  if (length(idx) == 0) {
    return(list(
      cu_timeseries_present = FALSE,
      required_columns = required_columns,
      populated_columns = character(),
      any_populated = FALSE
    ))
  }

  cu_tbl <- resources[[idx[[1]]]]
  present_columns <- intersect(required_columns, names(cu_tbl))
  populated_columns <- present_columns[vapply(present_columns, function(col_name) {
    .ms_column_has_populated_values(cu_tbl[[col_name]])
  }, logical(1))]

  list(
    cu_timeseries_present = TRUE,
    required_columns = required_columns,
    populated_columns = populated_columns,
    any_populated = length(populated_columns) > 0
  )
}

.ms_scalar_text <- function(value) {
  text <- as.character(value[[1]] %||% "")
  if (is.na(text)) {
    return("")
  }
  trimws(text)
}

.ms_code_target_has_review_context <- function(row) {
  code_value <- .ms_scalar_text(row$code_value)
  code_label <- .ms_scalar_text(row$code_label)
  code_description <- .ms_scalar_text(row$code_description)

  nzchar(code_description) || (nzchar(code_label) && (!nzchar(code_value) || !identical(tolower(code_label), tolower(code_value))))
}

.ms_prepare_review_suggestions <- function(suggestions) {
  if (is.null(suggestions)) {
    return(NULL)
  }

  suggestions <- tibble::as_tibble(suggestions)
  if (nrow(suggestions) == 0) {
    return(suggestions)
  }

  if (!all(c("target_scope", "code_value", "code_label", "code_description") %in% names(suggestions))) {
    return(suggestions)
  }

  keep <- vapply(seq_len(nrow(suggestions)), function(i) {
    row <- suggestions[i, , drop = FALSE]
    scope <- row$target_scope[[1]] %||% NA_character_
    if (!identical(scope, "code")) {
      return(TRUE)
    }
    .ms_code_target_has_review_context(row)
  }, logical(1))

  suggestions[keep, , drop = FALSE]
}

.ms_is_text_like_field_name <- function(x) {
  name_lower <- tolower(trimws(as.character(x %||% "")))
  if (!nzchar(name_lower)) {
    return(FALSE)
  }
  grepl("comment|note|remark|description|details?|memo|narrative|summary|reason|explanation|text", name_lower)
}

.ms_values_look_code_like <- function(values) {
  values <- as.character(values)
  values <- trimws(values[!is.na(values)])
  values <- values[nzchar(values)]
  if (length(values) == 0) {
    return(FALSE)
  }

  short_enough <- all(nchar(values) <= 24)
  single_token <- all(!grepl("\\s", values))
  code_chars <- all(grepl("^[[:alnum:]_./-]+$", values))

  short_enough && (single_token || code_chars)
}

.ms_column_is_semantic_code_candidate <- function(col_name, col) {
  if (!inherits(col, c("factor", "character"))) {
    return(FALSE)
  }
  if (.ms_is_text_like_field_name(col_name)) {
    return(FALSE)
  }

  vals <- as.character(col)
  vals <- trimws(vals[!is.na(vals)])
  vals <- vals[nzchar(vals)]
  if (length(vals) == 0) {
    return(FALSE)
  }

  unique_vals <- unique(vals)
  n_unique <- length(unique_vals)
  cardinality_ratio <- n_unique / length(vals)
  if (n_unique > 30) {
    return(FALSE)
  }

  if (cardinality_ratio <= 0.5) {
    return(TRUE)
  }

  n_unique <= 5 && .ms_values_look_code_like(unique_vals)
}

.ms_factor_code_keys <- function(resources) {
  if (is.null(resources) || length(resources) == 0) {
    return(tibble::tibble(table_id = character(), column_name = character()))
  }

  purrr::map_dfr(names(resources), function(tab_id) {
    df <- resources[[tab_id]]
    candidate_cols <- names(df)[vapply(names(df), function(col_name) {
      .ms_column_is_semantic_code_candidate(col_name, df[[col_name]])
    }, logical(1))]

    if (length(candidate_cols) == 0) {
      return(tibble::tibble(table_id = character(), column_name = character()))
    }

    tibble::tibble(
      table_id = tab_id,
      column_name = candidate_cols
    )
  })
}

.ms_non_measurement_target_tokens <- function(...) {
  text <- paste(unlist(list(...)), collapse = " ")
  text <- gsub("([a-z0-9])([A-Z])", "\\1 \\2", text)
  text <- tolower(text)
  text <- gsub("[^a-z0-9]+", " ", text)
  tokens <- unlist(strsplit(text, "\\s+"))
  tokens <- tokens[nzchar(tokens)]
  stop_words <- c(
    "the", "and", "for", "with", "from", "into", "column", "field", "data", "dataset", "table",
    "metadata", "missing", "review", "required", "code", "codes", "value", "values", "record",
    "records", "attribute", "attributes", "category", "categories", "variable", "variables",
    "measurement", "measurements", "type"
  )
  tokens[!(tokens %in% stop_words) & nchar(tokens) >= 3]
}

.ms_non_measurement_suggestion_is_compatible <- function(suggestion, dict_row) {
  role <- tolower(as.character(dict_row$column_role[[1]] %||% ""))
  if (!role %in% c("attribute", "categorical")) {
    return(TRUE)
  }

  label <- .ms_scalar_text(suggestion$label)
  if (!nzchar(label) || .ms_is_review_placeholder(label)) {
    return(FALSE)
  }

  role_hint_status <- if ("role_hint_status" %in% names(suggestion)) {
    tolower(.ms_scalar_text(suggestion$role_hint_status))
  } else {
    ""
  }
  if (role_hint_status %in% c("mismatch_property", "mismatch_variable")) {
    return(FALSE)
  }

  match_type <- if ("match_type" %in% names(suggestion)) {
    tolower(.ms_scalar_text(suggestion$match_type))
  } else {
    ""
  }
  if (nzchar(match_type) && !grepl("label", match_type)) {
    return(FALSE)
  }

  if ("score" %in% names(suggestion)) {
    score <- suppressWarnings(as.numeric(suggestion$score[[1]]))
    if (!is.na(score) && score < 0.75) {
      return(FALSE)
    }
  }

  query_tokens <- unique(.ms_non_measurement_target_tokens(
    if ("search_query" %in% names(suggestion)) suggestion$search_query else "",
    if ("target_label" %in% names(suggestion)) suggestion$target_label else "",
    if ("column_label" %in% names(suggestion)) suggestion$column_label else "",
    if ("column_name" %in% names(suggestion)) suggestion$column_name else ""
  ))
  label_tokens <- unique(.ms_non_measurement_target_tokens(label))
  if (length(query_tokens) == 0 || length(label_tokens) == 0) {
    return(FALSE)
  }

  length(intersect(query_tokens, label_tokens)) > 0
}

.ms_measurement_query_looks_physical <- function(...) {
  text <- paste(unlist(list(...)), collapse = " ")
  text <- gsub("([a-z0-9])([A-Z])", "\\1 \\2", text, perl = TRUE)
  text <- tolower(text)
  grepl(
    "\\b(water|level|discharge|flow|temperature|temp|rain|rainfall|snow|snowfall|precip|gust|wind|speed|depth|width|height|meter|metre|celsius)\\b",
    text,
    perl = TRUE
  )
}

.ms_normalize_measurement_unit_text <- function(x) {
  text <- tolower(.ms_scalar_text(x))
  text <- gsub("([a-z0-9])([A-Z])", "\\1 \\2", text, perl = TRUE)
  text <- gsub("\u00e2", "", text, fixed = TRUE)
  text <- gsub("\u00b0", " degree ", text, fixed = TRUE)
  text <- gsub("\u00b3", "3", text, fixed = TRUE)
  text <- gsub("[^a-z0-9/ ]+", " ", text)
  text <- trimws(gsub("\\s+", " ", text))
  if (!nzchar(text)) {
    return("")
  }

  if (grepl("^(cubic meter per second|cubic metre per second|m3/s|cms|cumec|cumecs)$", text)) return("cubic meter per second")
  if (grepl("^(degree celsius|degrees celsius|deg c|celsius)$", text)) return("degree celsius")
  if (grepl("^(kilometer per hour|kilometre per hour|km/h|kph)$", text)) return("kilometer per hour")
  if (grepl("^(square meter|square metre|square meters|square metres|sq m|m2)$", text)) return("square meter")
  if (grepl("^millimet(er|re)s?$", text)) return("millimeter")
  if (grepl("^centimet(er|re)s?$", text)) return("centimeter")
  if (grepl("^met(er|re)s?$", text)) return("meter")

  text
}

.ms_measurement_has_paired_unit_column <- function(dict_row, dict) {
  col_name <- .ms_scalar_text(dict_row$column_name)
  if (!nzchar(col_name) || !grepl("value$", col_name, ignore.case = TRUE)) {
    return(FALSE)
  }

  table_matches <- rep(TRUE, nrow(dict))
  for (key in intersect(c("dataset_id", "table_id"), names(dict_row))) {
    row_value <- .ms_scalar_text(dict_row[[key]])
    if (nzchar(row_value) && key %in% names(dict)) {
      table_matches <- table_matches & !is.na(dict[[key]]) & as.character(dict[[key]]) == row_value
    }
  }

  sibling_name <- paste0(sub("value$", "", col_name, ignore.case = TRUE), "unit")
  any(table_matches & !is.na(dict$column_name) & tolower(as.character(dict$column_name)) == tolower(sibling_name))
}

.ms_measurement_suggestion_is_compatible <- function(suggestion, dict_row, dict = NULL) {
  role <- tolower(as.character(dict_row$column_role[[1]] %||% ""))
  if (!identical(role, "measurement")) {
    return(TRUE)
  }

  query_text <- paste(
    if ("search_query" %in% names(suggestion)) .ms_scalar_text(suggestion$search_query) else "",
    if ("target_label" %in% names(suggestion)) .ms_scalar_text(suggestion$target_label) else "",
    if ("column_label" %in% names(suggestion)) .ms_scalar_text(suggestion$column_label) else "",
    if ("column_name" %in% names(suggestion)) .ms_scalar_text(suggestion$column_name) else ""
  )

  target_field <- if ("target_sdp_field" %in% names(suggestion)) {
    .ms_scalar_text(suggestion$target_sdp_field)
  } else {
    ""
  }
  if (!is.null(dict) && !identical(target_field, "unit_iri") && .ms_measurement_has_paired_unit_column(dict_row, dict)) {
    return(FALSE)
  }

  if (!.ms_measurement_query_looks_physical(query_text)) {
    return(TRUE)
  }

  label <- .ms_scalar_text(suggestion$label)
  if (!nzchar(label) || .ms_is_review_placeholder(label)) {
    return(FALSE)
  }

  target_field <- if ("target_sdp_field" %in% names(suggestion)) {
    .ms_scalar_text(suggestion$target_sdp_field)
  } else {
    ""
  }

  match_type <- if ("match_type" %in% names(suggestion)) {
    tolower(.ms_scalar_text(suggestion$match_type))
  } else {
    ""
  }

  if (identical(target_field, "unit_iri")) {
    query_unit <- .ms_normalize_measurement_unit_text(
      if ("search_query" %in% names(suggestion)) suggestion$search_query else ""
    )
    label_unit <- .ms_normalize_measurement_unit_text(label)
    if (!nzchar(query_unit) || !nzchar(label_unit) || !identical(query_unit, label_unit)) {
      return(FALSE)
    }
    if ("score" %in% names(suggestion)) {
      score <- suppressWarnings(as.numeric(suggestion$score[[1]]))
      if (!is.na(score) && score < 0.75) {
        return(FALSE)
      }
    }
    return(TRUE)
  }

  suggestion_iri <- tolower(.ms_scalar_text(suggestion$iri))
  suggestion_ontology <- tolower(.ms_scalar_text(suggestion$ontology))
  suggestion_source <- tolower(.ms_scalar_text(suggestion$source))
  if (grepl("rs\\.tdwg\\.org/dwc/terms/", suggestion_iri) ||
      suggestion_ontology %in% c("dwc", "darwin core") ||
      suggestion_source %in% c("dwc", "tdwg")) {
    return(FALSE)
  }

  if (nzchar(match_type) && !grepl("label|unit", match_type)) {
    return(FALSE)
  }

  if ("score" %in% names(suggestion)) {
    score <- suppressWarnings(as.numeric(suggestion$score[[1]]))
    if (!is.na(score) && score < 0.75) {
      return(FALSE)
    }
  }

  query_tokens <- unique(.ms_non_measurement_target_tokens(query_text))
  label_tokens <- unique(.ms_non_measurement_target_tokens(label))
  if (length(query_tokens) == 0 || length(label_tokens) == 0) {
    return(FALSE)
  }

  length(intersect(query_tokens, label_tokens)) > 0
}

.ms_filter_auto_apply_suggestions <- function(dict, suggestions) {
  if (is.null(suggestions) || nrow(suggestions) == 0) {
    return(suggestions)
  }

  suggestions[vapply(seq_len(nrow(suggestions)), function(i) {
    suggestion <- suggestions[i, , drop = FALSE]
    target_field <- if ("target_sdp_field" %in% names(suggestion)) {
      .ms_scalar_text(suggestion$target_sdp_field)
    } else {
      ""
    }
    if (!nzchar(target_field)) {
      return(TRUE)
    }

    matches <- dict$column_name == suggestion$column_name[[1]]
    for (key in intersect(c("dataset_id", "table_id"), names(dict))) {
      if (key %in% names(suggestion)) {
        key_value <- .ms_scalar_text(suggestion[[key]])
        if (nzchar(key_value)) {
          matches <- matches & !is.na(dict[[key]]) & as.character(dict[[key]]) == key_value
        }
      }
    }

    row_ids <- which(matches)
    if (length(row_ids) == 0) {
      return(FALSE)
    }

    any(vapply(row_ids, function(row_id) {
      dict_row <- dict[row_id, , drop = FALSE]
      role <- tolower(as.character(dict_row$column_role[[1]] %||% ""))
      if (role %in% c("identifier", "temporal")) {
        return(FALSE)
      }
      if (identical(role, "measurement")) {
        return(.ms_measurement_suggestion_is_compatible(suggestion, dict_row, dict = dict))
      }
      .ms_non_measurement_suggestion_is_compatible(suggestion, dict_row)
    }, logical(1)))
  }, logical(1)), , drop = FALSE]
}

.ms_select_semantic_seed_codes <- function(codes, resources, scope = c("factor", "all", "none")) {
  scope <- match.arg(scope)
  codes <- .ms_normalize_codes(codes)

  if (is.null(codes) || nrow(codes) == 0) {
    return(codes)
  }
  if (identical(scope, "all")) {
    return(codes)
  }
  if (identical(scope, "none")) {
    return(codes[0, , drop = FALSE])
  }

  factor_keys <- .ms_factor_code_keys(resources)
  if (nrow(factor_keys) == 0) {
    return(codes[0, , drop = FALSE])
  }

  dplyr::semi_join(codes, factor_keys, by = c("table_id", "column_name"))
}

.ms_create_sdp_seed_note <- function(seed_semantics = TRUE,
                                     seed_verbose = TRUE,
                                     semantic_code_scope = c("factor", "all", "none")) {
  if (!isTRUE(seed_semantics) || !isTRUE(seed_verbose)) {
    return(NULL)
  }

  semantic_code_scope <- match.arg(semantic_code_scope)
  scope_note <- switch(
    semantic_code_scope,
    factor = "Code-level semantic suggestions are limited to factor and low-cardinality character columns for this first pass.",
    all = "Code-level semantic suggestions are enabled for all inferred code lists in this run.",
    none = "Code-level semantic suggestions are skipped for this run."
  )

  paste(
    "Seeding semantic suggestions from online vocabularies.",
    "This may take a few minutes for wider tables.",
    scope_note,
    "Use {.code seed_semantics = FALSE} for the fastest first pass."
  )
}

.ms_create_sdp_update_note <- function(check_updates = interactive()) {
  if (!isTRUE(check_updates)) {
    return(NULL)
  }

  result <- tryCatch(
    check_for_updates(quiet = TRUE),
    error = function(e) NULL
  )

  if (is.null(result) || !inherits(result, "metasalmon_update_check")) {
    return(NULL)
  }
  if (!identical(result$status, "update_available") || !isTRUE(result$update_available)) {
    return(NULL)
  }

  latest_version <- result$latest_version %||% NA_character_
  install_command <- result$install_command %||% "remotes::install_github('dfo-pacific-science/metasalmon')"
  if (is.na(latest_version) || !nzchar(latest_version)) {
    latest_version <- "newer"
  }

  sprintf(
    "A newer {.pkg metasalmon} release (%s) is available. Update later with {.code %s} if you want.",
    latest_version,
    install_command
  )
}

.ms_normalize_resource_file_name <- function(file_name) {
  normalized <- gsub("\\\\", "/", trimws(as.character(file_name)[1]))
  normalized <- sub("^\\./", "", normalized)

  if (!nzchar(normalized)) {
    cli::cli_abort("{.field file_name} cannot be blank after normalization.")
  }
  if (grepl("^([A-Za-z]:)?/", normalized)) {
    cli::cli_abort("{.field file_name} must be a relative path inside the package, not an absolute path.")
  }
  if (grepl("(^|/)\\.\\.(/|$)", normalized)) {
    cli::cli_abort("{.field file_name} must not contain '..' path segments.")
  }

  normalized
}

.ms_force_data_subdir <- function(file_name) {
  normalized <- gsub("\\\\", "/", file_name)
  normalized <- sub("^\\./", "", normalized)

  if (startsWith(normalized, "data/")) {
    return(normalized)
  }

  file.path("data", basename(normalized))
}

.ms_write_sdp_review_readme <- function(pkg_path, dataset_id, has_suggestions = TRUE) {
  lines <- c(
    "Salmon Data Package Review Checklist",
    "",
    sprintf("Dataset ID: %s", dataset_id),
    "",
    "Congratulations! You made a Salmon Data Package.",
    "Before sharing your data, work through this short checklist so the package is complete, understandable, and ready for others to reuse.",
    "",
    "Checklist:",
    "[ ] 1. Start in metadata/*.csv and replace every value that begins with 'MISSING DESCRIPTION:' or 'MISSING METADATA:'.",
    "[ ] 2. In metadata/dataset.csv and metadata/tables.csv, confirm title, description, creator/contact, license, file_name paths, labels, observation units, and primary keys.",
    "[ ] 3. Open data/*.csv and confirm each exported table and column name matches metadata/column_dictionary.csv exactly.",
    "[ ] 4. If metadata/codes.csv exists, confirm each coded value used in data/*.csv is listed and described clearly.",
    if (isTRUE(has_suggestions)) "[ ] 5. Review semantic_suggestions.csv, then finalize semantic IRIs in metadata/column_dictionary.csv. For measurement columns, term_iri, property_iri, entity_iri, and unit_iri must all be present and correct." else "[ ] 5. No semantic_suggestions.csv was written for this package; manually review semantic IRIs in metadata/column_dictionary.csv.",
    "[ ] 6. Re-open the folder in R with read_salmon_datapackage(pkg_path), then run validate_salmon_datapackage(pkg_path, require_iris = TRUE). If you want the lower-level pieces too, run validate_dictionary(pkg$dictionary, require_iris = TRUE) and validate_semantics(pkg$dictionary, require_iris = TRUE).",
    "[ ] 7. Share the whole package folder (or a zip of the whole folder) so metadata files and data files stay together.",
    "",
    "Tip: if you edit CSV files in Excel, save them back to CSV before re-validating in R."
  )
  writeLines(lines, con = file.path(pkg_path, "README-review.txt"), useBytes = TRUE)
}

.ms_is_review_placeholder <- function(x) {
  text <- .ms_scalar_text(x)
  nzchar(text) && grepl("^\\s*(MISSING METADATA|MISSING DESCRIPTION|REVIEW REQUIRED)\\s*:", text, ignore.case = TRUE)
}

.ms_table_target_query_context <- function(row) {
  parts <- c(
    observation_unit = if ("observation_unit" %in% names(row) && !.ms_is_review_placeholder(row$observation_unit)) .ms_scalar_text(row$observation_unit) else "",
    description = if ("description" %in% names(row) && !.ms_is_review_placeholder(row$description)) .ms_scalar_text(row$description) else "",
    table_label = if ("table_label" %in% names(row)) .ms_scalar_text(row$table_label) else "",
    table_id = if ("table_id" %in% names(row)) .ms_scalar_text(row$table_id) else ""
  )

  basis <- names(parts)[match(TRUE, nzchar(parts), nomatch = 0)]
  if (length(basis) == 0 || identical(basis, 0L)) {
    basis <- ""
  }

  list(
    basis = basis,
    context = trimws(paste(parts[nzchar(parts)], collapse = " "))
  )
}

.ms_table_text_tokens <- function(x) {
  text <- tolower(.ms_scalar_text(x))
  text <- gsub("[^a-z0-9]+", " ", text)
  tokens <- unlist(strsplit(text, "\\s+"))
  tokens <- tokens[nzchar(tokens)]
  stop_words <- c(
    "the", "and", "for", "with", "from", "into", "table", "tables", "data", "dataset",
    "metadata", "missing", "review", "required", "describe", "what", "each", "row", "rows",
    "main", "records", "record", "values", "value", "observation", "unit", "identifier", "code", "field"
  )
  tokens[!(tokens %in% stop_words) & (nchar(tokens) >= 3 | tokens %in% c("cu", "id"))]
}

.ms_table_suggestion_is_compatible <- function(suggestion, table_row) {
  query_basis <- if ("target_query_basis" %in% names(suggestion)) .ms_scalar_text(suggestion$target_query_basis) else ""
  query_context <- if ("target_query_context" %in% names(suggestion)) .ms_scalar_text(suggestion$target_query_context) else ""

  if (!nzchar(query_basis) || !nzchar(query_context)) {
    derived <- .ms_table_target_query_context(table_row)
    if (!nzchar(query_basis)) {
      query_basis <- derived$basis
    }
    if (!nzchar(query_context)) {
      query_context <- derived$context
    }
  }

  if (!query_basis %in% c("observation_unit", "description")) {
    return(FALSE)
  }

  label <- .ms_scalar_text(suggestion$label)
  if (!nzchar(label) || .ms_is_review_placeholder(label) || grepl("\\b(missing|metadata|review required)\\b", label, ignore.case = TRUE)) {
    return(FALSE)
  }

  match_type <- tolower(.ms_scalar_text(suggestion$match_type))
  if (!nzchar(match_type) || !grepl("^label", match_type)) {
    return(FALSE)
  }

  if ("score" %in% names(suggestion)) {
    score <- suppressWarnings(as.numeric(suggestion$score[[1]]))
    if (!is.na(score) && score < 0.75) {
      return(FALSE)
    }
  }

  context_tokens <- unique(.ms_table_text_tokens(query_context))
  label_tokens <- unique(.ms_table_text_tokens(label))
  if (length(context_tokens) == 0 || length(label_tokens) == 0) {
    return(FALSE)
  }

  length(intersect(context_tokens, label_tokens)) > 0
}

.ms_apply_table_semantic_suggestions <- function(table_meta, suggestions, overwrite = FALSE) {
  table_meta <- .ms_normalize_table_meta(table_meta)
  suggestions <- tibble::as_tibble(suggestions)

  if (nrow(table_meta) == 0 || nrow(suggestions) == 0) {
    return(table_meta)
  }

  required_cols <- c("target_scope", "target_sdp_file", "target_sdp_field", "iri")
  if (!all(required_cols %in% names(suggestions))) {
    return(table_meta)
  }

  table_suggestions <- suggestions %>%
    dplyr::mutate(.row_id = dplyr::row_number()) %>%
    dplyr::filter(
      .data$target_scope == "table",
      .data$target_sdp_file == "tables.csv",
      .data$target_sdp_field == "observation_unit_iri",
      !is.na(.data$iri),
      .data$iri != ""
    ) %>%
    dplyr::arrange(.data$.row_id)

  if (nrow(table_suggestions) == 0) {
    return(table_meta)
  }

  key_cols <- intersect(c("dataset_id", "table_id"), names(table_suggestions))
  if (length(key_cols) == 0) {
    return(table_meta)
  }

  out <- table_meta
  for (row_id in seq_len(nrow(out))) {
    existing_iri <- .ms_scalar_text(out$observation_unit_iri[row_id])
    if (!isTRUE(overwrite) && nzchar(existing_iri)) {
      next
    }

    matches <- rep(TRUE, nrow(table_suggestions))
    for (key in key_cols) {
      row_value <- out[[key]][row_id]
      if (!is.na(row_value) && nzchar(as.character(row_value))) {
        matches <- matches & !is.na(table_suggestions[[key]]) & as.character(table_suggestions[[key]]) == as.character(row_value)
      }
    }

    candidate_rows <- which(matches)
    if (length(candidate_rows) == 0) {
      next
    }

    candidate_rows <- candidate_rows[vapply(candidate_rows, function(i) {
      .ms_table_suggestion_is_compatible(
        table_suggestions[i, , drop = FALSE],
        out[row_id, , drop = FALSE]
      )
    }, logical(1))]
    if (length(candidate_rows) == 0) {
      next
    }

    suggestion <- table_suggestions[candidate_rows[[1]], , drop = FALSE]
    out$observation_unit_iri[row_id] <- suggestion$iri[[1]]
    if ("observation_unit" %in% names(out) && "label" %in% names(suggestion)) {
      suggestion_label <- as.character(suggestion$label[[1]] %||% "")
      if (!is.na(suggestion_label) && nzchar(trimws(suggestion_label))) {
        existing_label <- as.character(out$observation_unit[row_id] %||% "")
        missing_label <- is.na(existing_label) | trimws(existing_label) == "" |
          grepl("^\\s*(MISSING METADATA|MISSING DESCRIPTION|REVIEW REQUIRED)\\s*:", existing_label, ignore.case = TRUE)
        if (isTRUE(missing_label)) {
          out$observation_unit[row_id] <- trimws(suggestion_label)
        }
      }
    }
  }

  out
}
