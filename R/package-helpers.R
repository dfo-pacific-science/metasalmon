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

  # Check if path exists
  if (dir.exists(path) && !overwrite) {
    cli::cli_abort(
      "Directory {.path {path}} already exists. Set {.code overwrite = TRUE} to replace."
    )
  }

  # Create directory
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  } else if (overwrite) {
    existing_files <- list.files(path, full.names = TRUE)
    unlink(existing_files, recursive = TRUE)
  }

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

  cli::cli_alert_success("Created Salmon Data Package at {.path {path}}")
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
    seed_table_meta = NULL,
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

  table_meta <- if (is.null(seed_table_meta)) {
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
#' @param overwrite Logical; if `FALSE` (default), errors if path exists
#' @param include_edh_xml Logical; when `TRUE`, writes an EDH XML metadata file into
#'   `metadata/` using `edh_build_iso19139_xml()`.
#' @param edh_profile One of "dfo_edh_hnap" (default) or "iso19139". Determines
#'   whether the richer HNAP-aware profile or compact fallback profile is written
#'   when `include_edh_xml = TRUE`.
#' @param edh_xml_path Optional file path for the EDH output when
#'   `include_edh_xml = TRUE`. If `NULL`, defaults to `metadata/metadata-edh-hnap.xml`
#'   for `edh_profile = "dfo_edh_hnap"` and `metadata/metadata-iso19139.xml`
#'   for `edh_profile = "iso19139"`.
#'
#' @return Invisibly returns the package path.
#'
#' @details This one-shot helper creates a review-ready package by default:
#' semantic suggestions are seeded and the top-ranked column-level suggestions
#' are auto-applied only into missing dictionary IRI fields. Top table-level
#' observation-unit suggestions are also auto-applied into missing
#' `tables.csv$observation_unit_iri` values (and can backfill
#' `tables.csv$observation_unit` labels when missing). To reduce review
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
#'   overwrite = TRUE
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
    seed_table_meta = NULL,
    seed_dataset_meta = NULL,
    semantic_code_scope = c("factor", "all", "none"),
    check_updates = interactive(),
    format = "csv",
    overwrite = FALSE,
    include_edh_xml = FALSE,
    edh_profile = c("dfo_edh_hnap", "iso19139"),
    edh_xml_path = NULL
) {
  if (is.null(path) || !nzchar(trimws(path))) {
    path <- file.path(getwd(), paste0(.ms_safe_path_slug(dataset_id), "-sdp"))
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
    artifacts$dict <- apply_semantic_suggestions(
      artifacts$dict,
      suggestions = suggestions,
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
    edh_profile <- match.arg(edh_profile)
    default_edh_path <- if (identical(edh_profile, "dfo_edh_hnap")) {
      .ms_metadata_path(pkg_path, "metadata-edh-hnap.xml")
    } else {
      .ms_metadata_path(pkg_path, "metadata-iso19139.xml")
    }

    if (is.null(edh_xml_path)) {
      edh_xml_path <- default_edh_path
    }

    edh_build_iso19139_xml(
      artifacts$dataset_meta,
      output_path = edh_xml_path,
      profile = edh_profile
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
    "i" = "Top column-level and table-level semantic suggestions were auto-applied only where target fields were blank.",
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

.ms_factor_code_keys <- function(resources) {
  if (is.null(resources) || length(resources) == 0) {
    return(tibble::tibble(table_id = character(), column_name = character()))
  }

  purrr::map_dfr(names(resources), function(tab_id) {
    df <- resources[[tab_id]]
    candidate_cols <- names(df)[vapply(names(df), function(col_name) {
      col <- df[[col_name]]
      if (inherits(col, "factor")) {
        return(TRUE)
      }
      if (!inherits(col, "character")) {
        return(FALSE)
      }

      vals <- as.character(col)
      vals <- trimws(vals[!is.na(vals)])
      vals <- vals[nzchar(vals)]
      if (length(vals) == 0) {
        return(FALSE)
      }

      n_unique <- length(unique(vals))
      cardinality_ratio <- n_unique / length(vals)
      n_unique <= 30 && (cardinality_ratio <= 0.5 || n_unique <= 5)
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
    "[ ] 6. Re-open the folder in R with read_salmon_datapackage(pkg_path), then run validate_dictionary(pkg$dictionary, require_iris = TRUE) and validate_semantics(pkg$dictionary, require_iris = TRUE).",
    "[ ] 7. Share the whole package folder (or a zip of the whole folder) so metadata files and data files stay together.",
    "",
    "Tip: if you edit CSV files in Excel, save them back to CSV before re-validating in R."
  )
  writeLines(lines, con = file.path(pkg_path, "README-review.txt"), useBytes = TRUE)
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

  grouped_id <- do.call(
    paste,
    c(
      lapply(table_suggestions[key_cols], function(x) ifelse(is.na(x), "<NA>", as.character(x))),
      sep = "\r"
    )
  )
  selected <- table_suggestions[!duplicated(grouped_id), , drop = FALSE]

  out <- table_meta
  for (i in seq_len(nrow(selected))) {
    suggestion <- selected[i, , drop = FALSE]

    matches <- rep(TRUE, nrow(out))
    for (key in key_cols) {
      key_value <- suggestion[[key]][[1]]
      if (!is.na(key_value) && nzchar(as.character(key_value))) {
        matches <- matches & !is.na(out[[key]]) & as.character(out[[key]]) == as.character(key_value)
      }
    }

    row_ids <- which(matches)
    if (length(row_ids) == 0) {
      next
    }

    if (!isTRUE(overwrite)) {
      row_ids <- row_ids[is.na(out$observation_unit_iri[row_ids]) | out$observation_unit_iri[row_ids] == ""]
      if (length(row_ids) == 0) {
        next
      }
    }

    out$observation_unit_iri[row_ids] <- suggestion$iri[[1]]
    if ("observation_unit" %in% names(out) && "label" %in% names(suggestion)) {
      suggestion_label <- as.character(suggestion$label[[1]] %||% "")
      if (!is.na(suggestion_label) && nzchar(trimws(suggestion_label))) {
        existing_label <- as.character(out$observation_unit[row_ids] %||% "")
        missing_label <- is.na(existing_label) | trimws(existing_label) == "" |
          grepl("^\\s*(MISSING METADATA|MISSING DESCRIPTION|REVIEW REQUIRED)\\s*:", existing_label, ignore.case = TRUE)
        if (any(missing_label)) {
          out$observation_unit[row_ids[missing_label]] <- trimws(suggestion_label)
        }
      }
    }
  }

  out
}
