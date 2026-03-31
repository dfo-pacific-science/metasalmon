#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tibble)
})

script_path <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    normalizePath(sub("^--file=", "", file_arg[[1L]]), winslash = "/", mustWork = TRUE)
  } else {
    normalizePath(".", winslash = "/", mustWork = TRUE)
  }
})

repo_root <- local({
  candidates <- unique(c(
    script_path,
    dirname(script_path),
    file.path(dirname(script_path), "..")
  ))
  for (candidate in candidates) {
    desc_path <- file.path(candidate, "DESCRIPTION")
    if (file.exists(desc_path)) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }
  normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
})

abort <- function(...) {
  stop(sprintf(...), call. = FALSE)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

trim_or_na <- function(x) {
  x <- trimws(as.character(x %||% ""))
  ifelse(is.na(x) | x == "", NA_character_, x)
}

split_csv_arg <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) {
    return(character())
  }
  out <- trimws(unlist(strsplit(x, ",", fixed = TRUE)))
  out[nzchar(out)]
}

usage_text <- function() {
  paste(
    "Usage:",
    "  Rscript scripts/llm-sanity-check.R [--output-dir=DIR] [--datasets=CASE1,CASE2]",
    "    [--direct-repeats=2] [--llm-top-n=3] [--semantic-max-per-role=1]",
    "    [--provider=chapi] [--model=ollama2.mistral:7b]",
    "    [--api-key=KEY] [--base-url=URL]",
    "    [--skip-sdp-loop] [--skip-focused-loop]",
    "",
    "Defaults:",
    "  provider: chapi",
    "  model: ollama2.mistral:7b",
    "  datasets: fraser_tiny,fraser_full,synthetic_field,multi_table",
    "",
    "Environment:",
    "  CHAPI_API_KEY and CHAPI_BASE_URL are respected by metasalmon.",
    "",
    "Outputs:",
    "  <output-dir>/sdp-loop-summary.csv",
    "  <output-dir>/focused-loop-summary.csv",
    "  <output-dir>/focused-loop-assessments.csv",
    "  <output-dir>/focused-loop-suggestions.csv",
    "  <output-dir>/focused-loop-flagged-targets.csv",
    "  <output-dir>/focused-loop-adjudication-template.csv",
    "  <output-dir>/context/<case-id>/...",
    "  <output-dir>/packages/<case-id>/...",
    "  <output-dir>/packages/<case-id>/metadata/metadata-edh-hnap.xml",
    sep = "\n"
  )
}

parse_args <- function(args, output_dir_default) {
  cfg <- list(
    help = FALSE,
    output_dir = output_dir_default,
    datasets = c("fraser_tiny", "fraser_full", "synthetic_field", "multi_table"),
    direct_repeats = 2L,
    llm_top_n = 3L,
    semantic_max_per_role = 1L,
    semantic_sources = c("smn", "gcdfo", "ols", "nvs"),
    semantic_code_scope = "factor",
    llm_timeout_seconds = 120,
    llm_provider = "chapi",
    llm_model = "ollama2.mistral:7b",
    llm_api_key = NULL,
    llm_base_url = NULL,
    run_sdp_loop = TRUE,
    run_focused_loop = TRUE
  )

  for (arg in args) {
    if (identical(arg, "-h") || identical(arg, "--help")) {
      cfg$help <- TRUE
      next
    }
    if (!startsWith(arg, "--")) {
      abort("Unsupported argument: %s", arg)
    }

    parts <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1L]]
    key <- parts[[1L]]
    value <- if (length(parts) > 1L) paste(parts[-1L], collapse = "=") else TRUE

    if (identical(key, "skip-sdp-loop")) {
      cfg$run_sdp_loop <- FALSE
      next
    }
    if (identical(key, "skip-focused-loop")) {
      cfg$run_focused_loop <- FALSE
      next
    }

    switch(
      key,
      `output-dir` = cfg$output_dir <- normalizePath(value, winslash = "/", mustWork = FALSE),
      `datasets` = cfg$datasets <- split_csv_arg(value),
      `direct-repeats` = cfg$direct_repeats <- as.integer(value),
      `llm-top-n` = cfg$llm_top_n <- as.integer(value),
      `semantic-max-per-role` = cfg$semantic_max_per_role <- as.integer(value),
      `semantic-sources` = cfg$semantic_sources <- split_csv_arg(value),
      `semantic-code-scope` = cfg$semantic_code_scope <- trimws(value),
      `timeout-seconds` = cfg$llm_timeout_seconds <- as.numeric(value),
      `provider` = cfg$llm_provider <- trimws(value),
      `model` = cfg$llm_model <- trimws(value),
      `api-key` = cfg$llm_api_key <- trimws(value),
      `base-url` = cfg$llm_base_url <- trimws(value),
      abort("Unknown argument: --%s", key)
    )
  }

  if (!cfg$help && !cfg$run_sdp_loop && !cfg$run_focused_loop) {
    abort("Both loops are disabled. Drop one of the skip flags.")
  }
  if (!cfg$help && length(cfg$datasets) == 0L) {
    abort("No datasets selected.")
  }
  if (!cfg$help && (is.na(cfg$direct_repeats) || cfg$direct_repeats < 1L)) {
    abort("--direct-repeats must be >= 1")
  }
  if (!cfg$help && (is.na(cfg$llm_top_n) || cfg$llm_top_n < 1L)) {
    abort("--llm-top-n must be >= 1")
  }
  if (!cfg$help && (is.na(cfg$semantic_max_per_role) || cfg$semantic_max_per_role < 1L)) {
    abort("--semantic-max-per-role must be >= 1")
  }
  if (!cfg$help && (is.na(cfg$llm_timeout_seconds) || cfg$llm_timeout_seconds <= 0)) {
    abort("--timeout-seconds must be > 0")
  }

  cfg
}

load_metasalmon_dev <- function(path) {
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(path, export_all = FALSE, helpers = FALSE, quiet = TRUE)
    return(invisible(TRUE))
  }
  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(path, export_all = FALSE, helpers = FALSE, quiet = TRUE)
    return(invisible(TRUE))
  }
  if (requireNamespace("metasalmon", quietly = TRUE)) {
    return(invisible(TRUE))
  }
  abort("Install either pkgload, devtools, or the metasalmon package before running this script.")
}

exported <- function(name) {
  getExportedValue("metasalmon", name)
}

repo_file <- function(...) {
  normalizePath(file.path(repo_root, ...), winslash = "/", mustWork = TRUE)
}

write_context_csv <- function(path, df) {
  if (!is.data.frame(df) || nrow(df) == 0L) {
    return(NULL)
  }
  readr::write_csv(df, path, na = "")
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

write_context_markdown <- function(path, spec, artifacts) {
  dict <- artifacts$dict
  focus_columns <- unique(dict$column_name[!is.na(dict$column_name) & nzchar(dict$column_name)])
  lines <- c(
    paste0("# ", spec$label),
    "",
    paste0("Dataset ID: ", spec$dataset_id),
    paste0("Tables: ", paste(names(spec$resources), collapse = ", ")),
    paste0("Seed title: ", trim_or_na(artifacts$dataset_meta$title[[1]]) %||% spec$dataset_id),
    paste0("Seed description: ", trim_or_na(artifacts$dataset_meta$description[[1]]) %||% "NA"),
    paste0("Focus columns: ", if (length(focus_columns) > 0L) paste(focus_columns, collapse = ", ") else "none"),
    "",
    "These notes describe a salmon monitoring package prepared for metasalmon LLM review.",
    "Use the metadata and code tables together when choosing column, table, dataset, and code semantics."
  )
  writeLines(lines, path, useBytes = TRUE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

write_context_pdf <- function(path, spec, artifacts) {
  lines <- c(
    spec$label,
    trim_or_na(artifacts$dataset_meta$title[[1]]) %||% spec$dataset_id,
    trim_or_na(artifacts$dataset_meta$description[[1]]) %||% "",
    paste("Tables:", paste(names(spec$resources), collapse = ", ")),
    "Use this technical note as supporting context for the metasalmon LLM review."
  )

  grDevices::pdf(path, width = 8.5, height = 11)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::plot.new()
  y <- seq(0.92, 0.60, length.out = length(lines))
  for (i in seq_along(lines)) {
    graphics::text(
      x = 0.05,
      y = y[[i]],
      labels = lines[[i]],
      adj = c(0, 0.5),
      cex = if (i == 1L) 1.2 else 0.9
    )
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

write_context_workbook <- function(path, spec, artifacts) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    return(NULL)
  }

  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb, "dataset")
  openxlsx::writeData(wb, "dataset", artifacts$dataset_meta)

  openxlsx::addWorksheet(wb, "tables")
  openxlsx::writeData(wb, "tables", artifacts$table_meta)

  openxlsx::addWorksheet(wb, "columns")
  openxlsx::writeData(wb, "columns", artifacts$dict)

  if (is.data.frame(artifacts$codes) && nrow(artifacts$codes) > 0L) {
    openxlsx::addWorksheet(wb, "codes")
    openxlsx::writeData(wb, "codes", artifacts$codes)
  }

  openxlsx::addWorksheet(wb, "readme")
  openxlsx::writeData(
    wb,
    "readme",
    tibble::tibble(
      section = c("case", "dataset", "notes"),
      text = c(
        spec$label,
        trim_or_na(artifacts$dataset_meta$title[[1]]) %||% spec$dataset_id,
        "Use workbook sheets together as context for create_sdp() and suggest_semantics()."
      )
    )
  )

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

infer_case_artifacts <- function(ms_fns, spec, cfg) {
  ms_fns$infer_salmon_datapackage_artifacts(
    resources = spec$resources,
    dataset_id = spec$dataset_id,
    table_id = spec$table_id,
    seed_semantics = FALSE,
    seed_codes = NULL,
    seed_table_meta = spec$seed_table_meta,
    seed_dataset_meta = spec$seed_dataset_meta,
    semantic_code_scope = cfg$semantic_code_scope
  )
}

prepare_case_context_files <- function(spec, artifacts, output_dir) {
  context_dir <- file.path(output_dir, "context", spec$case_id)
  dir.create(context_dir, recursive = TRUE, showWarnings = FALSE)

  paths <- spec$context_files %||% character()

  md_path <- file.path(context_dir, "context-notes.md")
  paths <- c(paths, write_context_markdown(md_path, spec, artifacts))

  dataset_csv <- file.path(context_dir, "dataset-context.csv")
  table_csv <- file.path(context_dir, "tables-context.csv")
  dict_csv <- file.path(context_dir, "column-dictionary-context.csv")
  code_csv <- file.path(context_dir, "codes-context.csv")

  paths <- c(
    paths,
    write_context_csv(dataset_csv, artifacts$dataset_meta),
    write_context_csv(table_csv, artifacts$table_meta),
    write_context_csv(dict_csv, artifacts$dict),
    write_context_csv(code_csv, artifacts$codes)
  )

  workbook_path <- file.path(context_dir, "context-bundle.xlsx")
  paths <- c(paths, write_context_workbook(workbook_path, spec, artifacts))

  if (requireNamespace("pdftools", quietly = TRUE)) {
    pdf_path <- file.path(context_dir, "technical-note.pdf")
    paths <- c(paths, write_context_pdf(pdf_path, spec, artifacts))
  }

  unique(paths[!is.na(paths) & nzchar(paths)])
}

make_seed_metadata <- function(resources,
                               dataset_id,
                               title,
                               description,
                               keywords,
                               table_labels,
                               table_descriptions,
                               observation_units,
                               primary_keys = NULL) {
  table_ids <- names(resources)
  if (is.null(primary_keys)) {
    primary_keys <- stats::setNames(rep(NA_character_, length(table_ids)), table_ids)
  }

  list(
    dataset_meta = tibble::tibble(
      dataset_id = dataset_id,
      title = title,
      description = description,
      creator = "metasalmon LLM sanity check",
      contact_name = "metasalmon LLM sanity check",
      contact_email = "llm-sanity@example.org",
      license = "Open Government Licence - Canada",
      spec_version = "sdp-0.1.0",
      keywords = keywords,
      temporal_start = NA_character_,
      temporal_end = NA_character_
    ),
    table_meta = tibble::tibble(
      dataset_id = dataset_id,
      table_id = table_ids,
      file_name = paste0(table_ids, ".csv"),
      table_label = unname(table_labels[table_ids]),
      description = unname(table_descriptions[table_ids]),
      observation_unit = unname(observation_units[table_ids]),
      observation_unit_iri = NA_character_,
      primary_key = unname(primary_keys[table_ids])
    )
  )
}

build_fraser_tiny_case <- function() {
  resources <- list(
    escapement = readr::read_csv(repo_file("inst", "extdata", "nuseds-fraser-coho-sample.csv"), show_col_types = FALSE)
  )

  meta <- make_seed_metadata(
    resources = resources,
    dataset_id = "fraser-coho-sample-llm-eval",
    title = "Fraser Coho Sample LLM Evaluation",
    description = "Tiny Fraser coho escapement sample used for a fast metasalmon LLM sanity check.",
    keywords = "salmon, coho, escapement, Fraser",
    table_labels = c(escapement = "Fraser coho escapement sample"),
    table_descriptions = c(escapement = "Escapement records summarised by Fraser population, area, and year."),
    observation_units = c(escapement = "population-year escapement record"),
    primary_keys = c(escapement = "POP_ID")
  )

  list(
    case_id = "fraser_tiny",
    label = "Fraser coho tiny sample",
    dataset_id = "fraser-coho-sample-llm-eval",
    table_id = "escapement",
    resources = resources,
    seed_dataset_meta = meta$dataset_meta,
    seed_table_meta = meta$table_meta,
    context_files = c(
      repo_file("inst", "extdata", "column_dictionary.csv"),
      repo_file("README.md")
    ),
    context_text = NULL,
    focus_targets = tibble::tibble(
      table_id = "escapement",
      column_name = c("NATURAL_SPAWNERS_TOTAL", "ESTIMATE_CLASSIFICATION", "WATERBODY")
    ),
    table_target_ids = "escapement"
  )
}

build_fraser_full_case <- function() {
  resources <- list(
    escapement = readr::read_csv(repo_file("inst", "extdata", "nuseds-fraser-coho-2023-2024.csv"), show_col_types = FALSE)
  )

  meta <- make_seed_metadata(
    resources = resources,
    dataset_id = "fraser-coho-full-llm-eval",
    title = "Fraser Coho Full LLM Evaluation",
    description = "Fraser coho 2023-2024 slice used for a realistic metasalmon LLM sanity check.",
    keywords = "salmon, coho, Fraser, escapement, NuSEDS",
    table_labels = c(escapement = "Fraser coho escapement 2023-2024"),
    table_descriptions = c(escapement = "Escapement records from the Fraser and BC Interior NuSEDS coho slice for 2023 and 2024."),
    observation_units = c(escapement = "population-year escapement record"),
    primary_keys = c(escapement = "POP_ID")
  )

  list(
    case_id = "fraser_full",
    label = "Fraser coho 2023-2024",
    dataset_id = "fraser-coho-full-llm-eval",
    table_id = "escapement",
    resources = resources,
    seed_dataset_meta = meta$dataset_meta,
    seed_table_meta = meta$table_meta,
    context_files = c(
      repo_file("inst", "extdata", "nuseds-fraser-coho-2023-2024-column_dictionary.csv"),
      repo_file("README.md")
    ),
    context_text = NULL,
    focus_targets = tibble::tibble(
      table_id = "escapement",
      column_name = c("NATURAL_ADULT_SPAWNERS", "ESTIMATE_CLASSIFICATION", "POPULATION")
    ),
    table_target_ids = "escapement"
  )
}

build_synthetic_field_case <- function() {
  resources <- list(
    visits = tibble::tibble(
      site_id = c("S01", "S01", "S02", "S03", "S03", "S04"),
      run_year = c(2024L, 2024L, 2024L, 2025L, 2025L, 2025L),
      obs_n = c(14L, 19L, 7L, 22L, 18L, 9L),
      sample_type = factor(c("trap", "trap", "visual", "trap", "visual", "visual")),
      method_code = factor(c("T1", "T1", "V1", "T2", "V1", "V1")),
      species = factor(c("Coho", "Coho", "Sockeye", "Coho", "Chinook", "Sockeye")),
      water_temp_c = c(8.2, 8.5, 9.1, 7.6, 8.0, 9.4),
      status_flag = factor(c("ok", "ok", "review", "ok", "ok", "review")),
      visit_start = as.Date(c("2024-09-01", "2024-09-08", "2024-09-12", "2025-09-03", "2025-09-10", "2025-09-17"))
    )
  )

  meta <- make_seed_metadata(
    resources = resources,
    dataset_id = "synthetic-field-llm-eval",
    title = "Synthetic Field Visit LLM Evaluation",
    description = "Synthetic field-visit dataset with counts, methods, status codes, and temperature measurements.",
    keywords = "field visits, salmon, counts, methods, temperature",
    table_labels = c(visits = "Synthetic field visits"),
    table_descriptions = c(visits = "Each row is a site visit summarising observed salmon counts, method codes, and water temperature."),
    observation_units = c(visits = "site visit observation"),
    primary_keys = c(visits = NA_character_)
  )

  list(
    case_id = "synthetic_field",
    label = "Synthetic field visits",
    dataset_id = "synthetic-field-llm-eval",
    table_id = "visits",
    resources = resources,
    seed_dataset_meta = meta$dataset_meta,
    seed_table_meta = meta$table_meta,
    context_files = NULL,
    context_text = "This synthetic field dataset records site visit observations. obs_n is the number of fish observed during a visit, water_temp_c is water temperature in degrees Celsius, and method_code identifies the field method.",
    focus_targets = tibble::tibble(
      table_id = "visits",
      column_name = c("obs_n", "method_code", "water_temp_c")
    ),
    table_target_ids = "visits"
  )
}

build_multi_table_case <- function() {
  resources <- list(
    catches = tibble::tibble(
      station_id = c("A", "A", "B", "C", "C"),
      sample_date = as.Date(c("2024-06-01", "2024-06-08", "2024-06-15", "2024-06-22", "2024-06-29")),
      species = factor(c("Coho", "Coho", "Sockeye", "Chinook", "Coho")),
      count = c(12L, 9L, 22L, 4L, 15L),
      method_code = factor(c("trap", "trap", "net", "net", "trap")),
      life_stage = factor(c("juvenile", "juvenile", "adult", "adult", "juvenile"))
    ),
    stations = tibble::tibble(
      station_id = c("A", "B", "C"),
      river_name = c("Fraser", "Skeena", "Cowichan"),
      station_type = factor(c("weir", "rotary screw trap", "test fishery")),
      latitude = c(49.10, 54.35, 48.78),
      longitude = c(-122.95, -128.60, -123.72)
    )
  )

  meta <- make_seed_metadata(
    resources = resources,
    dataset_id = "multi-table-llm-eval",
    title = "Synthetic Multi-table LLM Evaluation",
    description = "Synthetic multi-table salmon monitoring package used to exercise both column and table observation-unit suggestions.",
    keywords = "salmon, monitoring, stations, catches, observation unit",
    table_labels = c(
      catches = "Catch summaries",
      stations = "Monitoring stations"
    ),
    table_descriptions = c(
      catches = "Each row summarises a sampling event at a monitoring station with salmon catch counts and methods.",
      stations = "Each row describes a station used by the monitoring program."
    ),
    observation_units = c(
      catches = "station sampling event",
      stations = "monitoring station"
    ),
    primary_keys = c(
      catches = NA_character_,
      stations = "station_id"
    )
  )

  list(
    case_id = "multi_table",
    label = "Synthetic multi-table monitoring package",
    dataset_id = "multi-table-llm-eval",
    table_id = "catches",
    resources = resources,
    seed_dataset_meta = meta$dataset_meta,
    seed_table_meta = meta$table_meta,
    context_files = NULL,
    context_text = "The catches table holds station-level salmon sampling events. The stations table describes fixed monitoring locations used by the program.",
    focus_targets = tibble::tibble(
      table_id = c("catches", "catches", "stations"),
      column_name = c("count", "method_code", "station_type")
    ),
    table_target_ids = c("catches", "stations")
  )
}

build_case_registry <- function() {
  cases <- list(
    fraser_tiny = build_fraser_tiny_case(),
    fraser_full = build_fraser_full_case(),
    synthetic_field = build_synthetic_field_case(),
    multi_table = build_multi_table_case()
  )

  stats::setNames(cases, names(cases))
}

context_format_summary <- function(paths) {
  paths <- trim_or_na(paths)
  paths <- paths[!is.na(paths)]
  if (length(paths) == 0L) {
    return(NA_character_)
  }

  formats <- tolower(tools::file_ext(paths))
  formats[!nzchar(formats)] <- "<none>"
  paste(sort(unique(formats)), collapse = ",")
}

capture_warnings <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = unique(warnings))
}

safe_capture <- function(expr) {
  tryCatch(
    capture_warnings(expr),
    error = function(e) list(value = NULL, warnings = character(), error = conditionMessage(e))
  )
}

collapse_text <- function(x) {
  x <- unique(trim_or_na(x))
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return(NA_character_)
  }
  paste(x, collapse = " | ")
}

target_key <- function(df) {
  if (nrow(df) == 0L) {
    return(character())
  }

  cols <- c(
    "dataset_id",
    "table_id",
    "column_name",
    "code_value",
    "dictionary_role",
    "target_scope",
    "target_sdp_file",
    "target_sdp_field",
    "search_query"
  )
  cols <- intersect(cols, names(df))
  if (length(cols) == 0L) {
    return(rep(NA_character_, nrow(df)))
  }

  apply(
    df[cols],
    1,
    function(row) {
      row[is.na(row)] <- "<NA>"
      paste(row, collapse = "||")
    }
  )
}

count_review_markers <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0L) {
    return(0L)
  }
  iri_cols <- grep("_iri$", names(df), value = TRUE)
  if (length(iri_cols) == 0L) {
    return(0L)
  }

  total <- 0L
  for (col in iri_cols) {
    values <- as.character(df[[col]])
    total <- total + sum(grepl("^\\s*REVIEW\\s*:", values), na.rm = TRUE)
  }
  total
}

validate_package <- function(validate_fun, pkg_path, require_iris) {
  result <- safe_capture(validate_fun(pkg_path, require_iris = require_iris))
  list(
    ok = is.null(result$error),
    warnings = result$warnings %||% character(),
    error = result$error %||% NA_character_
  )
}

strip_review_prefix <- function(x) {
  x <- as.character(x)
  needs_strip <- !is.na(x) & grepl("^\\s*REVIEW\\s*:\\s*", x, ignore.case = TRUE)
  x[needs_strip] <- sub("^\\s*REVIEW\\s*:\\s*", "", x[needs_strip], ignore.case = TRUE)
  x
}

is_review_placeholder_text <- function(x) {
  !is.na(x) & grepl("^\\s*(MISSING METADATA|MISSING DESCRIPTION|REVIEW REQUIRED)\\s*:", x, ignore.case = TRUE)
}

coalesce_review_text <- function(x, fallback) {
  out <- as.character(x)
  fallback <- as.character(fallback)
  needs_fill <- is.na(out) | !nzchar(trimws(out)) | is_review_placeholder_text(out)
  out[needs_fill] <- fallback[needs_fill]
  out
}

prepare_reviewed_package_for_edh <- function(pkg_path, spec) {
  dataset_path <- file.path(pkg_path, "metadata", "dataset.csv")
  tables_path <- file.path(pkg_path, "metadata", "tables.csv")
  dict_path <- file.path(pkg_path, "metadata", "column_dictionary.csv")
  codes_path <- file.path(pkg_path, "metadata", "codes.csv")

  dataset_meta <- readr::read_csv(dataset_path, show_col_types = FALSE)
  seed_dataset <- spec$seed_dataset_meta
  dataset_fields <- intersect(
    c("title", "description", "creator", "contact_name", "contact_email", "license"),
    names(dataset_meta)
  )
  for (field in dataset_fields) {
    dataset_meta[[field]] <- coalesce_review_text(dataset_meta[[field]], seed_dataset[[field]])
  }
  dataset_iri_cols <- grep("_iri$", names(dataset_meta), value = TRUE)
  for (field in dataset_iri_cols) {
    dataset_meta[[field]] <- strip_review_prefix(dataset_meta[[field]])
  }
  readr::write_csv(dataset_meta, dataset_path, na = "")

  tables_meta <- readr::read_csv(tables_path, show_col_types = FALSE)
  seed_tables <- spec$seed_table_meta
  tables_meta <- dplyr::left_join(
    tables_meta,
    seed_tables[, intersect(c("table_id", "table_label", "description", "observation_unit"), names(seed_tables)), drop = FALSE],
    by = "table_id",
    suffix = c("", "_seed")
  )
  for (field in intersect(c("table_label", "description", "observation_unit"), names(spec$seed_table_meta))) {
    seed_field <- paste0(field, "_seed")
    if (seed_field %in% names(tables_meta)) {
      tables_meta[[field]] <- coalesce_review_text(tables_meta[[field]], tables_meta[[seed_field]])
      tables_meta[[seed_field]] <- NULL
    }
  }
  table_iri_cols <- grep("_iri$", names(tables_meta), value = TRUE)
  for (field in table_iri_cols) {
    tables_meta[[field]] <- strip_review_prefix(tables_meta[[field]])
  }
  readr::write_csv(tables_meta, tables_path, na = "")

  dict <- readr::read_csv(dict_path, show_col_types = FALSE)
  dict_iri_cols <- grep("_iri$", names(dict), value = TRUE)
  for (field in dict_iri_cols) {
    dict[[field]] <- strip_review_prefix(dict[[field]])
  }
  readr::write_csv(dict, dict_path, na = "")

  if (file.exists(codes_path) && isTRUE(file.info(codes_path)$size > 0)) {
    codes <- readr::read_csv(codes_path, show_col_types = FALSE)
    code_iri_cols <- grep("_iri$", names(codes), value = TRUE)
    for (field in code_iri_cols) {
      codes[[field]] <- strip_review_prefix(codes[[field]])
    }
    readr::write_csv(codes, codes_path, na = "")
  }

  invisible(pkg_path)
}

collect_focus_inputs <- function(spec, artifacts) {
  focus_targets <- spec$focus_targets
  dict <- artifacts$dict
  if (!all(c("table_id", "column_name") %in% names(focus_targets))) {
    abort("Case %s focus_targets must include table_id and column_name.", spec$case_id)
  }

  focus_keys <- paste(focus_targets$table_id, focus_targets$column_name, sep = "||")
  dict_keys <- paste(dict$table_id, dict$column_name, sep = "||")
  focus_dict <- dict[dict_keys %in% focus_keys, , drop = FALSE]
  if (nrow(focus_dict) == 0L) {
    abort("Case %s did not match any focus dictionary rows.", spec$case_id)
  }

  focus_codes <- artifacts$codes
  if (is.data.frame(focus_codes) && nrow(focus_codes) > 0L && "column_name" %in% names(focus_codes)) {
    code_keep <- focus_codes$column_name %in% focus_dict$column_name
    if ("table_id" %in% names(focus_codes)) {
      code_keep <- code_keep & focus_codes$table_id %in% focus_dict$table_id
    }
    focus_codes <- focus_codes[code_keep, , drop = FALSE]
  } else {
    focus_codes <- tibble::tibble()
  }

  focus_table_ids <- spec$table_target_ids %||% unique(focus_dict$table_id)
  focus_table_meta <- artifacts$table_meta
  if (is.data.frame(focus_table_meta) && nrow(focus_table_meta) > 0L) {
    focus_table_meta <- focus_table_meta[focus_table_meta$table_id %in% focus_table_ids, , drop = FALSE]
  } else {
    focus_table_meta <- tibble::tibble()
  }

  list(
    dict = focus_dict,
    codes = focus_codes,
    table_meta = focus_table_meta,
    dataset_meta = artifacts$dataset_meta
  )
}

summarise_sdp_case <- function(spec, cfg, ms_fns) {
  pkg_dir <- file.path(cfg$output_dir, "packages", spec$case_id)
  dir.create(dirname(pkg_dir), recursive = TRUE, showWarnings = FALSE)
  base_artifacts <- infer_case_artifacts(ms_fns, spec, cfg)
  context_files <- prepare_case_context_files(spec, base_artifacts, cfg$output_dir)

  started <- proc.time()[["elapsed"]]
  run <- safe_capture(
    ms_fns$create_sdp(
      resources = spec$resources,
      path = pkg_dir,
      dataset_id = spec$dataset_id,
      table_id = spec$table_id,
      seed_semantics = TRUE,
      semantic_sources = cfg$semantic_sources,
      semantic_max_per_role = cfg$semantic_max_per_role,
      seed_verbose = TRUE,
      seed_table_meta = spec$seed_table_meta,
      seed_dataset_meta = spec$seed_dataset_meta,
      semantic_code_scope = cfg$semantic_code_scope,
      llm_assess = TRUE,
      llm_provider = cfg$llm_provider,
      llm_model = cfg$llm_model,
      llm_api_key = cfg$llm_api_key,
      llm_base_url = cfg$llm_base_url,
      llm_top_n = cfg$llm_top_n,
      llm_context_files = context_files,
      llm_context_text = spec$context_text,
      llm_timeout_seconds = cfg$llm_timeout_seconds,
      check_updates = FALSE,
      include_edh_xml = TRUE,
      overwrite = TRUE
    )
  )
  runtime_seconds <- proc.time()[["elapsed"]] - started

  if (!is.null(run$error)) {
    return(tibble::tibble(
      case_id = spec$case_id,
      case_label = spec$label,
      loop = "create_sdp",
      runtime_seconds = runtime_seconds,
      package_path = pkg_dir,
      status = "error",
      error = run$error,
      warnings = collapse_text(run$warnings),
      context_bundle_dir = file.path(cfg$output_dir, "context", spec$case_id),
      context_file_count = length(context_files),
      context_formats = context_format_summary(context_files),
      n_tables = NA_integer_,
      n_dict_rows = NA_integer_,
      n_code_rows = NA_integer_,
      n_suggestion_rows = NA_integer_,
      n_target_groups = NA_integer_,
      n_llm_selected_targets = NA_integer_,
      n_review_decisions = NA_integer_,
      n_review_markers_dictionary = NA_integer_,
      n_review_markers_tables = NA_integer_,
      n_review_markers_codes = NA_integer_,
      initial_edh_xml_exists = NA,
      edh_rebuild_pre_review_ok = NA,
      edh_rebuild_pre_review_error = NA_character_,
      edh_rebuild_post_review_ok = NA,
      edh_rebuild_post_review_error = NA_character_,
      validation_review_ready_ok = NA,
      validation_review_ready_error = NA_character_,
      validation_strict_ok = NA,
      validation_strict_error = NA_character_
    ))
  }

  pkg <- ms_fns$read_salmon_datapackage(pkg_dir)
  suggestion_path <- file.path(pkg_dir, "semantic_suggestions.csv")
  suggestions <- if (file.exists(suggestion_path)) {
    readr::read_csv(suggestion_path, show_col_types = FALSE)
  } else {
    tibble::tibble()
  }

  distinct_targets <- unique(target_key(suggestions))
  selected_targets <- character()
  if (nrow(suggestions) > 0L && "llm_selected" %in% names(suggestions)) {
    selected_rows <- !is.na(suggestions$llm_selected) & suggestions$llm_selected
    selected_targets <- unique(target_key(suggestions[selected_rows, , drop = FALSE]))
  }

  decision_df <- tibble::tibble()
  if (nrow(suggestions) > 0L && "llm_decision" %in% names(suggestions)) {
    decision_df <- suggestions[, intersect(c(
      "dataset_id",
      "table_id",
      "column_name",
      "code_value",
      "dictionary_role",
      "target_scope",
      "target_sdp_file",
      "target_sdp_field",
      "search_query",
      "llm_decision"
    ), names(suggestions)), drop = FALSE]
    decision_df$.target_key <- target_key(decision_df)
    decision_df <- decision_df[!duplicated(decision_df$.target_key), , drop = FALSE]
  }

  review_validation <- validate_package(ms_fns$validate_salmon_datapackage, pkg_dir, require_iris = FALSE)
  strict_validation <- validate_package(ms_fns$validate_salmon_datapackage, pkg_dir, require_iris = TRUE)
  initial_edh_xml_path <- file.path(pkg_dir, "metadata", "metadata-edh-hnap.xml")
  rebuilt_edh_xml_path <- file.path(pkg_dir, "metadata", "metadata-edh-hnap-reviewed.xml")

  pre_review_rebuild <- safe_capture(
    ms_fns$write_edh_xml_from_sdp(
      pkg_dir,
      output_path = rebuilt_edh_xml_path,
      overwrite = TRUE
    )
  )

  prepare_reviewed_package_for_edh(pkg_dir, spec)

  post_review_rebuild <- safe_capture(
    ms_fns$write_edh_xml_from_sdp(
      pkg_dir,
      output_path = rebuilt_edh_xml_path,
      overwrite = TRUE
    )
  )

  tibble::tibble(
    case_id = spec$case_id,
    case_label = spec$label,
    loop = "create_sdp",
    runtime_seconds = runtime_seconds,
    package_path = pkg_dir,
    status = "ok",
    error = NA_character_,
    warnings = collapse_text(run$warnings),
    context_bundle_dir = file.path(cfg$output_dir, "context", spec$case_id),
    context_file_count = length(context_files),
    context_formats = context_format_summary(context_files),
    n_tables = length(pkg$resources),
    n_dict_rows = nrow(pkg$dictionary),
    n_code_rows = nrow(pkg$codes),
    n_suggestion_rows = nrow(suggestions),
    n_target_groups = length(distinct_targets),
    n_llm_selected_targets = length(selected_targets),
    n_review_decisions = if (nrow(decision_df) > 0L) sum(decision_df$llm_decision == "review", na.rm = TRUE) else 0L,
    n_review_markers_dictionary = count_review_markers(pkg$dictionary),
    n_review_markers_tables = count_review_markers(pkg$tables),
    n_review_markers_codes = count_review_markers(pkg$codes),
    initial_edh_xml_exists = file.exists(initial_edh_xml_path),
    edh_rebuild_pre_review_ok = is.null(pre_review_rebuild$error),
    edh_rebuild_pre_review_error = pre_review_rebuild$error %||% NA_character_,
    edh_rebuild_post_review_ok = is.null(post_review_rebuild$error) && file.exists(rebuilt_edh_xml_path),
    edh_rebuild_post_review_error = post_review_rebuild$error %||% NA_character_,
    validation_review_ready_ok = review_validation$ok,
    validation_review_ready_error = review_validation$error,
    validation_strict_ok = strict_validation$ok,
    validation_strict_error = strict_validation$error
  )
}

run_focused_case <- function(spec, cfg, ms_fns) {
  base_artifacts <- infer_case_artifacts(ms_fns, spec, cfg)
  context_files <- prepare_case_context_files(spec, base_artifacts, cfg$output_dir)

  focus <- collect_focus_inputs(spec, base_artifacts)
  assessment_rows <- list()
  suggestion_rows <- list()

  for (repeat_id in seq_len(cfg$direct_repeats)) {
    started <- proc.time()[["elapsed"]]
    run <- safe_capture(
      ms_fns$suggest_semantics(
        df = base_artifacts$resources,
        dict = focus$dict,
        sources = cfg$semantic_sources,
        max_per_role = cfg$semantic_max_per_role,
        codes = focus$codes,
        table_meta = focus$table_meta,
        dataset_meta = focus$dataset_meta,
        llm_assess = TRUE,
        llm_provider = cfg$llm_provider,
        llm_model = cfg$llm_model,
        llm_api_key = cfg$llm_api_key,
        llm_base_url = cfg$llm_base_url,
        llm_top_n = cfg$llm_top_n,
        llm_context_files = context_files,
        llm_context_text = spec$context_text,
        llm_timeout_seconds = cfg$llm_timeout_seconds
      )
    )
    runtime_seconds <- proc.time()[["elapsed"]] - started

    if (!is.null(run$error)) {
      assessment_rows[[length(assessment_rows) + 1L]] <- tibble::tibble(
        case_id = spec$case_id,
        case_label = spec$label,
        repeat_id = repeat_id,
        runtime_seconds = runtime_seconds,
        dataset_id = spec$dataset_id,
        table_id = NA_character_,
        column_name = NA_character_,
        code_value = NA_character_,
        dictionary_role = NA_character_,
        target_scope = NA_character_,
        target_sdp_file = NA_character_,
        target_sdp_field = NA_character_,
        search_query = NA_character_,
        llm_provider = cfg$llm_provider,
        llm_model = cfg$llm_model,
        llm_decision = NA_character_,
        llm_confidence = NA_real_,
        llm_selected_candidate_index = NA_integer_,
        llm_selected_iri = NA_character_,
        llm_selected_label = NA_character_,
        llm_rationale = NA_character_,
        llm_missing_context = NA_character_,
        llm_context_sources = collapse_text(context_files),
        llm_exploration_used = FALSE,
        llm_exploration_queries = NA_character_,
        llm_exploration_candidate_gain = NA_integer_,
        llm_error = run$error,
        warnings = collapse_text(run$warnings)
      )
      next
    }

    out <- run$value
    suggestions <- attr(out, "semantic_suggestions", exact = TRUE)
    assessments <- attr(out, "semantic_llm_assessments", exact = TRUE)

    if (is.null(suggestions)) {
      suggestions <- tibble::tibble()
    }
    if (is.null(assessments)) {
      assessments <- tibble::tibble()
    }

    if (nrow(suggestions) > 0L) {
      suggestions$case_id <- spec$case_id
      suggestions$case_label <- spec$label
      suggestions$repeat_id <- repeat_id
      suggestions$runtime_seconds <- runtime_seconds
      suggestion_rows[[length(suggestion_rows) + 1L]] <- suggestions
    }

    if (nrow(assessments) == 0L) {
      assessments <- tibble::tibble(
        dataset_id = spec$dataset_id,
        table_id = NA_character_,
        column_name = NA_character_,
        code_value = NA_character_,
        dictionary_role = NA_character_,
        target_scope = NA_character_,
        target_sdp_file = NA_character_,
        target_sdp_field = NA_character_,
        search_query = NA_character_,
        llm_provider = cfg$llm_provider,
        llm_model = cfg$llm_model,
        llm_decision = NA_character_,
        llm_confidence = NA_real_,
        llm_selected_candidate_index = NA_integer_,
        llm_selected_iri = NA_character_,
        llm_selected_label = NA_character_,
        llm_rationale = NA_character_,
        llm_missing_context = NA_character_,
        llm_context_sources = collapse_text(context_files),
        llm_exploration_used = FALSE,
        llm_exploration_queries = NA_character_,
        llm_exploration_candidate_gain = NA_integer_,
        llm_error = "No LLM assessments were returned."
      )
    }

    assessments$case_id <- spec$case_id
    assessments$case_label <- spec$label
    assessments$repeat_id <- repeat_id
    assessments$runtime_seconds <- runtime_seconds
    assessments$warnings <- collapse_text(run$warnings)
    assessment_rows[[length(assessment_rows) + 1L]] <- assessments
  }

  list(
    assessments = dplyr::bind_rows(assessment_rows),
    suggestions = dplyr::bind_rows(suggestion_rows)
  )
}

replace_na_chr <- function(x, replacement = "<NA>") {
  x <- as.character(x)
  x[is.na(x)] <- replacement
  x
}

summarise_focused_loop <- function(assessments) {
  if (nrow(assessments) == 0L) {
    return(tibble::tibble())
  }

  assessments$.target_key <- target_key(assessments)

  stability <- assessments %>%
    group_by(case_id, .data$.target_key) %>%
    summarise(
      repeats_seen = n(),
      decision_stable = dplyr::n_distinct(replace_na_chr(.data$llm_decision)) == 1L,
      selected_iri_stable = dplyr::n_distinct(replace_na_chr(.data$llm_selected_iri)) == 1L,
      .groups = "drop"
    )

  case_stability <- stability %>%
    group_by(case_id) %>%
    summarise(
      target_count = n(),
      decision_stability_rate = mean(.data$decision_stable, na.rm = TRUE),
      selected_iri_stability_rate = mean(.data$selected_iri_stable, na.rm = TRUE),
      unstable_target_count = sum(!.data$decision_stable | !.data$selected_iri_stable, na.rm = TRUE),
      .groups = "drop"
    )

  case_summary <- assessments %>%
    group_by(case_id, case_label) %>%
    summarise(
      repeats = n_distinct(.data$repeat_id),
      mean_runtime_seconds = mean(.data$runtime_seconds, na.rm = TRUE),
      assessed_targets = n(),
      selected_rate = mean(!is.na(.data$llm_selected_iri), na.rm = TRUE),
      review_rate = mean(.data$llm_decision == "review", na.rm = TRUE),
      exploration_rate = mean(isTRUE(.data$llm_exploration_used), na.rm = TRUE),
      error_rate = mean(!is.na(.data$llm_error) & nzchar(.data$llm_error), na.rm = TRUE),
      missing_context_rate = mean(!is.na(.data$llm_missing_context) & nzchar(trimws(.data$llm_missing_context)), na.rm = TRUE),
      mean_confidence = mean(.data$llm_confidence, na.rm = TRUE),
      low_confidence_count = sum(!is.na(.data$llm_confidence) & .data$llm_confidence < 0.6, na.rm = TRUE),
      .groups = "drop"
    )

  dplyr::left_join(case_summary, case_stability, by = "case_id")
}

build_flagged_targets <- function(assessments) {
  if (nrow(assessments) == 0L) {
    return(tibble::tibble())
  }

  assessments$.target_key <- target_key(assessments)

  assessments %>%
    group_by(case_id, case_label, .data$.target_key, dataset_id, table_id, column_name, code_value, dictionary_role, target_scope, target_sdp_field, search_query) %>%
    summarise(
      repeats_seen = n(),
      review_hits = sum(.data$llm_decision == "review", na.rm = TRUE),
      exploration_hits = sum(isTRUE(.data$llm_exploration_used), na.rm = TRUE),
      error_hits = sum(!is.na(.data$llm_error) & nzchar(.data$llm_error), na.rm = TRUE),
      min_confidence = suppressWarnings(min(.data$llm_confidence, na.rm = TRUE)),
      max_confidence = suppressWarnings(max(.data$llm_confidence, na.rm = TRUE)),
      selected_iri_values = collapse_text(.data$llm_selected_iri),
      decision_values = collapse_text(.data$llm_decision),
      missing_context = collapse_text(.data$llm_missing_context),
      warnings = collapse_text(.data$warnings),
      .groups = "drop"
    ) %>%
    mutate(
      min_confidence = ifelse(is.infinite(.data$min_confidence), NA_real_, .data$min_confidence),
      max_confidence = ifelse(is.infinite(.data$max_confidence), NA_real_, .data$max_confidence),
      needs_review = .data$review_hits > 0L |
        .data$exploration_hits > 0L |
        .data$error_hits > 0L |
        (!is.na(.data$min_confidence) & .data$min_confidence < 0.6) |
        grepl("|", dplyr::coalesce(.data$selected_iri_values, ""), fixed = TRUE)
    ) %>%
    filter(.data$needs_review)
}

build_adjudication_template <- function(assessments, suggestions) {
  if (nrow(assessments) == 0L) {
    return(tibble::tibble())
  }

  first_repeat <- min(assessments$repeat_id, na.rm = TRUE)
  assessment_slice <- assessments[assessments$repeat_id == first_repeat, , drop = FALSE]
  assessment_slice$.target_key <- target_key(assessment_slice)

  shortlist <- tibble::tibble(.target_key = character(), shortlist = character())
  if (nrow(suggestions) > 0L) {
    suggestion_slice <- suggestions[suggestions$repeat_id == first_repeat, , drop = FALSE]
    suggestion_slice$.target_key <- target_key(suggestion_slice)
    if ("llm_candidate_rank" %in% names(suggestion_slice)) {
      suggestion_slice <- suggestion_slice[order(suggestion_slice$.target_key, suggestion_slice$llm_candidate_rank), , drop = FALSE]
    }
    shortlist <- suggestion_slice %>%
      filter(!is.na(.data$llm_candidate_rank)) %>%
      group_by(.data$.target_key) %>%
      summarise(
        shortlist = paste0(
          "r", .data$llm_candidate_rank, ": ",
          .data$label, " <", .data$iri, "> [", .data$source, "]",
          collapse = " || "
        ),
        .groups = "drop"
      )
  }

  assessment_slice %>%
    left_join(shortlist, by = ".target_key") %>%
    transmute(
      case_id,
      case_label,
      repeat_id,
      dataset_id,
      table_id,
      column_name,
      code_value,
      dictionary_role,
      target_scope,
      target_sdp_field,
      search_query,
      llm_decision,
      llm_confidence,
      llm_selected_iri,
      llm_selected_label,
      llm_rationale,
      llm_missing_context,
      llm_exploration_used,
      llm_exploration_queries,
      llm_error,
      shortlist = dplyr::coalesce(.data$shortlist, NA_character_),
      human_outcome = NA_character_,
      improvement_bucket = NA_character_,
      review_notes = NA_character_
    )
}

write_csv_if_any <- function(x, path) {
  if (!is.data.frame(x) || nrow(x) == 0L) {
    readr::write_csv(tibble::tibble(), path, na = "")
    return(invisible(path))
  }
  readr::write_csv(x, path, na = "")
  invisible(path)
}

main <- function() {
  cfg <- parse_args(commandArgs(trailingOnly = TRUE), output_dir_default = file.path(repo_root, "artifacts", "llm-sanity"))
  if (isTRUE(cfg$help)) {
    cat(usage_text(), sep = "\n")
    return(invisible(NULL))
  }

  load_metasalmon_dev(repo_root)

  if (identical(cfg$llm_provider, "chapi")) {
    api_key <- trim_or_na(cfg$llm_api_key %||% Sys.getenv("CHAPI_API_KEY", unset = ""))
    if (is.na(api_key)) {
      abort("CHAPI requires an API key. Set CHAPI_API_KEY or pass --api-key=...")
    }
  }

  case_registry <- build_case_registry()
  missing_cases <- setdiff(cfg$datasets, names(case_registry))
  if (length(missing_cases) > 0L) {
    abort("Unknown dataset case(s): %s", paste(missing_cases, collapse = ", "))
  }
  cases <- case_registry[cfg$datasets]

  dir.create(cfg$output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(cfg$output_dir, "packages"), recursive = TRUE, showWarnings = FALSE)

  ms_fns <- list(
    create_sdp = exported("create_sdp"),
    infer_salmon_datapackage_artifacts = exported("infer_salmon_datapackage_artifacts"),
    suggest_semantics = exported("suggest_semantics"),
    read_salmon_datapackage = exported("read_salmon_datapackage"),
    validate_salmon_datapackage = exported("validate_salmon_datapackage"),
    write_edh_xml_from_sdp = exported("write_edh_xml_from_sdp")
  )

  sdp_rows <- list()
  focused_results <- list()

  for (spec in cases) {
    message(sprintf("[metasalmon] Case %s: %s", spec$case_id, spec$label))

    if (isTRUE(cfg$run_sdp_loop)) {
      message("  - running create_sdp() smoke loop")
      sdp_rows[[length(sdp_rows) + 1L]] <- summarise_sdp_case(spec, cfg, ms_fns)
    }

    if (isTRUE(cfg$run_focused_loop)) {
      message("  - running focused suggest_semantics() repeat loop")
      focused_results[[length(focused_results) + 1L]] <- run_focused_case(spec, cfg, ms_fns)
    }
  }

  sdp_summary <- dplyr::bind_rows(sdp_rows)
  focused_assessments <- dplyr::bind_rows(lapply(focused_results, `[[`, "assessments"))
  focused_suggestions <- dplyr::bind_rows(lapply(focused_results, `[[`, "suggestions"))
  focused_summary <- summarise_focused_loop(focused_assessments)
  focused_flagged <- build_flagged_targets(focused_assessments)
  adjudication_template <- build_adjudication_template(focused_assessments, focused_suggestions)

  write_csv_if_any(sdp_summary, file.path(cfg$output_dir, "sdp-loop-summary.csv"))
  write_csv_if_any(focused_summary, file.path(cfg$output_dir, "focused-loop-summary.csv"))
  write_csv_if_any(focused_assessments, file.path(cfg$output_dir, "focused-loop-assessments.csv"))
  write_csv_if_any(focused_suggestions, file.path(cfg$output_dir, "focused-loop-suggestions.csv"))
  write_csv_if_any(focused_flagged, file.path(cfg$output_dir, "focused-loop-flagged-targets.csv"))
  write_csv_if_any(adjudication_template, file.path(cfg$output_dir, "focused-loop-adjudication-template.csv"))

  message(sprintf("[metasalmon] Outputs written to %s", normalizePath(cfg$output_dir, winslash = "/", mustWork = FALSE)))
  invisible(normalizePath(cfg$output_dir, winslash = "/", mustWork = FALSE))
}

if (sys.nframe() == 0L) {
  main()
}
