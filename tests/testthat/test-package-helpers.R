test_that("write_salmon_datapackage creates valid package", {
  # Create test data
  resources <- list(
    main_table = tibble::tibble(
      species = c("Coho", "Chinook"),
      count = c(100L, 200L)
    )
  )

  dataset_meta <- tibble::tibble(
    dataset_id = "test-1",
    title = "Test Dataset",
    description = "A test dataset for validation",
    creator = "Test Author",
    contact_name = NA_character_,
    contact_email = NA_character_,
    license = "MIT",
    temporal_start = NA_character_,
    temporal_end = NA_character_,
    spatial_extent = NA_character_,
    dataset_type = NA_character_,
    source_citation = NA_character_
  )

  table_meta <- tibble::tibble(
    dataset_id = "test-1",
    table_id = "main_table",
    file_name = "data/main_table.csv",
    table_label = "Main Table",
    description = "Main data table",
    observation_unit = NA_character_,
    observation_unit_iri = NA_character_,
    primary_key = NA_character_
  )

  dict <- infer_dictionary(
    resources$main_table,
    dataset_id = "test-1",
    table_id = "main_table"
  )
  dict <- fill_measurement_components(dict)
  validate_dictionary(dict)

  # Create package in temp directory
  temp_dir <- withr::local_tempdir()
  pkg_path <- write_salmon_datapackage(
    resources,
    dataset_meta,
    table_meta,
    dict,
    path = temp_dir,
    format = "csv",
    overwrite = TRUE
  )

  expect_true(dir.exists(pkg_path))
  expect_true(file.exists(file.path(pkg_path, "metadata", "dataset.csv")))
  expect_true(file.exists(file.path(pkg_path, "metadata", "tables.csv")))
  expect_true(file.exists(file.path(pkg_path, "metadata", "column_dictionary.csv")))
  expect_true(file.exists(file.path(pkg_path, "datapackage.json")))
  expect_true(file.exists(file.path(pkg_path, "data", "main_table.csv")))
  expect_false(file.exists(file.path(pkg_path, "dataset.csv")))
  expect_false(file.exists(file.path(pkg_path, "tables.csv")))
  expect_false(file.exists(file.path(pkg_path, "column_dictionary.csv")))
})

test_that("create_sdp creates valid package", {
  resources <- list(
    catches = tibble::tibble(
      station_id = c("A", "B"),
      species = c("Coho", "Chinook"),
      count = c(10L, 20L),
      sample_date = as.Date(c("2024-01-01", "2024-01-02"))
    ),
    stations = tibble::tibble(
      station_id = c("A", "B"),
      lat = c(50.12, 50.34),
      lon = c(-125.5, -125.6),
      habitat = c("estu", "river")
    )
  )

  temp_dir <- withr::local_tempdir()
  pkg_path <- create_sdp(
    resources,
    path = file.path(temp_dir, "package"),
    dataset_id = "mt-demo",
    seed_semantics = FALSE,
    overwrite = TRUE
  )

  expect_true(dir.exists(pkg_path))
  expect_true(dir.exists(file.path(pkg_path, "metadata")))
  expect_true(dir.exists(file.path(pkg_path, "data")))
  expect_true(file.exists(file.path(pkg_path, "metadata", "dataset.csv")))
  expect_true(file.exists(file.path(pkg_path, "metadata", "tables.csv")))
  expect_true(file.exists(file.path(pkg_path, "metadata", "column_dictionary.csv")))
  expect_true(file.exists(file.path(pkg_path, "metadata", "codes.csv")))
  expect_true(file.exists(file.path(pkg_path, "datapackage.json")))
  expect_false(file.exists(file.path(pkg_path, "metadata", "metadata-edh-hnap.xml")))

  dataset <- readr::read_csv(file.path(pkg_path, "metadata", "dataset.csv"), show_col_types = FALSE)
  tables <- readr::read_csv(file.path(pkg_path, "metadata", "tables.csv"), show_col_types = FALSE)

  expect_equal(dataset$dataset_id[[1]], "mt-demo")
  expect_setequal(tables$table_id, c("catches", "stations"))
  expect_true(all(startsWith(tables$file_name, "data/")))

  seed_dataset_meta <- tibble::tibble(
    dataset_id = "mt-demo2",
    title = "MT Demo 2",
    description = "Two table demo with explicit metadata",
    creator = "Test",
    contact_name = NA_character_,
    contact_email = NA_character_,
    license = "MIT",
    temporal_start = NA_character_,
    temporal_end = NA_character_,
    spatial_extent = NA_character_
  )

  pkg_path_with_edh <- create_sdp(
    resources,
    path = file.path(temp_dir, "package-with-edh"),
    dataset_id = "mt-demo2",
    seed_semantics = FALSE,
    seed_dataset_meta = seed_dataset_meta,
    include_edh_xml = TRUE,
    overwrite = TRUE
  )

  expect_true(file.exists(file.path(pkg_path_with_edh, "metadata", "metadata-edh-hnap.xml")))
})

test_that("create_sdp auto-enables EDH XML export when legacy edh_profile is supplied", {
  temp_dir <- withr::local_tempdir()
  resources <- list(main = tibble::tibble(species = c("Coho"), count = c(1L)))
  seed_dataset_meta <- tibble::tibble(
    dataset_id = "edh-auto",
    title = "EDH Auto",
    description = "EDH auto-enable test",
    creator = "Test",
    contact_name = NA_character_,
    contact_email = NA_character_,
    license = "MIT",
    temporal_start = NA_character_,
    temporal_end = NA_character_,
    spatial_extent = NA_character_
  )

  expect_warning(
    expect_message(
      pkg_path <- create_sdp(
        resources,
        path = file.path(temp_dir, "package-auto-edh"),
        dataset_id = "edh-auto",
        seed_semantics = FALSE,
        check_updates = FALSE,
        seed_dataset_meta = seed_dataset_meta,
        edh_profile = "dfo_edh_hnap",
        overwrite = TRUE
      ),
      "include_edh_xml = TRUE",
      fixed = TRUE
    ),
    "deprecated"
  )

  expect_true(file.exists(file.path(pkg_path, "metadata", "metadata-edh-hnap.xml")))
})

test_that("create_sdp accepts deprecated EDH_Profile alias", {
  temp_dir <- withr::local_tempdir()
  resources <- list(main = tibble::tibble(species = c("Coho"), count = c(1L)))
  seed_dataset_meta <- tibble::tibble(
    dataset_id = "edh-alias",
    title = "EDH Alias",
    description = "EDH alias test",
    creator = "Test",
    contact_name = NA_character_,
    contact_email = NA_character_,
    license = "MIT",
    temporal_start = NA_character_,
    temporal_end = NA_character_,
    spatial_extent = NA_character_
  )

  expect_warning(
    expect_message(
      pkg_path <- create_sdp(
        resources,
        path = file.path(temp_dir, "package-alias-edh"),
        dataset_id = "edh-alias",
        seed_semantics = FALSE,
        check_updates = FALSE,
        seed_dataset_meta = seed_dataset_meta,
        EDH_Profile = "dfo_edh_hnap",
        overwrite = TRUE
      ),
      "include_edh_xml = TRUE",
      fixed = TRUE
    ),
    "deprecated"
  )

  expect_true(file.exists(file.path(pkg_path, "metadata", "metadata-edh-hnap.xml")))
})

test_that("normalized dictionary column order is preserved when writing package", {
  resources <- list(main = tibble::tibble(species = c("Coho"), count = c(1L)))
  dict <- infer_dictionary(resources$main, dataset_id = "ord-1", table_id = "main")

  dataset_meta <- tibble::tibble(
    dataset_id = "ord-1",
    title = "Order check",
    description = "Order check",
    creator = NA_character_,
    contact_name = NA_character_,
    contact_email = NA_character_,
    license = NA_character_
  )
  table_meta <- tibble::tibble(
    dataset_id = "ord-1",
    table_id = "main",
    file_name = "data/main.csv",
    table_label = "Main",
    description = NA_character_
  )

  pkg_path <- write_salmon_datapackage(
    resources = resources,
    dataset_meta = dataset_meta,
    table_meta = table_meta,
    dict = dict,
    path = withr::local_tempdir(),
    overwrite = TRUE
  )

  written <- readr::read_csv(file.path(pkg_path, "metadata", "column_dictionary.csv"), show_col_types = FALSE)
  expect_equal(
    names(written),
    c(
      "dataset_id", "table_id", "column_name", "column_label", "column_description",
      "term_iri", "property_iri", "entity_iri", "constraint_iri", "method_iri",
      "unit_label", "unit_iri", "term_type",
      "value_type", "column_role", "required"
    )
  )
})

test_that("create_sdp defaults path in getwd using dataset_id slug", {
  withr::local_tempdir() -> tmp
  withr::local_dir(tmp)

  df <- tibble::tibble(
    species = c("Coho", "Chinook"),
    count = c(10L, 20L)
  )

  pkg_path <- create_sdp(
    df,
    dataset_id = "Fraser Coho 2024",
    table_id = "escapement",
    seed_semantics = FALSE,
    overwrite = TRUE
  )

  expect_equal(normalizePath(pkg_path), normalizePath(file.path(getwd(), "fraser-coho-2024-sdp")))
  expect_true(file.exists(file.path(pkg_path, "README-review.txt")))
  expect_true(file.exists(file.path(pkg_path, "data", "escapement.csv")))
})

test_that("create_sdp requires overwrite=TRUE to write into an existing directory", {
  withr::local_tempdir() -> tmp
  output_dir <- file.path(tmp, "existing-package")
  dir.create(output_dir)
  # any existing file should still block and avoid running semantic inference
  readr::write_csv(tibble::tibble(a = 1), file.path(output_dir, "already.csv"))

  called <- 0L
  fake_suggest <- function(...) {
    called <<- called + 1L
    stop("should not run")
  }

  with_mocked_bindings(
    suggest_semantics = fake_suggest,
    {
      expect_error(
        create_sdp(
          tibble::tibble(col = 1:2),
          path = output_dir,
          dataset_id = "dup-test",
          table_id = "t1",
          overwrite = FALSE
        ),
        "already exists"
      )
    }
  )

  expect_equal(called, 0L)
})

test_that("create_sdp exposes seed_table_meta and seed_dataset_meta defaults as TRUE", {
  expect_true(identical(formals(create_sdp)$seed_table_meta, TRUE))
  expect_true(identical(formals(infer_salmon_datapackage_artifacts)$seed_table_meta, TRUE))
  expect_true(identical(formals(create_sdp)$seed_dataset_meta, TRUE))
  expect_true(identical(formals(infer_salmon_datapackage_artifacts)$seed_dataset_meta, TRUE))
})

test_that("create_sdp handles NuSEDS-style DD-MON-YY dates in built-in sample", {
  withr::local_tempdir() -> tmp

  sample_path <- system.file("extdata", "nuseds-fraser-coho-sample.csv", package = "metasalmon")
  fraser_coho <- readr::read_csv(sample_path, show_col_types = FALSE)

  pkg_path <- create_sdp(
    fraser_coho,
    path = file.path(tmp, "nuseds-sample"),
    dataset_id = "fraser-coho-2024",
    table_id = "escapement",
    seed_semantics = FALSE,
    overwrite = TRUE
  )

  dataset_written <- readr::read_csv(file.path(pkg_path, "metadata", "dataset.csv"), show_col_types = FALSE)
  expect_equal(as.character(dataset_written$temporal_start[[1]]), "1997-12-03")
  expect_equal(as.character(dataset_written$temporal_end[[1]]), "2024-11-20")
  expect_true(file.exists(file.path(pkg_path, "data", "escapement.csv")))
})

test_that("create_sdp writes review files and auto-applies compatible table suggestions", {
  resources <- list(
    catches = tibble::tibble(
      species = c("Coho", "Chinook"),
      count = c(10L, 20L)
    )
  )
  seed_table_meta <- tibble::tibble(
    dataset_id = "review-demo",
    table_id = "catches",
    file_name = "data/catches.csv",
    table_label = "Catches",
    description = "Catch records for survey events.",
    observation_unit = "Catch record",
    observation_unit_iri = NA_character_,
    primary_key = NA_character_
  )

  fake_suggest <- function(df, dict, sources = c("smn", "gcdfo", "ols", "nvs"),
                           include_dwc = FALSE, max_per_role = 3,
                           search_fn = find_terms, codes = NULL,
                           table_meta = NULL, dataset_meta = NULL) {
    dict$property_iri[dict$column_name == "count"] <- "https://example.org/property-existing"
    attr(dict, "semantic_suggestions") <- tibble::tibble(
      column_name = c("count", "count", "species", NA_character_),
      dictionary_role = c("variable", "property", "entity", "entity"),
      table_id = c("catches", "catches", "catches", "catches"),
      dataset_id = c("review-demo", "review-demo", "review-demo", "review-demo"),
      target_scope = c("column", "column", "code", "table"),
      target_sdp_file = c("column_dictionary.csv", "column_dictionary.csv", "codes.csv", "tables.csv"),
      target_sdp_field = c("term_iri", "property_iri", "term_iri", "observation_unit_iri"),
      target_row_key = c("review-demo/catches/count", "review-demo/catches/count", "review-demo/catches/species/POP1", "review-demo/catches"),
      target_query_basis = c(NA_character_, NA_character_, NA_character_, "observation_unit"),
      target_query_context = c(NA_character_, NA_character_, NA_character_, "Catch record Catches catches"),
      code_value = c(NA_character_, NA_character_, "POP1", NA_character_),
      code_label = c(NA_character_, NA_character_, "POP1", NA_character_),
      code_description = c(NA_character_, NA_character_, NA_character_, NA_character_),
      iri = c("https://example.org/term-top", "https://example.org/property-top", "https://example.org/pop1", "https://example.org/table-unit"),
      label = c("Count term", "Count property", "Population One", "Catch record"),
      source = c("smn", "smn", "smn", "smn"),
      ontology = c("demo", "demo", "demo", "demo"),
      role = c("variable", "property", "entity", "entity"),
      match_type = c("label_exact", "label_exact", "label_exact", "label_exact"),
      definition = c(NA_character_, NA_character_, NA_character_, NA_character_)
    )
    dict
  }

  pkg_path <- NULL
  expect_message(
    with_mocked_bindings(
      suggest_semantics = fake_suggest,
      {
        pkg_path <- create_sdp(
          resources,
          path = file.path(withr::local_tempdir(), "review-package"),
          dataset_id = "review-demo",
          seed_semantics = TRUE,
          seed_table_meta = seed_table_meta,
          overwrite = TRUE
        )
      }
    ),
    regexp = "Prefilled semantic values were written directly into the metadata CSVs"
  )

  expect_true(file.exists(file.path(pkg_path, "README-review.txt")))
  expect_true(file.exists(file.path(pkg_path, "semantic_suggestions.csv")))

  review_lines <- readLines(file.path(pkg_path, "README-review.txt"), warn = FALSE)
  expect_true(any(grepl("Salmon Data Package Review Checklist", review_lines, fixed = TRUE)))
  expect_true(any(grepl("Review the package in Excel", review_lines, fixed = TRUE)))
  expect_true(any(grepl("[ ] 1. Start in metadata/*.csv", review_lines, fixed = TRUE)))
  expect_true(any(grepl("metadata/column_dictionary.csv and metadata/tables.csv first", review_lines, fixed = TRUE)))
  expect_true(any(grepl("Use semantic_suggestions.csv only as a fallback shortlist", review_lines, fixed = TRUE)))
  expect_true(any(grepl("salmon-domain-ontology/issues/new/choose", review_lines, fixed = TRUE)))
  expect_true(any(grepl("dfo-salmon-ontology/issues/new/choose", review_lines, fixed = TRUE)))
  expect_true(any(grepl("Share the whole package folder", review_lines, fixed = TRUE)))
  expect_true(any(grepl("read_salmon_datapackage(pkg_path)", review_lines, fixed = TRUE)))
  if (file.exists(file.path(pkg_path, "metadata", "codes.csv"))) {
    expect_true(any(grepl("If metadata/codes.csv exists", review_lines, fixed = TRUE)))
  }
  expect_true(any(grepl("already lives there", review_lines, fixed = TRUE)))

  suggestions_written <- readr::read_csv(file.path(pkg_path, "semantic_suggestions.csv"), show_col_types = FALSE)
  expect_setequal(unique(suggestions_written$target_scope), c("column", "table"))

  dict_written <- readr::read_csv(file.path(pkg_path, "metadata", "column_dictionary.csv"), show_col_types = FALSE)
  count_row <- dict_written[dict_written$column_name == "count", , drop = FALSE]
  expect_equal(count_row$term_iri[[1]], paste0(metasalmon:::.ms_review_iri_prefix(), "https://example.org/term-top"))
  expect_equal(count_row$property_iri[[1]], "https://example.org/property-existing")
  expect_true(startsWith(count_row$column_description[[1]], "MISSING DESCRIPTION:"))

  tables_written <- readr::read_csv(file.path(pkg_path, "metadata", "tables.csv"), show_col_types = FALSE)
  expect_true(all(startsWith(tables_written$file_name, "data/")))
  expect_equal(tables_written$description[[1]], "Catch records for survey events.")
  expect_equal(tables_written$observation_unit_iri[[1]], paste0(metasalmon:::.ms_review_iri_prefix(), "https://example.org/table-unit"))
  expect_equal(tables_written$observation_unit[[1]], "Catch record")

  dataset_written <- readr::read_csv(file.path(pkg_path, "metadata", "dataset.csv"), show_col_types = FALSE)
  expect_true(startsWith(dataset_written$creator[[1]], "MISSING METADATA:"))
  expect_true(startsWith(dataset_written$contact_name[[1]], "MISSING METADATA:"))
  expect_true(startsWith(dataset_written$contact_email[[1]], "MISSING METADATA:"))
  expect_true(startsWith(dataset_written$license[[1]], "MISSING METADATA:"))
})

test_that("create_sdp auto-applies strong table-label observation-unit suggestions by default", {
  resources <- list(
    escapement = tibble::tibble(
      species = c("Coho", "Chinook"),
      count = c(10L, 20L)
    )
  )

  fake_suggest <- function(df, dict, sources = c("smn", "gcdfo", "ols", "nvs"),
                           include_dwc = FALSE, max_per_role = 3,
                           search_fn = find_terms, codes = NULL,
                           table_meta = NULL, dataset_meta = NULL) {
    attr(dict, "semantic_suggestions") <- tibble::tibble(
      column_name = NA_character_,
      dictionary_role = "entity",
      table_id = "escapement",
      dataset_id = "review-demo",
      target_scope = "table",
      target_sdp_file = "tables.csv",
      target_sdp_field = "observation_unit_iri",
      target_row_key = "review-demo/escapement",
      target_query_basis = "table_label",
      target_query_context = "Escapement escapement",
      code_value = NA_character_,
      code_label = NA_character_,
      code_description = NA_character_,
      iri = "https://example.org/observation-unit",
      label = "Escapement observation",
      source = "smn",
      ontology = "demo",
      role = "entity",
      match_type = "label_exact",
      definition = NA_character_,
      score = 2
    )
    dict
  }

  pkg_path <- NULL
  with_mocked_bindings(
    suggest_semantics = fake_suggest,
    {
      pkg_path <- create_sdp(
        resources,
        path = file.path(withr::local_tempdir(), "review-package-top-table-unit"),
        dataset_id = "review-demo",
        seed_semantics = TRUE,
        overwrite = TRUE
      )
    }
  )

  tables_written <- readr::read_csv(file.path(pkg_path, "metadata", "tables.csv"), show_col_types = FALSE)
  expect_equal(
    tables_written$observation_unit_iri[[1]],
    paste0(metasalmon:::.ms_review_iri_prefix(), "https://example.org/observation-unit")
  )
  expect_equal(tables_written$observation_unit[[1]], "Escapement observation")
})

test_that("create_sdp leaves weak or bogus table observation-unit suggestions as review only", {
  resources <- list(
    catches = tibble::tibble(
      species = c("Coho", "Chinook"),
      count = c(10L, 20L)
    )
  )

  fake_suggest <- function(df, dict, sources = c("smn", "gcdfo", "ols", "nvs"),
                           include_dwc = FALSE, max_per_role = 3,
                           search_fn = find_terms, codes = NULL,
                           table_meta = NULL, dataset_meta = NULL) {
    attr(dict, "semantic_suggestions") <- tibble::tibble(
      column_name = NA_character_,
      dictionary_role = "entity",
      table_id = "catches",
      dataset_id = "review-demo",
      target_scope = "table",
      target_sdp_file = "tables.csv",
      target_sdp_field = "observation_unit_iri",
      target_row_key = "review-demo/catches",
      target_query_basis = "table_label",
      target_query_context = "Catches catches",
      code_value = NA_character_,
      code_label = NA_character_,
      code_description = NA_character_,
      iri = "https://example.org/bad-table-unit",
      label = "Metadata note",
      source = "smn",
      ontology = "demo",
      role = "entity",
      match_type = "label_exact",
      definition = NA_character_,
      score = 2
    )
    dict
  }

  pkg_path <- NULL
  with_mocked_bindings(
    suggest_semantics = fake_suggest,
    {
      pkg_path <- create_sdp(
        resources,
        path = file.path(withr::local_tempdir(), "review-package-bad-table-unit"),
        dataset_id = "review-demo",
        seed_semantics = TRUE,
        overwrite = TRUE
      )
    }
  )

  tables_written <- readr::read_csv(file.path(pkg_path, "metadata", "tables.csv"), show_col_types = FALSE)
  expect_true(is.na(tables_written$observation_unit_iri[[1]]) || tables_written$observation_unit_iri[[1]] == "")
  expect_true(startsWith(tables_written$observation_unit[[1]], "MISSING METADATA:"))
})

test_that("create_sdp writes selected LLM table suggestions back to tables.csv as review drafts", {
  resources <- list(
    escapement = tibble::tibble(species = c("Coho", "Chinook"), count = c(10L, 20L)),
    age_composition = tibble::tibble(age = c("2", "3"), proportion = c(0.4, 0.6))
  )

  fake_suggest <- function(df, dict, ..., codes = NULL, table_meta = NULL, dataset_meta = NULL) {
    attr(dict, "semantic_suggestions") <- tibble::tibble(
      column_name = NA_character_,
      dictionary_role = "entity",
      table_id = "escapement",
      dataset_id = "review-demo",
      target_scope = "table",
      target_sdp_file = "tables.csv",
      target_sdp_field = "observation_unit_iri",
      target_row_key = "review-demo/escapement",
      target_query_basis = "table_label",
      target_query_context = "Escapement escapement",
      code_value = NA_character_,
      code_label = NA_character_,
      code_description = NA_character_,
      iri = "https://example.org/observation-unit",
      label = "Escapement observation",
      source = "smn",
      ontology = "demo",
      role = "entity",
      match_type = "label_exact",
      definition = NA_character_,
      llm_decision = "select",
      llm_confidence = 0.91,
      llm_selected = TRUE,
      llm_candidate_rank = 1L
    )
    dict
  }

  pkg_path <- NULL
  with_mocked_bindings(
    suggest_semantics = fake_suggest,
    {
      pkg_path <- create_sdp(
        resources,
        path = file.path(withr::local_tempdir(), "review-package-llm-table-unit"),
        dataset_id = "review-demo",
        seed_semantics = TRUE,
        llm_assess = TRUE,
        check_updates = FALSE,
        overwrite = TRUE
      )
    }
  )

  suggestions_written <- readr::read_csv(file.path(pkg_path, "semantic_suggestions.csv"), show_col_types = FALSE)
  expect_equal(suggestions_written$target_query_basis[[1]], "table_label")
  expect_true(isTRUE(suggestions_written$llm_selected[[1]]))

  tables_written <- readr::read_csv(file.path(pkg_path, "metadata", "tables.csv"), show_col_types = FALSE)
  escapement_row <- tables_written[tables_written$table_id == "escapement", , drop = FALSE]
  age_row <- tables_written[tables_written$table_id == "age_composition", , drop = FALSE]

  expect_equal(
    escapement_row$observation_unit_iri[[1]],
    paste0(metasalmon:::.ms_review_iri_prefix(), "https://example.org/observation-unit")
  )
  expect_equal(escapement_row$observation_unit[[1]], "Escapement observation")
  expect_true(is.na(age_row$observation_unit_iri[[1]]) || age_row$observation_unit_iri[[1]] == "")
})

test_that("create_sdp seed note explains slower semantic lookup", {
  note <- metasalmon:::.ms_create_sdp_seed_note(
    seed_semantics = TRUE,
    seed_verbose = TRUE,
    semantic_code_scope = "factor"
  )

  expect_match(note, "may take a few minutes")
  expect_match(note, "factor and low-cardinality character columns")
  expect_null(metasalmon:::.ms_create_sdp_seed_note(seed_semantics = FALSE))
  expect_null(metasalmon:::.ms_create_sdp_seed_note(seed_semantics = TRUE, seed_verbose = FALSE))
})

test_that("create_sdp default code-level semantic seeding includes low-cardinality character columns but skips free-text fields", {
  resources <- list(
    catches = tibble::tibble(
      run = factor(c("early", "late")),
      station = c("A", "B"),
      survey_comment = c("looks odd", "double check"),
      count = c(10L, 20L)
    )
  )

  seen_codes <- NULL
  fake_suggest <- function(df, dict, sources = c("smn", "gcdfo", "ols", "nvs"),
                           include_dwc = FALSE, max_per_role = 3,
                           search_fn = find_terms, codes = NULL,
                           table_meta = NULL, dataset_meta = NULL) {
    seen_codes <<- codes
    attr(dict, "semantic_suggestions") <- tibble::tibble()
    dict
  }

  with_mocked_bindings(
    suggest_semantics = fake_suggest,
    {
      create_sdp(
        resources,
        path = file.path(withr::local_tempdir(), "factor-code-scope"),
        dataset_id = "scope-demo",
        seed_semantics = TRUE,
        seed_verbose = FALSE,
        overwrite = TRUE
      )
    }
  )

  expect_s3_class(seen_codes, "tbl_df")
  expect_setequal(unique(seen_codes$column_name), c("run", "station"))
})

test_that("create_sdp filters bad non-measurement term IRIs before auto-apply", {
  fuller_path <- system.file("extdata", "nuseds-fraser-coho-2023-2024.csv", package = "metasalmon")
  fraser_coho_fuller <- readr::read_csv(fuller_path, show_col_types = FALSE)

  fake_suggest <- function(df, dict, sources = c("smn", "gcdfo", "ols", "nvs"),
                           include_dwc = FALSE, max_per_role = 3,
                           search_fn = find_terms, codes = NULL,
                           table_meta = NULL, dataset_meta = NULL) {
    attr(dict, "semantic_suggestions") <- tibble::tibble(
      dataset_id = c("fraser-coho-2023-2024", "fraser-coho-2023-2024", "fraser-coho-2023-2024", "fraser-coho-2023-2024"),
      table_id = c("escapement", "escapement", "escapement", "escapement"),
      column_name = c("AREA", "SPECIES", "RUN_TYPE", "WATERBODY"),
      dictionary_role = c("variable", "variable", "variable", "variable"),
      target_scope = c("column", "column", "column", "column"),
      target_sdp_file = c("column_dictionary.csv", "column_dictionary.csv", "column_dictionary.csv", "column_dictionary.csv"),
      target_sdp_field = c("term_iri", "term_iri", "term_iri", "term_iri"),
      search_query = c("Area", "Species", "Run type", "Waterbody"),
      column_label = c("AREA", "SPECIES", "RUN_TYPE", "WATERBODY"),
      label = c("In River Mortality Rate", "Ampharete lindstroemi", "Fish Length Measurement Type", "Waterbody"),
      iri = c(
        "https://example.org/in-river-mortality-rate",
        "https://example.org/ampharete-lindstroemi",
        "https://example.org/fish-length-measurement-type",
        "https://example.org/waterbody"
      ),
      match_type = c("label_exact", "label_exact", "label_exact", "label_exact"),
      score = c(0.95, 0.95, 0.95, 0.95)
    )
    dict
  }

  pkg_path <- with_mocked_bindings(
    suggest_semantics = fake_suggest,
    {
      create_sdp(
        fraser_coho_fuller,
        path = file.path(withr::local_tempdir(), "fraser-coho-fuller-seeded"),
        dataset_id = "fraser-coho-2023-2024",
        table_id = "escapement",
        seed_semantics = TRUE,
        seed_verbose = FALSE,
        check_updates = FALSE,
        overwrite = TRUE
      )
    }
  )

  dict_written <- readr::read_csv(file.path(pkg_path, "metadata", "column_dictionary.csv"), show_col_types = FALSE)
  expect_true(is.na(dict_written$term_iri[dict_written$column_name == "AREA"]) || dict_written$term_iri[dict_written$column_name == "AREA"] == "")
  expect_true(is.na(dict_written$term_iri[dict_written$column_name == "SPECIES"]) || dict_written$term_iri[dict_written$column_name == "SPECIES"] == "")
  expect_true(is.na(dict_written$term_iri[dict_written$column_name == "RUN_TYPE"]) || dict_written$term_iri[dict_written$column_name == "RUN_TYPE"] == "")
  expect_equal(dict_written$term_iri[dict_written$column_name == "WATERBODY"], paste0(metasalmon:::.ms_review_iri_prefix(), "https://example.org/waterbody"))
})

test_that("create_sdp keeps broad physical measurement matches review-only but still applies unit hits", {
  resources <- list(
    hydro = tibble::tibble(
      water_level = c(1.2, 1.3),
      spawner_count = c(10L, 20L)
    )
  )

  fake_suggest <- function(df, dict, sources = c("smn", "gcdfo", "ols", "nvs"),
                           include_dwc = FALSE, max_per_role = 3,
                           search_fn = find_terms, codes = NULL,
                           table_meta = NULL, dataset_meta = NULL) {
    attr(dict, "semantic_suggestions") <- tibble::tibble(
      dataset_id = c(rep("hydro-demo", 5), rep("hydro-demo", 2)),
      table_id = c(rep("hydro", 5), rep("hydro", 2)),
      column_name = c(rep("water_level", 5), rep("spawner_count", 2)),
      dictionary_role = c("variable", "property", "entity", "method", "unit", "variable", "property"),
      target_scope = "column",
      target_sdp_file = "column_dictionary.csv",
      target_sdp_field = c("term_iri", "property_iri", "entity_iri", "method_iri", "unit_iri", "term_iri", "property_iri"),
      search_query = c("water level", "water level", "water level", "water level", "meter", "adult spawner count", "count"),
      column_label = c(rep("Water Level (m)", 5), rep("Spawner Count", 2)),
      label = c("Escapement", "Mainstem phase", "Population", "uses observation procedure", "Meter", "Spawner abundance", "count"),
      iri = c(
        "https://w3id.org/smn/Escapement",
        "https://w3id.org/smn/MainstemPhase",
        "https://w3id.org/smn/Population",
        "https://w3id.org/smn/usesObservationProcedure",
        "http://qudt.org/vocab/unit/M",
        "https://w3id.org/gcdfo/salmon#SpawnerAbundance",
        "http://purl.obolibrary.org/obo/STATO_0000047"
      ),
      source = c("smn", "smn", "smn", "smn", "qudt", "gcdfo", "ols"),
      ontology = c("smn", "smn", "smn", "smn", "qudt", "gcdfo", "stato"),
      role = c("variable", "property", "entity", "method", "unit", "variable", "property"),
      match_type = c("class", "concept", "class", "objectproperty", "unit", "class", "label_exact"),
      definition = NA_character_,
      score = c(8, 8, 8, 8, 4.4, 3, 0.8)
    )
    dict
  }

  pkg_path <- with_mocked_bindings(
    suggest_semantics = fake_suggest,
    {
      create_sdp(
        resources,
        path = file.path(withr::local_tempdir(), "hydro-review-only"),
        dataset_id = "hydro-demo",
        seed_semantics = TRUE,
        seed_verbose = FALSE,
        check_updates = FALSE,
        overwrite = TRUE
      )
    }
  )

  dict_written <- readr::read_csv(file.path(pkg_path, "metadata", "column_dictionary.csv"), show_col_types = FALSE)
  water_row <- dict_written[dict_written$column_name == "water_level", , drop = FALSE]
  count_row <- dict_written[dict_written$column_name == "spawner_count", , drop = FALSE]

  expect_true(is.na(water_row$term_iri[[1]]) || water_row$term_iri[[1]] == "")
  expect_true(is.na(water_row$property_iri[[1]]) || water_row$property_iri[[1]] == "")
  expect_true(is.na(water_row$entity_iri[[1]]) || water_row$entity_iri[[1]] == "")
  expect_true(is.na(water_row$method_iri[[1]]) || water_row$method_iri[[1]] == "")
  expect_equal(water_row$unit_iri[[1]], paste0(metasalmon:::.ms_review_iri_prefix(), "http://qudt.org/vocab/unit/M"))

  expect_equal(count_row$term_iri[[1]], paste0(metasalmon:::.ms_review_iri_prefix(), "https://w3id.org/gcdfo/salmon#SpawnerAbundance"))
  expect_equal(count_row$property_iri[[1]], paste0(metasalmon:::.ms_review_iri_prefix(), "http://purl.obolibrary.org/obo/STATO_0000047"))
})

test_that("create_sdp keeps camelCase physical DwC property suggestions review-only but still applies unit hits", {
  resources <- list(
    event = tibble::tibble(
      minimumDepthInMeters = c(12.4, 15.1)
    )
  )

  fake_find_terms <- function(query, role = NA_character_, sources = c("smn", "gcdfo", "ols", "nvs"), ...) {
    query <- tolower(query)

    if (identical(role, "unit") && identical(query, "meter")) {
      return(tibble::tibble(
        label = "Meter",
        iri = "http://qudt.org/vocab/unit/M",
        source = "qudt",
        ontology = "qudt",
        role = "unit",
        match_type = "label_exact",
        definition = "Length unit",
        score = 4.5
      ))
    }

    if (identical(role, "variable") && grepl("minimum depth", query, fixed = TRUE)) {
      return(tibble::tibble(
        label = "Minimum Depth In Meters",
        iri = "http://rs.tdwg.org/dwc/terms/minimumDepthInMeters",
        source = "ols",
        ontology = "dwc",
        role = "variable",
        match_type = "dataProperty",
        definition = "Darwin Core publication property",
        score = 2.5
      ))
    }

    if (identical(role, "property") && grepl("minimum depth", query, fixed = TRUE)) {
      return(tibble::tibble(
        label = "Total length measurement",
        iri = "https://w3id.org/smn/TotalLengthMeasurement",
        source = "smn",
        ontology = "smn",
        role = "property",
        match_type = "class",
        definition = "Wrong fish-length property candidate",
        score = 6.8
      ))
    }

    if (identical(role, "entity") && grepl("minimum depth", query, fixed = TRUE)) {
      return(tibble::tibble(
        label = "Minimum Depth In Meters",
        iri = "http://rs.tdwg.org/dwc/terms/minimumDepthInMeters",
        source = "ols",
        ontology = "dwc",
        role = "entity",
        match_type = "dataProperty",
        definition = "Darwin Core publication property",
        score = 2.5
      ))
    }

    if (identical(role, "method") && grepl("minimum depth", query, fixed = TRUE)) {
      return(tibble::tibble(
        label = "Enumeration method",
        iri = "https://w3id.org/smn/EnumerationMethod",
        source = "smn",
        ontology = "smn",
        role = "method",
        match_type = "class",
        definition = "Generic method class",
        score = 6.1
      ))
    }

    tibble::tibble()
  }

  pkg_path <- with_mocked_bindings(
    find_terms = fake_find_terms,
    {
      create_sdp(
        resources,
        path = file.path(withr::local_tempdir(), "dwc-camel-guard"),
        dataset_id = "dwc-depth-demo",
        seed_semantics = TRUE,
        seed_verbose = FALSE,
        check_updates = FALSE,
        overwrite = TRUE
      )
    }
  )

  dict_written <- readr::read_csv(file.path(pkg_path, "metadata", "column_dictionary.csv"), show_col_types = FALSE)
  depth_row <- dict_written[dict_written$column_name == "minimumDepthInMeters", , drop = FALSE]

  expect_equal(depth_row$column_role[[1]], "measurement")
  expect_true(is.na(depth_row$term_iri[[1]]) || depth_row$term_iri[[1]] == "")
  expect_true(is.na(depth_row$property_iri[[1]]) || depth_row$property_iri[[1]] == "")
  expect_true(is.na(depth_row$entity_iri[[1]]) || depth_row$entity_iri[[1]] == "")
  expect_true(is.na(depth_row$method_iri[[1]]) || depth_row$method_iri[[1]] == "")
  expect_equal(depth_row$unit_iri[[1]], paste0(metasalmon:::.ms_review_iri_prefix(), "http://qudt.org/vocab/unit/M"))
})

test_that("create_sdp paired value/unit measurements auto-apply only the unit hit", {
  resources <- list(
    event = tibble::tibble(
      sampleSizeValue = c(2046.33, 131340.85),
      sampleSizeUnit = c("square metre", "square metre"),
      eventType = c("deployment", "deployment")
    )
  )

  fake_find_terms <- function(query, role = NA_character_, sources = c("smn", "gcdfo", "ols", "nvs"), ...) {
    query <- tolower(query)

    if (identical(role, "unit") && identical(query, "square meter")) {
      return(tibble::tibble(
        label = "Square Meter",
        iri = "http://qudt.org/vocab/unit/M2",
        source = "qudt",
        ontology = "qudt",
        role = "unit",
        match_type = "label_exact",
        definition = "Area unit",
        score = 4.8
      ))
    }

    if (identical(role, "variable") && grepl("sample size", query, fixed = TRUE)) {
      return(tibble::tibble(
        label = "Sample size",
        iri = "http://example.org/sample-size",
        source = "ols",
        ontology = "demo",
        role = "variable",
        match_type = "label_exact",
        definition = "Still too generic to auto-apply safely here",
        score = 3.2
      ))
    }

    if (identical(role, "property") && grepl("sample size", query, fixed = TRUE)) {
      return(tibble::tibble(
        label = "collection size",
        iri = "http://example.org/collection-size",
        source = "ols",
        ontology = "demo",
        role = "property",
        match_type = "label_exact",
        definition = "Generic size property",
        score = 3.2
      ))
    }

    tibble::tibble()
  }

  pkg_path <- with_mocked_bindings(
    find_terms = fake_find_terms,
    {
      create_sdp(
        resources,
        path = file.path(withr::local_tempdir(), "paired-value-unit"),
        dataset_id = "paired-value-demo",
        seed_semantics = TRUE,
        seed_verbose = FALSE,
        check_updates = FALSE,
        overwrite = TRUE
      )
    }
  )

  dict_written <- readr::read_csv(file.path(pkg_path, "metadata", "column_dictionary.csv"), show_col_types = FALSE)
  sample_row <- dict_written[dict_written$column_name == "sampleSizeValue", , drop = FALSE]

  expect_equal(sample_row$column_role[[1]], "measurement")
  expect_true(is.na(sample_row$term_iri[[1]]) || sample_row$term_iri[[1]] == "")
  expect_true(is.na(sample_row$property_iri[[1]]) || sample_row$property_iri[[1]] == "")
  expect_true(is.na(sample_row$entity_iri[[1]]) || sample_row$entity_iri[[1]] == "")
  expect_equal(sample_row$unit_iri[[1]], paste0(metasalmon:::.ms_review_iri_prefix(), "http://qudt.org/vocab/unit/M2"))
})

test_that("create_sdp uses the matching table context when semantic seeding multi-table resources", {
  resources <- list(
    catches = tibble::tibble(
      species = c("Coho", "Chinook"),
      count = c(10L, 20L)
    ),
    event = tibble::tibble(
      sampleSizeValue = c(2046.33, 131340.85),
      sampleSizeUnit = c("square metre", "square metre"),
      eventType = c("deployment", "deployment")
    )
  )

  fake_find_terms <- function(query, role = NA_character_, sources = c("smn", "gcdfo", "ols", "nvs"), ...) {
    query <- tolower(query)

    if (identical(role, "unit") && identical(query, "square meter")) {
      return(tibble::tibble(
        label = "Square Meter",
        iri = "http://qudt.org/vocab/unit/M2",
        source = "qudt",
        ontology = "qudt",
        role = "unit",
        match_type = "label_exact",
        definition = "Area unit",
        score = 4.8
      ))
    }

    if (identical(role, "variable") && grepl("sample size", query, fixed = TRUE)) {
      return(tibble::tibble(
        label = "Sample size",
        iri = "http://example.org/sample-size",
        source = "ols",
        ontology = "demo",
        role = "variable",
        match_type = "label_exact",
        definition = "Still too generic to auto-apply safely here",
        score = 3.2
      ))
    }

    if (identical(role, "property") && grepl("sample size", query, fixed = TRUE)) {
      return(tibble::tibble(
        label = "collection size",
        iri = "http://example.org/collection-size",
        source = "ols",
        ontology = "demo",
        role = "property",
        match_type = "label_exact",
        definition = "Generic size property",
        score = 3.2
      ))
    }

    tibble::tibble()
  }

  pkg_path <- with_mocked_bindings(
    find_terms = fake_find_terms,
    {
      create_sdp(
        resources,
        path = file.path(withr::local_tempdir(), "paired-value-unit-multi-table"),
        dataset_id = "paired-value-multi-demo",
        seed_semantics = TRUE,
        seed_verbose = FALSE,
        check_updates = FALSE,
        overwrite = TRUE
      )
    }
  )

  dict_written <- readr::read_csv(file.path(pkg_path, "metadata", "column_dictionary.csv"), show_col_types = FALSE)
  sample_row <- dict_written[dict_written$table_id == "event" & dict_written$column_name == "sampleSizeValue", , drop = FALSE]

  expect_equal(sample_row$column_role[[1]], "measurement")
  expect_true(is.na(sample_row$term_iri[[1]]) || sample_row$term_iri[[1]] == "")
  expect_true(is.na(sample_row$property_iri[[1]]) || sample_row$property_iri[[1]] == "")
  expect_true(is.na(sample_row$entity_iri[[1]]) || sample_row$entity_iri[[1]] == "")
  expect_equal(sample_row$unit_iri[[1]], paste0(metasalmon:::.ms_review_iri_prefix(), "http://qudt.org/vocab/unit/M2"))
})

test_that("create_sdp unit seeding can use role-augmented unit sources", {
  resources <- list(
    hydro = tibble::tibble(
      `Water Level / Niveau d'eau (m)` = c(1.2, 1.3),
      temperature_degree_c = c(6.1, 6.4)
    )
  )

  calls <- list()
  fake_find_terms <- function(query, role = NA_character_, sources = c("smn", "gcdfo", "ols", "nvs"), ...) {
    calls[[length(calls) + 1]] <<- tibble::tibble(
      query = query,
      role = role,
      sources = list(sources)
    )

    if (identical(role, "unit")) {
      if ("qudt" %in% sources) {
        if (identical(query, "degree celsius")) {
          return(tibble::tibble(
            label = "Degree Celsius",
            iri = "http://qudt.org/vocab/unit/DEG_C",
            source = "qudt",
            ontology = "qudt",
            role = "unit",
            match_type = "label_exact",
            definition = "Temperature unit",
            score = 4.5
          ))
        }
        return(tibble::tibble(
          label = "Meter",
          iri = "http://qudt.org/vocab/unit/M",
          source = "qudt",
          ontology = "qudt",
          role = "unit",
          match_type = "label_exact",
          definition = "Length unit",
          score = 4.5
        ))
      }
      return(tibble::tibble(
        label = "Degrees Celsius kilogram per square metre",
        iri = "http://vocab.nerc.ac.uk/collection/P06/current/UFAKE/",
        source = "nvs",
        ontology = "P06",
        role = "unit",
        match_type = "label_partial",
        definition = "Bad blended candidate",
        score = 0.1
      ))
    }

    tibble::tibble(
      label = "Escapement",
      iri = paste0("https://example.org/", role),
      source = "ols",
      ontology = "demo",
      role = role,
      match_type = "label_partial",
      definition = "Generic candidate",
      score = 0.2
    )
  }

  pkg_path <- with_mocked_bindings(
    find_terms = fake_find_terms,
    {
      create_sdp(
        resources,
        path = file.path(withr::local_tempdir(), "hydro-unit-sources"),
        dataset_id = "hydro-unit-demo",
        seed_semantics = TRUE,
        semantic_sources = c("smn", "gcdfo", "ols", "nvs"),
        seed_verbose = FALSE,
        check_updates = FALSE,
        overwrite = TRUE
      )
    }
  )

  dict_written <- readr::read_csv(file.path(pkg_path, "metadata", "column_dictionary.csv"), show_col_types = FALSE)
  water_row <- dict_written[dict_written$column_name == "Water Level / Niveau d'eau (m)", , drop = FALSE]
  temp_row <- dict_written[dict_written$column_name == "temperature_degree_c", , drop = FALSE]

  expect_equal(water_row$unit_iri[[1]], paste0(metasalmon:::.ms_review_iri_prefix(), "http://qudt.org/vocab/unit/M"))
  expect_true(is.na(water_row$term_iri[[1]]) || water_row$term_iri[[1]] == "")
  expect_true(is.na(water_row$property_iri[[1]]) || water_row$property_iri[[1]] == "")
  expect_equal(temp_row$unit_iri[[1]], paste0(metasalmon:::.ms_review_iri_prefix(), "http://qudt.org/vocab/unit/DEG_C"))
  expect_true(is.na(temp_row$term_iri[[1]]) || temp_row$term_iri[[1]] == "")
  expect_true(is.na(temp_row$property_iri[[1]]) || temp_row$property_iri[[1]] == "")

  call_df <- dplyr::bind_rows(calls)
  unit_sources <- call_df$sources[call_df$role == "unit"][[1]]
  expect_true("qudt" %in% unit_sources)
  expect_true(all(c("smn", "gcdfo", "ols", "nvs") %in% unit_sources))
})

test_that("create_sdp can broaden code-level semantic seeding and optionally check for updates", {
  resources <- list(
    catches = tibble::tibble(
      run = factor(c("early", "late")),
      station = c("A", "B"),
      count = c(10L, 20L)
    )
  )

  seen_codes <- NULL
  update_calls <- 0L
  fake_suggest <- function(df, dict, sources = c("smn", "gcdfo", "ols", "nvs"),
                           include_dwc = FALSE, max_per_role = 3,
                           search_fn = find_terms, codes = NULL,
                           table_meta = NULL, dataset_meta = NULL) {
    seen_codes <<- codes
    attr(dict, "semantic_suggestions") <- tibble::tibble()
    dict
  }
  fake_check_for_updates <- function(...) {
    update_calls <<- update_calls + 1L
    structure(
      list(
        status = "update_available",
        update_available = TRUE,
        latest_version = "9.9.9",
        install_command = "remotes::install_github('dfo-pacific-science/metasalmon')"
      ),
      class = "metasalmon_update_check"
    )
  }

  with_mocked_bindings(
    suggest_semantics = fake_suggest,
    check_for_updates = fake_check_for_updates,
    {
      create_sdp(
        resources,
        path = file.path(withr::local_tempdir(), "all-code-scope"),
        dataset_id = "scope-demo-all",
        seed_semantics = TRUE,
        seed_verbose = FALSE,
        semantic_code_scope = "all",
        check_updates = TRUE,
        overwrite = TRUE
      )

      create_sdp(
        resources,
        path = file.path(withr::local_tempdir(), "no-update-check"),
        dataset_id = "scope-demo-no-update",
        seed_semantics = TRUE,
        seed_verbose = FALSE,
        semantic_code_scope = "all",
        check_updates = FALSE,
        overwrite = TRUE
      )
    }
  )

  expect_setequal(unique(seen_codes$column_name), c("run", "station"))
  expect_equal(update_calls, 1L)
})

test_that("read_salmon_datapackage reads package correctly", {
  # Create test package
  resources <- list(
    main_table = tibble::tibble(
      species = c("Coho", "Chinook"),
      count = c(100L, 200L)
    )
  )

  dataset_meta <- tibble::tibble(
    dataset_id = "test-1",
    title = "Test Dataset",
    description = "A test dataset",
    creator = "Test Author",
    contact_name = NA_character_,
    contact_email = NA_character_,
    license = "MIT",
    temporal_start = NA_character_,
    temporal_end = NA_character_,
    spatial_extent = NA_character_,
    dataset_type = NA_character_,
    source_citation = NA_character_
  )

  table_meta <- tibble::tibble(
    dataset_id = "test-1",
    table_id = "main_table",
    file_name = "data/main_table.csv",
    table_label = "Main Table",
    description = "Main data table",
    observation_unit = NA_character_,
    observation_unit_iri = NA_character_,
    primary_key = NA_character_
  )

  dict <- infer_dictionary(
    resources$main_table,
    dataset_id = "test-1",
    table_id = "main_table"
  )
  dict <- fill_measurement_components(dict)
  validate_dictionary(dict)

  temp_dir <- withr::local_tempdir()
  write_salmon_datapackage(
    resources,
    dataset_meta,
    table_meta,
    dict,
    path = temp_dir,
    format = "csv",
    overwrite = TRUE
  )

  # Read it back
  pkg <- read_salmon_datapackage(temp_dir)

  expect_true("dataset" %in% names(pkg))
  expect_true("tables" %in% names(pkg))
  expect_true("dictionary" %in% names(pkg))
  expect_true("resources" %in% names(pkg))

  expect_equal(nrow(pkg$dataset), 1)
  expect_equal(pkg$dataset$dataset_id, "test-1")
  expect_equal(pkg$dataset$title, "Test Dataset")

  expect_true("main_table" %in% names(pkg$resources))
  expect_equal(nrow(pkg$resources$main_table), 2)
  expect_equal(ncol(pkg$resources$main_table), 2)
})

test_that("read_salmon_datapackage prefers canonical CSV metadata when datapackage.json is absent", {
  resources <- list(
    main_table = tibble::tibble(
      species = c("Coho", "Chinook"),
      count = c(100L, 200L)
    )
  )

  dataset_meta <- tibble::tibble(
    dataset_id = "test-1",
    title = "Test Dataset",
    description = "A test dataset",
    creator = "Test Author",
    contact_name = NA_character_,
    contact_email = NA_character_,
    license = "MIT"
  )

  table_meta <- tibble::tibble(
    dataset_id = "test-1",
    table_id = "main_table",
    file_name = "data/main_table.csv",
    table_label = "Main Table",
    description = "Main data table"
  )

  dict <- infer_dictionary(
    resources$main_table,
    dataset_id = "test-1",
    table_id = "main_table"
  )
  dict <- fill_measurement_components(dict)
  validate_dictionary(dict)

  temp_dir <- withr::local_tempdir()
  write_salmon_datapackage(
    resources,
    dataset_meta,
    table_meta,
    dict,
    path = temp_dir,
    format = "csv",
    overwrite = TRUE
  )

  unlink(file.path(temp_dir, "datapackage.json"))

  pkg <- read_salmon_datapackage(temp_dir)

  expect_equal(pkg$dataset$dataset_id, "test-1")
  expect_equal(pkg$tables$table_id, "main_table")
  expect_equal(pkg$dictionary$column_name, c("species", "count"))
  expect_true("main_table" %in% names(pkg$resources))
})

test_that("read_salmon_datapackage still reads legacy root-level metadata CSVs", {
  resources <- list(
    main_table = tibble::tibble(
      species = c("Coho", "Chinook"),
      count = c(100L, 200L)
    )
  )

  dataset_meta <- tibble::tibble(
    dataset_id = "legacy-1",
    title = "Legacy Dataset",
    description = "Legacy layout test",
    creator = "Test Author",
    contact_name = NA_character_,
    contact_email = NA_character_,
    license = "MIT"
  )

  table_meta <- tibble::tibble(
    dataset_id = "legacy-1",
    table_id = "main_table",
    file_name = "data/main_table.csv",
    table_label = "Main Table",
    description = "Main data table"
  )

  dict <- infer_dictionary(
    resources$main_table,
    dataset_id = "legacy-1",
    table_id = "main_table"
  )
  dict <- fill_measurement_components(dict)
  validate_dictionary(dict)

  temp_dir <- withr::local_tempdir()
  write_salmon_datapackage(
    resources,
    dataset_meta,
    table_meta,
    dict,
    path = temp_dir,
    format = "csv",
    overwrite = TRUE
  )

  file.copy(file.path(temp_dir, "metadata", "dataset.csv"), file.path(temp_dir, "dataset.csv"), overwrite = TRUE)
  file.copy(file.path(temp_dir, "metadata", "tables.csv"), file.path(temp_dir, "tables.csv"), overwrite = TRUE)
  file.copy(file.path(temp_dir, "metadata", "column_dictionary.csv"), file.path(temp_dir, "column_dictionary.csv"), overwrite = TRUE)
  unlink(file.path(temp_dir, "metadata"), recursive = TRUE)

  pkg <- read_salmon_datapackage(temp_dir)

  expect_equal(pkg$dataset$dataset_id, "legacy-1")
  expect_equal(pkg$tables$table_id, "main_table")
  expect_true("main_table" %in% names(pkg$resources))
})

test_that("write_salmon_datapackage round-trip preserves data", {
  # Create test data
  original_df <- tibble::tibble(
    species = c("Coho", "Chinook", "Sockeye"),
    count = c(100L, 200L, 150L),
    date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))
  )

  resources <- list(main_table = original_df)

  dataset_meta <- tibble::tibble(
    dataset_id = "test-1",
    title = "Test Dataset",
    description = "A test dataset",
    creator = "Test Author",
    contact_name = NA_character_,
    contact_email = NA_character_,
    license = "MIT",
    temporal_start = NA_character_,
    temporal_end = NA_character_,
    spatial_extent = NA_character_,
    dataset_type = NA_character_,
    source_citation = NA_character_
  )

  table_meta <- tibble::tibble(
    dataset_id = "test-1",
    table_id = "main_table",
    file_name = "data/main_table.csv",
    table_label = "Main Table",
    description = "Main data table",
    observation_unit = NA_character_,
    observation_unit_iri = NA_character_,
    primary_key = NA_character_
  )

  dict <- infer_dictionary(
    original_df,
    dataset_id = "test-1",
    table_id = "main_table"
  )
  dict <- fill_measurement_components(dict)
  validate_dictionary(dict)

  temp_dir <- withr::local_tempdir()
  write_salmon_datapackage(
    resources,
    dataset_meta,
    table_meta,
    dict,
    path = temp_dir,
    format = "csv",
    overwrite = TRUE
  )

  # Read it back
  pkg <- read_salmon_datapackage(temp_dir)
  read_df <- pkg$resources$main_table

  # Compare data (column names may differ due to renaming)
  expect_equal(nrow(read_df), nrow(original_df))
  expect_equal(ncol(read_df), ncol(original_df))

  # Check that data values are preserved (may need to account for renaming)
  # This is a basic check; full round-trip would require applying dictionary
})

test_that("I-ADOPT fields round-trip through datapackage.json", {
  resources <- list(
    main_table = tibble::tibble(
      count = c(1L, 2L)
    )
  )

  dataset_meta <- tibble::tibble(
    dataset_id = "test-1",
    title = "Test Dataset",
    description = "A test dataset",
    creator = "Test Author",
    contact_name = NA_character_,
    contact_email = NA_character_,
    license = "MIT",
    temporal_start = NA_character_,
    temporal_end = NA_character_,
    spatial_extent = NA_character_,
    dataset_type = NA_character_,
    source_citation = NA_character_
  )

  table_meta <- tibble::tibble(
    dataset_id = "test-1",
    table_id = "main_table",
    file_name = "data/main_table.csv",
    table_label = "Main Table",
    description = "Main data table",
    observation_unit = NA_character_,
    observation_unit_iri = NA_character_,
    primary_key = NA_character_
  )

  dict <- tibble::tibble(
    dataset_id = "test-1",
    table_id = "main_table",
    column_name = c("count"),
    column_label = c("count"),
    column_description = c("Example count"),
    column_role = c("measurement"),
    value_type = c("integer"),
    unit_label = NA_character_,
    unit_iri = c("https://qudt.org/vocab/unit/Each"),
    term_iri = c("https://w3id.org/example/term"),
    term_type = c("skos_concept"),
    required = FALSE,
    property_iri = c("https://qudt.org/vocab/quantitykind/NumberOfOrganisms"),
    entity_iri = c("https://w3id.org/example/entity"),
    constraint_iri = c("https://w3id.org/example/constraint"),
    method_iri = c("https://w3id.org/example/method")
  )

  temp_dir <- withr::local_tempdir()
  write_salmon_datapackage(
    resources,
    dataset_meta,
    table_meta,
    dict,
    path = temp_dir,
    format = "csv",
    overwrite = TRUE
  )

  pkg <- read_salmon_datapackage(temp_dir)

  expect_true(all(c("property_iri", "entity_iri", "constraint_iri", "method_iri", "unit_iri") %in% names(pkg$dictionary)))
  expect_equal(pkg$dictionary$property_iri, dict$property_iri)
  expect_equal(pkg$dictionary$entity_iri, dict$entity_iri)
  expect_equal(pkg$dictionary$constraint_iri, dict$constraint_iri)
  expect_equal(pkg$dictionary$method_iri, dict$method_iri)
  expect_equal(pkg$dictionary$unit_iri, dict$unit_iri)
})

test_that("write_salmon_datapackage errors on existing path without overwrite", {
  temp_dir <- withr::local_tempdir()

  # Create a file in the directory
  writeLines("test", file.path(temp_dir, "test.txt"))

  resources <- list(main_table = tibble::tibble(x = 1))
  dataset_meta <- tibble::tibble(
    dataset_id = "test-1",
    title = "Test",
    description = "Test",
    creator = NA_character_,
    contact_name = NA_character_,
    contact_email = NA_character_,
    license = NA_character_,
    temporal_start = NA_character_,
    temporal_end = NA_character_,
    spatial_extent = NA_character_,
    dataset_type = NA_character_,
    source_citation = NA_character_
  )
  table_meta <- tibble::tibble(
    dataset_id = "test-1",
    table_id = "main_table",
    file_name = "data/main_table.csv",
    table_label = "Main",
    description = NA_character_,
    observation_unit = NA_character_,
    observation_unit_iri = NA_character_,
    primary_key = NA_character_
  )
  dict <- infer_dictionary(resources$main_table, dataset_id = "test-1", table_id = "main_table")
  dict <- fill_measurement_components(dict)

  expect_error(
    write_salmon_datapackage(
      resources,
      dataset_meta,
      table_meta,
      dict,
      path = temp_dir,
      overwrite = FALSE
    ),
    "already exists"
  )
})

test_that("write_salmon_datapackage refuses overwrite for non-metasalmon directories", {
  temp_dir <- withr::local_tempdir()
  writeLines("do not delete", file.path(temp_dir, "keep.txt"))

  resources <- list(main_table = tibble::tibble(x = 1))
  dataset_meta <- tibble::tibble(
    dataset_id = "test-1",
    title = "Test",
    description = "Test",
    creator = NA_character_,
    contact_name = NA_character_,
    contact_email = NA_character_,
    license = NA_character_,
    temporal_start = NA_character_,
    temporal_end = NA_character_,
    spatial_extent = NA_character_,
    dataset_type = NA_character_,
    source_citation = NA_character_
  )
  table_meta <- tibble::tibble(
    dataset_id = "test-1",
    table_id = "main_table",
    file_name = "data/main_table.csv",
    table_label = "Main",
    description = NA_character_,
    observation_unit = NA_character_,
    observation_unit_iri = NA_character_,
    primary_key = NA_character_
  )
  dict <- infer_dictionary(resources$main_table, dataset_id = "test-1", table_id = "main_table")
  dict <- fill_measurement_components(dict)

  expect_error(
    write_salmon_datapackage(
      resources,
      dataset_meta,
      table_meta,
      dict,
      path = temp_dir,
      overwrite = TRUE
    ),
    "Refusing to overwrite non-metasalmon directory"
  )
  expect_true(file.exists(file.path(temp_dir, "keep.txt")))
})

test_that("write_salmon_datapackage can overwrite an existing metasalmon package", {
  temp_dir <- withr::local_tempdir()

  resources <- list(main_table = tibble::tibble(x = 1))
  dataset_meta <- tibble::tibble(
    dataset_id = "test-1",
    title = "Test",
    description = "Test",
    creator = NA_character_,
    contact_name = NA_character_,
    contact_email = NA_character_,
    license = NA_character_,
    temporal_start = NA_character_,
    temporal_end = NA_character_,
    spatial_extent = NA_character_,
    dataset_type = NA_character_,
    source_citation = NA_character_
  )
  table_meta <- tibble::tibble(
    dataset_id = "test-1",
    table_id = "main_table",
    file_name = "data/main_table.csv",
    table_label = "Main",
    description = NA_character_,
    observation_unit = NA_character_,
    observation_unit_iri = NA_character_,
    primary_key = NA_character_
  )
  dict <- infer_dictionary(resources$main_table, dataset_id = "test-1", table_id = "main_table")
  dict <- fill_measurement_components(dict)

  write_salmon_datapackage(
    resources,
    dataset_meta,
    table_meta,
    dict,
    path = temp_dir,
    overwrite = TRUE
  )
  writeLines("stale", file.path(temp_dir, "stale.txt"))

  write_salmon_datapackage(
    resources,
    dataset_meta,
    table_meta,
    dict,
    path = temp_dir,
    overwrite = TRUE
  )

  expect_false(file.exists(file.path(temp_dir, "stale.txt")))
  expect_true(file.exists(file.path(temp_dir, ".metasalmon-package")))
  expect_true(file.exists(file.path(temp_dir, "metadata", "dataset.csv")))
})

test_that("validate_salmon_datapackage validates a CU/composite-style package", {
  resources <- list(
    cu_composite_escapement = tibble::tibble(
      cu_id = c("CU-001", "CU-002"),
      year = c(2023L, 2024L),
      escapement = c(1250, 1325),
      estimate_type = c("point", "point")
    )
  )

  dataset_meta <- tibble::tibble(
    dataset_id = "cu-composite-demo",
    title = "CU composite escapement demo",
    description = "Package-first CU/composite escapement example",
    creator = "Test Author",
    contact_name = "Test Contact",
    contact_email = "test@example.org",
    license = "Open Government Licence - Canada"
  )

  table_meta <- tibble::tibble(
    dataset_id = "cu-composite-demo",
    table_id = "cu_composite_escapement",
    file_name = "data/cu_composite_escapement.csv",
    table_label = "CU Composite Escapement",
    description = "One row per CU-year estimate.",
    observation_unit = "CU-year escapement estimate",
    observation_unit_iri = "https://w3id.org/smn/Observation",
    primary_key = "cu_id,year"
  )

  dict <- infer_dictionary(
    resources$cu_composite_escapement,
    dataset_id = "cu-composite-demo",
    table_id = "cu_composite_escapement"
  )
  dict <- fill_measurement_components(dict)
  dict$column_description <- c(
    "Conservation unit identifier.",
    "Observation year.",
    "Escapement estimate value.",
    "Estimate type code."
  )

  codes <- tibble::tibble(
    dataset_id = "cu-composite-demo",
    table_id = "cu_composite_escapement",
    column_name = "estimate_type",
    code_value = "point",
    code_label = "Point estimate",
    code_description = "Single-value escapement estimate.",
    vocabulary_iri = NA_character_,
    term_iri = NA_character_,
    term_type = NA_character_
  )

  temp_dir <- withr::local_tempdir()
  write_salmon_datapackage(
    resources,
    dataset_meta,
    table_meta,
    dict,
    codes = codes,
    path = temp_dir,
    overwrite = TRUE
  )

  result <- suppressMessages(validate_salmon_datapackage(temp_dir, require_iris = TRUE))

  expect_true(is.list(result))
  expect_equal(nrow(result$issues), 0)
  expect_true("cu_composite_escapement" %in% names(result$package$resources))
  expect_equal(nrow(result$semantic_validation$issues), 0)
})

test_that("validate_salmon_datapackage catches missing codes.csv values", {
  resources <- list(
    cu_composite_escapement = tibble::tibble(
      cu_id = c("CU-001", "CU-002"),
      year = c(2023L, 2024L),
      escapement = c(1250, 1325),
      estimate_type = c("point", "provisional")
    )
  )

  dataset_meta <- tibble::tibble(
    dataset_id = "cu-composite-demo",
    title = "CU composite escapement demo",
    description = "Package-first CU/composite escapement example",
    creator = "Test Author",
    contact_name = "Test Contact",
    contact_email = "test@example.org",
    license = "Open Government Licence - Canada"
  )

  table_meta <- tibble::tibble(
    dataset_id = "cu-composite-demo",
    table_id = "cu_composite_escapement",
    file_name = "data/cu_composite_escapement.csv",
    table_label = "CU Composite Escapement",
    description = "One row per CU-year estimate.",
    observation_unit = "CU-year escapement estimate",
    observation_unit_iri = "https://w3id.org/smn/Observation",
    primary_key = "cu_id,year"
  )

  dict <- infer_dictionary(
    resources$cu_composite_escapement,
    dataset_id = "cu-composite-demo",
    table_id = "cu_composite_escapement"
  )
  dict <- fill_measurement_components(dict)

  codes <- tibble::tibble(
    dataset_id = "cu-composite-demo",
    table_id = "cu_composite_escapement",
    column_name = "estimate_type",
    code_value = "point",
    code_label = "Point estimate",
    code_description = "Single-value escapement estimate.",
    vocabulary_iri = NA_character_,
    term_iri = NA_character_,
    term_type = NA_character_
  )

  temp_dir <- withr::local_tempdir()
  write_salmon_datapackage(
    resources,
    dataset_meta,
    table_meta,
    dict,
    codes = codes,
    path = temp_dir,
    overwrite = TRUE
  )

  expect_error(
    suppressMessages(validate_salmon_datapackage(temp_dir, require_iris = FALSE)),
    "not listed in codes.csv"
  )
})

.ms_write_semantic_validation_fixture <- function(
    dict_term_iri = "https://example.org/variable",
    table_observation_unit_iri = "https://w3id.org/smn/Observation",
    dataset_description = "Fixture for semantic validation regression tests.",
    dataset_creator = "Test Author",
    table_description = "One row per record.",
    table_observation_unit = "record",
    escapement_column_description = "Escapement count"
) {
  resources <- list(
    main = tibble::tibble(
      id = "A",
      escapement = 1250
    )
  )

  dataset_meta <- tibble::tibble(
    dataset_id = "semantic-validation-demo",
    title = "Semantic validation demo",
    description = dataset_description,
    creator = dataset_creator,
    contact_name = "Test Contact",
    contact_email = "test@example.org",
    license = "Open Government Licence - Canada"
  )

  table_meta <- tibble::tibble(
    dataset_id = "semantic-validation-demo",
    table_id = "main",
    file_name = "data/main.csv",
    table_label = "Main",
    description = table_description,
    observation_unit = table_observation_unit,
    observation_unit_iri = table_observation_unit_iri,
    primary_key = "id"
  )

  dict <- infer_dictionary(
    resources$main,
    dataset_id = "semantic-validation-demo",
    table_id = "main"
  )
  dict <- fill_measurement_components(dict)
  dict$term_iri[dict$column_name == "escapement"] <- dict_term_iri
  dict$column_description[dict$column_name == "escapement"] <- escapement_column_description

  temp_dir <- tempfile("semantic-validation-")
  dir.create(temp_dir, recursive = TRUE)
  write_salmon_datapackage(
    resources,
    dataset_meta,
    table_meta,
    dict,
    path = temp_dir,
    overwrite = TRUE
  )

  temp_dir
}

test_that("validate_salmon_datapackage warns cleanly for semantic issues without crashing cli pluralization", {
  pkg_path <- .ms_write_semantic_validation_fixture(
    dict_term_iri = "http://w3id.org/salmon/SpawnerAbundance"
  )

  expect_warning(
    result <- suppressMessages(validate_salmon_datapackage(pkg_path, require_iris = FALSE)),
    "reported 1 semantic issue"
  )

  expect_true(is.list(result))
  expect_true(any(grepl("legacy SMN namespace", result$semantic_validation$issues$message, fixed = TRUE)))
})

test_that("validate_salmon_datapackage fails final validation when tables.csv keeps REVIEW-prefixed IRIs", {
  pkg_path <- .ms_write_semantic_validation_fixture(
    table_observation_unit_iri = "REVIEW: https://w3id.org/smn/Observation"
  )

  expect_error(
    suppressMessages(validate_salmon_datapackage(pkg_path, require_iris = TRUE)),
    "REVIEW-prefixed IRI"
  )
})

test_that("validate_salmon_datapackage fails final validation on unresolved metadata placeholders", {
  pkg_path <- .ms_write_semantic_validation_fixture(
    dataset_description = "MISSING DESCRIPTION: describe the dataset before final review.",
    dataset_creator = "MISSING METADATA: add creator, team, or originating program.",
    table_description = "MISSING DESCRIPTION: describe what each row means.",
    table_observation_unit = "MISSING METADATA: describe the observation unit.",
    escapement_column_description = "MISSING DESCRIPTION: define what 'escapement' means."
  )

  expect_error(
    suppressMessages(validate_salmon_datapackage(pkg_path, require_iris = TRUE)),
    "unresolved review placeholder"
  )
})

test_that("validate_salmon_datapackage fails final validation when tables.csv observation_unit_iri is blank", {
  pkg_path <- .ms_write_semantic_validation_fixture(
    table_observation_unit_iri = ""
  )

  expect_error(
    suppressMessages(validate_salmon_datapackage(pkg_path, require_iris = TRUE)),
    "observation_unit_iri is blank"
  )
})

test_that("validate_salmon_datapackage keeps review-ready placeholder packages valid in non-strict mode", {
  pkg_path <- .ms_write_semantic_validation_fixture(
    table_observation_unit_iri = "",
    dataset_description = "MISSING DESCRIPTION: describe the dataset before final review.",
    table_observation_unit = "MISSING METADATA: describe the observation unit.",
    escapement_column_description = "MISSING DESCRIPTION: define what 'escapement' means."
  )

  expect_no_error(
    suppressMessages(validate_salmon_datapackage(pkg_path, require_iris = FALSE))
  )
})

.ms_write_composite_guardrail_fixture <- function(
    route_value = NULL,
    datapackage_route_value = NULL,
    populate_signal_column = FALSE
) {
  resources <- list(
    cu_timeseries = tibble::tibble(
      cu_id = c("CU-001", "CU-002"),
      year = c(2023L, 2024L),
      escapement = c(1250, 1325),
      SPN_ABD_WILD = if (populate_signal_column) c("present", NA_character_) else c(NA_character_, NA_character_),
      SPN_TREND_WILD = c(NA_character_, NA_character_),
      RAPID_STATUS = c(NA_character_, NA_character_)
    )
  )

  dataset_meta <- tibble::tibble(
    dataset_id = "cu-composite-guardrail",
    title = "CU composite guardrail demo",
    description = "Fixture for composite-intent package validation.",
    creator = "Test Author",
    contact_name = "Test Contact",
    contact_email = "test@example.org",
    license = "Open Government Licence - Canada"
  )
  if (!is.null(route_value)) {
    dataset_meta$route <- route_value
  }

  table_meta <- tibble::tibble(
    dataset_id = "cu-composite-guardrail",
    table_id = "cu_timeseries",
    file_name = "data/cu_timeseries.csv",
    table_label = "CU Timeseries",
    description = "CU timeseries values for validation tests.",
    observation_unit = "CU-year observation",
    observation_unit_iri = "https://w3id.org/smn/Observation",
    primary_key = "cu_id,year"
  )

  dict <- infer_dictionary(
    resources$cu_timeseries,
    dataset_id = "cu-composite-guardrail",
    table_id = "cu_timeseries"
  )
  dict <- fill_measurement_components(dict)

  temp_dir <- tempfile("composite-guardrail-")
  dir.create(temp_dir, recursive = TRUE)
  write_salmon_datapackage(
    resources,
    dataset_meta,
    table_meta,
    dict,
    path = temp_dir,
    overwrite = TRUE
  )

  if (!is.null(datapackage_route_value)) {
    datapackage_path <- file.path(temp_dir, "datapackage.json")
    datapackage <- jsonlite::read_json(datapackage_path, simplifyVector = FALSE)
    datapackage$route <- datapackage_route_value
    jsonlite::write_json(
      datapackage,
      datapackage_path,
      pretty = TRUE,
      auto_unbox = TRUE,
      null = "null"
    )
  }

  temp_dir
}

test_that("validate_salmon_datapackage catches explicit composite route metadata without WSP signals", {
  pkg_path <- .ms_write_composite_guardrail_fixture(route_value = "cu_composite")

  expect_error(
    suppressMessages(validate_salmon_datapackage(pkg_path, require_iris = FALSE)),
    "Explicit composite route intent detected"
  )
})

test_that("validate_salmon_datapackage allows explicit composite route metadata when WSP signals are populated", {
  pkg_path <- .ms_write_composite_guardrail_fixture(
    route_value = "cu_composite",
    populate_signal_column = TRUE
  )

  result <- suppressWarnings(
    suppressMessages(validate_salmon_datapackage(pkg_path, require_iris = FALSE))
  )

  expect_equal(nrow(result$issues), 0)
  expect_true("cu_timeseries" %in% names(result$package$resources))
})

test_that("validate_salmon_datapackage does not require WSP signals without explicit composite intent", {
  pkg_path <- .ms_write_composite_guardrail_fixture()

  result <- suppressWarnings(
    suppressMessages(validate_salmon_datapackage(pkg_path, require_iris = FALSE))
  )

  expect_equal(nrow(result$issues), 0)
})

test_that("validate_salmon_datapackage catches datapackage route hints without WSP signals", {
  pkg_path <- .ms_write_composite_guardrail_fixture(
    datapackage_route_value = "cu_composite"
  )

  expect_error(
    suppressMessages(validate_salmon_datapackage(pkg_path, require_iris = FALSE)),
    "Explicit composite route intent detected"
  )
})
