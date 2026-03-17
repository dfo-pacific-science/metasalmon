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
    edh_profile = "dfo_edh_hnap",
    overwrite = TRUE
  )

  expect_true(file.exists(file.path(pkg_path_with_edh, "metadata", "metadata-edh-hnap.xml")))
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

test_that("create_sdp writes review files and auto-applies top column suggestions", {
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
    dict$property_iri[dict$column_name == "count"] <- "https://example.org/property-existing"
    attr(dict, "semantic_suggestions") <- tibble::tibble(
      column_name = c("count", "count", "species"),
      dictionary_role = c("variable", "property", "entity"),
      table_id = c("catches", "catches", "catches"),
      dataset_id = c("review-demo", "review-demo", "review-demo"),
      target_scope = c("column", "column", "code"),
      target_sdp_file = c("column_dictionary.csv", "column_dictionary.csv", "codes.csv"),
      target_sdp_field = c("term_iri", "property_iri", "term_iri"),
      target_row_key = c("review-demo/catches/count", "review-demo/catches/count", "review-demo/catches/species/POP1"),
      code_value = c(NA_character_, NA_character_, "POP1"),
      code_label = c(NA_character_, NA_character_, "POP1"),
      code_description = c(NA_character_, NA_character_, NA_character_),
      iri = c("https://example.org/term-top", "https://example.org/property-top", "https://example.org/pop1"),
      label = c("Count term", "Count property", "Population One"),
      source = c("smn", "smn", "smn"),
      ontology = c("demo", "demo", "demo"),
      role = c("variable", "property", "entity"),
      match_type = c("label_exact", "label_exact", "label_exact"),
      definition = c(NA_character_, NA_character_, NA_character_)
    )
    dict
  }

  pkg_path <- NULL
  with_mocked_bindings(
    suggest_semantics = fake_suggest,
    {
      pkg_path <- create_sdp(
        resources,
        path = file.path(withr::local_tempdir(), "review-package"),
        dataset_id = "review-demo",
        seed_semantics = TRUE,
        overwrite = TRUE
      )
    }
  )

  expect_true(file.exists(file.path(pkg_path, "README-review.txt")))
  expect_true(file.exists(file.path(pkg_path, "semantic_suggestions.csv")))

  review_lines <- readLines(file.path(pkg_path, "README-review.txt"), warn = FALSE)
  expect_true(any(grepl("Salmon Data Package Review Checklist", review_lines, fixed = TRUE)))
  expect_true(any(grepl("[ ] 1. Confirm the package has the expected metadata/ and data/ files", review_lines, fixed = TRUE)))
  expect_true(any(grepl("The canonical Salmon Data Package is the whole folder", review_lines, fixed = TRUE)))
  expect_true(any(grepl("send the whole folder or a zip of the whole folder", review_lines, fixed = TRUE)))
  expect_true(any(grepl("read_salmon_datapackage(pkg_path)", review_lines, fixed = TRUE)))

  suggestions_written <- readr::read_csv(file.path(pkg_path, "semantic_suggestions.csv"), show_col_types = FALSE)
  expect_true(all(suggestions_written$target_scope == "column"))

  dict_written <- readr::read_csv(file.path(pkg_path, "metadata", "column_dictionary.csv"), show_col_types = FALSE)
  count_row <- dict_written[dict_written$column_name == "count", , drop = FALSE]
  expect_equal(count_row$term_iri[[1]], "https://example.org/term-top")
  expect_equal(count_row$property_iri[[1]], "https://example.org/property-existing")
  expect_true(startsWith(count_row$column_description[[1]], "REVIEW REQUIRED:"))

  tables_written <- readr::read_csv(file.path(pkg_path, "metadata", "tables.csv"), show_col_types = FALSE)
  expect_true(all(startsWith(tables_written$file_name, "data/")))
  expect_true(all(startsWith(tables_written$description, "REVIEW REQUIRED:")))

  dataset_written <- readr::read_csv(file.path(pkg_path, "metadata", "dataset.csv"), show_col_types = FALSE)
  expect_true(startsWith(dataset_written$creator[[1]], "REVIEW REQUIRED:"))
  expect_true(startsWith(dataset_written$contact_name[[1]], "REVIEW REQUIRED:"))
  expect_true(startsWith(dataset_written$contact_email[[1]], "REVIEW REQUIRED:"))
  expect_true(startsWith(dataset_written$license[[1]], "REVIEW REQUIRED:"))
})

test_that("create_sdp limits default code-level semantic seeding to factor columns", {
  resources <- list(
    catches = tibble::tibble(
      run = factor(c("early", "late")),
      station = c("A", "B"),
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
  expect_setequal(unique(seen_codes$column_name), "run")
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
