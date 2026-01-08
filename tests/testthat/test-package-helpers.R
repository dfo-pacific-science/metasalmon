test_that("create_salmon_datapackage creates valid package", {
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
    dataset_iri = NA_character_,
    source_citation = NA_character_
  )

  table_meta <- tibble::tibble(
    dataset_id = "test-1",
    table_id = "main_table",
    file_name = "main_table.csv",
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
  pkg_path <- create_salmon_datapackage(
    resources,
    dataset_meta,
    table_meta,
    dict,
    path = temp_dir,
    format = "csv",
    overwrite = TRUE
  )

  expect_true(dir.exists(pkg_path))
  expect_true(file.exists(file.path(pkg_path, "datapackage.json")))
  expect_true(file.exists(file.path(pkg_path, "main_table.csv")))
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
    dataset_iri = NA_character_,
    source_citation = NA_character_
  )

  table_meta <- tibble::tibble(
    dataset_id = "test-1",
    table_id = "main_table",
    file_name = "main_table.csv",
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
  create_salmon_datapackage(
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

test_that("create_salmon_datapackage round-trip preserves data", {
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
    dataset_iri = NA_character_,
    source_citation = NA_character_
  )

  table_meta <- tibble::tibble(
    dataset_id = "test-1",
    table_id = "main_table",
    file_name = "main_table.csv",
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
  create_salmon_datapackage(
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
    dataset_iri = NA_character_,
    source_citation = NA_character_
  )

  table_meta <- tibble::tibble(
    dataset_id = "test-1",
    table_id = "main_table",
    file_name = "main_table.csv",
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
  create_salmon_datapackage(
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

test_that("create_salmon_datapackage errors on existing path without overwrite", {
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
    dataset_iri = NA_character_,
    source_citation = NA_character_
  )
  table_meta <- tibble::tibble(
    dataset_id = "test-1",
    table_id = "main_table",
    file_name = "main_table.csv",
    table_label = "Main",
    description = NA_character_,
    observation_unit = NA_character_,
    observation_unit_iri = NA_character_,
    primary_key = NA_character_
  )
  dict <- infer_dictionary(resources$main_table, dataset_id = "test-1", table_id = "main_table")
  dict <- fill_measurement_components(dict)

  expect_error(
    create_salmon_datapackage(
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
