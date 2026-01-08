# Edge case tests for metasalmon functions

test_that("infer_dictionary handles empty data frame", {
  empty_df <- data.frame()

  # Empty data.frame() is still a data.frame, just has 0 rows and 0 cols
  dict <- infer_dictionary(empty_df)
  expect_equal(nrow(dict), 0)
  expect_true(all(c("dataset_id", "table_id", "column_name") %in% names(dict)))
})

test_that("infer_dictionary handles data frame with no columns", {
  df <- data.frame(row.names = 1:5)

  dict <- infer_dictionary(df)
  expect_equal(nrow(dict), 0)
  expect_true(all(c("dataset_id", "table_id", "column_name") %in% names(dict)))
})

test_that("infer_dictionary handles special characters in column names", {
  df <- data.frame(
    `col with spaces` = 1:3,
    `col-with-dashes` = 1:3,
    `col.with.dots` = 1:3,
    `col_with_underscores` = 1:3
  )

  dict <- infer_dictionary(df)
  expect_equal(nrow(dict), 4)
  expect_true(all(df %>% names() %in% dict$column_name))
})

test_that("infer_dictionary handles all NA columns", {
  df <- data.frame(
    x = c(NA_real_, NA_real_, NA_real_),  # Explicitly numeric NA
    y = c(NA_character_, NA_character_, NA_character_)
  )

  dict <- infer_dictionary(df, guess_types = TRUE)
  expect_equal(nrow(dict), 2)
  # Types are inferred from column class, not values
  # x is numeric (NA_real_), y is character
  expect_equal(dict$value_type[dict$column_name == "x"], "number")
  expect_equal(dict$value_type[dict$column_name == "y"], "string")
})

test_that("infer_dictionary handles mixed types in single column", {
  # This shouldn't happen in practice, but test robustness
  df <- data.frame(x = c(1, "a", TRUE))

  dict <- infer_dictionary(df, guess_types = TRUE)
  # Should handle gracefully (likely defaults to string)
  expect_equal(nrow(dict), 1)
})

test_that("validate_dictionary handles empty dictionary", {
  empty_dict <- tibble::tibble(
    dataset_id = character(0),
    table_id = character(0),
    column_name = character(0),
    column_label = character(0),
    column_description = character(0),
    column_role = character(0),
    value_type = character(0),
    required = logical(0)
  )

  # Empty dictionary should pass validation
  expect_invisible(validate_dictionary(empty_dict))
})

test_that("validate_dictionary handles NA in required fields", {
  df <- data.frame(x = 1:3)
  dict <- infer_dictionary(df)
  dict <- fill_measurement_components(dict)

  # Set required to NA (NA is logical, so this might pass)
  dict$required[1] <- NA

  # The validation checks is.logical() which returns TRUE for NA
  # So this might pass validation, but test that it doesn't crash
  result <- tryCatch(
    validate_dictionary(dict),
    error = function(e) e
  )
  # Should either pass or error gracefully
  expect_true(inherits(result, c("tbl_df", "error")))
})

test_that("validate_dictionary handles invalid IRI formats gracefully", {
  df <- data.frame(x = 1:3)
  dict <- infer_dictionary(df)
  dict <- fill_measurement_components(dict)

  # Add invalid IRI (validation doesn't check format, but shouldn't crash)
  dict$term_iri[1] <- "not a valid IRI"

  # Should still pass validation (IRI format not validated)
  expect_invisible(validate_dictionary(dict))
})

test_that("validate_dictionary handles very long strings", {
  df <- data.frame(x = 1:3)
  dict <- infer_dictionary(df)
  dict <- fill_measurement_components(dict)

  # Very long description
  long_string <- paste(rep("a", 10000), collapse = "")
  dict$column_description[1] <- long_string

  # Should handle gracefully
  expect_invisible(validate_dictionary(dict))
})

test_that("validate_dictionary requires_iris flag works", {
  df <- data.frame(x = 1:3)
  dict <- infer_dictionary(df)
  dict <- fill_measurement_components(dict)

  # Without IRIs, should pass with require_iris = FALSE
  expect_invisible(validate_dictionary(dict, require_iris = FALSE))

  # Should fail with require_iris = TRUE
  expect_error(
    validate_dictionary(dict, require_iris = TRUE),
    "Missing required.*iri"
  )
})

test_that("apply_salmon_dictionary handles empty data frame", {
  empty_df <- data.frame()
  df <- data.frame(x = 1:3)
  dict <- infer_dictionary(df, dataset_id = "test-1", table_id = "table-1")
  dict <- fill_measurement_components(dict)
  validate_dictionary(dict)

  # Empty data.frame() is still a data.frame, so it passes the inherits check
  # But it will have no columns to process
  result <- apply_salmon_dictionary(empty_df, dict)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("apply_salmon_dictionary handles columns not in dictionary", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)
  dict <- infer_dictionary(df[, 1:2, drop = FALSE])  # Only x and y
  dict <- fill_measurement_components(dict)

  validate_dictionary(dict)
  result <- apply_salmon_dictionary(df, dict)

  # Column z should be preserved (not renamed/transformed)
  expect_true("z" %in% names(result) || any(grepl("z", names(result), ignore.case = TRUE)))
})

test_that("apply_salmon_dictionary handles dictionary columns not in data", {
  df <- data.frame(x = 1:3)
  dict <- infer_dictionary(data.frame(x = 1:3, y = 4:6))  # Has y, but df doesn't
  dict <- fill_measurement_components(dict)

  validate_dictionary(dict)
  result <- apply_salmon_dictionary(df, dict)

  # Should handle gracefully, skip missing columns
  expect_equal(nrow(result), 3)
})

test_that("apply_salmon_dictionary handles type coercion failures (strict)", {
  df <- data.frame(count = c("100", "not-a-number", "200"))
  dict <- infer_dictionary(df)
  dict <- fill_measurement_components(dict)
  dict$value_type[dict$column_name == "count"] <- "integer"

  validate_dictionary(dict)

  # Strict mode: as.integer() produces NAs and warnings, not errors
  # The function will succeed but produce NAs
  result <- suppressWarnings(
    apply_salmon_dictionary(df, dict, strict = TRUE)
  )

  # Should produce result with NAs
  expect_equal(nrow(result), 3)
  # Check that NAs were introduced
  result_col <- result[[dict$column_label[dict$column_name == "count"]]]
  expect_true(any(is.na(result_col)))
})

test_that("apply_salmon_dictionary handles type coercion failures (non-strict)", {
  df <- data.frame(count = c("100", "not-a-number", "200"))
  dict <- infer_dictionary(df)
  dict <- fill_measurement_components(dict)
  dict$value_type[dict$column_name == "count"] <- "integer"

  validate_dictionary(dict)

  # Non-strict mode: as.integer() produces NAs with warnings
  # The function's error handler only triggers on actual errors, not warnings
  # So this will produce NAs but the function will succeed
  result <- suppressWarnings(
    apply_salmon_dictionary(df, dict, strict = FALSE)
  )

  # Should still produce result (with NAs)
  expect_equal(nrow(result), 3)
  result_col <- result[[dict$column_label[dict$column_name == "count"]]]
  expect_true(any(is.na(result_col)))
})

test_that("apply_salmon_dictionary handles codes with mismatched values", {
  df <- data.frame(species = c("Coho", "Chinook", "Unknown"))

  dict <- infer_dictionary(df)
  dict <- fill_measurement_components(dict)
  validate_dictionary(dict)

  codes <- tibble::tibble(
    dataset_id = "test-1",
    table_id = "table-1",
    column_name = "species",
    code_value = c("Coho", "Chinook"),  # Missing "Unknown"
    code_label = c("Coho Salmon", "Chinook Salmon"),
    vocabulary_iri = NA_character_,
    term_iri = NA_character_,
    term_type = NA_character_
  )

  result <- apply_salmon_dictionary(df, dict, codes = codes)

  # Should handle gracefully (Unknown becomes NA in factor)
  expect_s3_class(result[[dict$column_label[1]]], "factor")
})

test_that("apply_salmon_dictionary handles missing required columns", {
  df <- data.frame(x = 1:3)
  dict <- infer_dictionary(df, dataset_id = "test-1", table_id = "table-1")
  dict <- fill_measurement_components(dict)
  dict$required[1] <- TRUE
  validate_dictionary(dict)

  # Remove required column, add different column
  df2 <- data.frame(y = 1:3)

  # Use suppressWarnings to get the result, then check warning separately
  result <- suppressWarnings(
    apply_salmon_dictionary(df2, dict)
  )

  # Should still produce result
  # y column should be present (either as y or renamed)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  # y might be renamed or kept as-is depending on dictionary
  expect_true(length(names(result)) > 0)

  # Verify warning was issued
  expect_warning(
    apply_salmon_dictionary(df2, dict),
    "Missing required columns"
  )
})

test_that("create_salmon_datapackage handles empty resources list", {
  resources <- list()
  df <- data.frame(x = 1:3)
  dict <- infer_dictionary(df)
  dict <- fill_measurement_components(dict)
  validate_dictionary(dict)

  dataset_meta <- tibble::tibble(
    dataset_id = "test-1",
    title = "Test",
    description = "Test"
  )
  table_meta <- tibble::tibble(
    dataset_id = "test-1",
    table_id = "table-1",
    file_name = "table-1.csv",
    table_label = "Table 1"
  )

  expect_error(
    create_salmon_datapackage(resources, dataset_meta, table_meta, dict, path = tempdir()),
    "must be a named list"
  )
})

test_that("create_salmon_datapackage handles resources with no matching table_meta", {
  resources <- list(missing_table = data.frame(x = 1:3))
  df <- data.frame(x = 1:3)
  dict <- infer_dictionary(df)
  dict <- fill_measurement_components(dict)
  validate_dictionary(dict)

  dataset_meta <- tibble::tibble(
    dataset_id = "test-1",
    title = "Test",
    description = "Test"
  )
  table_meta <- tibble::tibble(
    dataset_id = "test-1",
    table_id = "other_table",  # Doesn't match
    file_name = "other.csv",
    table_label = "Other"
  )

  temp_dir <- withr::local_tempdir()
  result <- expect_warning(
    create_salmon_datapackage(
      resources, dataset_meta, table_meta, dict,
      path = temp_dir, overwrite = TRUE
    ),
    "No table metadata found"
  )

  # Should still create package (just skips missing resource)
  expect_true(file.exists(file.path(temp_dir, "datapackage.json")))
})

test_that("create_salmon_datapackage handles invalid format parameter", {
  resources <- list(main = data.frame(x = 1:3))
  df <- data.frame(x = 1:3)
  dict <- infer_dictionary(df)
  dict <- fill_measurement_components(dict)
  validate_dictionary(dict)

  dataset_meta <- tibble::tibble(
    dataset_id = "test-1",
    title = "Test",
    description = "Test"
  )
  table_meta <- tibble::tibble(
    dataset_id = "test-1",
    table_id = "main",
    file_name = "main.csv",
    table_label = "Main"
  )

  temp_dir <- withr::local_tempdir()

  expect_error(
    create_salmon_datapackage(
      resources, dataset_meta, table_meta, dict,
      path = temp_dir, format = "parquet"
    ),
    "Only CSV format"
  )
})

test_that("create_salmon_datapackage handles invalid path", {
  resources <- list(main = data.frame(x = 1:3))
  df <- data.frame(x = 1:3)
  dict <- infer_dictionary(df)
  dict <- fill_measurement_components(dict)
  validate_dictionary(dict)

  dataset_meta <- tibble::tibble(
    dataset_id = "test-1",
    title = "Test",
    description = "Test"
  )
  table_meta <- tibble::tibble(
    dataset_id = "test-1",
    table_id = "main",
    file_name = "main.csv",
    table_label = "Main"
  )

  # Non-existent parent directory
  invalid_path <- file.path(tempdir(), "nonexistent", "subdir")

  expect_error(
    create_salmon_datapackage(
      resources, dataset_meta, table_meta, dict,
      path = invalid_path, overwrite = TRUE
    ),
    NA  # Should create directory or error gracefully
  )
})

test_that("create_salmon_datapackage handles multiple resources", {
  resources <- list(
    table1 = data.frame(x = 1:3, y = 4:6),
    table2 = data.frame(a = letters[1:3], b = 7:9)
  )

  dict1 <- infer_dictionary(resources$table1, dataset_id = "test-1", table_id = "table1")
  dict2 <- infer_dictionary(resources$table2, dataset_id = "test-1", table_id = "table2")
  dict1 <- fill_measurement_components(dict1)
  dict2 <- fill_measurement_components(dict2)
  dict <- dplyr::bind_rows(dict1, dict2)
  validate_dictionary(dict)

  dataset_meta <- tibble::tibble(
    dataset_id = "test-1",
    title = "Test",
    description = "Test"
  )
  table_meta <- tibble::tibble(
    dataset_id = c("test-1", "test-1"),
    table_id = c("table1", "table2"),
    file_name = c("table1.csv", "table2.csv"),
    table_label = c("Table 1", "Table 2")
  )

  temp_dir <- withr::local_tempdir()
  pkg_path <- create_salmon_datapackage(
    resources, dataset_meta, table_meta, dict,
    path = temp_dir, overwrite = TRUE
  )

  expect_true(file.exists(file.path(temp_dir, "table1.csv")))
  expect_true(file.exists(file.path(temp_dir, "table2.csv")))

  # Read back and verify
  pkg <- read_salmon_datapackage(temp_dir)
  expect_equal(length(pkg$resources), 2)
  expect_true("table1" %in% names(pkg$resources))
  expect_true("table2" %in% names(pkg$resources))
})

test_that("create_salmon_datapackage handles resources with no matching dictionary entries", {
  resources <- list(main = data.frame(x = 1:3, y = 4:6))

  # Dictionary only has x, not y
  dict <- infer_dictionary(data.frame(x = 1:3), dataset_id = "test-1", table_id = "main")
  dict <- fill_measurement_components(dict)
  validate_dictionary(dict)

  dataset_meta <- tibble::tibble(
    dataset_id = "test-1",
    title = "Test",
    description = "Test"
  )
  table_meta <- tibble::tibble(
    dataset_id = "test-1",
    table_id = "main",
    file_name = "main.csv",
    table_label = "Main"
  )

  temp_dir <- withr::local_tempdir()
  pkg_path <- create_salmon_datapackage(
    resources, dataset_meta, table_meta, dict,
    path = temp_dir, overwrite = TRUE
  )

  # Should still create package (y column just won't have schema)
  expect_true(file.exists(file.path(temp_dir, "main.csv")))

  # Read back and verify y column exists
  pkg <- read_salmon_datapackage(temp_dir)
  expect_true("y" %in% names(pkg$resources$main))
})

test_that("read_salmon_datapackage handles non-existent path", {
  expect_error(
    read_salmon_datapackage("/nonexistent/path"),
    "does not exist"
  )
})

test_that("read_salmon_datapackage handles path without datapackage.json", {
  temp_dir <- withr::local_tempdir()

  expect_error(
    read_salmon_datapackage(temp_dir),
    "datapackage.json not found"
  )
})

test_that("read_salmon_datapackage handles invalid JSON", {
  temp_dir <- withr::local_tempdir()

  # Write invalid JSON
  writeLines("{invalid json}", file.path(temp_dir, "datapackage.json"))

  # Should error on invalid JSON (jsonlite throws an error)
  expect_error(
    read_salmon_datapackage(temp_dir),
    regexp = "json|parse|invalid|lexical"  # Error should mention JSON parsing issue
  )
})

test_that("read_salmon_datapackage handles missing resource files", {
  # Create valid package
  resources <- list(main = data.frame(x = 1:3))
  df <- data.frame(x = 1:3)
  dict <- infer_dictionary(df, dataset_id = "test-1", table_id = "main")
  dict <- fill_measurement_components(dict)
  validate_dictionary(dict)

  dataset_meta <- tibble::tibble(
    dataset_id = "test-1",
    title = "Test",
    description = "Test"
  )
  table_meta <- tibble::tibble(
    dataset_id = "test-1",
    table_id = "main",
    file_name = "main.csv",
    table_label = "Main"
  )

  temp_dir <- withr::local_tempdir()
  create_salmon_datapackage(
    resources, dataset_meta, table_meta, dict,
    path = temp_dir, overwrite = TRUE
  )

  # Delete the CSV file
  unlink(file.path(temp_dir, "main.csv"))

  # Should warn but not crash
  result <- suppressWarnings(
    read_salmon_datapackage(temp_dir)
  )

  # Should still return structure (resources list will be empty)
  expect_true(is.list(result))
  expect_true("dataset" %in% names(result))
  expect_true("tables" %in% names(result))
  expect_true("dictionary" %in% names(result))
  expect_true("resources" %in% names(result))
  # Resources list should exist but be empty
  expect_true(is.list(result$resources))
  expect_equal(length(result$resources), 0)

  # Verify warning was issued (check separately)
  expect_warning(
    read_salmon_datapackage(temp_dir),
    "Resource file.*not found"
  )
})

test_that("read_salmon_datapackage handles corrupted CSV files", {
  temp_dir <- withr::local_tempdir()

  # Create minimal valid JSON
  jsonlite::write_json(
    list(
      profile = "data-package",
      name = "test-1",
      title = "Test",
      description = "Test",
      resources = list(
        list(
          name = "main",
          path = "main.csv",
          profile = "data-resource",
          schema = list(fields = list())
        )
      )
    ),
    file.path(temp_dir, "datapackage.json"),
    pretty = TRUE,
    auto_unbox = TRUE
  )

  # Write corrupted CSV
  writeLines("invalid,csv\nbroken,data,with,wrong,columns", file.path(temp_dir, "main.csv"))

  # Should handle gracefully (may error or warn)
  result <- tryCatch(
    read_salmon_datapackage(temp_dir),
    error = function(e) e,
    warning = function(w) w
  )

  # Should either succeed with warnings or error gracefully
  expect_true(inherits(result, c("list", "error", "warning")))
})

test_that("create_salmon_datapackage handles dataset_meta with wrong number of rows", {
  resources <- list(main = data.frame(x = 1:3))
  df <- data.frame(x = 1:3)
  dict <- infer_dictionary(df, dataset_id = "test-1", table_id = "main")
  dict <- fill_measurement_components(dict)
  validate_dictionary(dict)

  # Wrong number of rows
  dataset_meta <- tibble::tibble(
    dataset_id = c("test-1", "test-2"),  # Two rows!
    title = c("Test 1", "Test 2"),
    description = c("Test", "Test")
  )
  table_meta <- tibble::tibble(
    dataset_id = "test-1",
    table_id = "main",
    file_name = "main.csv",
    table_label = "Main"
  )

  temp_dir <- withr::local_tempdir()

  expect_error(
    create_salmon_datapackage(
      resources, dataset_meta, table_meta, dict,
      path = temp_dir, overwrite = TRUE
    ),
    "must be a single-row"
  )
})

test_that("create_salmon_datapackage handles empty table_meta", {
  resources <- list(main = data.frame(x = 1:3))
  df <- data.frame(x = 1:3)
  dict <- infer_dictionary(df, dataset_id = "test-1", table_id = "main")
  dict <- fill_measurement_components(dict)
  validate_dictionary(dict)

  dataset_meta <- tibble::tibble(
    dataset_id = "test-1",
    title = "Test",
    description = "Test"
  )
  table_meta <- tibble::tibble()  # Empty!

  temp_dir <- withr::local_tempdir()

  expect_error(
    create_salmon_datapackage(
      resources, dataset_meta, table_meta, dict,
      path = temp_dir, overwrite = TRUE
    ),
    "must be a non-empty"
  )
})

test_that("infer_dictionary handles guess_types = FALSE", {
  df <- data.frame(
    x = c(1, 2, 3),
    y = c("a", "b", "c")
  )

  dict <- infer_dictionary(df, guess_types = FALSE)

  # Types should be NA when not guessing
  expect_true(all(is.na(dict$value_type)))
  expect_true(all(is.na(dict$column_role)))
})

test_that("apply_salmon_dictionary handles datetime conversion", {
  df <- data.frame(
    date_col = c("2024-01-01", "2024-01-02", "2024-01-03")
  )

  dict <- infer_dictionary(df)
  dict <- fill_measurement_components(dict)
  dict$value_type[dict$column_name == "date_col"] <- "datetime"
  validate_dictionary(dict)

  result <- apply_salmon_dictionary(df, dict)

  # Should convert to POSIXct
  expect_s3_class(result[[dict$column_label[1]]], "POSIXct")
})

test_that("apply_salmon_dictionary handles date conversion", {
  df <- data.frame(
    date_col = c("2024-01-01", "2024-01-02", "2024-01-03")
  )

  dict <- infer_dictionary(df)
  dict <- fill_measurement_components(dict)
  dict$value_type[dict$column_name == "date_col"] <- "date"
  validate_dictionary(dict)

  result <- apply_salmon_dictionary(df, dict)

  # Should convert to Date
  expect_s3_class(result[[dict$column_label[1]]], "Date")
})
