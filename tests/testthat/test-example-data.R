example_extdata_path <- function(name) {
  installed_path <- system.file("extdata", name, package = "metasalmon")
  if (nzchar(installed_path)) {
    return(installed_path)
  }

  testthat::test_path("..", "..", "inst", "extdata", name)
}

test_that("bundled Fraser coho examples are available and sized as documented", {
  tiny_path <- example_extdata_path("nuseds-fraser-coho-sample.csv")
  fuller_path <- example_extdata_path("nuseds-fraser-coho-2023-2024.csv")
  provenance_path <- example_extdata_path("example-data-README.md")

  expect_true(file.exists(tiny_path))
  expect_true(file.exists(fuller_path))
  expect_true(file.exists(provenance_path))

  tiny <- readr::read_csv(tiny_path, show_col_types = FALSE)
  fuller <- readr::read_csv(fuller_path, show_col_types = FALSE)

  expect_equal(nrow(tiny), 30)
  expect_equal(nrow(fuller), 173)
  expect_equal(range(fuller$ANALYSIS_YR, na.rm = TRUE), c(2023, 2024))
  expect_true("NATURAL_ADULT_SPAWNERS" %in% names(fuller))
})

test_that("create_sdp works with the fuller Fraser coho example", {
  tmp <- withr::local_tempdir()

  fuller_path <- example_extdata_path("nuseds-fraser-coho-2023-2024.csv")
  fraser_coho_fuller <- readr::read_csv(fuller_path, show_col_types = FALSE)

  pkg_path <- create_sdp(
    fraser_coho_fuller,
    path = file.path(tmp, "fraser-coho-fuller"),
    dataset_id = "fraser-coho-2023-2024",
    table_id = "escapement",
    seed_semantics = FALSE,
    overwrite = TRUE
  )

  expect_true(file.exists(file.path(pkg_path, "data", "escapement.csv")))
  expect_true(file.exists(file.path(pkg_path, "metadata", "dataset.csv")))

  dataset_written <- readr::read_csv(
    file.path(pkg_path, "metadata", "dataset.csv"),
    show_col_types = FALSE
  )

  expect_equal(as.character(dataset_written$temporal_start[[1]]), "2023-09-14")
  expect_equal(as.character(dataset_written$temporal_end[[1]]), "2024-12-11")
})
