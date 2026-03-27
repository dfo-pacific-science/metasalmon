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
  fuller_dict_path <- example_extdata_path("nuseds-fraser-coho-2023-2024-column_dictionary.csv")
  provenance_path <- example_extdata_path("example-data-README.md")

  expect_true(file.exists(tiny_path))
  expect_true(file.exists(fuller_path))
  expect_true(file.exists(fuller_dict_path))
  expect_true(file.exists(provenance_path))

  tiny <- readr::read_csv(tiny_path, show_col_types = FALSE)
  fuller <- readr::read_csv(fuller_path, show_col_types = FALSE)
  fuller_dict <- readr::read_csv(fuller_dict_path, show_col_types = FALSE)

  expect_equal(nrow(tiny), 30)
  expect_equal(nrow(fuller), 173)
  expect_equal(range(fuller$ANALYSIS_YR, na.rm = TRUE), c(2023, 2024))
  expect_true("NATURAL_ADULT_SPAWNERS" %in% names(fuller))
  expect_setequal(fuller_dict$column_name, names(fuller))
})

test_that("infer_dictionary treats key fuller Fraser coho fields as measurement/temporal", {
  fuller_path <- example_extdata_path("nuseds-fraser-coho-2023-2024.csv")
  fraser_coho_fuller <- readr::read_csv(fuller_path, show_col_types = FALSE)

  dict <- infer_dictionary(
    fraser_coho_fuller,
    dataset_id = "fraser-coho-2023-2024",
    table_id = "escapement",
    seed_semantics = FALSE
  )

  expect_equal(dict$column_role[dict$column_name == "ANALYSIS_YR"], "temporal")
  expect_equal(dict$column_role[dict$column_name == "NATURAL_ADULT_SPAWNERS"], "measurement")
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

test_that("Fraser coho example uses improved deterministic semantic queries for known problem fields", {
  dict_path <- example_extdata_path("nuseds-fraser-coho-2023-2024-column_dictionary.csv")
  fuller_path <- example_extdata_path("nuseds-fraser-coho-2023-2024.csv")
  dict <- dplyr::filter(
    readr::read_csv(dict_path, show_col_types = FALSE),
    .data$column_name %in% c("ESTIMATE_CLASSIFICATION", "NATURAL_ADULT_SPAWNERS")
  )
  fuller <- readr::read_csv(fuller_path, show_col_types = FALSE)
  codes <- dplyr::filter(
    infer_codes_from_resources(list(escapement = fuller), dataset_id = "fraser-coho-2023-2024"),
    .data$column_name == "ESTIMATE_CLASSIFICATION"
  )

  suggestions <- suggest_semantics(
    NULL,
    dict,
    codes = codes,
    sources = "ols",
    max_per_role = 1,
    search_fn = function(query, role, sources) {
      tibble::tibble(
        label = paste("candidate", role),
        iri = paste0("https://example.org/", role),
        source = "ols",
        ontology = "demo",
        role = role,
        match_type = "label_partial",
        definition = ""
      )
    }
  )
  semantic_suggestions <- attr(suggestions, "semantic_suggestions")

  estimate <- dplyr::filter(
    semantic_suggestions,
    .data$column_name == "ESTIMATE_CLASSIFICATION",
    .data$target_scope == "column",
    .data$target_sdp_field == "term_iri"
  )
  expect_equal(unique(estimate$search_role), "constraint")
  expect_equal(unique(estimate$search_query), "abundance data type")

  natural_entity <- dplyr::filter(
    semantic_suggestions,
    .data$column_name == "NATURAL_ADULT_SPAWNERS",
    .data$target_scope == "column",
    .data$target_sdp_field == "entity_iri"
  )
  expect_equal(unique(natural_entity$search_query), "population")

  natural_constraint <- dplyr::filter(
    semantic_suggestions,
    .data$column_name == "NATURAL_ADULT_SPAWNERS",
    .data$target_scope == "column",
    .data$target_sdp_field == "constraint_iri"
  )
  expect_equal(unique(natural_constraint$search_query), "natural origin")
})
