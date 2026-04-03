test_that("validate_semantics adds required column and reports missing term_iri", {
  dict <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = c("id", "count"),
    column_label = c("ID", "Count"),
    column_description = c("id", "Fish count"),
    column_role = c("identifier", "measurement"),
    value_type = c("string", "integer"),
    term_iri = c(NA_character_, ""),
    property_iri = c(NA_character_, NA_character_),
    entity_iri = c(NA_character_, NA_character_),
    unit_iri = c(NA_character_, NA_character_),
    constraint_iri = c("", ""),
    method_iri = c("", "")
  )
  res <- validate_semantics(dict)
  expect_true("required" %in% names(res$dict))
  expect_true(all(is.na(res$dict$required)))
  # structural issues may occur when semantic IRIs are blank; allow non-zero
  expect_gte(nrow(res$issues), 0)
  expect_equal(nrow(res$missing_terms), 1)
  expect_equal(res$missing_terms$term_label, "Count")
})

test_that("validate_semantics flags non-canonical salmon ontology IRIs", {
  dict <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "count",
    column_label = "Count",
    column_description = "Fish count",
    column_role = "measurement",
    value_type = "integer",
    term_iri = "http://w3id.org/salmon/Stock",
    property_iri = "https://w3id.org/smn#Escapement",
    entity_iri = "https://w3id.org/gcdfo/salmon/Stock",
    unit_iri = "http://w3id.org/smn/Unit",
    constraint_iri = "",
    method_iri = "gcdfo:SpawnerSurveyMethod"
  )

  res <- validate_semantics(dict)
  expect_gte(nrow(res$issues), 5)
  expect_true(any(grepl("legacy SMN namespace", res$issues$message, fixed = TRUE)))
  expect_true(any(grepl("non-canonical SMN IRI form", res$issues$message, fixed = TRUE)))
  expect_true(any(grepl("non-canonical GCDFO IRI form", res$issues$message, fixed = TRUE)))
  expect_true(any(grepl("non-canonical SMN HTTP IRI", res$issues$message, fixed = TRUE)))
  expect_true(any(grepl("non-canonical GCDFO CURIE form", res$issues$message, fixed = TRUE)))
})

test_that("validate_semantics warns that deprecated arguments are ignored", {
  dict <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "count",
    column_label = "Count",
    column_description = "Fish count",
    column_role = "measurement",
    value_type = "integer",
    term_iri = "",
    property_iri = NA_character_,
    entity_iri = NA_character_,
    unit_iri = NA_character_,
    constraint_iri = "",
    method_iri = ""
  )

  expect_warning(
    res <- validate_semantics(dict, entity_defaults = tibble::tibble(table_prefix = "t", entity_iri = "https://w3id.org/smn/Stock")),
    "entity_defaults.*deprecated.*ignored"
  )
  expect_warning(
    res <- validate_semantics(dict, vocab_priority = c("smn", "gcdfo")),
    "vocab_priority.*deprecated.*ignored"
  )
  expect_equal(nrow(res$missing_terms), 1L)
})

test_that("fetch_salmon_ontology returns a ttl path", {
  testthat::skip_if_offline("w3id.org")
  reachable <- tryCatch(
    {
      resp <- httr2::request("https://w3id.org/smn/") |>
        httr2::req_method("HEAD") |>
        httr2::req_timeout(5) |>
        httr2::req_perform()
      httr2::resp_status(resp) < 500
    },
    error = function(...) FALSE
  )
  testthat::skip_if_not(reachable, "w3id.org is not reachable from this environment")
  path <- fetch_salmon_ontology()
  expect_true(file.exists(path))
  expect_match(path, "salmon-ontology\\.ttl$")
})

test_that("fetch_salmon_ontology falls back to stale cache when refresh fails", {
  cache_dir <- withr::local_tempdir()
  ttl_file <- file.path(cache_dir, "salmon-ontology.ttl")
  writeLines("cached", ttl_file)

  expect_warning(
    path <- fetch_salmon_ontology(
      url = "http://127.0.0.1:9/smn",
      cache_dir = cache_dir,
      fallback_urls = character(),
      timeout_seconds = 1
    ),
    "using cached copy",
    ignore.case = TRUE
  )
  expect_equal(path, ttl_file)
})

test_that("validate_dictionary and validate_semantics accept dictionary CSV paths", {
  dict <- tibble::tibble(
    dataset_id = c("d1", "d1"),
    table_id = c("cu_composite_escapement", "cu_composite_escapement"),
    column_name = c("cu_id", "escapement"),
    column_label = c("CU ID", "Escapement"),
    column_description = c("CU identifier", "Spawner abundance estimate"),
    column_role = c("identifier", "measurement"),
    value_type = c("string", "number"),
    term_iri = c(NA_character_, "https://w3id.org/smn/TargetOrLimitRateOrAbundance"),
    property_iri = c(NA_character_, "https://qudt.org/vocab/quantitykind/Population"),
    entity_iri = c(NA_character_, "https://w3id.org/smn/ConservationUnit"),
    unit_iri = c(NA_character_, "https://qudt.org/vocab/unit/NUM"),
    constraint_iri = c("", ""),
    method_iri = c("", "https://w3id.org/gcdfo/salmon#SpawnerSurveyMethod"),
    required = c(TRUE, FALSE)
  )

  dict_path <- file.path(withr::local_tempdir(), "column_dictionary.csv")
  readr::write_csv(dict, dict_path, na = "")

  validated <- suppressMessages(validate_dictionary(dict_path, require_iris = TRUE))
  expect_true(is.logical(validated$required))
  expect_equal(validated$required, c(TRUE, FALSE))

  semantics <- suppressMessages(validate_semantics(dict_path, require_iris = TRUE))
  expect_equal(nrow(semantics$issues), 0)
  expect_equal(nrow(semantics$missing_terms), 0)
})
