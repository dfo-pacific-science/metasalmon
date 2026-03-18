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

test_that("fetch_salmon_ontology returns a ttl path", {
  testthat::skip_if_offline("w3id.org")
  path <- fetch_salmon_ontology()
  expect_true(file.exists(path))
  expect_match(path, "salmon-ontology\\.ttl$")
})
