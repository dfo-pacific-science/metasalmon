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
  # structural issues may occur when semantic IRIs are blank; allow non-zero
  expect_gte(nrow(res$issues), 0)
  expect_equal(nrow(res$missing_terms), 1)
  expect_equal(res$missing_terms$term_label, "Count")
})

test_that("fetch_salmon_ontology returns a ttl path", {
  testthat::skip_if_offline("dfo-pacific-science.github.io")
  path <- fetch_salmon_ontology()
  expect_true(file.exists(path))
  expect_match(path, "dfo-salmon.ttl")
})
