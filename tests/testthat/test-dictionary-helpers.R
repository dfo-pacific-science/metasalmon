test_that("infer_dictionary creates valid structure", {
  df <- data.frame(
    species = c("Coho", "Chinook"),
    count = c(100L, 200L),
    date = as.Date(c("2024-01-01", "2024-01-02")),
    is_active = c(TRUE, FALSE)
  )

  dict <- infer_dictionary(df, dataset_id = "test-1", table_id = "table-1")

  expect_s3_class(dict, "tbl_df")
  expect_equal(nrow(dict), 4)
  expect_equal(ncol(dict), 16)

  # Check required columns exist
  required_cols <- c(
    "dataset_id", "table_id", "column_name", "column_label",
    "column_description", "column_role", "value_type", "required"
  )
  expect_true(all(required_cols %in% names(dict)))
  expect_true(all(c("property_iri", "entity_iri", "constraint_iri", "method_iri") %in% names(dict)))

  # Check inferred types
  expect_equal(dict$value_type[dict$column_name == "count"], "integer")
  expect_equal(dict$value_type[dict$column_name == "species"], "string")
  expect_equal(dict$value_type[dict$column_name == "date"], "date")
  expect_equal(dict$value_type[dict$column_name == "is_active"], "boolean")
})

test_that("infer_dictionary can seed semantic suggestions", {
  fake_suggest <- function(df, dict, sources = c("ols", "nvs"), max_per_role = 1, include_dwc = FALSE, ...) {
    expect_equal(sources, c("ols", "nvs", "qudt"))
    expect_equal(max_per_role, 1)
    attr(dict, "semantic_suggestions") <- tibble::tibble(
      column_name = c("count"),
      dictionary_role = c("variable"),
      label = c("Count"),
      iri = c("https://example.org/count"),
      source = c("ols"),
      ontology = c("demo"),
      definition = c(NA_character_)
    )
    dict
  }

  with_mocked_bindings(
    suggest_semantics = fake_suggest,
    {
      dict <- infer_dictionary(
        data.frame(count = c(1L, 2L), species = c("Coho", "Chinook")),
        seed_semantics = TRUE,
        semantic_sources = c("ols", "nvs", "qudt"),
        seed_verbose = FALSE
      )
      sugg <- attr(dict, "semantic_suggestions")
      expect_s3_class(sugg, "tbl_df")
      expect_equal(nrow(sugg), 1)
      expect_equal(sugg$iri, "https://example.org/count")
    }
  )
})

test_that("suggest_semantics attaches empty suggestions when sources disabled", {
  dict <- tibble::tibble(
    dataset_id = "test",
    table_id = "t1",
    column_name = "value",
    column_label = "Spawner abundance",
    column_description = "Spawner abundance estimate",
    column_role = "measurement",
    value_type = "number",
    unit_label = NA_character_,
    unit_iri = NA_character_,
    term_iri = NA_character_,
    property_iri = NA_character_,
    entity_iri = NA_character_,
    constraint_iri = NA_character_,
    method_iri = NA_character_
  )

  res <- suggest_semantics(NULL, dict, sources = character(0))
  expect_equal(res$column_name, dict$column_name)
  suggestions <- attr(res, "semantic_suggestions")
  expect_s3_class(suggestions, "tbl_df")
  expect_equal(nrow(suggestions), 0)
})

test_that("suggest_semantics captures suggestions with dictionary_role and column_name", {
  dict <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "value",
    column_label = "Spawner abundance",
    column_description = "Spawner abundance estimate",
    column_role = "measurement",
    value_type = "number",
    unit_label = NA_character_,
    unit_iri = NA_character_,
    term_iri = NA_character_,
    property_iri = NA_character_,
    entity_iri = NA_character_,
    constraint_iri = NA_character_,
    method_iri = NA_character_
  )

  fake_search <- function(query, role, sources) {
    tibble::tibble(
      label = c("Option A", "Option B"),
      iri = c("http://example.org/a", "http://example.org/b"),
      source = c("ols", "ols"),
      ontology = c("demo", "demo"),
      role = role,
      match_type = "",
      definition = ""
    )
  }

  res <- suggest_semantics(NULL, dict, sources = "ols", max_per_role = 1, search_fn = fake_search)
  suggestions <- attr(res, "semantic_suggestions")
  expect_equal(nrow(suggestions), 6) # variable/property/entity/unit/constraint/method
  expect_true(all(c("dataset_id", "table_id", "dictionary_role", "column_name") %in% names(suggestions)))
  expect_true(all(suggestions$dictionary_role %in% c("variable", "property", "entity", "unit", "constraint", "method")))
  expect_true(all(suggestions$column_name == "value"))
  expect_true(all(suggestions$dataset_id == "d1"))
  expect_true(all(suggestions$table_id == "t1"))

  res_dwc <- suggest_semantics(NULL, dict, sources = "ols", max_per_role = 1, search_fn = fake_search, include_dwc = TRUE)
  dwc_map <- attr(res_dwc, "dwc_mappings")
  expect_true(is.data.frame(dwc_map))
})

test_that("suggest_semantics uses role-specific hints when available", {
  queries <- list()
  fake_search <- function(query, role, sources = NULL) {
    queries <<- append(queries, list(list(query = query, role = role)))
    tibble::tibble(
      label = "x",
      iri = "y",
      source = "ols",
      ontology = "",
      role = role,
      match_type = "",
      definition = ""
    )
  }

  dict <- tibble::tibble(
    dataset_id = "d",
    table_id = "t",
    column_name = "MEAS",
    column_label = "Spawner count",
    column_description = "Spawner count estimate",
    column_role = "measurement",
    value_type = "number",
    unit_label = "fish",
    unit_iri = NA_character_,
    term_iri = NA_character_,
    property_iri = NA_character_,
    entity_iri = NA_character_,
    constraint_iri = NA_character_,
    method_iri = NA_character_
  )

  suggest_semantics(NULL, dict, sources = "ols", max_per_role = 1, search_fn = fake_search)
  unit_queries <- purrr::map_chr(queries, "query")
  expect_true(any(unit_queries == "fish"))
})

test_that("apply_semantic_suggestions matches by column_name and dictionary_role", {
  dict <- tibble::tibble(
    dataset_id = c("d1", "d1"),
    table_id = c("t1", "t1"),
    column_name = c("count_a", "count_b"),
    column_label = c("Count A", "Count B"),
    column_description = c("Spawner count A", "Spawner count B"),
    column_role = c("measurement", "measurement"),
    value_type = c("number", "number"),
    unit_label = c(NA_character_, NA_character_),
    unit_iri = c(NA_character_, NA_character_),
    term_iri = c(NA_character_, NA_character_),
    term_type = c(NA_character_, NA_character_),
    required = c(FALSE, FALSE),
    property_iri = c(NA_character_, NA_character_),
    entity_iri = c(NA_character_, NA_character_),
    constraint_iri = c(NA_character_, NA_character_),
    method_iri = c(NA_character_, NA_character_)
  )

  suggestions <- tibble::tibble(
    dataset_id = c("d1", "d1", "d1", "d1"),
    table_id = c("t1", "t1", "t1", "t1"),
    column_name = c("count_b", "count_a", "count_b", "count_a"),
    dictionary_role = c("variable", "variable", "property", "property"),
    iri = c(
      "https://example.org/variable-b",
      "https://example.org/variable-a",
      "https://example.org/property-b",
      "https://example.org/property-a"
    ),
    score = c(10, 9, 8, 7)
  )

  out <- apply_semantic_suggestions(dict, suggestions = suggestions, verbose = FALSE)

  expect_equal(out$term_iri[out$column_name == "count_a"], "https://example.org/variable-a")
  expect_equal(out$term_iri[out$column_name == "count_b"], "https://example.org/variable-b")
  expect_equal(out$property_iri[out$column_name == "count_a"], "https://example.org/property-a")
  expect_equal(out$property_iri[out$column_name == "count_b"], "https://example.org/property-b")
})

test_that("apply_semantic_suggestions fills only missing fields unless overwrite is TRUE", {
  dict <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "count",
    column_label = "Count",
    column_description = "Spawner count",
    column_role = "measurement",
    value_type = "number",
    unit_label = NA_character_,
    unit_iri = NA_character_,
    term_iri = "https://example.org/existing-term",
    term_type = NA_character_,
    required = FALSE,
    property_iri = NA_character_,
    entity_iri = NA_character_,
    constraint_iri = NA_character_,
    method_iri = NA_character_
  )

  suggestions <- tibble::tibble(
    dataset_id = c("d1", "d1"),
    table_id = c("t1", "t1"),
    column_name = c("count", "count"),
    dictionary_role = c("variable", "property"),
    iri = c("https://example.org/new-term", "https://example.org/property"),
    score = c(10, 9)
  )

  out_safe <- apply_semantic_suggestions(dict, suggestions = suggestions, verbose = FALSE)
  expect_equal(out_safe$term_iri, "https://example.org/existing-term")
  expect_equal(out_safe$property_iri, "https://example.org/property")

  out_overwrite <- apply_semantic_suggestions(dict, suggestions = suggestions, overwrite = TRUE, verbose = FALSE)
  expect_equal(out_overwrite$term_iri, "https://example.org/new-term")
  expect_equal(out_overwrite$property_iri, "https://example.org/property")
})

test_that("apply_semantic_suggestions can filter by score when available", {
  dict <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "count",
    column_label = "Count",
    column_description = "Spawner count",
    column_role = "measurement",
    value_type = "number",
    unit_label = NA_character_,
    unit_iri = NA_character_,
    term_iri = NA_character_,
    term_type = NA_character_,
    required = FALSE,
    property_iri = NA_character_,
    entity_iri = NA_character_,
    constraint_iri = NA_character_,
    method_iri = NA_character_
  )

  suggestions <- tibble::tibble(
    dataset_id = c("d1", "d1"),
    table_id = c("t1", "t1"),
    column_name = c("count", "count"),
    dictionary_role = c("variable", "property"),
    iri = c("https://example.org/term", "https://example.org/property"),
    score = c(0.4, 0.9)
  )

  out <- apply_semantic_suggestions(
    dict,
    suggestions = suggestions,
    min_score = 0.5,
    verbose = FALSE
  )

  expect_true(is.na(out$term_iri))
  expect_equal(out$property_iri, "https://example.org/property")
})

test_that("apply_semantic_suggestions errors when min_score is requested without score", {
  dict <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "count",
    column_label = "Count",
    column_description = "Spawner count",
    column_role = "measurement",
    value_type = "number",
    unit_label = NA_character_,
    unit_iri = NA_character_,
    term_iri = NA_character_,
    term_type = NA_character_,
    required = FALSE,
    property_iri = NA_character_,
    entity_iri = NA_character_,
    constraint_iri = NA_character_,
    method_iri = NA_character_
  )

  suggestions <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "count",
    dictionary_role = "variable",
    iri = "https://example.org/term"
  )

  expect_error(
    apply_semantic_suggestions(dict, suggestions = suggestions, min_score = 0.5, verbose = FALSE),
    "min_score"
  )
})

test_that("validate_dictionary passes valid dictionary", {
  df <- data.frame(x = 1:5, y = letters[1:5])
  dict <- infer_dictionary(df)
  dict <- fill_measurement_components(dict)

  # Validation passes (may produce success messages)
  expect_invisible(validate_dictionary(dict))
  # Should return the dictionary
  result <- validate_dictionary(dict)
  expect_equal(result, dict)
})

test_that("validate_dictionary catches missing columns", {
  dict <- tibble::tibble(
    dataset_id = "test",
    column_name = "x"
    # Missing required columns
  )

  expect_error(
    validate_dictionary(dict),
    "missing required columns"
  )
})

test_that("validate_dictionary catches invalid value types", {
  df <- data.frame(x = 1:5)
  dict <- infer_dictionary(df)
  dict$value_type[1] <- "invalid_type"

  expect_error(
    validate_dictionary(dict),
    "Invalid.*value_type"
  )
})

test_that("validate_dictionary catches duplicate column names", {
  df <- data.frame(x = 1:5)
  dict <- infer_dictionary(df)
  dict <- dplyr::bind_rows(dict, dict)  # Duplicate

  expect_error(
    validate_dictionary(dict),
    "Duplicate column names"
  )
})

test_that("validate_dictionary warns when measurement semantic fields are missing (non-strict)", {
  df <- data.frame(count = c(10L, 20L), species = c("Coho", "Chinook"))
  dict <- infer_dictionary(df, dataset_id = "test-1", table_id = "table-1")
  dict <- dplyr::mutate(
    dict,
    term_iri = "https://example.org/term",
    property_iri = "https://example.org/property",
    entity_iri = "https://example.org/entity",
    unit_iri = "https://example.org/unit"
  )

  # Make the known measurement field incomplete in all semantic columns
  dict$term_iri[dict$column_name == "count"] <- NA_character_
  dict$property_iri[dict$column_name == "count"] <- NA_character_
  dict$entity_iri[dict$column_name == "count"] <- NA_character_
  dict$unit_iri[dict$column_name == "count"] <- NA_character_

  expect_warning(
    expect_invisible(validate_dictionary(dict)),
    "Hey, you definitely should fill those out before publishing"
  )
})

test_that("validate_dictionary can require semantic fields in strict mode", {
  df <- data.frame(count = c(10L, 20L), species = c("Coho", "Chinook"))
  dict <- infer_dictionary(df, dataset_id = "test-1", table_id = "table-1")

  expect_error(
    validate_dictionary(dict, require_iris = TRUE),
    "Measurement columns require"
  )
})

test_that("apply_salmon_dictionary renames columns", {
  df <- data.frame(
    species = c("Coho", "Chinook"),
    count = c(100L, 200L)
  )

  dict <- infer_dictionary(df, dataset_id = "test-1", table_id = "table-1")
  dict$column_label[dict$column_name == "species"] <- "Species Name"
  dict$column_label[dict$column_name == "count"] <- "Total Count"
  dict <- fill_measurement_components(dict)

  validate_dictionary(dict)
  result <- apply_salmon_dictionary(df, dict)

  expect_true("Species Name" %in% names(result))
  expect_true("Total Count" %in% names(result))
  expect_false("species" %in% names(result))
  expect_false("count" %in% names(result))
})

test_that("apply_salmon_dictionary coerces types", {
  df <- data.frame(
    count = c("100", "200"),  # Character, should become integer
    value = c("1.5", "2.5")   # Character, should become number
  )

  dict <- infer_dictionary(df, dataset_id = "test-1", table_id = "table-1")
  dict$value_type[dict$column_name == "count"] <- "integer"
  dict$value_type[dict$column_name == "value"] <- "number"
  dict <- fill_measurement_components(dict)

  validate_dictionary(dict)
  result <- apply_salmon_dictionary(df, dict, strict = TRUE)

  expect_type(result[[dict$column_label[dict$column_name == "count"]]], "integer")
  expect_type(result[[dict$column_label[dict$column_name == "value"]]], "double")
})

test_that("apply_salmon_dictionary applies factor levels from codes", {
  df <- data.frame(species = c("Coho", "Chinook", "Coho"))

  dict <- infer_dictionary(df, dataset_id = "test-1", table_id = "table-1")
  dict <- fill_measurement_components(dict)
  validate_dictionary(dict)

  codes <- tibble::tibble(
    dataset_id = "test-1",
    table_id = "table-1",
    column_name = "species",
    code_value = c("Coho", "Chinook"),
    code_label = c("Coho Salmon", "Chinook Salmon"),
    vocabulary_iri = NA_character_,
    term_iri = NA_character_,
    term_type = NA_character_
  )

  result <- apply_salmon_dictionary(df, dict, codes = codes)

  expect_s3_class(result[[dict$column_label[1]]], "factor")
  expect_equal(levels(result[[dict$column_label[1]]]), c("Coho Salmon", "Chinook Salmon"))
})
