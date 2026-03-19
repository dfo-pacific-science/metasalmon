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
  expect_equal(
    names(dict),
    c(
      "dataset_id", "table_id", "column_name", "column_label", "column_description",
      "term_iri", "property_iri", "entity_iri", "constraint_iri", "method_iri",
      "unit_label", "unit_iri", "term_type",
      "value_type", "column_role", "required"
    )
  )

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

test_that("infer_dictionary marks factor columns as categorical", {
  df <- data.frame(
    run = factor(c("early", "late")),
    count = c(100L, 200L)
  )

  dict <- infer_dictionary(df, dataset_id = "test-1", table_id = "table-1")

  expect_equal(dict$column_role[dict$column_name == "run"], "categorical")
  expect_equal(dict$column_role[dict$column_name == "count"], "measurement")
})

test_that("infer_dictionary marks obvious identifier columns as required", {
  df <- data.frame(
    station_id = c("A", "B"),
    species = c("Coho", "Chinook")
  )

  dict <- infer_dictionary(df, dataset_id = "test-1", table_id = "table-1")
  expect_true(isTRUE(dict$required[dict$column_name == "station_id"]))
  expect_true(is.na(dict$required[dict$column_name == "species"]))
})

test_that("infer_dictionary better distinguishes temporal and measurement NuSEDS-style fields", {
  df <- tibble::tibble(
    ANALYSIS_YR = c("2023", "2024"),
    NATURAL_ADULT_SPAWNERS = c(12, 15),
    POP_ID = c("A", "B")
  )

  dict <- infer_dictionary(df, dataset_id = "test-1", table_id = "table-1")

  expect_equal(dict$column_role[dict$column_name == "ANALYSIS_YR"], "temporal")
  expect_equal(dict$column_role[dict$column_name == "NATURAL_ADULT_SPAWNERS"], "measurement")
  expect_equal(dict$column_role[dict$column_name == "POP_ID"], "identifier")
})

test_that("infer_dictionary keeps method-like count fields out of measurement role", {
  df <- tibble::tibble(
    counting_method = c("Visual", "Electronic"),
    measurement_method = c("Direct", "Estimated"),
    cwt_1st_mark_count = c(12, 15),
    avg_weight = c(0.3, 0.5)
  )

  dict <- infer_dictionary(df, dataset_id = "test-1", table_id = "table-1")

  expect_equal(dict$column_role[dict$column_name == "counting_method"], "attribute")
  expect_equal(dict$column_role[dict$column_name == "measurement_method"], "attribute")
  expect_equal(dict$column_role[dict$column_name == "cwt_1st_mark_count"], "measurement")
  expect_equal(dict$column_role[dict$column_name == "avg_weight"], "measurement")
})

test_that("infer_dictionary recognizes wide numeric and percent metrics without promoting QA or reference fields", {
  df <- tibble::tibble(
    `Facility Reference Number` = c(1001, 1002),
    `Environmental (%/month)` = c("0.00%", "4.56%"),
    `Water Level / Niveau d'eau (m)` = c(1.2, 1.4),
    `Discharge / Débit (cms)` = c(10.5, 11.1),
    water_temp_c__temp_eau_c = c(12.3, 12.8),
    width_middle = c(4.2, 4.5),
    depth_1_lower = c(0.5, 0.7),
    `Grade...4` = c(10, 10),
    `QA/QC...6` = c("Approved", "Approved")
  )

  dict <- infer_dictionary(df, dataset_id = "test-1", table_id = "table-1")

  expect_equal(dict$column_role[dict$column_name == "Facility Reference Number"], "identifier")
  expect_equal(dict$column_role[dict$column_name == "Environmental (%/month)"], "measurement")
  expect_equal(dict$column_role[dict$column_name == "Water Level / Niveau d'eau (m)"], "measurement")
  expect_equal(dict$column_role[dict$column_name == "Discharge / Débit (cms)"], "measurement")
  expect_equal(dict$column_role[dict$column_name == "water_temp_c__temp_eau_c"], "measurement")
  expect_equal(dict$column_role[dict$column_name == "width_middle"], "measurement")
  expect_equal(dict$column_role[dict$column_name == "depth_1_lower"], "measurement")
  expect_equal(dict$column_role[dict$column_name == "Grade...4"], "attribute")
  expect_equal(dict$column_role[dict$column_name == "QA/QC...6"], "attribute")
})

test_that("infer_dictionary can seed semantic suggestions", {
  fake_suggest <- function(df, dict, sources = c("ols", "nvs"), max_per_role = 1, include_dwc = FALSE,
                           codes = NULL, table_meta = NULL, dataset_meta = NULL, ...) {
    expect_equal(sources, c("ols", "nvs", "qudt"))
    expect_equal(max_per_role, 1)
    expect_null(codes)
    expect_null(table_meta)
    expect_null(dataset_meta)
    attr(dict, "semantic_suggestions") <- tibble::tibble(
      column_name = c("count"),
      dictionary_role = c("variable"),
      table_id = c("table-1"),
      dataset_id = c("dataset-1"),
      target_scope = c("column"),
      target_sdp_file = c("column_dictionary.csv"),
      target_sdp_field = c("term_iri"),
      search_query = c("count"),
      column_label = c("count"),
      column_description = c(NA_character_),
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

test_that("infer_dictionary accepts named resource lists and can seed metadata-aware suggestions", {
  resources <- list(
    catches = data.frame(
      trawl_id = c(1L, 2L),
      species = c("Coho", "Chinook"),
      count = c(10L, 20L)
    ),
    environments = data.frame(
      station = c("A", "B"),
      temperature = c(10.2, 11.4),
      sample_date = as.Date(c("2024-01-01", "2024-01-02"))
    )
  )

  fake_suggest <- function(df, dict, sources = c("smn", "gcdfo", "ols", "nvs"), max_per_role = 1,
                            include_dwc = FALSE, codes = NULL, table_meta = NULL, dataset_meta = NULL, ...) {
    expect_true(all(c("catches", "environments") %in% table_meta$table_id))
    expect_equal(dataset_meta$dataset_id[[1]], "dataset-1")
    expect_true("keywords" %in% names(dataset_meta))
    expect_true(all(c("table_id", "column_name", "code_value") %in% names(codes)))

    attr(dict, "semantic_suggestions") <- tibble::tibble(
      column_name = c("count"),
      dictionary_role = c("variable"),
      table_id = c("catches"),
      dataset_id = c("dataset-1"),
      target_scope = c("column"),
      target_sdp_file = c("column_dictionary.csv"),
      target_sdp_field = c("term_iri"),
      search_query = c("count"),
      column_label = c("count"),
      column_description = c(NA_character_),
      label = c("Catch count"),
      iri = c("https://example.org/count"),
      source = c("smn"),
      ontology = c("demo"),
      definition = c(NA_character_)
    )
    dict
  }

  with_mocked_bindings(
    suggest_semantics = fake_suggest,
    {
      dict <- infer_dictionary(
        resources,
        seed_semantics = TRUE,
        seed_verbose = FALSE
      )
      expect_s3_class(dict, "tbl_df")
      expect_equal(nrow(dict), 6)
      expect_true(all(c("catches", "environments") %in% dict$table_id))
      expect_equal(ncol(attr(dict, "inferred_table_meta")), 8)
      expect_true(all(c("species", "station") %in% attr(dict, "inferred_codes")$column_name))
      expect_true(is.data.frame(attr(dict, "inferred_dataset_meta")))
      expect_true("dataset-1" %in% attr(dict, "inferred_dataset_meta")$dataset_id)
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
  expect_equal(nrow(suggestions), 6) # includes count-like unit fallback query
  expect_equal(names(suggestions)[1:4], c("column_name", "dictionary_role", "table_id", "dataset_id"))
  expect_true(all(c("target_scope", "target_sdp_file", "target_sdp_field", "search_query") %in% names(suggestions)))
  expect_true(all(suggestions$dictionary_role %in% c("variable", "property", "entity", "unit", "constraint", "method")))
  expect_true(all(suggestions$column_name == "value"))
  expect_true(all(suggestions$dataset_id == "d1"))
  expect_true(all(suggestions$table_id == "t1"))
  expect_true(all(suggestions$target_scope == "column"))
  expect_true(all(suggestions$target_sdp_file == "column_dictionary.csv"))
  expect_equal(suggestions$search_query[suggestions$dictionary_role == "variable"], "spawner abundance")
  expect_equal(suggestions$search_query[suggestions$dictionary_role == "unit"], "count")
  expect_equal(suggestions$search_query[suggestions$dictionary_role == "entity"], "population")
  expect_equal(
    unique(suggestions[, c("dictionary_role", "target_sdp_field")]),
    tibble::tibble(
      dictionary_role = c("variable", "property", "entity", "unit", "constraint", "method"),
      target_sdp_field = c("term_iri", "property_iri", "entity_iri", "unit_iri", "constraint_iri", "method_iri")
    )
  )

  res_dwc <- suggest_semantics(NULL, dict, sources = "ols", max_per_role = 1, search_fn = fake_search, include_dwc = TRUE)
  dwc_map <- attr(res_dwc, "dwc_mappings")
  expect_true(is.data.frame(dwc_map))
})

test_that("suggest_semantics strips review placeholders and applies role-aware column queries", {
  dict <- tibble::tibble(
    dataset_id = c("d1", "d1"),
    table_id = c("t1", "t1"),
    column_name = c("NATURAL_SPAWNERS_TOTAL", "POPULATION"),
    column_label = c("NATURAL_SPAWNERS_TOTAL", "Population"),
    column_description = c(
      "MISSING DESCRIPTION: define what 'NATURAL_SPAWNERS_TOTAL' means in table 'escapement'.",
      "Population identifier"
    ),
    column_role = c("measurement", "categorical"),
    value_type = c("number", "string"),
    unit_label = c(NA_character_, NA_character_),
    unit_iri = c(NA_character_, NA_character_),
    term_iri = c(NA_character_, NA_character_),
    property_iri = c(NA_character_, NA_character_),
    entity_iri = c(NA_character_, NA_character_),
    constraint_iri = c(NA_character_, NA_character_),
    method_iri = c(NA_character_, NA_character_)
  )

  calls <- list()
  fake_search <- function(query, role, sources) {
    calls[[length(calls) + 1]] <<- list(query = query, role = role)
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

  suggest_semantics(NULL, dict, sources = "ols", max_per_role = 1, search_fn = fake_search)

  call_df <- tibble::as_tibble(purrr::map_dfr(calls, tibble::as_tibble))
  expect_true(any(call_df$role == "unit" & call_df$query == "count"))
  expect_true(any(call_df$role == "variable" & call_df$query == "spawner abundance"))
  expect_true(any(call_df$role == "property" & call_df$query == "count"))
  expect_true(any(call_df$role == "constraint" & call_df$query == "natural origin"))
  expect_true(any(call_df$role == "entity" & call_df$query == "population"))
})

test_that("suggest_semantics uses count-like measurement queries for adult spawner and mark-count fields", {
  dict <- tibble::tibble(
    dataset_id = c("d1", "d1"),
    table_id = c("t1", "t1"),
    column_name = c("NATURAL_ADULT_SPAWNERS", "cwt_1st_mark_count"),
    column_label = c("NATURAL_ADULT_SPAWNERS", "cwt_1st_mark_count"),
    column_description = c("Estimated natural-origin adult spawners", "First-mark coded-wire-tag count"),
    column_role = c("measurement", "measurement"),
    value_type = c("integer", "integer"),
    unit_label = c(NA_character_, NA_character_),
    unit_iri = c(NA_character_, NA_character_),
    term_iri = c(NA_character_, NA_character_),
    property_iri = c(NA_character_, NA_character_),
    entity_iri = c(NA_character_, NA_character_),
    constraint_iri = c(NA_character_, NA_character_),
    method_iri = c(NA_character_, NA_character_)
  )

  calls <- list()
  fake_search <- function(query, role, sources) {
    calls[[length(calls) + 1]] <<- list(query = query, role = role)
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

  suggest_semantics(NULL, dict, sources = "ols", max_per_role = 1, search_fn = fake_search)

  call_df <- tibble::as_tibble(purrr::map_dfr(calls, tibble::as_tibble))
  expect_true(any(call_df$role == "variable" & call_df$query == "adult spawner count"))
  expect_true(any(call_df$role == "property" & call_df$query == "count"))
  expect_true(any(call_df$role == "variable" & call_df$query == "count"))
  expect_true(any(call_df$role == "unit" & call_df$query == "count"))
})

test_that("suggest_semantics ignores review placeholders when building table observation-unit queries", {
  dict <- tibble::tibble(
    dataset_id = "d1",
    table_id = "escapement",
    column_name = c("population", "count"),
    column_label = c("Population", "Spawner count"),
    column_description = c("Population identifier", "Spawner count estimate"),
    column_role = c("categorical", "measurement"),
    value_type = c("string", "number"),
    unit_label = c(NA_character_, NA_character_),
    unit_iri = c(NA_character_, NA_character_),
    term_iri = c(NA_character_, NA_character_),
    property_iri = c(NA_character_, NA_character_),
    entity_iri = c(NA_character_, NA_character_),
    constraint_iri = c(NA_character_, NA_character_),
    method_iri = c(NA_character_, NA_character_)
  )
  table_meta <- tibble::tibble(
    dataset_id = "d1",
    table_id = "escapement",
    table_label = "Escapement",
    description = "MISSING DESCRIPTION: describe what each row in table 'escapement' represents.",
    observation_unit = "MISSING METADATA: describe the observation unit for table 'escapement'.",
    observation_unit_iri = NA_character_
  )

  calls <- list()
  fake_search <- function(query, role, sources) {
    calls[[length(calls) + 1]] <<- list(query = query, role = role)
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

  res <- suggest_semantics(
    NULL,
    dict,
    sources = "ols",
    max_per_role = 1,
    search_fn = fake_search,
    table_meta = table_meta
  )
  suggestions <- attr(res, "semantic_suggestions")
  table_suggestions <- suggestions[suggestions$target_scope == "table", , drop = FALSE]
  call_df <- tibble::as_tibble(purrr::map_dfr(calls, tibble::as_tibble))

  expect_true(any(call_df$role == "entity" & call_df$query == "Escapement"))
  expect_false(any(grepl("MISSING METADATA|describe the observation unit", table_suggestions$search_query, ignore.case = TRUE)))
  expect_equal(unique(table_suggestions$target_query_basis), "table_label")
  expect_equal(unique(table_suggestions$target_query_context), "Escapement escapement")
})

test_that("suggest_semantics adds lighter non-measurement term suggestions for categorical and controlled attributes", {
  dict <- tibble::tibble(
    dataset_id = c("d1", "d1", "d1", "d1"),
    table_id = c("t1", "t1", "t1", "t1"),
    column_name = c("species", "origin", "record_id", "survey_comment"),
    column_label = c("Species", "Origin", "Record ID", "Survey comment"),
    column_description = c("Observed species", "Origin code", "Internal record id", "Free-text review note"),
    column_role = c("categorical", "attribute", "identifier", "attribute"),
    value_type = c("string", "string", "string", "string"),
    unit_label = c(NA_character_, NA_character_, NA_character_, NA_character_),
    unit_iri = c(NA_character_, NA_character_, NA_character_, NA_character_),
    term_iri = c(NA_character_, NA_character_, NA_character_, NA_character_),
    property_iri = c(NA_character_, NA_character_, NA_character_, NA_character_),
    entity_iri = c(NA_character_, NA_character_, NA_character_, NA_character_),
    constraint_iri = c(NA_character_, NA_character_, NA_character_, NA_character_),
    method_iri = c(NA_character_, NA_character_, NA_character_, NA_character_)
  )
  codes <- tibble::tibble(
    dataset_id = c("d1", "d1", "d1"),
    table_id = c("t1", "t1", "t1"),
    column_name = c("species", "origin", "survey_comment"),
    code_value = c("CO", "NAT", "looks odd"),
    code_label = c("Coho", "Natural", "looks odd"),
    code_description = c("Coho salmon", "Natural origin", "Free-text note"),
    vocabulary_iri = c(NA_character_, NA_character_, NA_character_),
    term_iri = c(NA_character_, NA_character_, NA_character_),
    term_type = c(NA_character_, NA_character_, NA_character_)
  )

  fake_search <- function(query, role, sources) {
    tibble::tibble(
      label = paste("candidate", role),
      iri = paste0("https://example.org/", role, "/", gsub("\\s+", "-", tolower(query))),
      source = "ols",
      ontology = "demo",
      role = role,
      match_type = "label_partial",
      definition = ""
    )
  }

  res <- suggest_semantics(
    NULL,
    dict,
    sources = "ols",
    max_per_role = 1,
    search_fn = fake_search,
    codes = codes
  )
  suggestions <- attr(res, "semantic_suggestions")

  non_measurement <- suggestions[suggestions$target_scope == "column" & suggestions$target_sdp_field == "term_iri", , drop = FALSE]
  expect_true(any(non_measurement$column_name == "species"))
  expect_true(any(non_measurement$column_name == "origin"))
  expect_false(any(non_measurement$column_name == "record_id"))
  expect_false(any(non_measurement$column_name == "survey_comment"))
})

test_that("suggest_semantics uses role-aware search roles for controlled attribute term suggestions", {
  dict <- tibble::tibble(
    dataset_id = c("d1", "d1", "d1", "d1", "d1"),
    table_id = c("t1", "t1", "t1", "t1", "t1"),
    column_name = c("RUN_TYPE", "ESTIMATE_METHOD", "CU_NAME", "WATERSHED_CDE", "SPECIES_QUALIFIED"),
    column_label = c("RUN_TYPE", "ESTIMATE_METHOD", "CU_NAME", "WATERSHED_CDE", "SPECIES_QUALIFIED"),
    column_description = c(
      "Run timing code",
      "Estimate method code",
      "Conservation unit name",
      "Watershed code",
      "Qualified species label"
    ),
    column_role = c("attribute", "attribute", "attribute", "attribute", "attribute"),
    value_type = c("string", "string", "string", "string", "string"),
    unit_label = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    unit_iri = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    term_iri = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    property_iri = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    entity_iri = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    constraint_iri = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    method_iri = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_)
  )
  codes <- tibble::tibble(
    dataset_id = c("d1", "d1", "d1", "d1", "d1"),
    table_id = c("t1", "t1", "t1", "t1", "t1"),
    column_name = c("RUN_TYPE", "ESTIMATE_METHOD", "CU_NAME", "WATERSHED_CDE", "SPECIES_QUALIFIED"),
    code_value = c("EARLY", "VIS", "FRASER COHO SOUTH", "08MH", "COHO SALMON"),
    code_label = c("Early", "Visual", "Fraser Coho South", "08MH", "Coho salmon"),
    code_description = c("Early run timing", "Visual estimate", "Conservation unit", "Watershed code", "Species label"),
    vocabulary_iri = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    term_iri = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    term_type = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_)
  )

  calls <- list()
  fake_search <- function(query, role, sources) {
    calls[[length(calls) + 1]] <<- list(query = query, role = role)
    tibble::tibble(
      label = paste("candidate", role),
      iri = paste0("https://example.org/", role, "/", gsub("\\s+", "-", tolower(query))),
      source = "ols",
      ontology = "demo",
      role = role,
      match_type = "label_partial",
      definition = ""
    )
  }

  res <- suggest_semantics(
    NULL,
    dict,
    sources = "ols",
    max_per_role = 1,
    search_fn = fake_search,
    codes = codes
  )

  suggestions <- attr(res, "semantic_suggestions")
  call_df <- tibble::as_tibble(purrr::map_dfr(calls, tibble::as_tibble))
  column_suggestions <- suggestions[suggestions$target_scope == "column", , drop = FALSE]

  expect_true(any(call_df$role == "constraint" & call_df$query == "run context"))
  expect_true(any(call_df$role == "method" & call_df$query == "estimate method"))
  expect_true(any(call_df$role == "entity" & call_df$query == "conservation unit"))
  expect_true(any(call_df$role == "entity" & call_df$query == "watershed"))
  expect_true(any(call_df$role == "entity" & call_df$query == "species"))
  expect_true(all(column_suggestions$dictionary_role == "variable"))
  expect_true(all(column_suggestions$target_sdp_field == "term_iri"))
})

test_that("suggest_semantics keeps site and management-unit attributes on entity-style queries and rewrites catch methods", {
  dict <- tibble::tibble(
    dataset_id = c("d1", "d1", "d1"),
    table_id = c("t1", "t1", "t1"),
    column_name = c("site_type", "aquaculture_management_unit", "catch_method"),
    column_label = c("Site Type", "Aquaculture Management Unit", "Catch Method"),
    column_description = c(
      "Type of sampling site",
      "Aquaculture management unit name",
      "Method used to catch the fish"
    ),
    column_role = c("attribute", "attribute", "attribute"),
    value_type = c("string", "string", "string"),
    unit_label = c(NA_character_, NA_character_, NA_character_),
    unit_iri = c(NA_character_, NA_character_, NA_character_),
    term_iri = c(NA_character_, NA_character_, NA_character_),
    property_iri = c(NA_character_, NA_character_, NA_character_),
    entity_iri = c(NA_character_, NA_character_, NA_character_),
    constraint_iri = c(NA_character_, NA_character_, NA_character_),
    method_iri = c(NA_character_, NA_character_, NA_character_),
    term_type = c(NA_character_, NA_character_, NA_character_)
  )
  codes <- tibble::tibble(
    dataset_id = c("d1", "d1", "d1"),
    table_id = c("t1", "t1", "t1"),
    column_name = c("site_type", "aquaculture_management_unit", "catch_method"),
    code_value = c("OPEN", "CLAYOQUOT", "TROLL"),
    code_label = c("Open", "Clayoquot Sound", "Trolling"),
    code_description = c("Open site", "Management unit", "Fishing capture method"),
    vocabulary_iri = c(NA_character_, NA_character_, NA_character_),
    term_iri = c(NA_character_, NA_character_, NA_character_),
    term_type = c(NA_character_, NA_character_, NA_character_)
  )

  calls <- list()
  fake_search <- function(query, role, sources) {
    calls[[length(calls) + 1]] <<- list(query = query, role = role)
    tibble::tibble(
      label = paste("candidate", role),
      iri = paste0("https://example.org/", role, "/", gsub("\\s+", "-", tolower(query))),
      source = "ols",
      ontology = "demo",
      role = role,
      match_type = "label_partial",
      definition = ""
    )
  }

  suggest_semantics(
    NULL,
    dict,
    sources = "ols",
    max_per_role = 1,
    search_fn = fake_search,
    codes = codes
  )

  call_df <- tibble::as_tibble(purrr::map_dfr(calls, tibble::as_tibble))
  expect_true(any(call_df$role == "entity" & call_df$query == "site"))
  expect_true(any(call_df$role == "entity" & call_df$query == "aquaculture management unit"))
  expect_true(any(call_df$role == "method" & call_df$query == "capture method"))
})

test_that("suggest_semantics seeds location-like attribute term suggestions without low-cardinality codes", {
  dict <- tibble::tibble(
    dataset_id = c("d1", "d1", "d1", "d1"),
    table_id = c("t1", "t1", "t1", "t1"),
    column_name = c("WATERSHED_CDE", "release_location_code", "SYSTEM_SITE", "SPECIES_QUALIFIED"),
    column_label = c("WATERSHED_CDE", "release_location_code", "SYSTEM_SITE", "SPECIES_QUALIFIED"),
    column_description = c(
      "Watershed code",
      "Release location code",
      "System site label",
      "Qualified species label"
    ),
    column_role = c("attribute", "attribute", "attribute", "attribute"),
    value_type = c("string", "string", "string", "string"),
    unit_label = c(NA_character_, NA_character_, NA_character_, NA_character_),
    unit_iri = c(NA_character_, NA_character_, NA_character_, NA_character_),
    term_iri = c(NA_character_, NA_character_, NA_character_, NA_character_),
    property_iri = c(NA_character_, NA_character_, NA_character_, NA_character_),
    entity_iri = c(NA_character_, NA_character_, NA_character_, NA_character_),
    constraint_iri = c(NA_character_, NA_character_, NA_character_, NA_character_),
    method_iri = c(NA_character_, NA_character_, NA_character_, NA_character_),
    term_type = c(NA_character_, NA_character_, NA_character_, NA_character_)
  )
  codes <- tibble::tibble()

  calls <- list()
  fake_search <- function(query, role, sources) {
    calls[[length(calls) + 1]] <<- list(query = query, role = role)
    tibble::tibble(
      label = paste("candidate", role),
      iri = paste0("https://example.org/", role, "/", gsub("\\s+", "-", tolower(query))),
      source = "ols",
      ontology = "demo",
      role = role,
      match_type = "label_partial",
      definition = ""
    )
  }

  res <- suggest_semantics(
    NULL,
    dict,
    sources = "ols",
    max_per_role = 1,
    search_fn = fake_search,
    codes = codes
  )

  suggestions <- attr(res, "semantic_suggestions")
  column_suggestions <- suggestions[suggestions$target_scope == "column" & suggestions$target_sdp_field == "term_iri", , drop = FALSE]
  call_df <- tibble::as_tibble(purrr::map_dfr(calls, tibble::as_tibble))

  expect_true(any(column_suggestions$column_name == "WATERSHED_CDE"))
  expect_true(any(column_suggestions$column_name == "release_location_code"))
  expect_true(any(column_suggestions$column_name == "SYSTEM_SITE"))
  expect_false(any(column_suggestions$column_name == "SPECIES_QUALIFIED"))
  expect_true(any(call_df$role == "entity" & call_df$query == "watershed"))
  expect_true(any(call_df$role == "entity" & call_df$query == "site"))
})

test_that("suggest_semantics uses taxon-style entity queries for species confirmation attributes", {
  dict <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "confirmed_atlantic_salmon",
    column_label = "Confirmed Atlantic Salmon?",
    column_description = "Boolean confirmation that the capture was Atlantic salmon.",
    column_role = "attribute",
    value_type = "boolean",
    unit_label = NA_character_,
    unit_iri = NA_character_,
    term_iri = NA_character_,
    property_iri = NA_character_,
    entity_iri = NA_character_,
    constraint_iri = NA_character_,
    method_iri = NA_character_,
    term_type = NA_character_
  )
  codes <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "confirmed_atlantic_salmon",
    code_value = c("Yes", "No"),
    code_label = c("Yes", "No"),
    code_description = NA_character_,
    term_iri = NA_character_
  )

  calls <- list()
  fake_search <- function(query, role, ...) {
    calls[[length(calls) + 1L]] <<- list(query = query, role = role)
    tibble::tibble(
      label = "Atlantic salmon",
      iri = "http://purl.obolibrary.org/obo/NCBITaxon_8030",
      source = "ols",
      ontology = "mro",
      role = role,
      match_type = "class",
      definition = "Atlantic salmon taxon."
    )
  }

  suggest_semantics(
    NULL,
    dict,
    sources = "ols",
    max_per_role = 1,
    search_fn = fake_search,
    codes = codes
  )

  call_df <- tibble::as_tibble(purrr::map_dfr(calls, tibble::as_tibble))
  expect_true(any(call_df$role == "entity" & call_df$query == "atlantic salmon"))
})

test_that("suggest_semantics expands waterbody-style attribute queries for auto-apply compatibility", {
  dict <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "WATERBODY",
    column_label = "WATERBODY",
    column_description = "Waterbody code for the river system",
    column_role = "attribute",
    value_type = "string",
    unit_label = NA_character_,
    unit_iri = NA_character_,
    term_iri = NA_character_,
    property_iri = NA_character_,
    entity_iri = NA_character_,
    constraint_iri = NA_character_,
    method_iri = NA_character_,
    term_type = NA_character_
  )
  codes <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "WATERBODY",
    code_value = c("FRASER", "THOMPSON"),
    code_label = c("Fraser River", "Thompson River"),
    code_description = NA_character_,
    term_iri = NA_character_
  )

  calls <- list()
  fake_search <- function(query, role, ...) {
    calls[[length(calls) + 1L]] <<- list(query = query, role = role)
    tibble::tibble(
      label = "water body",
      iri = "http://purl.obolibrary.org/obo/ENVO_00000063",
      source = "ols",
      ontology = "envo",
      role = role,
      match_type = "class",
      definition = "A body of water."
    )
  }

  suggest_semantics(
    NULL,
    dict,
    sources = "ols",
    max_per_role = 1,
    search_fn = fake_search,
    codes = codes
  )

  call_df <- tibble::as_tibble(purrr::map_dfr(calls, tibble::as_tibble))
  expect_true(any(call_df$role == "entity" & call_df$query == "water body"))
})

test_that("apply_semantic_suggestions keeps compatible non-measurement term IRIs and skips bad fits", {
  dict <- tibble::tibble(
    dataset_id = c("d1", "d1", "d1", "d1"),
    table_id = c("t1", "t1", "t1", "t1"),
    column_name = c("origin", "AREA", "year", "count"),
    column_label = c("Origin", "Area", "Year", "Count"),
    column_description = c("Origin code", "Area code", "Analysis year", "Spawner count"),
    column_role = c("attribute", "attribute", "temporal", "measurement"),
    value_type = c("string", "string", "string", "integer"),
    unit_label = c(NA_character_, NA_character_, NA_character_, NA_character_),
    unit_iri = c(NA_character_, NA_character_, NA_character_, NA_character_),
    term_iri = c(NA_character_, NA_character_, NA_character_, NA_character_),
    property_iri = c(NA_character_, NA_character_, NA_character_, NA_character_),
    entity_iri = c(NA_character_, NA_character_, NA_character_, NA_character_),
    constraint_iri = c(NA_character_, NA_character_, NA_character_, NA_character_),
    method_iri = c(NA_character_, NA_character_, NA_character_, NA_character_),
    term_type = c(NA_character_, NA_character_, NA_character_, NA_character_)
  )

  suggestions <- tibble::tibble(
    dataset_id = c("d1", "d1", "d1", "d1"),
    table_id = c("t1", "t1", "t1", "t1"),
    column_name = c("origin", "AREA", "year", "count"),
    dictionary_role = c("variable", "variable", "variable", "variable"),
    target_scope = c("column", "column", "column", "column"),
    target_sdp_file = c("column_dictionary.csv", "column_dictionary.csv", "column_dictionary.csv", "column_dictionary.csv"),
    target_sdp_field = c("term_iri", "term_iri", "term_iri", "term_iri"),
    search_query = c("Origin", "Area", "Year", "Spawner abundance"),
    column_label = c("Origin", "Area", "Year", "Count"),
    label = c("Origin", "In River Mortality Rate", "Year", "Spawner abundance"),
    iri = c("https://example.org/origin", "https://example.org/in-river-mortality-rate", "https://example.org/year", "https://example.org/spawner-abundance"),
    match_type = c("label_exact", "label_exact", "label_exact", "label_exact"),
    score = c(0.95, 0.95, 0.95, 0.95)
  )

  out <- apply_semantic_suggestions(dict, suggestions = suggestions, verbose = FALSE)

  expect_equal(out$term_iri[out$column_name == "origin"], "https://example.org/origin")
  expect_true(is.na(out$term_iri[out$column_name == "AREA"]) || out$term_iri[out$column_name == "AREA"] == "")
  expect_true(is.na(out$term_iri[out$column_name == "year"]) || out$term_iri[out$column_name == "year"] == "")
  expect_equal(out$term_iri[out$column_name == "count"], "https://example.org/spawner-abundance")
})

test_that("suggest_semantics skips unit guesses when measurement has no unit clue", {
  dict <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "temperature",
    column_label = "Temperature",
    column_description = "Water temperature measurement",
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

  calls <- list()
  fake_search <- function(query, role, sources) {
    calls[[length(calls) + 1]] <<- list(query = query, role = role)
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

  suggest_semantics(NULL, dict, sources = "ols", max_per_role = 1, search_fn = fake_search)
  call_df <- tibble::as_tibble(purrr::map_dfr(calls, tibble::as_tibble))
  expect_false(any(call_df$role == "unit"))
})

test_that("apply_semantic_suggestions fills unit_label when applying unit_iri", {
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
    property_iri = NA_character_,
    entity_iri = NA_character_,
    constraint_iri = NA_character_,
    method_iri = NA_character_
  )

  suggestions <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "count",
    dictionary_role = "unit",
    iri = "http://example.org/unit/count",
    label = "count"
  )

  out <- apply_semantic_suggestions(dict, suggestions = suggestions, verbose = FALSE)
  expect_equal(out$unit_iri, "http://example.org/unit/count")
  expect_equal(out$unit_label, "count")
})

test_that("count-like unit suggestions can be applied with unit_label backfill", {
  dict <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "NATURAL_SPAWNERS_TOTAL",
    column_label = "NATURAL_SPAWNERS_TOTAL",
    column_description = "Total natural spawners",
    column_role = "measurement",
    value_type = "integer",
    unit_label = NA_character_,
    unit_iri = NA_character_,
    term_iri = NA_character_,
    property_iri = NA_character_,
    entity_iri = NA_character_,
    constraint_iri = NA_character_,
    method_iri = NA_character_
  )

  fake_search <- function(query, role, sources) {
    if (role == "unit") {
      return(tibble::tibble(
        label = "Count",
        iri = "https://qudt.org/vocab/unit/COUNT",
        source = "qudt",
        ontology = "qudt",
        role = role,
        match_type = "label_exact",
        definition = "Count unit"
      ))
    }
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

  suggested <- suggest_semantics(NULL, dict, sources = c("qudt", "ols"), max_per_role = 1, search_fn = fake_search)
  suggestions <- attr(suggested, "semantic_suggestions")
  unit_rows <- suggestions[suggestions$dictionary_role == "unit", , drop = FALSE]
  expect_true(nrow(unit_rows) > 0)
  expect_true(all(unit_rows$search_query == "count"))

  out <- apply_semantic_suggestions(dict, suggestions = suggestions, roles = "unit", verbose = FALSE)
  expect_equal(out$unit_iri, "https://qudt.org/vocab/unit/COUNT")
  expect_equal(out$unit_label, "Count")
})

test_that("suggest_semantics supports code, table, and dataset targets", {
  dict <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "species_code",
    column_label = "Species code",
    column_description = "Species code used in the counts table",
    column_role = "measurement",
    value_type = "string",
    unit_label = NA_character_,
    unit_iri = NA_character_,
    term_iri = NA_character_,
    property_iri = NA_character_,
    entity_iri = NA_character_,
    constraint_iri = NA_character_,
    method_iri = NA_character_
  )
  codes <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "species_code",
    code_value = "CO",
    code_label = "Coho",
    code_description = "Coho salmon code",
    vocabulary_iri = NA_character_,
    term_iri = NA_character_,
    term_type = NA_character_
  )
  table_meta <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    file_name = "t1.csv",
    table_label = "Main table",
    description = "Fish observations",
    observation_unit = "salmon population",
    observation_unit_iri = NA_character_,
    primary_key = "species_code"
  )
  dataset_meta <- tibble::tibble(
    dataset_id = "d1",
    title = "Fraser salmon observations",
    description = "Monitoring dataset for salmon runs",
    keywords = NA_character_
  )

  fake_search <- function(query, role, sources) {
    tibble::tibble(
      label = paste("candidate", role),
      iri = paste0("https://example.org/", role),
      source = "ols",
      ontology = "demo",
      role = role,
      role_hints = role,
      match_type = "label_partial",
      definition = "",
      score = 1
    )
  }

  res <- suggest_semantics(
    NULL,
    dict,
    sources = "ols",
    max_per_role = 1,
    search_fn = fake_search,
    codes = codes,
    table_meta = table_meta,
    dataset_meta = dataset_meta
  )
  suggestions <- attr(res, "semantic_suggestions")

  expect_true(all(c("target_scope", "target_sdp_file", "target_sdp_field", "target_row_key") %in% names(suggestions)))
  expect_true(all(c("column", "code", "table", "dataset") %in% unique(suggestions$target_scope)))
  expect_true(all(c("column_dictionary.csv", "codes.csv", "tables.csv", "dataset.csv") %in% unique(suggestions$target_sdp_file)))
  expect_true(any(suggestions$target_scope == "code" & suggestions$target_sdp_field == "term_iri"))
  expect_true(any(suggestions$target_scope == "table" & suggestions$target_sdp_field == "observation_unit_iri"))
  expect_true(any(suggestions$target_scope == "dataset" & suggestions$target_sdp_field == "keywords"))
})

test_that("suggest_semantics marks variable vs property collisions with destination notes", {
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
      label = "Spawner abundance",
      iri = paste0("https://example.org/", role),
      source = "smn",
      ontology = "demo",
      role = role,
      role_hints = if (role == "variable") "variable|property" else if (role == "property") "variable|property" else role,
      match_type = "label_exact",
      definition = "",
      score = 1
    )
  }

  res <- suggest_semantics(NULL, dict, sources = "smn", max_per_role = 1, search_fn = fake_search)
  suggestions <- attr(res, "semantic_suggestions")
  vp <- suggestions[suggestions$dictionary_role %in% c("variable", "property"), , drop = FALSE]

  expect_true(all(vp$role_collision))
  expect_true(all(grepl("targets", vp$role_collision_note)))
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

test_that("suggest_semantics deduplicates by source plus IRI without rewriting", {
  dict <- tibble::tibble(
    dataset_id = "d",
    table_id = "t",
    column_name = "MEAS",
    column_label = "Stock",
    column_description = "Stock count",
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

  fake_search <- function(query, role, sources = NULL) {
    role_suffix <- if (role == "entity") "entity" else role
    tibble::tibble(
      label = c("Stock", "Stock", "Stock"),
      iri = c(
        "https://w3id.org/smn/Stock",
        "http://w3id.org/salmon/Stock",
        "https://w3id.org/smn/Stock"
      ),
      source = c("smn", "smn", "smn"),
      ontology = c(role_suffix, role_suffix, role_suffix),
      role = role,
      match_type = "label",
      definition = "",
      score = c(10, 9, 8)
    )
  }

  res <- suggest_semantics(NULL, dict, sources = "smn", max_per_role = 2, search_fn = fake_search)
  suggestions <- attr(res, "semantic_suggestions")

  # Expect duplicate rows removed but no namespace rewriting.
  entity_rows <- suggestions[suggestions$dictionary_role == "entity", , drop = FALSE]
  var_rows <- suggestions[suggestions$dictionary_role == "variable", , drop = FALSE]
  expect_equal(nrow(entity_rows), 2)
  expect_equal(nrow(var_rows), 2)
  expect_true(all(c("https://w3id.org/smn/Stock", "http://w3id.org/salmon/Stock") %in% unique(entity_rows$iri)))
  expect_true(all(c("https://w3id.org/smn/Stock", "http://w3id.org/salmon/Stock") %in% unique(var_rows$iri)))
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
  expect_equal(out_safe$term_type, "skos_concept")
  expect_equal(out_safe$property_iri, "https://example.org/property")

  out_overwrite <- apply_semantic_suggestions(dict, suggestions = suggestions, overwrite = TRUE, verbose = FALSE)
  expect_equal(out_overwrite$term_iri, "https://example.org/new-term")
  expect_equal(out_overwrite$term_type, "skos_concept")
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

test_that("apply_semantic_suggestions ignores non-column targets", {
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
    table_id = c("t1", NA_character_),
    column_name = c("count", NA_character_),
    dictionary_role = c("variable", "entity"),
    target_scope = c("column", "dataset"),
    target_sdp_file = c("column_dictionary.csv", "dataset.csv"),
    target_sdp_field = c("term_iri", "keywords"),
    iri = c("https://example.org/term", "https://example.org/dataset-keyword")
  )

  out <- apply_semantic_suggestions(dict, suggestions = suggestions, verbose = FALSE)
  expect_equal(out$term_iri, "https://example.org/term")
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
    "Missing semantic fields for measurement columns",
    fixed = TRUE
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

test_that("infer_salmon_datapackage_artifacts infers multi-table SDP artifacts", {
  resources <- list(
    catches = tibble::tibble(
      station_id = c(1L, 2L),
      species = c("Coho", "Chinook"),
      count = c(10L, 20L),
      observation_date = as.Date(c("2024-01-01", "2024-01-02"))
    ),
    stations = tibble::tibble(
      station_id = c(1L, 2L),
      lat = c(50.12, 50.34),
      lon = c(-125.5, -125.6),
      habitat = c("estu", "river")
    )
  )

  fake_suggest <- function(df, dict, sources = c("smn", "gcdfo", "ols", "nvs"), max_per_role = 1,
                           include_dwc = FALSE, codes = NULL, table_meta = NULL, dataset_meta = NULL, ...) {
    expect_true(all(c("catches", "stations") %in% table_meta$table_id))
    expect_true("dataset_id" %in% names(dataset_meta))
    expect_true("keywords" %in% names(dataset_meta))
    expect_true(!is.null(codes))
    expect_equal(sources, c("smn", "gcdfo", "ols", "nvs"))

    attr(dict, "semantic_suggestions") <- tibble::tibble(
      column_name = c("count", "observation_date"),
      dictionary_role = c("variable", "property"),
      table_id = c("catches", "catches"),
      dataset_id = c("dataset-1", "dataset-1"),
      target_scope = c("column", "column"),
      target_sdp_file = c("column_dictionary.csv", "column_dictionary.csv"),
      target_sdp_field = c("term_iri", "entity_iri"),
      target_row_key = as.character(NA),
      target_label = c("count", "observation_date"),
      target_description = c(NA_character_, NA_character_),
      search_query = c("count", "observation_date"),
      column_label = c("count", "observation_date"),
      column_description = c(NA_character_, NA_character_),
      code_value = as.character(NA),
      code_label = as.character(NA),
      code_description = as.character(NA),
      label = c("Catch count", "Observation date"),
      iri = c("https://example.org/count", "https://example.org/date"),
      source = c("smn", "smn"),
      ontology = c("demo", "demo"),
      definition = c(NA_character_, NA_character_)
    )

    dict
  }

  artifacts <- with_mocked_bindings(
    suggest_semantics = fake_suggest,
    {
      infer_salmon_datapackage_artifacts(
        resources,
        dataset_id = "dataset-1",
        seed_semantics = TRUE,
        seed_verbose = FALSE
      )
    }
  )

  expect_type(artifacts$resources, "list")
  expect_equal(sort(names(artifacts$resources)), c("catches", "stations"))
  expect_equal(artifacts$dataset_id, "dataset-1")
  expect_s3_class(artifacts$dict, "tbl_df")
  expect_s3_class(artifacts$table_meta, "tbl_df")
  expect_true(all(c("catches", "stations") %in% artifacts$table_meta$table_id))
  expect_true(nrow(artifacts$codes) > 0)
  expect_equal(artifacts$dataset_meta$dataset_id[[1]], "dataset-1")
  expect_true(is.null(artifacts$semantic_suggestions) || is.data.frame(artifacts$semantic_suggestions))
})
