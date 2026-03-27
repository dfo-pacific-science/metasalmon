test_that("detect_semantic_term_gaps classifies non-SMN candidates", {
  suggestions <- tibble::tibble(
    dataset_id = c("d1", "d1"),
    table_id = c("t1", "t1"),
    column_name = c("run_id", "run_id"),
    code_value = NA_character_,
    dictionary_role = c("variable", "variable"),
    target_scope = c("column", "column"),
    target_sdp_file = c("column_dictionary.csv", "column_dictionary.csv"),
    target_sdp_field = c("term_iri", "term_iri"),
    target_row_key = c("run_id", "run_id"),
    search_query = c("run id", "run id"),
    column_label = c("Run ID", "Run ID"),
    column_description = c("Local run identifier", "Local run identifier"),
    label = c("run id", "run identifier"),
    iri = c(NA_character_, NA_character_),
    source = c("gbif", "worms"),
    ontology = c("gbif", "worms"),
    match_type = c("label", "label"),
    definition = c(NA_character_, NA_character_),
    score = c(0.9, 0.85)
  )

  gaps <- detect_semantic_term_gaps(
    dict = NULL,
    suggestions = suggestions,
    include_dictionary_roles = c("variable")
  )

  expect_s3_class(gaps, c("tbl_df", "tbl", "data.frame"))
  expect_true(nrow(gaps) >= 1L)
  expect_equal(gaps$placement_recommendation[[1]], "profile")
})


test_that("detect_semantic_term_gaps can recommend shared placement", {
  suggestions <- tibble::tibble(
    dataset_id = c("d1", "d1"),
    table_id = c("t1", "t1"),
    column_name = c("escape_rate", "escape_rate"),
    code_value = NA_character_,
    dictionary_role = c("variable", "variable"),
    target_scope = c("column", "column"),
    target_sdp_file = c("column_dictionary.csv", "column_dictionary.csv"),
    target_sdp_field = c("term_iri", "term_iri"),
    target_row_key = c("escape_rate", "escape_rate"),
    search_query = c("escape rate", "escape rate"),
    column_label = c("Escape rate", "Escape rate"),
    column_description = c("Percent of fish escaping", "Percent of fish escaping"),
    label = c("escape rate", "escape rate"),
    iri = c(NA_character_, NA_character_),
    source = c("gcdfo", "ols"),
    ontology = c("https://w3id.org/gcdfo/salmon#", "https://www.ebi.ac.uk/ols/"),
    match_type = c("label", "label"),
    definition = c(NA_character_, NA_character_),
    score = c(0.95, 0.9)
  )

  gaps <- detect_semantic_term_gaps(suggestions = suggestions)
  expect_true(nrow(gaps) >= 1L)
  expect_equal(gaps$placement_recommendation[[1]], "smn")
})


test_that("render_ontology_term_request defaults to salmon-domain ontology repo", {
  gaps <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "run_id",
    code_value = NA_character_,
    target_scope = "column",
    target_sdp_file = "column_dictionary.csv",
    target_sdp_field = "term_iri",
    target_row_key = "run_id",
    dictionary_role = "variable",
    search_query = "run id",
    column_label = "Run ID",
    column_description = "Dataset-specific run identifier",
    top_non_smn_source = "gbif",
    top_non_smn_label = "Run event id",
    top_non_smn_iri = NA_character_,
    top_non_smn_ontology = NA_character_,
    top_non_smn_match_type = "label",
    top_non_smn_score = 0.9,
    candidate_count = 1L,
    non_smn_sources = "gbif",
    placement_recommendation = "smn",
    placement_confidence = 0.82,
    placement_rationale = "shared domain concept"
  )

  reqs <- render_ontology_term_request(gaps, ask = FALSE)

  expect_equal(reqs$ontology_repo, "salmon-data-mobilization/salmon-domain-ontology")
  expect_true(grepl("salmon-domain-ontology", reqs$request_body, fixed = TRUE))
})

test_that("render_ontology_term_request uses profile scope", {
  gaps <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "run_id",
    code_value = NA_character_,
    target_scope = "column",
    target_sdp_file = "column_dictionary.csv",
    target_sdp_field = "term_iri",
    target_row_key = "run_id",
    dictionary_role = "variable",
    search_query = "run id",
    column_label = "Run ID",
    column_description = "Dataset-specific run identifier",
    top_non_smn_source = "gbif",
    top_non_smn_label = "Run event id",
    top_non_smn_iri = NA_character_,
    top_non_smn_ontology = NA_character_,
    top_non_smn_match_type = "label",
    top_non_smn_score = 0.9,
    candidate_count = 1L,
    non_smn_sources = "gbif",
    placement_recommendation = "profile",
    placement_confidence = 0.82,
    placement_rationale = "contains internal identifier signal"
  )

  reqs <- render_ontology_term_request(
    gaps,
    scope = "profile",
    ask = FALSE,
    profile_name = "pacific-monitoring"
  )

  expect_equal(reqs$request_scope, "profile")
  expect_true(grepl("pacific-monitoring", reqs$request_title, fixed = TRUE))
  expect_true(grepl("New term template", reqs$request_body, fixed = TRUE))
})


test_that("render_ontology_term_request errors when auto routing needs a profile name", {
  gaps <- tibble::tibble(
    dataset_id = c("d1", "d1"),
    table_id = c("t1", "t1"),
    column_name = c("run_id", "escape_rate"),
    code_value = c(NA_character_, NA_character_),
    target_scope = c("column", "column"),
    target_sdp_file = c("column_dictionary.csv", "column_dictionary.csv"),
    target_sdp_field = c("term_iri", "term_iri"),
    target_row_key = c("run_id", "escape_rate"),
    dictionary_role = c("variable", "variable"),
    search_query = c("run id", "escape rate"),
    column_label = c("Run ID", "Escape rate"),
    column_description = c("Dataset-specific run identifier", "Percent of fish escaping"),
    top_non_smn_source = c("gbif", "gcdfo"),
    top_non_smn_label = c("Run event id", "Escape rate"),
    top_non_smn_iri = c(NA_character_, NA_character_),
    top_non_smn_ontology = c(NA_character_, "https://w3id.org/gcdfo/salmon#"),
    top_non_smn_match_type = c("label", "label"),
    top_non_smn_score = c(0.9, 0.95),
    candidate_count = c(1L, 1L),
    non_smn_sources = c("gbif", "gcdfo"),
    placement_recommendation = c("profile", "smn"),
    placement_confidence = c(0.82, 0.95),
    placement_rationale = c("contains internal identifier signal", "shared domain concept")
  )

  expect_error(
    render_ontology_term_request(gaps, scope = "auto", ask = FALSE),
    "Non-interactive profile-scoped requests require `profile_name`"
  )
})


test_that("submit_term_request_issues dry run and mock post", {
  reqs <- tibble::tibble(
    request_title = c("Request new shared SMN term: escape rate"),
    request_body = c("body"),
    request_scope = c("smn"),
    ontology_repo = c("salmon-data-mobilization/salmon-domain-ontology"),
    issue_labels = list(NULL)
  )

  dry <- submit_term_request_issues(reqs, dry_run = TRUE, confirm = FALSE)
  expect_equal(dry$status, "dry_run")
  expect_true(all(is.na(dry$issue_number)))

  called <- 0L
  with_mocked_bindings(
    .metasalmon_post_issue = function(...) {
      called <<- called + 1L
      list(number = 42L, html_url = "https://github.com/salmon-data-mobilization/salmon-domain-ontology/issues/42")
    },
    {
      submitted <- submit_term_request_issues(reqs, dry_run = FALSE, confirm = FALSE, token = "test-token")
      expect_equal(called, 1L)
      expect_equal(submitted$status, "submitted")
      expect_equal(submitted$issue_number, 42L)
    }
  )
})

test_that("submit_term_request_issues posts each request to its row-level ontology repo", {
  reqs <- tibble::tibble(
    request_title = c("Request A", "Request B"),
    request_body = c("body a", "body b"),
    request_scope = c("smn", "profile"),
    ontology_repo = c(
      "salmon-data-mobilization/salmon-domain-ontology",
      "dfo-pacific-science/salmon-profile-ontology"
    ),
    issue_labels = list(NULL, NULL)
  )

  called_repos <- character()
  with_mocked_bindings(
    .metasalmon_post_issue = function(repo, ...) {
      called_repos <<- c(called_repos, repo)
      list(number = 1L, html_url = paste0("https://github.com/", repo, "/issues/1"))
    },
    {
      submitted <- submit_term_request_issues(reqs, dry_run = FALSE, confirm = FALSE, token = "test-token")
      expect_equal(nrow(submitted), 2L)
      expect_equal(submitted$status, c("submitted", "submitted"))
    }
  )

  expect_equal(called_repos, reqs$ontology_repo)
})
