demo_decomp_dict <- function() {
  tibble::tibble(
    dataset_id = "demo-dataset",
    table_id = "main",
    column_name = "spawner_count",
    column_label = "Spawner count",
    column_description = "Estimated natural-origin spawner abundance",
    column_role = "measurement",
    value_type = "integer",
    unit_label = "count",
    unit_iri = NA_character_,
    term_iri = NA_character_,
    property_iri = NA_character_,
    entity_iri = NA_character_,
    constraint_iri = NA_character_,
    method_iri = NA_character_
  )
}

demo_decomp_suggestions <- function() {
  tibble::tibble(
    dataset_id = c("demo-dataset", "demo-dataset", "demo-dataset"),
    table_id = c("main", "main", "main"),
    column_name = c("spawner_count", "spawner_count", "spawner_count"),
    code_value = c(NA_character_, NA_character_, NA_character_),
    dictionary_role = c("variable", "variable", "property"),
    target_scope = c("column", "column", "column"),
    target_sdp_file = c("column_dictionary.csv", "column_dictionary.csv", "column_dictionary.csv"),
    target_sdp_field = c("term_iri", "term_iri", "property_iri"),
    target_row_key = c("demo-dataset/main/spawner_count", "demo-dataset/main/spawner_count", "demo-dataset/main/spawner_count"),
    target_label = c("Spawner count", "Spawner count", "Spawner count"),
    target_description = c("Estimated natural-origin spawner abundance", "Estimated natural-origin spawner abundance", "Estimated natural-origin spawner abundance"),
    search_query = c("spawner abundance", "natural-origin spawner abundance", "abundance"),
    target_query_basis = c("column_description", "column_description", "column_description"),
    target_query_context = c("spawner count abundance", "spawner count abundance", "spawner count abundance"),
    column_label = c("Spawner count", "Spawner count", "Spawner count"),
    column_description = c("Estimated natural-origin spawner abundance", "Estimated natural-origin spawner abundance", "Estimated natural-origin spawner abundance"),
    label = c("Spawner abundance", "Natural-origin spawner abundance", "Abundance"),
    iri = c(
      "https://example.org/variable/spawner-abundance",
      "https://example.org/variable/natural-origin-spawner-abundance",
      "https://example.org/property/abundance"
    ),
    source = c("smn", "smn", "smn"),
    ontology = c("demo", "demo", "demo"),
    definition = c(
      "Spawner abundance variable",
      "Natural-origin spawner abundance variable",
      "Abundance property"
    ),
    score = c(0.92, 0.88, 0.96),
    llm_selected = c(FALSE, FALSE, FALSE)
  )
}

test_that("chat_decomposition persists state separately from transcript and resumes cleanly", {
  tmp <- withr::local_tempdir()
  dict <- demo_decomp_dict()
  suggestions <- demo_decomp_suggestions()

  out1 <- chat_decomposition(
    dict,
    column_name = "spawner_count",
    suggestions = suggestions,
    session_root = tmp,
    commands = c(
      "abundance",
      "spawner population",
      "natural-origin adults",
      "/quit"
    ),
    output_fn = function(text) invisible(text)
  )

  expect_equal(out1$approval_status, "draft")
  expect_true(file.exists(file.path(out1$session_dir, "state.rds")))
  expect_true(file.exists(file.path(out1$session_dir, "transcript.rds")))

  state1 <- readRDS(file.path(out1$session_dir, "state.rds"))
  transcript1 <- readRDS(file.path(out1$session_dir, "transcript.rds"))

  expect_false("transcript" %in% names(state1))
  expect_length(state1$approved_facts, 3L)
  expect_equal(state1$turn_summaries[[1]]$group, "core_observable")
  expect_true(all(c("matrix", "context_object", "used_procedure") %in% state1$unresolved_items))
  expect_true(length(transcript1) > 3L)

  out2 <- chat_decomposition(
    dict,
    column_name = "spawner_count",
    suggestions = suggestions,
    session_root = tmp,
    session_id = out1$session_id,
    commands = c(
      "escapement survey medium",
      "spawning reach",
      "visual count protocol",
      "/more",
      "count",
      "annual total",
      "estimated from observation",
      "/more",
      "annual",
      "spawning reach",
      "/choose 2",
      "/approve"
    ),
    output_fn = function(text) invisible(text)
  )

  state2 <- readRDS(file.path(out2$session_dir, "state.rds"))
  transcript2 <- readRDS(file.path(out2$session_dir, "transcript.rds"))

  expect_equal(out2$approval_status, "approved")
  expect_equal(state2$approval$status, "approved")
  expect_false(is.null(out2$approved_patch))
  expect_equal(state2$proposed_patch$selected_candidate_index, 2L)
  expect_equal(state2$proposed_patch$term_type, "skos_concept")
  expect_gt(length(transcript2), length(transcript1))
})

test_that("chat_decomposition can reuse suggest_semantics when suggestions are not supplied", {
  tmp <- withr::local_tempdir()
  dict <- demo_decomp_dict()
  suggestions <- demo_decomp_suggestions()
  captured <- NULL

  fake_suggest <- function(df, dict, ...) {
    captured <<- list(...)
    attr(dict, "semantic_suggestions") <- suggestions
    dict
  }

  out <- with_mocked_bindings(
    suggest_semantics = fake_suggest,
    {
      chat_decomposition(
        dict,
        column_name = "spawner_count",
        session_root = tmp,
        commands = c(
          "abundance",
          "spawner population",
          "natural-origin adults",
          "/quit"
        ),
        output_fn = function(text) invisible(text)
      )
    },
    .package = "metasalmon"
  )

  expect_true(!is.null(captured))
  expect_equal(captured$llm_assess, FALSE)
  expect_equal(nrow(out$state$candidate_rows), 2L)
  expect_true(all(out$state$candidate_rows$dictionary_role == "variable"))
  expect_true(all(out$state$candidate_rows$target_sdp_field == "term_iri"))
})

test_that("mocked chat adapter can steer candidate choice and keeps usedProcedure wording", {
  tmp <- withr::local_tempdir()
  dict <- demo_decomp_dict()
  suggestions <- demo_decomp_suggestions()
  captured_messages <- NULL

  fake_chat <- function(messages, config) {
    captured_messages <<- messages
    list(
      decision = "accept",
      selected_candidate_index = 2,
      confidence = 0.93,
      rationale = "Candidate 2 fits the usedProcedure-aware decomposition.",
      missing_context = ""
    )
  }

  out <- chat_decomposition(
    dict,
    column_name = "spawner_count",
    suggestions = suggestions,
    session_root = tmp,
    chat_request_fn = fake_chat,
    commands = c(
      "abundance",
      "spawner population",
      "natural-origin adults",
      "/more",
      "escapement survey medium",
      "spawning reach",
      "mark-recapture protocol",
      "/more",
      "count",
      "annual total",
      "estimated",
      "/more",
      "annual",
      "spawning reach",
      "/approve"
    ),
    output_fn = function(text) invisible(text)
  )

  prompt_text <- paste(vapply(captured_messages, function(msg) msg$content, character(1)), collapse = "\n")

  expect_equal(out$approval_status, "approved")
  expect_equal(out$approved_patch$selected_candidate_index, 2L)
  expect_equal(out$approved_patch$proposal_source, "chat")
  expect_match(prompt_text, "usedProcedure", fixed = TRUE)
  expect_match(prompt_text, "SKOS concept", fixed = TRUE)
  expect_false(grepl("iadoptMethod", prompt_text, fixed = TRUE))
})
