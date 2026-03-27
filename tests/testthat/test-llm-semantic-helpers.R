test_that("suggest_semantics defaults OpenRouter LLM review to openrouter/free", {
  tmp <- withr::local_tempdir()
  context_path <- file.path(tmp, "README-context.md")
  writeLines(
    c(
      "# Escapement context",
      "Spawner abundance counts were reviewed in the annual escapement report.",
      "Natural-origin spawners are counted per population."
    ),
    context_path
  )

  dict <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "spawner_count",
    column_label = "Spawner count",
    column_description = "Natural-origin spawner abundance estimate",
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
    tibble::tibble(
      label = c(paste(role, "best"), paste(role, "alt")),
      iri = c(
        paste0("https://example.org/", role, "/best"),
        paste0("https://example.org/", role, "/alt")
      ),
      source = c("smn", "smn"),
      ontology = c("demo", "demo"),
      role = c(role, role),
      match_type = c("label_partial", "label_partial"),
      definition = c("Best match from retrieved shortlist", "Alternative match from retrieved shortlist"),
      score = c(0.9, 0.5)
    )
  }

  fake_request <- function(messages, config) {
    expect_equal(config$provider, "openrouter")
    expect_equal(config$model, "openrouter/free")
    expect_true(grepl("README-context.md", messages[[2]]$content, fixed = TRUE))
    expect_true(grepl("Spawner abundance counts were reviewed", messages[[2]]$content, fixed = TRUE))

    list(
      decision = "accept",
      selected_candidate_index = 1,
      confidence = 0.92,
      rationale = "The context report explicitly describes spawner abundance counts.",
      missing_context = ""
    )
  }

  res <- suggest_semantics(
    NULL,
    dict,
    sources = "smn",
    max_per_role = 2,
    search_fn = fake_search,
    llm_assess = TRUE,
    llm_provider = "openrouter",
    llm_api_key = "dummy-key",
    llm_top_n = 2,
    llm_context_files = context_path,
    llm_request_fn = fake_request
  )

  suggestions <- attr(res, "semantic_suggestions")
  assessments <- attr(res, "semantic_llm_assessments")

  expect_true("llm_selected" %in% names(suggestions))
  expect_true("llm_candidate_rank" %in% names(suggestions))
  expect_true(any(suggestions$llm_selected))
  expect_true(all(assessments$llm_provider == "openrouter"))
  expect_true(all(assessments$llm_model == "openrouter/free"))
  expect_true(any(grepl("README-context.md", assessments$llm_context_sources, fixed = TRUE)))
})

test_that("openrouter free config defaults to smaller batched live requests but not for custom hooks", {
  config_live <- metasalmon:::.ms_llm_resolve_config(
    provider = "openrouter",
    api_key = "dummy-key"
  )
  config_custom <- metasalmon:::.ms_llm_resolve_config(
    provider = "openrouter",
    api_key = "dummy-key",
    request_fn = function(messages, config) list()
  )

  expect_equal(config_live$model, "openrouter/free")
  expect_equal(metasalmon:::.ms_llm_batch_size(config_live), 2L)
  expect_equal(metasalmon:::.ms_llm_batch_size(config_custom), 1L)
  expect_equal(metasalmon:::.ms_llm_effective_top_n(config_live, 5L), 3L)
  expect_equal(metasalmon:::.ms_llm_effective_top_n(config_custom, 5L), 3L)
})

test_that("openrouter free config gets a longer timeout and retries transient failures", {
  attempts <- 0L
  config <- metasalmon:::.ms_llm_resolve_config(
    provider = "openrouter",
    api_key = "dummy-key",
    timeout_seconds = 30,
    request_fn = function(messages, config) {
      attempts <<- attempts + 1L
      if (attempts == 1L) {
        stop("Failed to perform HTTP request. Timeout was reached [openrouter.ai].")
      }
      list(
        decision = "accept",
        selected_candidate_index = 1,
        confidence = 0.9,
        rationale = "Recovered after retry.",
        missing_context = ""
      )
    }
  )

  expect_equal(config$model, "openrouter/free")
  expect_equal(config$timeout_seconds, 90)

  result <- metasalmon:::.ms_llm_request_with_retries(
    messages = list(list(role = "user", content = "test")),
    config = config
  )

  expect_equal(attempts, 2L)
  expect_equal(result$decision, "accept")
})

test_that("invalid candidate indexes degrade to review instead of erroring", {
  candidates <- tibble::tibble(
    iri = c("https://example.org/a", "https://example.org/b"),
    label = c("A", "B")
  )

  result <- metasalmon:::.ms_validate_llm_assessment(
    list(
      decision = "accept",
      selected_candidate_index = 99,
      confidence = 0.7,
      rationale = "Bad index from model.",
      missing_context = ""
    ),
    candidates
  )

  expect_equal(result$decision, "review")
  expect_true(is.na(result$selected_candidate_index))
  expect_match(result$rationale, "out-of-range candidate index")
})

test_that("batched LLM responses are mapped back onto target records", {
  suggestions <- tibble::tibble(
    dataset_id = c("d1", "d1", "d1", "d1"),
    table_id = c("t1", "t1", "t1", "t1"),
    column_name = c("a", "a", "b", "b"),
    code_value = c(NA_character_, NA_character_, NA_character_, NA_character_),
    dictionary_role = c("variable", "variable", "variable", "variable"),
    target_scope = c("column", "column", "column", "column"),
    target_sdp_file = c("column_dictionary.csv", "column_dictionary.csv", "column_dictionary.csv", "column_dictionary.csv"),
    target_sdp_field = c("term_iri", "term_iri", "term_iri", "term_iri"),
    search_query = c("alpha", "alpha", "beta", "beta"),
    target_label = c("Alpha", "Alpha", "Beta", "Beta"),
    target_description = c("Alpha target", "Alpha target", "Beta target", "Beta target"),
    target_query_basis = c("label", "label", "label", "label"),
    target_query_context = c("ctx", "ctx", "ctx", "ctx"),
    label = c("Alpha best", "Alpha alt", "Beta best", "Beta alt"),
    iri = c("https://example.org/a1", "https://example.org/a2", "https://example.org/b1", "https://example.org/b2"),
    source = c("smn", "smn", "smn", "smn"),
    ontology = c("demo", "demo", "demo", "demo"),
    definition = c("A1", "A2", "B1", "B2"),
    score = c(0.9, 0.5, 0.8, 0.4),
    .ms_group_key = c("g1", "g1", "g2", "g2"),
    .ms_row_order = 1:4
  )

  config <- metasalmon:::.ms_llm_resolve_config(
    provider = "openrouter",
    api_key = "dummy-key",
    request_fn = function(messages, config) list()
  )
  records <- list(
    metasalmon:::.ms_llm_prepare_record("g1", suggestions[suggestions$.ms_group_key == "g1", , drop = FALSE], config, 2L, NULL, NULL),
    metasalmon:::.ms_llm_prepare_record("g2", suggestions[suggestions$.ms_group_key == "g2", , drop = FALSE], config, 2L, NULL, NULL)
  )

  fake_batch_result <- list(
    assessments = list(
      list(target_key = "g1", decision = "accept", selected_candidate_index = 1, confidence = 0.9, rationale = "Alpha best", missing_context = ""),
      list(target_key = "g2", decision = "review", selected_candidate_index = NULL, confidence = 0.4, rationale = "Need more context", missing_context = "run timing")
    )
  )

  out <- metasalmon:::.ms_llm_validate_batch_assessments(fake_batch_result, records, config)
  expect_equal(nrow(out), 2L)
  expect_equal(sort(out$column_name), c("a", "b"))
  expect_equal(out$llm_selected_iri[out$column_name == "a"], "https://example.org/a1")
  expect_true(is.na(out$llm_selected_iri[out$column_name == "b"]))
})

test_that("apply_semantic_suggestions can use llm strategy with a confidence threshold", {
  dict <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "spawner_count",
    column_label = "Spawner count",
    column_description = "Spawner abundance",
    term_iri = NA_character_,
    property_iri = NA_character_,
    entity_iri = NA_character_,
    constraint_iri = NA_character_,
    method_iri = NA_character_,
    unit_label = NA_character_,
    unit_iri = NA_character_,
    term_type = NA_character_,
    value_type = "integer",
    column_role = "measurement",
    required = NA
  )

  suggestions <- tibble::tibble(
    column_name = c("spawner_count", "spawner_count"),
    dictionary_role = c("variable", "variable"),
    table_id = c("t1", "t1"),
    dataset_id = c("d1", "d1"),
    target_scope = c("column", "column"),
    target_sdp_file = c("column_dictionary.csv", "column_dictionary.csv"),
    target_sdp_field = c("term_iri", "term_iri"),
    iri = c("https://example.org/variable/alt", "https://example.org/variable/best"),
    label = c("Alternative variable", "Best variable"),
    llm_selected = c(FALSE, TRUE),
    llm_confidence = c(0.3, 0.96)
  )

  out <- apply_semantic_suggestions(
    dict,
    suggestions = suggestions,
    strategy = "llm",
    min_llm_confidence = 0.9,
    verbose = FALSE
  )

  expect_equal(out$term_iri[[1]], "https://example.org/variable/best")

  expect_error(
    apply_semantic_suggestions(
      dict,
      suggestions = suggestions[, setdiff(names(suggestions), "llm_selected")],
      strategy = "llm",
      verbose = FALSE
    ),
    "LLM-reviewed suggestions"
  )
})

test_that("infer_salmon_datapackage_artifacts forwards llm options into suggest_semantics", {
  captured <- NULL
  fake_suggest <- function(df, dict, ...) {
    captured <<- list(...)
    attr(dict, "semantic_suggestions") <- tibble::tibble()
    attr(dict, "semantic_llm_assessments") <- tibble::tibble()
    dict
  }

  with_mocked_bindings(
    suggest_semantics = fake_suggest,
    {
      infer_salmon_datapackage_artifacts(
        resources = list(main = tibble::tibble(spawner_count = c(1L, 2L))),
        dataset_id = "d1",
        seed_semantics = TRUE,
        llm_assess = TRUE,
        llm_provider = "openrouter",
        llm_model = "openai/gpt-oss-20b:free",
        llm_api_key = "dummy-key",
        llm_context_text = "Spawner context from inline note.",
        llm_top_n = 4L
      )
    },
    .package = "metasalmon"
  )

  expect_true(isTRUE(captured$llm_assess))
  expect_equal(captured$llm_provider, "openrouter")
  expect_equal(captured$llm_model, "openai/gpt-oss-20b:free")
  expect_equal(captured$llm_api_key, "dummy-key")
  expect_equal(captured$llm_context_text, "Spawner context from inline note.")
  expect_equal(captured$llm_top_n, 4L)
})

test_that("PDF context files either extract text or fail clearly when pdftools is unavailable", {
  tmp <- withr::local_tempdir()
  pdf_path <- file.path(tmp, "context.pdf")

  grDevices::pdf(pdf_path)
  plot.new()
  text(0.5, 0.5, "Spawner context PDF")
  grDevices::dev.off()

  if (requireNamespace("pdftools", quietly = TRUE)) {
    result <- metasalmon:::.ms_context_text_from_file(pdf_path)
    expect_true(is.list(result))
    expect_true(nzchar(result$text))
    expect_equal(result$source, "context.pdf")
  } else {
    expect_error(
      metasalmon:::.ms_context_text_from_file(pdf_path),
      "pdftools"
    )
  }
})
