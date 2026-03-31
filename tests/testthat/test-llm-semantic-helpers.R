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

test_that("suggest_semantics defaults chapi LLM review to the internal mistral endpoint", {
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
    expect_equal(config$provider, "chapi")
    expect_equal(config$model, "ollama2.mistral:7b")
    expect_equal(config$base_url, "https://chapi-dev.intra.azure.cloud.dfo-mpo.gc.ca/api")

    list(
      decision = "accept",
      selected_candidate_index = 1,
      confidence = 0.92,
      rationale = "The internal Mistral endpoint returned a clear best match.",
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
    llm_provider = "chapi",
    llm_api_key = "dummy-key",
    llm_top_n = 2,
    llm_request_fn = fake_request
  )

  suggestions <- attr(res, "semantic_suggestions")
  assessments <- attr(res, "semantic_llm_assessments")

  expect_true("llm_selected" %in% names(suggestions))
  expect_true(any(suggestions$llm_selected))
  expect_true(all(assessments$llm_provider == "chapi"))
  expect_true(all(assessments$llm_model == "ollama2.mistral:7b"))
})

test_that("chapi config reads provider-specific env vars before generic fallbacks", {
  withr::local_envvar(c(
    CHAPI_API_KEY = "env-chapi-key",
    CHAPI_BASE_URL = "https://example.internal/api",
    CHAPI_MODEL = "ollama2.llama3:8b",
    METASALMON_LLM_API_KEY = "",
    METASALMON_LLM_BASE_URL = "",
    METASALMON_LLM_MODEL = ""
  ))

  config <- metasalmon:::.ms_llm_resolve_config(provider = "chapi")

  expect_equal(config$api_key, "env-chapi-key")
  expect_equal(config$base_url, "https://example.internal/api")
  expect_equal(config$model, "ollama2.llama3:8b")
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

test_that("chapi gpt-oss config gets a longer timeout and retries transient failures", {
  attempts <- 0L
  config <- metasalmon:::.ms_llm_resolve_config(
    provider = "chapi",
    model = "gpt-oss:latest",
    api_key = "dummy-key",
    timeout_seconds = 45,
    request_fn = function(messages, config) {
      attempts <<- attempts + 1L
      if (attempts == 1L) {
        stop("Failed to perform HTTP request. Timeout was reached [chapi-dev.intra.azure.cloud.dfo-mpo.gc.ca].")
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

  expect_equal(config$timeout_seconds, 120)

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

test_that("accept without a selected candidate degrades to review", {
  candidates <- tibble::tibble(
    iri = c("https://example.org/a", "https://example.org/b"),
    label = c("A", "B")
  )

  result <- metasalmon:::.ms_validate_llm_assessment(
    list(
      decision = "accept",
      selected_candidate_index = NULL,
      confidence = 0.81,
      rationale = "Top candidate looks right.",
      missing_context = ""
    ),
    candidates
  )

  expect_equal(result$decision, "review")
  expect_true(is.na(result$selected_candidate_index))
  expect_match(result$rationale, "without selecting a candidate")
})

test_that("vector-valued confidence uses the first element instead of erroring", {
  candidates <- tibble::tibble(
    iri = c("https://example.org/a", "https://example.org/b"),
    label = c("A", "B")
  )

  result <- metasalmon:::.ms_validate_llm_assessment(
    list(
      decision = "accept",
      selected_candidate_index = 1,
      confidence = c(0.82, 0.31),
      rationale = "Primary confidence is usable.",
      missing_context = ""
    ),
    candidates
  )

  expect_equal(result$decision, "accept")
  expect_equal(result$selected_candidate_index, 1L)
  expect_equal(result$confidence, 0.82)
})

test_that("nested structured values are flattened to the first scalar", {
  candidates <- tibble::tibble(
    iri = c("https://example.org/a", "https://example.org/b"),
    label = c("A", "B")
  )

  result <- metasalmon:::.ms_validate_llm_assessment(
    list(
      decision = list(c("accept", "review")),
      selected_candidate_index = list(c(2L, 1L)),
      confidence = list(c(0.74, 0.12)),
      rationale = list(c("Use the first rationale.", "Ignore this one.")),
      missing_context = list(c("", "extra"))
    ),
    candidates
  )

  expect_equal(result$decision, "accept")
  expect_equal(result$selected_candidate_index, 2L)
  expect_equal(result$confidence, 0.74)
  expect_equal(result$rationale, "Use the first rationale.")
})

test_that("falsey missing_context strings normalize to NA", {
  candidates <- tibble::tibble(
    iri = c("https://example.org/a", "https://example.org/b"),
    label = c("A", "B")
  )

  result <- metasalmon:::.ms_validate_llm_assessment(
    list(
      decision = "review",
      selected_candidate_index = NULL,
      confidence = 0.42,
      rationale = "Need more context.",
      missing_context = "FALSE"
    ),
    candidates
  )

  expect_true(is.na(result$missing_context))
})

test_that("JSON cleaner extracts the first balanced object from wrapper text", {
  text <- paste(
    "Here is the result you requested:",
    '{\"alternate_queries\":[\"fish abundance\",\"run timing\"],\"rationale\":\"better fit\"}',
    "Use that JSON only.",
    sep = " "
  )

  cleaned <- metasalmon:::.ms_llm_clean_json_text(text)

  expect_equal(
    cleaned,
    "{\"alternate_queries\":[\"fish abundance\",\"run timing\"],\"rationale\":\"better fit\"}"
  )
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
  expect_equal(captured$max_per_role, 4L)
  expect_equal(captured$llm_provider, "openrouter")
  expect_equal(captured$llm_model, "openai/gpt-oss-20b:free")
  expect_equal(captured$llm_api_key, "dummy-key")
  expect_equal(captured$llm_context_text, "Spawner context from inline note.")
  expect_equal(captured$llm_top_n, 4L)
})

test_that("LLM context files are parsed and chunked once per assessment run", {
  tmp <- withr::local_tempdir()
  context_path <- file.path(tmp, "context.md")
  writeLines(
    c(
      "# Context",
      "Spawner abundance and juvenile abundance are both described here.",
      "These notes should only be read and chunked once per run."
    ),
    context_path
  )

  dict <- tibble::tibble(
    dataset_id = c("d1", "d1"),
    table_id = c("t1", "t1"),
    column_name = c("spawner_count", "juvenile_count"),
    column_label = c("Spawner count", "Juvenile count"),
    column_description = c("Spawner abundance", "Juvenile abundance"),
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

  fake_search <- function(query, role, sources) {
    tibble::tibble(
      label = c(paste(query, "best"), paste(query, "alt")),
      iri = c(
        paste0("https://example.org/", gsub("[^a-z]+", "-", tolower(query)), "/best"),
        paste0("https://example.org/", gsub("[^a-z]+", "-", tolower(query)), "/alt")
      ),
      source = c("smn", "smn"),
      ontology = c("demo", "demo"),
      role = c(role, role),
      match_type = c("label_partial", "label_partial"),
      definition = c("Best match", "Alt match"),
      score = c(0.9, 0.6)
    )
  }

  fake_request <- function(messages, config) {
    list(
      decision = "accept",
      selected_candidate_index = 1,
      confidence = 0.9,
      rationale = "Top candidate is fine.",
      missing_context = ""
    )
  }

  read_calls <- 0L
  chunk_calls <- 0L
  orig_context_text_from_file <- metasalmon:::.ms_context_text_from_file
  orig_chunk_context_text <- metasalmon:::.ms_chunk_context_text

  with_mocked_bindings(
    `.ms_context_text_from_file` = function(path) {
      read_calls <<- read_calls + 1L
      orig_context_text_from_file(path)
    },
    `.ms_chunk_context_text` = function(text, source, chunk_chars = 2200L, overlap_chars = 200L) {
      chunk_calls <<- chunk_calls + 1L
      orig_chunk_context_text(text, source, chunk_chars = chunk_chars, overlap_chars = overlap_chars)
    },
    {
      out <- suggest_semantics(
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

      expect_gt(nrow(attr(out, "semantic_llm_assessments")), 1L)
    },
    .package = "metasalmon"
  )

  expect_equal(read_calls, 1L)
  expect_equal(chunk_calls, 1L)
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

test_that("Excel context files either extract sheet text or fail clearly when readxl is unavailable", {
  testthat::skip_if_not_installed("openxlsx")

  tmp <- withr::local_tempdir()
  xlsx_path <- file.path(tmp, "context.xlsx")

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "dictionary")
  openxlsx::writeData(
    wb,
    "dictionary",
    tibble::tibble(
      field = c("column_name", "column_description"),
      value = c("spawner_count", "Spawner abundance estimate")
    )
  )
  openxlsx::addWorksheet(wb, "notes")
  openxlsx::writeData(
    wb,
    "notes",
    tibble::tibble(
      section = "overview",
      text = "Spawner abundance counts are summarized by population and year."
    )
  )
  openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)

  if (requireNamespace("readxl", quietly = TRUE)) {
    result <- metasalmon:::.ms_context_text_from_file(xlsx_path)
    expect_true(is.list(result))
    expect_equal(result$source, "context.xlsx")
    expect_match(result$text, "Sheet: dictionary", fixed = TRUE)
    expect_match(result$text, "Spawner abundance counts are summarized", fixed = TRUE)
  } else {
    expect_error(
      metasalmon:::.ms_context_text_from_file(xlsx_path),
      "readxl"
    )
  }
})

test_that("chapi/mistral review can use mixed context files across dataset, table, column, and code targets", {
  testthat::skip_if_not_installed("openxlsx")
  testthat::skip_if_not_installed("readxl")
  testthat::skip_if_not_installed("pdftools")

  tmp <- withr::local_tempdir()
  md_path <- file.path(tmp, "context.md")
  csv_path <- file.path(tmp, "context.csv")
  xlsx_path <- file.path(tmp, "context.xlsx")
  pdf_path <- file.path(tmp, "context.pdf")

  writeLines(
    c(
      "# Monitoring context",
      "Spawner abundance and method codes are summarized by station and year.",
      "Use the package metadata to review dataset keywords and table observation units."
    ),
    md_path
  )

  readr::write_csv(
    tibble::tibble(
      field = c("dataset_keywords", "observation_unit", "method_code"),
      note = c(
        "salmon monitoring keywords",
        "station visit observation",
        "field method vocabulary"
      )
    ),
    csv_path,
    na = ""
  )

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "dictionary")
  openxlsx::writeData(
    wb,
    "dictionary",
    tibble::tibble(
      column_name = c("count", "method_code"),
      description = c("Spawner abundance count", "Field method code")
    )
  )
  openxlsx::addWorksheet(wb, "dataset")
  openxlsx::writeData(
    wb,
    "dataset",
    tibble::tibble(
      title = "Spawner monitoring package",
      description = "Dataset keywords should reflect salmon monitoring and station visits."
    )
  )
  openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)

  grDevices::pdf(pdf_path)
  plot.new()
  text(0.5, 0.6, "Spawner monitoring technical note")
  text(0.5, 0.4, "Method codes, observation units, and dataset keywords are described here.")
  grDevices::dev.off()

  resources <- list(
    visits = tibble::tibble(
      station_id = c("S1", "S2", "S3"),
      visit_year = c(2024L, 2024L, 2025L),
      count = c(12L, 18L, 9L),
      method_code = factor(c("trap", "visual", "trap"))
    )
  )

  seed_dataset_meta <- tibble::tibble(
    dataset_id = "demo-dataset",
    title = "Spawner monitoring package",
    description = "Station visit summaries for salmon monitoring.",
    creator = "Demo team",
    contact_name = "Demo contact",
    contact_email = "demo@example.org",
    license = "Open Government Licence - Canada",
    spec_version = "sdp-0.1.0",
    keywords = NA_character_,
    temporal_start = NA_character_,
    temporal_end = NA_character_
  )

  seed_table_meta <- tibble::tibble(
    dataset_id = "demo-dataset",
    table_id = "visits",
    file_name = "visits.csv",
    table_label = "Spawner monitoring visits",
    description = "Each row is a station visit observation with a salmon count and method code.",
    observation_unit = "station visit observation",
    observation_unit_iri = NA_character_,
    primary_key = NA_character_
  )

  artifacts <- infer_salmon_datapackage_artifacts(
    resources = resources,
    dataset_id = "demo-dataset",
    table_id = "visits",
    seed_semantics = FALSE,
    seed_table_meta = seed_table_meta,
    seed_dataset_meta = seed_dataset_meta,
    semantic_code_scope = "all"
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

  seen_messages <- character()
  fake_request <- function(messages, config) {
    seen_messages <<- c(seen_messages, messages[[2]]$content)
    expect_equal(config$provider, "chapi")
    expect_equal(config$model, "ollama2.mistral:7b")

    list(
      decision = "accept",
      selected_candidate_index = 1,
      confidence = 0.87,
      rationale = "The mixed context bundle supports the first candidate.",
      missing_context = ""
    )
  }

  out <- suggest_semantics(
    df = artifacts$resources,
    dict = artifacts$dict,
    codes = artifacts$codes,
    table_meta = artifacts$table_meta,
    dataset_meta = artifacts$dataset_meta,
    sources = "smn",
    max_per_role = 2,
    search_fn = fake_search,
    llm_assess = TRUE,
    llm_provider = "chapi",
    llm_model = "ollama2.mistral:7b",
    llm_api_key = "dummy-key",
    llm_context_files = c(md_path, csv_path, xlsx_path, pdf_path),
    llm_request_fn = fake_request
  )

  suggestions <- attr(out, "semantic_suggestions")
  assessments <- attr(out, "semantic_llm_assessments")

  expect_true(all(c("column_dictionary.csv", "codes.csv", "tables.csv", "dataset.csv") %in% unique(suggestions$target_sdp_file)))
  expect_true(all(assessments$llm_provider == "chapi"))
  expect_true(all(assessments$llm_model == "ollama2.mistral:7b"))
  expect_true(any(grepl("context.md", assessments$llm_context_sources, fixed = TRUE)))
  expect_true(any(grepl("context.csv", assessments$llm_context_sources, fixed = TRUE)))
  expect_true(any(grepl("context.xlsx", assessments$llm_context_sources, fixed = TRUE)))
  expect_true(any(grepl("context.pdf", assessments$llm_context_sources, fixed = TRUE)))
  expect_true(length(seen_messages) >= 4L)
})

test_that("create_sdp auto-writes LLM-selected IRIs with REVIEW prefix", {
  tmp <- withr::local_tempdir()
  resources <- list(main = tibble::tibble(spawner_count = c(1L, 2L)))
  fake_suggest <- function(df, dict, ...) {
    suggestions <- tibble::tibble(
      dataset_id = "demo",
      table_id = "main",
      column_name = "spawner_count",
      code_value = NA_character_,
      dictionary_role = "variable",
      target_scope = "column",
      target_sdp_file = "column_dictionary.csv",
      target_sdp_field = "term_iri",
      search_query = "spawner abundance",
      target_label = "Spawner count",
      target_description = "Spawner abundance",
      target_query_basis = "label",
      target_query_context = "demo",
      label = "Spawner abundance",
      iri = "https://w3id.org/smn/SpawnerAbundance",
      source = "smn",
      ontology = "smn",
      definition = "Spawner abundance term",
      score = 0.95,
      llm_provider = "openai",
      llm_model = "gpt-4.1-mini",
      llm_decision = "accept",
      llm_confidence = 0.93,
      llm_selected_candidate_index = 1L,
      llm_selected_iri = "https://w3id.org/smn/SpawnerAbundance",
      llm_selected_label = "Spawner abundance",
      llm_rationale = "Best semantic fit.",
      llm_missing_context = NA_character_,
      llm_context_sources = NA_character_,
      llm_error = NA_character_,
      llm_candidate_rank = 1L,
      llm_selected = TRUE
    )
    attr(dict, "semantic_suggestions") <- suggestions
    attr(dict, "semantic_llm_assessments") <- tibble::tibble()
    dict
  }

  with_mocked_bindings(
    suggest_semantics = fake_suggest,
    {
      pkg_path <- create_sdp(
        resources,
        path = file.path(tmp, "pkg-llm-review"),
        dataset_id = "demo",
        table_id = "main",
        seed_semantics = TRUE,
        llm_assess = TRUE,
        check_updates = FALSE,
        overwrite = TRUE
      )

      dict_written <- readr::read_csv(file.path(pkg_path, "metadata", "column_dictionary.csv"), show_col_types = FALSE)
      review_txt <- paste(readLines(file.path(pkg_path, "README-review.txt"), warn = FALSE), collapse = "\n")

      expect_equal(dict_written$term_iri[[1]], "REVIEW: https://w3id.org/smn/SpawnerAbundance")
      expect_match(review_txt, "REVIEW:", fixed = TRUE)
      expect_match(review_txt, "already lives there", fixed = TRUE)
      expect_match(review_txt, "salmon-domain-ontology/issues/new/choose", fixed = TRUE)
    },
    .package = "metasalmon"
  )
})

test_that("validate_dictionary keeps strong non-strict warnings for direct review markers and gaps", {
  dict_with_review <- tibble::tibble(
    dataset_id = "demo",
    table_id = "main",
    column_name = "spawner_count",
    column_label = "Spawner count",
    column_description = "Spawner abundance",
    column_role = "measurement",
    value_type = "integer",
    required = TRUE,
    term_iri = "REVIEW: https://w3id.org/smn/SpawnerAbundance",
    property_iri = "https://w3id.org/smn/SpawnerAbundance",
    entity_iri = "https://w3id.org/smn/Spawner",
    unit_iri = "https://qudt.org/vocab/unit/NUM",
    unit_label = "count",
    constraint_iri = NA_character_,
    method_iri = NA_character_
  )

  dict_with_missing <- tibble::tibble(
    dataset_id = "demo",
    table_id = "main",
    column_name = "spawner_count",
    column_label = "Spawner count",
    column_description = "Spawner abundance",
    column_role = "measurement",
    value_type = "integer",
    required = TRUE,
    term_iri = NA_character_,
    property_iri = NA_character_,
    entity_iri = NA_character_,
    unit_iri = NA_character_,
    unit_label = "count",
    constraint_iri = NA_character_,
    method_iri = NA_character_
  )

  expect_warning(
    validate_dictionary(dict_with_review, require_iris = FALSE),
    "REVIEW-prefixed IRI values were found"
  )
  expect_warning(
    validate_dictionary(dict_with_missing, require_iris = FALSE),
    "definitely should fill those out"
  )
})

test_that("validate_dictionary fails final validation when REVIEW-prefixed IRIs remain", {
  dict <- tibble::tibble(
    dataset_id = "demo",
    table_id = "main",
    column_name = "spawner_count",
    column_label = "Spawner count",
    column_description = "Spawner abundance",
    column_role = "measurement",
    value_type = "integer",
    required = TRUE,
    term_iri = "REVIEW: https://w3id.org/smn/SpawnerAbundance",
    property_iri = "https://w3id.org/smn/SpawnerAbundance",
    entity_iri = "https://w3id.org/smn/Spawner",
    unit_iri = "https://qudt.org/vocab/unit/NUM",
    unit_label = "count",
    constraint_iri = NA_character_,
    method_iri = NA_character_
  )

  expect_error(
    validate_dictionary(dict, require_iris = TRUE),
    "REVIEW-prefixed IRI"
  )
})

test_that("weak LLM shortlist review triggers one bounded alternate-query pass", {
  search_calls <- character()
  request_calls <- 0L

  dict <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "fish_total",
    column_label = "Fish total",
    column_description = "Total fish observed",
    column_role = "measurement",
    value_type = "integer",
    unit_label = "count",
    unit_iri = "https://qudt.org/vocab/unit/NUM",
    term_iri = NA_character_,
    property_iri = "https://example.org/property/count",
    entity_iri = "https://example.org/entity/fish",
    constraint_iri = "https://example.org/constraint/all",
    method_iri = "https://example.org/method/visual"
  )

  fake_search <- function(query, role, sources) {
    search_calls <<- c(search_calls, paste(role, query, sep = "::"))

    if (identical(query, "count")) {
      return(tibble::tibble(
        label = c("Count", "Observation count"),
        iri = c("https://example.org/count", "https://example.org/observation-count"),
        source = c("smn", "smn"),
        ontology = c("demo", "demo"),
        role = c(role, role),
        match_type = c("label_partial", "label_partial"),
        definition = c("Generic count concept", "Count of observations"),
        score = c(0.55, 0.5)
      ))
    }

    if (identical(query, "fish abundance")) {
      return(tibble::tibble(
        label = c("Fish abundance", "Fish total count"),
        iri = c("https://example.org/fish-abundance", "https://example.org/fish-total-count"),
        source = c("smn", "smn"),
        ontology = c("demo", "demo"),
        role = c(role, role),
        match_type = c("label_exact", "label_partial"),
        definition = c("Abundance of fish", "Total fish count"),
        score = c(0.98, 0.72)
      ))
    }

    tibble::tibble()
  }

  fake_request <- function(messages, config) {
    request_calls <<- request_calls + 1L
    prompt <- messages[[2]]$content

    if (grepl("Exploration payload:", prompt, fixed = TRUE)) {
      return(list(
        alternate_queries = list("fish abundance", "https://w3id.org/smn/InventedIri"),
        rationale = "The initial query is too generic."
      ))
    }

    if (request_calls == 1L) {
      return(list(
        decision = "review",
        selected_candidate_index = NULL,
        confidence = 0.31,
        rationale = "The shortlist is too generic.",
        missing_context = "A species-specific abundance phrase would help."
      ))
    }

    list(
      decision = "accept",
      selected_candidate_index = 1,
      confidence = 0.93,
      rationale = "Fish abundance is the best retrieved match.",
      missing_context = ""
    )
  }

  out <- suggest_semantics(
    NULL,
    dict,
    sources = "smn",
    max_per_role = 2,
    search_fn = fake_search,
    llm_assess = TRUE,
    llm_provider = "openrouter",
    llm_api_key = "dummy-key",
    llm_top_n = 2,
    llm_request_fn = fake_request
  )

  suggestions <- attr(out, "semantic_suggestions")
  assessments <- attr(out, "semantic_llm_assessments")

  expect_equal(search_calls, c("variable::count", "variable::fish abundance"))
  expect_equal(request_calls, 3L)
  expect_true(any(suggestions$retrieval_pass == 2L))
  expect_true(any(suggestions$llm_selected))
  expect_equal(
    suggestions$iri[suggestions$llm_selected][[1]],
    "https://example.org/fish-abundance"
  )
  expect_true(isTRUE(assessments$llm_exploration_used[[1]]))
  expect_equal(assessments$llm_exploration_queries[[1]], "fish abundance")
  expect_equal(assessments$llm_selected_iri[[1]], "https://example.org/fish-abundance")
})

test_that("strong LLM shortlist acceptance skips bounded exploration", {
  search_calls <- character()
  request_calls <- 0L

  dict <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "fish_total",
    column_label = "Fish total",
    column_description = "Total fish observed",
    column_role = "measurement",
    value_type = "integer",
    unit_label = "count",
    unit_iri = "https://qudt.org/vocab/unit/NUM",
    term_iri = NA_character_,
    property_iri = "https://example.org/property/count",
    entity_iri = "https://example.org/entity/fish",
    constraint_iri = "https://example.org/constraint/all",
    method_iri = "https://example.org/method/visual"
  )

  fake_search <- function(query, role, sources) {
    search_calls <<- c(search_calls, paste(role, query, sep = "::"))
    tibble::tibble(
      label = c("Count", "Fish count"),
      iri = c("https://example.org/count", "https://example.org/fish-count"),
      source = c("smn", "smn"),
      ontology = c("demo", "demo"),
      role = c(role, role),
      match_type = c("label_exact", "label_partial"),
      definition = c("Count concept", "Fish count concept"),
      score = c(0.96, 0.88)
    )
  }

  fake_request <- function(messages, config) {
    request_calls <<- request_calls + 1L
    list(
      decision = "accept",
      selected_candidate_index = 1,
      confidence = 0.91,
      rationale = "The first shortlist is already good enough.",
      missing_context = ""
    )
  }

  out <- suggest_semantics(
    NULL,
    dict,
    sources = "smn",
    max_per_role = 2,
    search_fn = fake_search,
    llm_assess = TRUE,
    llm_provider = "openrouter",
    llm_api_key = "dummy-key",
    llm_top_n = 2,
    llm_request_fn = fake_request
  )

  suggestions <- attr(out, "semantic_suggestions")
  assessments <- attr(out, "semantic_llm_assessments")

  expect_equal(search_calls, c("variable::count"))
  expect_equal(request_calls, 1L)
  expect_true(all(suggestions$retrieval_pass == 1L))
  expect_false(isTRUE(assessments$llm_exploration_used[[1]]))
  expect_true(is.na(assessments$llm_exploration_queries[[1]]))
})
