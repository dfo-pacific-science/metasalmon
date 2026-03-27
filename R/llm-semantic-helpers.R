.ms_llm_non_empty_string <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  x <- trimws(as.character(x[[1]]))
  if (!nzchar(x) || is.na(x)) {
    return(NA_character_)
  }
  x
}

.ms_llm_uses_openrouter_free <- function(provider, model) {
  identical(provider, "openrouter") &&
    !is.na(model) &&
    (identical(model, "openrouter/free") || grepl(":free$", model))
}

.ms_llm_context_chunk_limit <- function(config) {
  if (.ms_llm_uses_openrouter_free(config$provider, config$model)) {
    return(2L)
  }
  4L
}

.ms_llm_effective_top_n <- function(config, top_n) {
  top_n <- max(1L, as.integer(top_n[[1]] %||% 5L))
  if (.ms_llm_uses_openrouter_free(config$provider, config$model)) {
    return(min(top_n, 3L))
  }
  top_n
}

.ms_llm_batch_size <- function(config) {
  if (!identical(config$request_fn, .ms_llm_chat_json_request)) {
    return(1L)
  }
  if (.ms_llm_uses_openrouter_free(config$provider, config$model)) {
    return(2L)
  }
  1L
}

.ms_llm_retry_limit <- function(config) {
  if (.ms_llm_uses_openrouter_free(config$provider, config$model)) {
    return(2L)
  }
  1L
}

.ms_llm_is_retryable_error <- function(message) {
  msg <- tolower(paste(message, collapse = " "))
  patterns <- c(
    "timeout was reached",
    "timed out",
    "http 408",
    "http 429",
    "http 500",
    "http 502",
    "http 503",
    "http 504",
    "temporarily unavailable",
    "connection reset",
    "empty reply",
    "failed to perform http request"
  )
  any(vapply(patterns, function(pattern) grepl(pattern, msg, fixed = TRUE), logical(1)))
}

.ms_llm_request_with_retries <- function(messages, config) {
  attempts <- .ms_llm_retry_limit(config)
  last_error <- NULL

  for (attempt in seq_len(attempts)) {
    result <- tryCatch(
      config$request_fn(messages = messages, config = config),
      error = function(e) e
    )

    if (!inherits(result, "error")) {
      return(result)
    }

    last_error <- result
    if (attempt >= attempts || !.ms_llm_is_retryable_error(conditionMessage(result))) {
      stop(result)
    }

    Sys.sleep(min(2, attempt * 0.5))
  }

  stop(last_error)
}

.ms_llm_resolve_config <- function(provider = c("openai", "openrouter", "openai_compatible"),
                                   model = NULL,
                                   api_key = NULL,
                                   base_url = NULL,
                                   timeout_seconds = 60,
                                   request_fn = NULL) {
  provider <- match.arg(provider)

  model <- .ms_llm_non_empty_string(model)
  if (is.na(model)) {
    model <- .ms_llm_non_empty_string(Sys.getenv("METASALMON_LLM_MODEL", unset = ""))
  }
  if (is.na(model) && identical(provider, "openrouter")) {
    model <- "openrouter/free"
  }
  if (is.na(model)) {
    cli::cli_abort(
      c(
        "LLM assessment requires a model.",
        "i" = "Pass {.arg llm_model} or set {.envvar METASALMON_LLM_MODEL}.",
        "i" = "For {.code llm_provider = 'openrouter'}, the default is {.code 'openrouter/free'}."
      )
    )
  }

  api_key <- .ms_llm_non_empty_string(api_key)
  if (is.na(api_key)) {
    env_name <- switch(
      provider,
      openai = "OPENAI_API_KEY",
      openrouter = "OPENROUTER_API_KEY",
      openai_compatible = "METASALMON_LLM_API_KEY"
    )
    api_key <- .ms_llm_non_empty_string(Sys.getenv(env_name, unset = ""))
  }
  if (is.na(api_key)) {
    api_key <- .ms_llm_non_empty_string(Sys.getenv("METASALMON_LLM_API_KEY", unset = ""))
  }
  if (is.na(api_key)) {
    cli::cli_abort(
      c(
        "LLM assessment requires an API key.",
        "i" = "Pass {.arg llm_api_key} or set the provider-specific environment variable."
      )
    )
  }

  base_url <- .ms_llm_non_empty_string(base_url)
  if (is.na(base_url)) {
    base_url <- .ms_llm_non_empty_string(Sys.getenv("METASALMON_LLM_BASE_URL", unset = ""))
  }
  if (is.na(base_url)) {
    base_url <- switch(
      provider,
      openai = "https://api.openai.com/v1",
      openrouter = "https://openrouter.ai/api/v1",
      openai_compatible = NA_character_
    )
  }
  if (provider == "openai_compatible" && is.na(base_url)) {
    cli::cli_abort(
      c(
        "OpenAI-compatible LLM assessment requires a base URL.",
        "i" = "Pass {.arg llm_base_url} or set {.envvar METASALMON_LLM_BASE_URL}."
      )
    )
  }

  timeout_seconds <- suppressWarnings(as.numeric(timeout_seconds[[1]] %||% 60))
  if (is.na(timeout_seconds) || timeout_seconds <= 0) {
    cli::cli_abort("{.arg llm_timeout_seconds} must be a positive number.")
  }
  if (.ms_llm_uses_openrouter_free(provider, model)) {
    timeout_seconds <- max(timeout_seconds, 90)
  }

  list(
    provider = provider,
    model = model,
    api_key = api_key,
    base_url = sub("/$", "", base_url),
    timeout_seconds = timeout_seconds,
    request_fn = request_fn %||% .ms_llm_chat_json_request
  )
}

.ms_supported_context_extensions <- function() {
  c("md", "txt", "csv", "tsv", "json", "yaml", "yml", "rst", "pdf")
}

.ms_context_text_from_file <- function(path) {
  normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!file.exists(normalized)) {
    cli::cli_abort("Context file does not exist: {.path {path}}")
  }

  ext <- tolower(tools::file_ext(normalized))
  if (!ext %in% .ms_supported_context_extensions()) {
    cli::cli_warn(
      "Skipping unsupported context file {.path {path}}. Supported extensions: {.val {.ms_supported_context_extensions()}}"
    )
    return(NULL)
  }

  if (identical(ext, "pdf")) {
    if (!requireNamespace("pdftools", quietly = TRUE)) {
      cli::cli_abort(
        c(
          "PDF context files require the optional {.pkg pdftools} package.",
          "i" = "Install it with {.code install.packages('pdftools')} or remove the PDF from {.arg llm_context_files}."
        )
      )
    }
    pages <- pdftools::pdf_text(normalized)
    text <- paste(pages, collapse = "\n\n")
  } else {
    text <- paste(readLines(normalized, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }

  text <- enc2utf8(text)
  text <- trimws(text)
  if (!nzchar(text)) {
    cli::cli_warn("Skipping empty context file {.path {path}}.")
    return(NULL)
  }

  list(
    path = normalized,
    source = basename(normalized),
    text = text
  )
}

.ms_chunk_context_text <- function(text, source, chunk_chars = 2200L, overlap_chars = 200L) {
  text <- enc2utf8(as.character(text[[1]] %||% ""))
  if (!nzchar(trimws(text))) {
    return(tibble::tibble())
  }

  chunk_chars <- max(400L, as.integer(chunk_chars[[1]] %||% 2200L))
  overlap_chars <- max(0L, min(as.integer(overlap_chars[[1]] %||% 200L), chunk_chars %/% 2L))
  starts <- seq.int(1L, nchar(text), by = max(1L, chunk_chars - overlap_chars))

  purrr::map_dfr(seq_along(starts), function(i) {
    start <- starts[[i]]
    end <- min(nchar(text), start + chunk_chars - 1L)
    tibble::tibble(
      source = source,
      chunk_id = paste0(source, "#", i),
      chunk_text = trimws(substr(text, start, end))
    )
  }) |> dplyr::filter(nzchar(.data$chunk_text))
}

.ms_context_tokens <- function(...) {
  text <- paste(unlist(list(...)), collapse = " ")
  text <- tolower(text)
  text <- gsub("([a-z0-9])([A-Z])", "\\1 \\2", text, perl = TRUE)
  text <- gsub("[^a-z0-9]+", " ", text)
  tokens <- unlist(strsplit(text, "\\s+"))
  tokens <- tokens[nzchar(tokens)]
  tokens[nchar(tokens) >= 3L]
}

.ms_score_context_chunks <- function(chunks, target_row, candidate_rows, max_chunks = 4L) {
  if (nrow(chunks) == 0) {
    return(chunks)
  }

  query_bits <- c(
    target_row$search_query[[1]] %||% "",
    target_row$target_label[[1]] %||% "",
    target_row$target_description[[1]] %||% "",
    target_row$column_label[[1]] %||% "",
    target_row$column_description[[1]] %||% "",
    candidate_rows$label %||% character(),
    candidate_rows$definition %||% character()
  )
  query_tokens <- unique(.ms_context_tokens(query_bits))
  if (length(query_tokens) == 0) {
    return(utils::head(chunks, max_chunks))
  }

  chunks$context_score <- vapply(chunks$chunk_text, function(text) {
    chunk_tokens <- .ms_context_tokens(text)
    if (length(chunk_tokens) == 0) {
      return(0)
    }
    sum(query_tokens %in% chunk_tokens)
  }, numeric(1))

  chunks <- chunks[order(-chunks$context_score, nchar(chunks$chunk_text), chunks$source), , drop = FALSE]
  utils::head(chunks, max(1L, as.integer(max_chunks[[1]] %||% 4L)))
}

.ms_prepare_context_chunks <- function(context_files = NULL,
                                       context_text = NULL,
                                       target_row,
                                       candidate_rows,
                                       max_chunks = 4L) {
  raw_context <- list()
  if (!is.null(context_files) && length(context_files) > 0) {
    raw_context <- c(raw_context, lapply(context_files, .ms_context_text_from_file))
  }
  if (!is.null(context_text) && nzchar(trimws(paste(context_text, collapse = " ")))) {
    raw_context <- c(raw_context, list(list(
      path = NA_character_,
      source = "inline_context",
      text = paste(context_text, collapse = "\n\n")
    )))
  }

  raw_context <- Filter(Negate(is.null), raw_context)
  if (length(raw_context) == 0) {
    return(tibble::tibble())
  }

  chunks <- purrr::map_dfr(raw_context, function(item) {
    .ms_chunk_context_text(item$text, item$source)
  })
  .ms_score_context_chunks(chunks, target_row = target_row, candidate_rows = candidate_rows, max_chunks = max_chunks)
}

.ms_llm_target_payload <- function(target_row, candidate_rows, context_chunks, target_key = NULL) {
  candidate_payload <- purrr::map(seq_len(nrow(candidate_rows)), function(i) {
    list(
      candidate_index = i,
      label = candidate_rows$label[[i]] %||% "",
      iri = candidate_rows$iri[[i]] %||% "",
      source = candidate_rows$source[[i]] %||% "",
      ontology = candidate_rows$ontology[[i]] %||% "",
      definition = candidate_rows$definition[[i]] %||% "",
      lexical_score = if ("score" %in% names(candidate_rows)) candidate_rows$score[[i]] else NA_real_
    )
  })

  context_payload <- purrr::map(seq_len(nrow(context_chunks)), function(i) {
    list(
      source = context_chunks$source[[i]],
      chunk_id = context_chunks$chunk_id[[i]],
      excerpt = context_chunks$chunk_text[[i]]
    )
  })

  payload <- list(
    target = list(
      dataset_id = target_row$dataset_id[[1]] %||% NA_character_,
      table_id = target_row$table_id[[1]] %||% NA_character_,
      column_name = target_row$column_name[[1]] %||% NA_character_,
      dictionary_role = target_row$dictionary_role[[1]] %||% NA_character_,
      target_scope = target_row$target_scope[[1]] %||% NA_character_,
      target_sdp_field = target_row$target_sdp_field[[1]] %||% NA_character_,
      target_label = target_row$target_label[[1]] %||% NA_character_,
      target_description = target_row$target_description[[1]] %||% NA_character_,
      search_query = target_row$search_query[[1]] %||% NA_character_,
      target_query_basis = target_row$target_query_basis[[1]] %||% NA_character_,
      target_query_context = target_row$target_query_context[[1]] %||% NA_character_
    ),
    candidates = candidate_payload,
    context_excerpts = context_payload
  )

  if (!is.null(target_key) && !is.na(target_key) && nzchar(target_key)) {
    payload$target_key <- target_key
  }

  payload
}

.ms_llm_messages_for_target <- function(target_row, candidate_rows, context_chunks) {
  payload <- .ms_llm_target_payload(target_row, candidate_rows, context_chunks)

  system_prompt <- paste(
    "You are assessing ontology candidate matches for the metasalmon R package.",
    "Choose only from the provided candidates; never invent an IRI.",
    "Return JSON only with keys decision, selected_candidate_index, confidence, rationale, missing_context.",
    "decision must be one of accept, review, propose_new_term.",
    "selected_candidate_index must be null when no candidate should be selected.",
    "confidence must be a number between 0 and 1."
  )

  user_prompt <- paste(
    "Assessment payload:",
    jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE, null = "null"),
    "\n\nReturn JSON only."
  )

  list(
    list(role = "system", content = system_prompt),
    list(role = "user", content = user_prompt)
  )
}

.ms_llm_messages_for_batch <- function(records) {
  payload <- purrr::map(records, function(record) {
    .ms_llm_target_payload(
      record$group[1, , drop = FALSE],
      record$candidate_rows,
      record$context_chunks,
      target_key = record$group_name
    )
  })

  system_prompt <- paste(
    "You are assessing ontology candidate matches for the metasalmon R package.",
    "Choose only from the provided candidates for each target; never invent an IRI.",
    "Return JSON only with a single top-level key named assessments.",
    "assessments must be an array of objects with keys target_key, decision, selected_candidate_index, confidence, rationale, missing_context.",
    "decision must be one of accept, review, propose_new_term.",
    "selected_candidate_index must be null when no candidate should be selected.",
    "confidence must be a number between 0 and 1."
  )

  user_prompt <- paste(
    "Assessment batch:",
    jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE, null = "null"),
    "\n\nReturn JSON only."
  )

  list(
    list(role = "system", content = system_prompt),
    list(role = "user", content = user_prompt)
  )
}

.ms_llm_extract_message_content <- function(body) {
  choices <- body$choices %||% list()
  if (length(choices) == 0) {
    cli::cli_abort("LLM response did not include any choices.")
  }
  message <- choices[[1]]$message %||% list()
  content <- message$content %||% ""

  if (is.character(content)) {
    return(paste(content, collapse = "\n"))
  }
  if (is.list(content)) {
    text_parts <- vapply(content, function(part) {
      if (is.list(part) && identical(part$type %||% NA_character_, "text")) {
        return(as.character(part$text %||% ""))
      }
      if (is.character(part)) {
        return(part[[1]])
      }
      ""
    }, character(1))
    return(paste(text_parts[nzchar(text_parts)], collapse = "\n"))
  }

  as.character(content)
}

.ms_llm_clean_json_text <- function(text) {
  text <- trimws(as.character(text[[1]] %||% ""))
  text <- sub("^```json\\s*", "", text, perl = TRUE)
  text <- sub("^```\\s*", "", text, perl = TRUE)
  text <- sub("\\s*```$", "", text, perl = TRUE)
  trimws(text)
}

.ms_llm_chat_json_request <- function(messages, config) {
  req <- httr2::request(paste0(config$base_url, "/chat/completions")) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization = paste("Bearer", config$api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_user_agent(ms_user_agent()) |>
    httr2::req_timeout(seconds = config$timeout_seconds) |>
    httr2::req_body_json(list(
      model = config$model,
      messages = messages,
      temperature = 0
    ), auto_unbox = TRUE)

  if (identical(config$provider, "openrouter")) {
    req <- httr2::req_headers(
      req,
      `HTTP-Referer` = "https://dfo-pacific-science.github.io/metasalmon/",
      `X-Title` = "metasalmon"
    )
  }

  resp <- httr2::req_perform(req)
  httr2::resp_check_status(resp)
  body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  content <- .ms_llm_clean_json_text(.ms_llm_extract_message_content(body))
  parsed <- jsonlite::fromJSON(content, simplifyVector = FALSE)

  if (!is.list(parsed)) {
    cli::cli_abort("LLM response was not a JSON object.")
  }
  parsed
}

.ms_validate_llm_assessment <- function(result, candidate_rows) {
  decision <- .ms_llm_non_empty_string(result$decision %||% NA_character_)
  if (is.na(decision) || !decision %in% c("accept", "review", "propose_new_term")) {
    cli::cli_abort("LLM assessment must return decision = accept, review, or propose_new_term.")
  }

  selected_index <- result$selected_candidate_index %||% NULL
  if (is.null(selected_index) || identical(selected_index, "") || isFALSE(length(selected_index) > 0)) {
    selected_index <- NA_integer_
  } else {
    selected_index <- suppressWarnings(as.integer(selected_index[[1]]))
  }

  confidence <- suppressWarnings(as.numeric(result$confidence %||% NA_real_))
  if (is.na(confidence) || confidence < 0 || confidence > 1) {
    cli::cli_abort("LLM assessment confidence must be numeric and between 0 and 1.")
  }

  rationale <- .ms_llm_non_empty_string(result$rationale %||% NA_character_)
  if (!is.na(selected_index) && (selected_index < 1L || selected_index > nrow(candidate_rows))) {
    decision <- "review"
    selected_index <- NA_integer_
    rationale <- paste(
      c(
        rationale,
        "Model returned an out-of-range candidate index; downgraded to review."
      )[nzchar(c(rationale, "Model returned an out-of-range candidate index; downgraded to review."))],
      collapse = " "
    )
  }
  if (identical(decision, "propose_new_term")) {
    selected_index <- NA_integer_
  }

  list(
    decision = decision,
    selected_candidate_index = selected_index,
    confidence = confidence,
    rationale = rationale,
    missing_context = .ms_llm_non_empty_string(result$missing_context %||% NA_character_)
  )
}

.ms_empty_llm_assessment <- function(group, config, error = NA_character_) {
  target <- group[1, , drop = FALSE]
  tibble::tibble(
    dataset_id = target$dataset_id[[1]] %||% NA_character_,
    table_id = target$table_id[[1]] %||% NA_character_,
    column_name = target$column_name[[1]] %||% NA_character_,
    code_value = target$code_value[[1]] %||% NA_character_,
    dictionary_role = target$dictionary_role[[1]] %||% NA_character_,
    target_scope = target$target_scope[[1]] %||% NA_character_,
    target_sdp_file = target$target_sdp_file[[1]] %||% NA_character_,
    target_sdp_field = target$target_sdp_field[[1]] %||% NA_character_,
    search_query = target$search_query[[1]] %||% NA_character_,
    llm_provider = config$provider,
    llm_model = config$model,
    llm_decision = NA_character_,
    llm_confidence = NA_real_,
    llm_selected_candidate_index = NA_integer_,
    llm_selected_iri = NA_character_,
    llm_selected_label = NA_character_,
    llm_rationale = NA_character_,
    llm_missing_context = NA_character_,
    llm_context_sources = NA_character_,
    llm_error = .ms_llm_non_empty_string(error)
  )
}

.ms_llm_success_assessment <- function(record, config, validated) {
  group <- record$group
  candidate_rows <- record$candidate_rows
  context_chunks <- record$context_chunks

  tibble::tibble(
    dataset_id = group$dataset_id[[1]] %||% NA_character_,
    table_id = group$table_id[[1]] %||% NA_character_,
    column_name = group$column_name[[1]] %||% NA_character_,
    code_value = group$code_value[[1]] %||% NA_character_,
    dictionary_role = group$dictionary_role[[1]] %||% NA_character_,
    target_scope = group$target_scope[[1]] %||% NA_character_,
    target_sdp_file = group$target_sdp_file[[1]] %||% NA_character_,
    target_sdp_field = group$target_sdp_field[[1]] %||% NA_character_,
    search_query = group$search_query[[1]] %||% NA_character_,
    llm_provider = config$provider,
    llm_model = config$model,
    llm_decision = validated$decision,
    llm_confidence = validated$confidence,
    llm_selected_candidate_index = validated$selected_candidate_index,
    llm_selected_iri = if (!is.na(validated$selected_candidate_index)) candidate_rows$iri[[validated$selected_candidate_index]] else NA_character_,
    llm_selected_label = if (!is.na(validated$selected_candidate_index)) candidate_rows$label[[validated$selected_candidate_index]] else NA_character_,
    llm_rationale = validated$rationale,
    llm_missing_context = validated$missing_context,
    llm_context_sources = if (nrow(context_chunks) > 0) paste(unique(context_chunks$source), collapse = "; ") else NA_character_,
    llm_error = NA_character_
  )
}

.ms_llm_prepare_record <- function(group_name,
                                   group,
                                   config,
                                   top_n,
                                   context_files,
                                   context_text) {
  group <- group[order(group$.ms_row_order), , drop = FALSE]
  candidate_rows <- utils::head(group, top_n)
  context_chunks <- .ms_prepare_context_chunks(
    context_files = context_files,
    context_text = context_text,
    target_row = group[1, , drop = FALSE],
    candidate_rows = candidate_rows,
    max_chunks = .ms_llm_context_chunk_limit(config)
  )

  list(
    group_name = group_name,
    group = group,
    candidate_rows = candidate_rows,
    context_chunks = context_chunks
  )
}

.ms_llm_assess_one_record <- function(record, config) {
  messages <- .ms_llm_messages_for_target(record$group[1, , drop = FALSE], record$candidate_rows, record$context_chunks)

  tryCatch(
    {
      validated <- .ms_validate_llm_assessment(
        .ms_llm_request_with_retries(messages = messages, config = config),
        record$candidate_rows
      )
      .ms_llm_success_assessment(record, config, validated)
    },
    error = function(e) {
      cli::cli_warn("LLM assessment failed for {.field {record$group$column_name[[1]] %||% record$group$target_sdp_field[[1]]}}: {conditionMessage(e)}")
      .ms_empty_llm_assessment(record$group, config, error = conditionMessage(e))
    }
  )
}

.ms_llm_validate_batch_assessments <- function(result, records, config) {
  assessments <- result$assessments %||% NULL
  if (is.null(assessments) || !is.list(assessments) || length(assessments) == 0) {
    cli::cli_abort("LLM batch assessment must return a non-empty assessments array.")
  }

  records_by_key <- stats::setNames(records, vapply(records, `[[`, character(1), "group_name"))
  rows <- vector("list", length(records_by_key))
  names(rows) <- names(records_by_key)

  for (item in assessments) {
    key <- .ms_llm_non_empty_string(item$target_key %||% NA_character_)
    if (is.na(key) || !key %in% names(records_by_key)) {
      next
    }
    validated <- .ms_validate_llm_assessment(item, records_by_key[[key]]$candidate_rows)
    rows[[key]] <- .ms_llm_success_assessment(records_by_key[[key]], config, validated)
  }

  missing_keys <- names(rows)[vapply(rows, is.null, logical(1))]
  if (length(missing_keys) > 0) {
    cli::cli_abort(
      "LLM batch response did not include usable assessments for target keys: {.val {missing_keys}}"
    )
  }

  dplyr::bind_rows(rows)
}

.ms_llm_assess_record_batch <- function(records, config) {
  if (length(records) <= 1L) {
    return(dplyr::bind_rows(lapply(records, .ms_llm_assess_one_record, config = config)))
  }

  messages <- .ms_llm_messages_for_batch(records)
  batch_result <- tryCatch(
    .ms_llm_request_with_retries(messages = messages, config = config),
    error = function(e) e
  )

  if (inherits(batch_result, "error")) {
    cli::cli_warn(
      "LLM batch assessment failed for {length(records)} targets; falling back to per-target review: {conditionMessage(batch_result)}"
    )
    return(dplyr::bind_rows(lapply(records, .ms_llm_assess_one_record, config = config)))
  }

  validated <- tryCatch(
    .ms_llm_validate_batch_assessments(batch_result, records = records, config = config),
    error = function(e) e
  )

  if (inherits(validated, "error")) {
    cli::cli_warn(
      "LLM batch response was unusable for {length(records)} targets; falling back to per-target review: {conditionMessage(validated)}"
    )
    return(dplyr::bind_rows(lapply(records, .ms_llm_assess_one_record, config = config)))
  }

  validated
}

.ms_assess_semantic_suggestions_llm <- function(suggestions,
                                                provider = c("openai", "openrouter", "openai_compatible"),
                                                model = NULL,
                                                api_key = NULL,
                                                base_url = NULL,
                                                top_n = 5L,
                                                context_files = NULL,
                                                context_text = NULL,
                                                timeout_seconds = 60,
                                                request_fn = NULL) {
  suggestions <- tibble::as_tibble(suggestions)
  if (nrow(suggestions) == 0) {
    return(list(
      suggestions = suggestions,
      assessments = tibble::tibble()
    ))
  }

  config <- .ms_llm_resolve_config(
    provider = provider,
    model = model,
    api_key = api_key,
    base_url = base_url,
    timeout_seconds = timeout_seconds,
    request_fn = request_fn
  )
  top_n <- .ms_llm_effective_top_n(config, top_n)

  suggestions$.ms_group_key <- do.call(
    paste,
    c(
      lapply(suggestions[c("dataset_id", "table_id", "column_name", "code_value", "dictionary_role", "target_scope", "target_sdp_file", "target_sdp_field")], function(x) {
        ifelse(is.na(x), "<NA>", as.character(x))
      }),
      sep = "\r"
    )
  )
  suggestions$.ms_row_order <- seq_len(nrow(suggestions))

  suggestion_groups <- split(suggestions, suggestions$.ms_group_key)
  records <- purrr::map(
    names(suggestion_groups),
    ~ .ms_llm_prepare_record(
      group_name = .x,
      group = suggestion_groups[[.x]],
      config = config,
      top_n = top_n,
      context_files = context_files,
      context_text = context_text
    )
  )

  batch_size <- .ms_llm_batch_size(config)
  batch_ids <- ceiling(seq_along(records) / batch_size)
  assessment_rows <- lapply(split(records, batch_ids), .ms_llm_assess_record_batch, config = config)

  assessments <- dplyr::bind_rows(assessment_rows)
  suggestions <- suggestions |>
    dplyr::group_by(.data$.ms_group_key) |>
    dplyr::mutate(llm_candidate_rank = dplyr::if_else(dplyr::row_number() <= top_n, dplyr::row_number(), NA_integer_)) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      assessments,
      by = c(
        "dataset_id",
        "table_id",
        "column_name",
        "code_value",
        "dictionary_role",
        "target_scope",
        "target_sdp_file",
        "target_sdp_field",
        "search_query"
      )
    ) |>
    dplyr::mutate(
      llm_selected = !is.na(.data$llm_selected_candidate_index) &
        !is.na(.data$llm_candidate_rank) &
        .data$llm_selected_candidate_index == .data$llm_candidate_rank
    ) |>
    dplyr::select(-dplyr::any_of(c(".ms_group_key", ".ms_row_order")))

  list(
    suggestions = suggestions,
    assessments = assessments
  )
}
