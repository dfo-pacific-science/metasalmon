 .ms_chat_trim_string <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0) {
    return(default)
  }

  text <- trimws(as.character(x[[1]]))
  if (is.na(text) || !nzchar(text)) {
    return(default)
  }

  text
}

.ms_chat_now <- function(time = Sys.time()) {
  format(as.POSIXct(time, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

.ms_chat_default_session_root <- function(session_root = NULL) {
  root <- .ms_chat_trim_string(session_root)
  if (is.na(root)) {
    root <- file.path(tools::R_user_dir("metasalmon", which = "state"), "curation-sessions")
  }

  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  normalizePath(root, winslash = "/", mustWork = FALSE)
}

.ms_chat_new_session_id <- function(prefix = "mscur") {
  stamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  suffix <- paste(sample(c(letters[1:6], 0:9), 8L, replace = TRUE), collapse = "")
  paste(prefix, stamp, suffix, sep = "-")
}

.ms_chat_session_paths <- function(session_id, session_root = NULL) {
  root <- .ms_chat_default_session_root(session_root)
  id <- .ms_chat_trim_string(session_id)
  if (is.na(id)) {
    cli::cli_abort("Session id must be a non-empty string.")
  }

  session_dir <- file.path(root, id)
  list(
    root = root,
    session_dir = session_dir,
    state_path = file.path(session_dir, "state.rds"),
    transcript_path = file.path(session_dir, "transcript.rds")
  )
}

.ms_chat_make_output <- function(output_fn = NULL) {
  if (is.null(output_fn)) {
    return(function(text) cat(text, "\n", sep = ""))
  }

  output_fn
}

.ms_chat_make_input <- function(commands = NULL, input_fn = readline) {
  if (is.null(commands)) {
    return(input_fn)
  }

  command_queue <- as.character(commands)
  idx <- 0L

  function(prompt = "") {
    idx <<- idx + 1L
    if (idx > length(command_queue)) {
      return(NA_character_)
    }
    command_queue[[idx]]
  }
}

.ms_chat_append_transcript <- function(transcript, role, content, turn_index = NA_integer_) {
  transcript[[length(transcript) + 1L]] <- list(
    role = role,
    content = content,
    turn_index = turn_index,
    timestamp = .ms_chat_now()
  )
  transcript
}

.ms_chat_save_session <- function(state, transcript, session_root = NULL) {
  paths <- .ms_chat_session_paths(state$session_id, session_root = session_root)
  dir.create(paths$session_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(state, paths$state_path)
  saveRDS(transcript, paths$transcript_path)
  invisible(paths)
}

.ms_chat_load_session <- function(session_id, session_root = NULL) {
  paths <- .ms_chat_session_paths(session_id, session_root = session_root)
  if (!file.exists(paths$state_path) || !file.exists(paths$transcript_path)) {
    cli::cli_abort(
      c(
        "Curation session {.val {session_id}} was not found.",
        "i" = "Looked for {.file state.rds} and {.file transcript.rds} under {.path {paths$session_dir}}."
      )
    )
  }

  list(
    state = readRDS(paths$state_path),
    transcript = readRDS(paths$transcript_path),
    session_dir = paths$session_dir
  )
}

.ms_chat_first_non_empty <- function(...) {
  values <- unlist(list(...), use.names = FALSE)
  values <- vapply(values, .ms_chat_trim_string, character(1), default = NA_character_)
  values <- values[!is.na(values)]
  if (length(values) == 0L) {
    return(NA_character_)
  }
  values[[1]]
}

.ms_chat_named_candidate_rows <- function(candidate_rows = NULL) {
  if (is.null(candidate_rows)) {
    candidate_rows <- tibble::tibble()
  }

  candidate_rows <- tibble::as_tibble(candidate_rows)
  required <- c("label", "iri", "source", "ontology", "definition")
  for (nm in required) {
    if (!nm %in% names(candidate_rows)) {
      candidate_rows[[nm]] <- NA_character_
    }
  }
  if (!"score" %in% names(candidate_rows)) {
    candidate_rows$score <- NA_real_
  }
  if (!"llm_selected" %in% names(candidate_rows)) {
    candidate_rows$llm_selected <- NA
  }

  candidate_rows
}

.ms_chat_decomposition_target_row <- function(dict_row, candidate_rows = NULL) {
  dict_row <- tibble::as_tibble(dict_row)
  candidate_rows <- .ms_chat_named_candidate_rows(candidate_rows)

  if (nrow(candidate_rows) > 0L) {
    target_cols <- c(
      "dataset_id", "table_id", "column_name", "code_value", "dictionary_role",
      "search_role", "target_scope", "target_sdp_file", "target_sdp_field",
      "target_row_key", "target_label", "target_description", "search_query",
      "target_query_basis", "target_query_context", "column_label",
      "column_description", "code_label", "code_description"
    )
    keep <- intersect(target_cols, names(candidate_rows))
    target <- candidate_rows[1, keep, drop = FALSE]
    missing <- setdiff(target_cols, names(target))
    for (nm in missing) {
      target[[nm]] <- NA_character_
    }
    return(target[, target_cols, drop = FALSE])
  }

  dataset_id <- if ("dataset_id" %in% names(dict_row)) dict_row$dataset_id[[1]] else NA_character_
  table_id <- if ("table_id" %in% names(dict_row)) dict_row$table_id[[1]] else NA_character_
  column_name <- if ("column_name" %in% names(dict_row)) dict_row$column_name[[1]] else NA_character_
  column_label <- if ("column_label" %in% names(dict_row)) dict_row$column_label[[1]] else column_name
  column_description <- if ("column_description" %in% names(dict_row)) dict_row$column_description[[1]] else NA_character_
  search_query <- .ms_chat_first_non_empty(column_description, column_label, column_name)

  tibble::tibble(
    dataset_id = dataset_id,
    table_id = table_id,
    column_name = column_name,
    code_value = NA_character_,
    dictionary_role = "variable",
    search_role = "variable",
    target_scope = "column",
    target_sdp_file = "column_dictionary.csv",
    target_sdp_field = "term_iri",
    target_row_key = paste(dataset_id, table_id, column_name, sep = "/"),
    target_label = column_label,
    target_description = column_description,
    search_query = search_query,
    target_query_basis = dplyr::case_when(
      !is.na(.ms_chat_trim_string(column_description)) ~ "column_description",
      !is.na(.ms_chat_trim_string(column_label)) ~ "column_label",
      TRUE ~ "column_name"
    ),
    target_query_context = .ms_chat_first_non_empty(
      paste(column_label, column_description),
      column_name
    ),
    column_label = column_label,
    column_description = column_description,
    code_label = NA_character_,
    code_description = NA_character_
  )
}

.ms_chat_decomposition_slot_guesses <- function(dict_row) {
  dict_row <- tibble::as_tibble(dict_row)

  list(
    property = .ms_chat_trim_string(dict_row$property_iri %||% NA_character_),
    entity = .ms_chat_trim_string(dict_row$entity_iri %||% NA_character_),
    constraints = .ms_chat_trim_string(dict_row$constraint_iri %||% NA_character_),
    matrix = NA_character_,
    context_object = NA_character_,
    used_procedure = .ms_chat_trim_string(dict_row$method_iri %||% NA_character_),
    unit = .ms_chat_first_non_empty(dict_row$unit_label %||% NA_character_, dict_row$unit_iri %||% NA_character_),
    statistic = NA_character_,
    derivation_status = NA_character_,
    temporal_granularity = NA_character_,
    spatial_granularity = NA_character_
  )
}

.ms_chat_decomposition_slot_state <- function(dict_row) {
  guesses <- .ms_chat_decomposition_slot_guesses(dict_row)
  labels <- c(
    property = "property",
    entity = "entity",
    constraints = "constraints",
    matrix = "matrix or medium",
    context_object = "context object",
    used_procedure = "usedProcedure context",
    unit = "unit",
    statistic = "statistic or aggregation",
    derivation_status = "derivation or estimation status",
    temporal_granularity = "temporal granularity",
    spatial_granularity = "spatial granularity"
  )

  purrr::imap(guesses, function(current_guess, slot) {
    list(
      slot = slot,
      label = labels[[slot]],
      current_guess = current_guess,
      value = NA_character_,
      status = "open",
      answered_at = NA_character_
    )
  })
}

.ms_chat_decomposition_question_queue <- function(dict_row) {
  slot_state <- .ms_chat_decomposition_slot_state(dict_row)
  specs <- list(
    list(
      id = "core_property",
      slot = "property",
      group = "core_observable",
      prompt = "What property or observable is this variable really about?",
      why = "This anchors the SKOS variable concept and keeps the variable separate from the entity or unit.",
      expected_answer = "short noun phrase"
    ),
    list(
      id = "core_entity",
      slot = "entity",
      group = "core_observable",
      prompt = "What is the object of interest or entity?",
      why = "I-ADOPT decomposition needs the entity in object-of-interest role, not just a nearby noun.",
      expected_answer = "short noun phrase"
    ),
    list(
      id = "core_constraints",
      slot = "constraints",
      group = "core_observable",
      prompt = "What constraints or qualifiers narrow the meaning?",
      why = "Constraints often decide whether a broad candidate is wrong or acceptable.",
      expected_answer = "short phrase or comma-separated list"
    ),
    list(
      id = "context_matrix",
      slot = "matrix",
      group = "context",
      prompt = "Does a matrix or medium need to be explicit?",
      why = "Matrix or medium can change the intended variable concept.",
      expected_answer = "short phrase or blank if not needed"
    ),
    list(
      id = "context_object",
      slot = "context_object",
      group = "context",
      prompt = "Is there a context object that should stay attached to the variable?",
      why = "Context objects sometimes matter even when they are not the primary entity.",
      expected_answer = "short phrase or blank if not needed"
    ),
    list(
      id = "context_used_procedure",
      slot = "used_procedure",
      group = "context",
      prompt = "Is a procedure or protocol part of the semantic meaning, i.e. usedProcedure-style context?",
      why = "Procedure context can matter, but it is adjacent context rather than a native decomposition slot.",
      expected_answer = "short phrase or blank if it is just provenance"
    ),
    list(
      id = "value_unit",
      slot = "unit",
      group = "value_character",
      prompt = "What unit matters for interpreting the variable?",
      why = "Units often distinguish similar-looking variable candidates.",
      expected_answer = "short unit label or blank"
    ),
    list(
      id = "value_statistic",
      slot = "statistic",
      group = "value_character",
      prompt = "What statistic or aggregation is implied (if any)?",
      why = "Statistic or aggregation can shift the intended variable concept.",
      expected_answer = "short phrase or blank"
    ),
    list(
      id = "value_derivation",
      slot = "derivation_status",
      group = "value_character",
      prompt = "Is the value directly observed, estimated, modeled, or otherwise derived?",
      why = "Derived versus direct variables should not be collapsed casually.",
      expected_answer = "short phrase or blank"
    ),
    list(
      id = "granularity_temporal",
      slot = "temporal_granularity",
      group = "granularity",
      prompt = "What temporal granularity matters here?",
      why = "Temporal granularity can define a distinct variable concept.",
      expected_answer = "short phrase or blank"
    ),
    list(
      id = "granularity_spatial",
      slot = "spatial_granularity",
      group = "granularity",
      prompt = "What spatial granularity matters here?",
      why = "Spatial granularity can define a distinct variable concept.",
      expected_answer = "short phrase or blank"
    )
  )

  purrr::map(specs, function(spec) {
    current_guess <- slot_state[[spec$slot]]$current_guess
    c(
      spec,
      list(
        current_guess = current_guess,
        answer = NA_character_,
        status = "open",
        answered_at = NA_character_
      )
    )
  })
}

.ms_chat_decomposition_question_groups <- function(question_queue) {
  unique(vapply(question_queue, function(question) question$group, character(1)))
}

.ms_chat_decomposition_open_questions <- function(state) {
  Filter(function(question) !identical(question$status, "answered"), state$question_queue)
}

.ms_chat_decomposition_next_round <- function(state, round_size = 3L) {
  open_questions <- .ms_chat_decomposition_open_questions(state)
  if (length(open_questions) == 0L) {
    return(list())
  }

  group_name <- open_questions[[1]]$group
  grouped <- Filter(function(question) identical(question$group, group_name), open_questions)
  grouped[seq_len(min(length(grouped), max(1L, as.integer(round_size[[1]] %||% 3L))))]
}

.ms_chat_format_current_guess <- function(x) {
  guess <- .ms_chat_trim_string(x)
  if (is.na(guess)) {
    return("none")
  }
  guess
}

.ms_chat_decomposition_round_text <- function(round_questions, state) {
  if (length(round_questions) == 0L) {
    return("No open decomposition questions remain.")
  }

  group_title <- switch(
    round_questions[[1]]$group,
    core_observable = "Round: core observable",
    context = "Round: context",
    value_character = "Round: value character",
    granularity = "Round: granularity",
    paste("Round:", round_questions[[1]]$group)
  )

  target <- state$target
  intro <- c(
    group_title,
    sprintf(
      "Target: %s / %s / %s",
      target$dataset_id[[1]] %||% NA_character_,
      target$table_id[[1]] %||% NA_character_,
      target$column_name[[1]] %||% NA_character_
    ),
    "Answer each prompt briefly. Blank answers are kept as unresolved for now.",
    "Procedure context is tracked as usedProcedure-style context, not as a native decomposition slot.",
    ""
  )

  question_lines <- unlist(purrr::imap(round_questions, function(question, idx) {
    c(
      sprintf("%d. %s", idx, question$prompt),
      sprintf("   Why: %s", question$why),
      sprintf("   Expected: %s", question$expected_answer),
      sprintf("   Current guess: %s", .ms_chat_format_current_guess(question$current_guess)),
      ""
    )
  }), use.names = FALSE)

  paste(c(intro, question_lines), collapse = "\n")
}

.ms_chat_decomposition_candidate_tokens <- function(candidate_row) {
  .ms_context_tokens(
    candidate_row$label %||% "",
    candidate_row$definition %||% "",
    candidate_row$ontology %||% ""
  )
}

.ms_chat_decomposition_query_tokens <- function(state) {
  facts <- vapply(state$approved_facts, function(item) item$value %||% "", character(1))
  slots <- vapply(state$slot_state, function(item) item$value %||% item$current_guess %||% "", character(1))
  .ms_context_tokens(
    state$target$search_query[[1]] %||% "",
    state$target$target_label[[1]] %||% "",
    state$target$target_description[[1]] %||% "",
    facts,
    slots
  )
}

.ms_chat_decomposition_candidate_scores <- function(state) {
  candidate_rows <- .ms_chat_named_candidate_rows(state$candidate_rows)
  if (nrow(candidate_rows) == 0L) {
    return(numeric())
  }

  query_tokens <- .ms_chat_decomposition_query_tokens(state)
  if (length(query_tokens) == 0L) {
    query_tokens <- .ms_context_tokens(state$target$search_query[[1]] %||% "")
  }

  vapply(seq_len(nrow(candidate_rows)), function(i) {
    candidate_tokens <- .ms_chat_decomposition_candidate_tokens(candidate_rows[i, , drop = FALSE])
    overlap <- if (length(query_tokens) == 0L || length(candidate_tokens) == 0L) 0 else sum(query_tokens %in% candidate_tokens)
    lexical <- suppressWarnings(as.numeric(candidate_rows$score[[i]] %||% NA_real_))
    if (is.na(lexical)) {
      lexical <- 0
    }
    llm_bonus <- if ("llm_selected" %in% names(candidate_rows) && isTRUE(candidate_rows$llm_selected[[i]])) 1 else 0
    overlap + lexical + llm_bonus
  }, numeric(1))
}

.ms_chat_decomposition_selected_candidate <- function(state) {
  candidate_rows <- .ms_chat_named_candidate_rows(state$candidate_rows)
  if (nrow(candidate_rows) == 0L) {
    return(list(index = NA_integer_, candidate = NULL, source = "none", rationale = "No shortlist candidates are available yet."))
  }

  manual_index <- suppressWarnings(as.integer(state$manual_candidate_index %||% NA_integer_))
  if (!is.na(manual_index) && manual_index >= 1L && manual_index <= nrow(candidate_rows)) {
    return(list(
      index = manual_index,
      candidate = candidate_rows[manual_index, , drop = FALSE],
      source = "manual",
      rationale = sprintf("User explicitly chose candidate %d for preview.", manual_index)
    ))
  }

  scores <- .ms_chat_decomposition_candidate_scores(state)
  picked <- which.max(scores)
  if (length(picked) == 0L || is.infinite(scores[[picked]]) || is.na(scores[[picked]])) {
    picked <- 1L
  }

  list(
    index = picked,
    candidate = candidate_rows[picked, , drop = FALSE],
    source = "heuristic",
    rationale = "Deterministic fallback preferred the candidate with the strongest overlap to the approved decomposition facts and retrieval context."
  )
}

.ms_chat_invoke_request <- function(request_fn, messages, config, response_schema = NULL, temperature = 0.2) {
  fn_formals <- tryCatch(names(formals(request_fn)), error = function(e) NULL)
  args <- list(messages = messages, config = config)
  if (!is.null(fn_formals) && "response_schema" %in% fn_formals) {
    args$response_schema <- response_schema
  }
  if (!is.null(fn_formals) && "temperature" %in% fn_formals) {
    args$temperature <- temperature
  }
  do.call(request_fn, args)
}

.ms_chat_http_request <- function(messages, config, response_schema = NULL, temperature = 0.2) {
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
      temperature = temperature
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
  content <- .ms_llm_extract_message_content(body)
  parsed <- tryCatch(
    jsonlite::fromJSON(.ms_llm_clean_json_text(content), simplifyVector = FALSE),
    error = function(e) NULL
  )

  list(
    content = content,
    data = parsed,
    raw = body
  )
}

.ms_chat <- function(messages,
                     provider = NULL,
                     model = NULL,
                     api_key = NULL,
                     base_url = NULL,
                     response_schema = NULL,
                     temperature = 0.2,
                     timeout_seconds = 60,
                     request_fn = NULL) {
  if (is.null(provider) && is.null(request_fn)) {
    cli::cli_abort("Provide a chat provider or a request function for {.fn .ms_chat}.")
  }

  if (is.null(provider) && !is.null(request_fn)) {
    config <- list(
      provider = "mock",
      model = .ms_chat_trim_string(model, default = "mock-chat"),
      api_key = api_key,
      base_url = base_url,
      timeout_seconds = timeout_seconds,
      request_fn = request_fn
    )
  } else {
    config <- .ms_llm_resolve_config(
      provider = provider,
      model = model,
      api_key = api_key,
      base_url = base_url,
      timeout_seconds = timeout_seconds,
      request_fn = request_fn %||% .ms_chat_http_request
    )
  }

  result <- .ms_chat_invoke_request(
    config$request_fn,
    messages = messages,
    config = config,
    response_schema = response_schema,
    temperature = temperature
  )

  if (is.character(result)) {
    return(list(
      content = paste(result, collapse = "\n"),
      data = NULL,
      provider = config$provider,
      model = config$model
    ))
  }

  if (is.list(result) && ("content" %in% names(result) || "data" %in% names(result))) {
    result$provider <- config$provider
    result$model <- config$model
    return(result)
  }

  list(
    content = if (is.list(result)) jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE, null = "null") else as.character(result),
    data = if (is.list(result)) result else NULL,
    provider = config$provider,
    model = config$model
  )
}

.ms_chat_decomposition_payload <- function(state) {
  payload <- .ms_llm_target_payload(
    target_row = state$target,
    candidate_rows = .ms_chat_named_candidate_rows(state$candidate_rows),
    context_chunks = tibble::tibble(),
    target_key = state$session_id
  )

  payload$session_state <- list(
    session_id = state$session_id,
    mode = state$mode,
    approved_facts = lapply(state$approved_facts, function(item) {
      item[c("slot", "value", "timestamp")]
    }),
    unresolved_items = state$unresolved_items,
    decomposition_slots = purrr::imap(state$slot_state, function(item, slot) {
      list(
        slot = slot,
        label = item$label,
        value = item$value %||% NA_character_,
        status = item$status,
        current_guess = item$current_guess %||% NA_character_
      )
    }),
    turn_summaries = lapply(state$turn_summaries, function(item) {
      item[c("round_id", "summary", "answered_slots", "unresolved_items")]
    })
  )

  payload
}

.ms_chat_decomposition_messages_for_candidate_review <- function(state) {
  payload <- .ms_chat_decomposition_payload(state)

  system_prompt <- paste(
    "You are reviewing a metasalmon decomposition session for a measurement or compound-variable target.",
    "Treat the selected variable as a SKOS concept, not an OWL class.",
    "Use local decomposition language around property, entity, constraints, and optional usedProcedure context.",
    "Procedure context is optional adjacent context, not a native decomposition slot.",
    "Choose only from the provided candidates; never invent an IRI.",
    "Return JSON only with keys decision, selected_candidate_index, confidence, rationale, missing_context.",
    "decision must be one of accept, review, propose_new_term.",
    "selected_candidate_index must be null when no candidate should be selected.",
    "confidence must be numeric between 0 and 1."
  )

  user_prompt <- paste(
    "Decomposition review payload:",
    jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE, null = "null"),
    "\n\nReturn JSON only."
  )

  list(
    list(role = "system", content = system_prompt),
    list(role = "user", content = user_prompt)
  )
}

.ms_chat_decomposition_llm_assessment <- function(state,
                                                  chat_provider = NULL,
                                                  chat_model = NULL,
                                                  chat_api_key = NULL,
                                                  chat_base_url = NULL,
                                                  chat_timeout_seconds = 60,
                                                  chat_request_fn = NULL) {
  response <- .ms_chat(
    messages = .ms_chat_decomposition_messages_for_candidate_review(state),
    provider = chat_provider,
    model = chat_model,
    api_key = chat_api_key,
    base_url = chat_base_url,
    timeout_seconds = chat_timeout_seconds,
    request_fn = chat_request_fn,
    temperature = 0.2
  )

  result <- response$data
  if (is.null(result)) {
    result <- tryCatch(
      jsonlite::fromJSON(.ms_llm_clean_json_text(response$content), simplifyVector = FALSE),
      error = function(e) NULL
    )
  }
  if (is.null(result)) {
    cli::cli_abort("Chat adapter did not return a usable JSON object for decomposition review.")
  }

  .ms_validate_llm_assessment(result, .ms_chat_named_candidate_rows(state$candidate_rows))
}

.ms_chat_decomposition_collect_values <- function(state) {
  values <- purrr::imap_chr(state$slot_state, function(item, slot) {
    .ms_chat_first_non_empty(item$value, item$current_guess)
  })
  values[!is.na(values)]
}

.ms_chat_decomposition_new_term_request <- function(state, why = NULL) {
  values <- .ms_chat_decomposition_collect_values(state)
  property <- state$slot_state$property$value %||% state$slot_state$property$current_guess %||% "unspecified property"
  entity <- state$slot_state$entity$value %||% state$slot_state$entity$current_guess %||% "unspecified entity"
  constraints <- state$slot_state$constraints$value %||% state$slot_state$constraints$current_guess %||% NA_character_
  used_procedure <- state$slot_state$used_procedure$value %||% state$slot_state$used_procedure$current_guess %||% NA_character_
  label_parts <- c(property, entity, constraints)
  label_parts <- label_parts[!is.na(label_parts) & nzchar(label_parts)]
  proposed_label <- paste(label_parts, collapse = " ")
  if (!nzchar(proposed_label)) {
    proposed_label <- .ms_chat_first_non_empty(
      state$target$search_query[[1]] %||% NA_character_,
      state$target$target_label[[1]] %||% NA_character_,
      state$target$column_name[[1]] %||% "proposed variable"
    )
  }

  definition_bits <- c(
    sprintf("Proposed SKOS concept for %s of %s.", property, entity),
    if (!is.na(constraints) && nzchar(constraints)) sprintf("Constraints: %s.", constraints),
    if (!is.na(used_procedure) && nzchar(used_procedure)) sprintf("Optional usedProcedure context: %s.", used_procedure)
  )

  list(
    proposed_label = proposed_label,
    proposed_definition = paste(definition_bits, collapse = " "),
    candidate_parent = NA_character_,
    synonyms = character(),
    why_existing_terms_failed = .ms_chat_first_non_empty(
      why,
      "Existing shortlist candidates still look weak after decomposition review."
    ),
    supporting_evidence = values,
    affected_columns = state$target$column_name[[1]] %||% NA_character_,
    target_row_key = state$target$target_row_key[[1]] %||% NA_character_
  )
}

.ms_chat_decomposition_recompute_state <- function(state,
                                                   chat_provider = NULL,
                                                   chat_model = NULL,
                                                   chat_api_key = NULL,
                                                   chat_base_url = NULL,
                                                   chat_timeout_seconds = 60,
                                                   chat_request_fn = NULL) {
  state$updated_at <- .ms_chat_now()

  unresolved <- purrr::keep(state$slot_state, ~ !identical(.x$status, "answered"))
  state$unresolved_items <- names(unresolved)

  candidate_rows <- .ms_chat_named_candidate_rows(state$candidate_rows)
  selected <- NULL
  decision <- "review"
  confidence <- NA_real_
  rationale <- NULL
  missing_context <- NULL
  proposal_source <- "heuristic"

  if (isTRUE(state$force_new_term) || nrow(candidate_rows) == 0L) {
    decision <- "propose_new_term"
    rationale <- if (nrow(candidate_rows) == 0L) {
      "No shortlist candidates are available, so the session is carrying a new-term request preview."
    } else {
      "The session is carrying a user-requested new-term preview."
    }
    state$new_term_requests[[length(state$new_term_requests) + 1L]] <- .ms_chat_decomposition_new_term_request(state, why = rationale)
  } else if (!is.null(chat_provider) || !is.null(chat_request_fn)) {
    assessed <- tryCatch(
      .ms_chat_decomposition_llm_assessment(
        state,
        chat_provider = chat_provider,
        chat_model = chat_model,
        chat_api_key = chat_api_key,
        chat_base_url = chat_base_url,
        chat_timeout_seconds = chat_timeout_seconds,
        chat_request_fn = chat_request_fn
      ),
      error = function(e) e
    )

    if (!inherits(assessed, "error")) {
      proposal_source <- "chat"
      decision <- assessed$decision
      confidence <- assessed$confidence
      missing_context <- assessed$missing_context
      rationale <- assessed$rationale
      if (!is.na(assessed$selected_candidate_index)) {
        selected <- list(
          index = assessed$selected_candidate_index,
          candidate = candidate_rows[assessed$selected_candidate_index, , drop = FALSE],
          source = proposal_source,
          rationale = rationale
        )
      }
      if (identical(decision, "propose_new_term")) {
        state$new_term_requests[[length(state$new_term_requests) + 1L]] <- .ms_chat_decomposition_new_term_request(state, why = rationale)
      }
    }
  }

  if (is.null(selected) && !identical(decision, "propose_new_term")) {
    selected <- .ms_chat_decomposition_selected_candidate(state)
    proposal_source <- selected$source
    rationale <- .ms_chat_first_non_empty(rationale, selected$rationale)
    resolved_slots <- sum(vapply(state$slot_state, function(item) identical(item$status, "answered"), logical(1)))
    decision <- if (resolved_slots >= 3L || length(state$approved_facts) >= 2L) "accept" else "review"
    confidence <- if (identical(decision, "accept")) 0.7 else 0.45
  }

  decomposition <- list(
    property = state$slot_state$property$value %||% state$slot_state$property$current_guess %||% NA_character_,
    entity = state$slot_state$entity$value %||% state$slot_state$entity$current_guess %||% NA_character_,
    constraints = state$slot_state$constraints$value %||% state$slot_state$constraints$current_guess %||% NA_character_,
    matrix = state$slot_state$matrix$value %||% state$slot_state$matrix$current_guess %||% NA_character_,
    context_object = state$slot_state$context_object$value %||% state$slot_state$context_object$current_guess %||% NA_character_,
    usedProcedure = state$slot_state$used_procedure$value %||% state$slot_state$used_procedure$current_guess %||% NA_character_,
    unit = state$slot_state$unit$value %||% state$slot_state$unit$current_guess %||% NA_character_,
    statistic = state$slot_state$statistic$value %||% state$slot_state$statistic$current_guess %||% NA_character_,
    derivation_status = state$slot_state$derivation_status$value %||% state$slot_state$derivation_status$current_guess %||% NA_character_,
    temporal_granularity = state$slot_state$temporal_granularity$value %||% state$slot_state$temporal_granularity$current_guess %||% NA_character_,
    spatial_granularity = state$slot_state$spatial_granularity$value %||% state$slot_state$spatial_granularity$current_guess %||% NA_character_
  )

  state$proposed_patch <- list(
    decision = decision,
    selected_candidate_index = if (!is.null(selected)) selected$index else NA_integer_,
    selected_candidate = if (!is.null(selected) && !is.null(selected$candidate)) as.list(selected$candidate[1, , drop = FALSE]) else NULL,
    term_type = "skos_concept",
    rationale = rationale,
    confidence = confidence,
    missing_context = missing_context,
    proposal_source = proposal_source,
    unresolved_items = state$unresolved_items,
    decomposition = decomposition,
    new_term_request = if (length(state$new_term_requests) > 0L) state$new_term_requests[[length(state$new_term_requests)]] else NULL
  )

  state
}

.ms_chat_decomposition_round_summary <- function(state, round_questions, answers) {
  answered_slots <- names(Filter(Negate(is.na), answers))
  unresolved <- state$unresolved_items
  summary_parts <- c()
  if (length(answered_slots) > 0L) {
    answered_text <- paste(sprintf("%s=%s", answered_slots, unname(answers[answered_slots])), collapse = "; ")
    summary_parts <- c(summary_parts, sprintf("Captured %s.", answered_text))
  }
  if (length(unresolved) > 0L) {
    summary_parts <- c(summary_parts, sprintf("Still unresolved: %s.", paste(unresolved, collapse = ", ")))
  } else {
    summary_parts <- c(summary_parts, "No unresolved decomposition slots remain.")
  }

  list(
    round_id = length(state$turn_summaries) + 1L,
    group = round_questions[[1]]$group,
    answered_slots = answered_slots,
    unresolved_items = unresolved,
    summary = paste(summary_parts, collapse = " "),
    timestamp = .ms_chat_now()
  )
}

.ms_chat_decomposition_record_answer <- function(state, question, answer) {
  question_idx <- which(vapply(state$question_queue, function(item) identical(item$id, question$id), logical(1)))
  if (length(question_idx) != 1L) {
    cli::cli_abort("Internal error: could not locate decomposition question {.val {question$id}} in the queue.")
  }

  cleaned <- .ms_chat_trim_string(answer)
  if (is.na(cleaned)) {
    state$question_queue[[question_idx]]$answer <- NA_character_
    state$question_queue[[question_idx]]$status <- "unknown"
    state$question_queue[[question_idx]]$answered_at <- .ms_chat_now()
    state$slot_state[[question$slot]]$status <- "unknown"
    state$slot_state[[question$slot]]$answered_at <- .ms_chat_now()
    return(state)
  }

  state$question_queue[[question_idx]]$answer <- cleaned
  state$question_queue[[question_idx]]$status <- "answered"
  state$question_queue[[question_idx]]$answered_at <- .ms_chat_now()
  state$slot_state[[question$slot]]$value <- cleaned
  state$slot_state[[question$slot]]$status <- "answered"
  state$slot_state[[question$slot]]$answered_at <- .ms_chat_now()
  state$approved_facts[[length(state$approved_facts) + 1L]] <- list(
    slot = question$slot,
    label = state$slot_state[[question$slot]]$label,
    value = cleaned,
    source = "user",
    timestamp = .ms_chat_now()
  )

  state
}

.ms_chat_decomposition_parse_command <- function(text) {
  command_text <- .ms_chat_trim_string(text)
  if (is.na(command_text)) {
    return(list(name = "quit", argument = NA_character_))
  }

  if (!startsWith(command_text, "/")) {
    return(list(name = "text", argument = command_text))
  }

  pieces <- strsplit(sub("^/", "", command_text), "\\s+", perl = TRUE)[[1]]
  name <- tolower(pieces[[1]])
  argument <- if (length(pieces) > 1L) paste(pieces[-1], collapse = " ") else NA_character_
  list(name = name, argument = argument)
}

.ms_chat_decomposition_candidate_preview <- function(state) {
  candidate_rows <- .ms_chat_named_candidate_rows(state$candidate_rows)
  if (nrow(candidate_rows) == 0L) {
    return("Candidate shortlist: none yet")
  }

  preview_rows <- utils::head(candidate_rows, 3L)
  lines <- vapply(seq_len(nrow(preview_rows)), function(i) {
    candidate <- preview_rows[i, , drop = FALSE]
    sprintf(
      "[%d] %s <%s>",
      i,
      candidate$label[[1]] %||% "unlabelled candidate",
      candidate$iri[[1]] %||% "no IRI"
    )
  }, character(1))

  paste(c("Candidate shortlist:", paste("  ", lines)), collapse = "\n")
}

.ms_chat_decomposition_preview_text <- function(state) {
  patch <- state$proposed_patch
  selected_candidate <- patch$selected_candidate
  selected_line <- if (!is.null(selected_candidate)) {
    sprintf(
      "Selected candidate: [%s] %s <%s>",
      patch$selected_candidate_index %||% NA_integer_,
      selected_candidate$label[[1]] %||% selected_candidate$label %||% "unlabelled candidate",
      selected_candidate$iri[[1]] %||% selected_candidate$iri %||% "no IRI"
    )
  } else {
    "Selected candidate: none"
  }

  decomposition_lines <- purrr::imap_chr(patch$decomposition, function(value, name) {
    label <- if (identical(name, "usedProcedure")) "usedProcedure" else gsub("_", " ", name)
    sprintf("  - %s: %s", label, .ms_chat_format_current_guess(value))
  })

  new_term_block <- NULL
  if (identical(patch$decision, "propose_new_term") && !is.null(patch$new_term_request)) {
    new_term_block <- c(
      "New-term request preview:",
      sprintf("  - proposed_label: %s", patch$new_term_request$proposed_label %||% "unspecified"),
      sprintf("  - proposed_definition: %s", patch$new_term_request$proposed_definition %||% "unspecified")
    )
  }

  paste(
    c(
      "Patch preview",
      sprintf("Decision: %s", patch$decision %||% "review"),
      selected_line,
      sprintf("term_type: %s", patch$term_type %||% "skos_concept"),
      sprintf("proposal_source: %s", patch$proposal_source %||% "heuristic"),
      if (!is.na(.ms_chat_trim_string(patch$rationale))) sprintf("Rationale: %s", patch$rationale),
      if (!is.na(.ms_chat_trim_string(patch$missing_context))) sprintf("Missing context: %s", patch$missing_context),
      sprintf(
        "Unresolved items: %s",
        if (length(state$unresolved_items) == 0L) "none" else paste(state$unresolved_items, collapse = ", ")
      ),
      "Decomposition:",
      decomposition_lines,
      .ms_chat_decomposition_candidate_preview(state),
      new_term_block,
      "",
      "Actions: /more, /preview, /choose <n>, /approve, /newterm, /quit"
    ),
    collapse = "\n"
  )
}

.ms_chat_decomposition_create_state <- function(dict_row,
                                                candidate_rows,
                                                session_id = NULL) {
  target <- .ms_chat_decomposition_target_row(dict_row, candidate_rows = candidate_rows)
  list(
    session_id = .ms_chat_trim_string(session_id, default = .ms_chat_new_session_id()),
    mode = "decomposition",
    created_at = .ms_chat_now(),
    updated_at = .ms_chat_now(),
    target = target,
    candidate_rows = .ms_chat_named_candidate_rows(candidate_rows),
    slot_state = .ms_chat_decomposition_slot_state(dict_row),
    question_queue = .ms_chat_decomposition_question_queue(dict_row),
    approved_facts = list(),
    unresolved_items = character(),
    turn_summaries = list(),
    proposed_patch = NULL,
    new_term_requests = list(),
    approval = list(status = "draft", approved_at = NA_character_),
    manual_candidate_index = NA_integer_,
    force_new_term = FALSE
  )
}

.ms_chat_decomposition_filter_suggestions <- function(suggestions, dict_row) {
  suggestions <- .ms_chat_named_candidate_rows(suggestions)
  if (nrow(suggestions) == 0L) {
    return(suggestions)
  }

  keep <- rep(TRUE, nrow(suggestions))
  if ("dataset_id" %in% names(suggestions) && "dataset_id" %in% names(dict_row)) {
    keep <- keep & (is.na(suggestions$dataset_id) | suggestions$dataset_id == dict_row$dataset_id[[1]])
  }
  if ("table_id" %in% names(suggestions) && "table_id" %in% names(dict_row)) {
    keep <- keep & (is.na(suggestions$table_id) | suggestions$table_id == dict_row$table_id[[1]])
  }
  if ("column_name" %in% names(suggestions)) {
    keep <- keep & suggestions$column_name == dict_row$column_name[[1]]
  }
  if ("target_sdp_field" %in% names(suggestions)) {
    keep <- keep & suggestions$target_sdp_field == "term_iri"
  }
  if ("dictionary_role" %in% names(suggestions)) {
    keep <- keep & suggestions$dictionary_role == "variable"
  }

  suggestions[keep, , drop = FALSE]
}

.ms_chat_decomposition_resolve_candidates <- function(df,
                                                      dict,
                                                      dict_row,
                                                      suggestions = NULL,
                                                      sources = c("smn", "gcdfo", "ols", "nvs"),
                                                      search_fn = find_terms,
                                                      max_per_role = 5L) {
  if (!is.null(suggestions)) {
    return(.ms_chat_decomposition_filter_suggestions(suggestions, dict_row))
  }

  suggested <- suggest_semantics(
    df = df,
    dict = dict,
    sources = sources,
    max_per_role = max_per_role,
    search_fn = search_fn,
    llm_assess = FALSE
  )
  suggestion_rows <- attr(suggested, "semantic_suggestions")
  .ms_chat_decomposition_filter_suggestions(suggestion_rows, dict_row)
}

.ms_chat_decomposition_find_row <- function(dict,
                                            column_name,
                                            table_id = NULL,
                                            dataset_id = NULL) {
  dict <- tibble::as_tibble(dict)
  if (nrow(dict) == 0L) {
    cli::cli_abort("{.arg dict} must contain at least one row.")
  }
  if (!"column_name" %in% names(dict)) {
    cli::cli_abort("{.arg dict} must contain a {.field column_name} column.")
  }

  keep <- dict$column_name == column_name
  if (!is.null(table_id) && "table_id" %in% names(dict)) {
    keep <- keep & dict$table_id == table_id
  }
  if (!is.null(dataset_id) && "dataset_id" %in% names(dict)) {
    keep <- keep & dict$dataset_id == dataset_id
  }

  matched <- dict[keep, , drop = FALSE]
  if (nrow(matched) == 0L) {
    cli::cli_abort(
      "Could not find {.val {column_name}} in {.arg dict} with the supplied dataset/table filters."
    )
  }
  if (nrow(matched) > 1L) {
    cli::cli_abort(
      c(
        "{.fn chat_decomposition} matched more than one dictionary row.",
        "i" = "Pass {.arg table_id} and/or {.arg dataset_id} to disambiguate the target column."
      )
    )
  }

  matched
}

.ms_chat_decomposition_help_text <- function() {
  paste(
    c(
      "Available actions:",
      "  /more      ask the next grouped question round",
      "  /preview   show the current patch preview again",
      "  /choose n  force candidate n for the preview",
      "  /approve   approve the current patch or new-term request",
      "  /newterm   switch the preview into a new-term request artifact",
      "  /quit      save the session and stop"
    ),
    collapse = "\n"
  )
}

#' Interactive decomposition review for measurement variables
#'
#' Starts or resumes a lightweight R-console decomposition session for one
#' measurement column. The session keeps structured state separately from the
#' raw transcript, asks grouped decomposition questions in small rounds, and
#' ends in an explicit preview/approve or new-term decision.
#'
#' The current first slice is intentionally narrow: it focuses on measurement
#' `term_iri` review and reuses existing [suggest_semantics()] retrieval
#' machinery when a shortlist is not supplied directly.
#'
#' @param dict A dictionary tibble containing the target measurement column.
#' @param column_name Column name to review.
#' @param df Optional data frame or named list of data frames, forwarded to
#'   [suggest_semantics()] when `suggestions` are not supplied.
#' @param table_id,dataset_id Optional keys used to disambiguate `column_name`
#'   when `dict` contains multiple matching rows.
#' @param suggestions Optional `semantic_suggestions`-like tibble. When omitted,
#'   [suggest_semantics()] is called and the results are filtered down to the
#'   selected measurement column's `term_iri` variable shortlist.
#' @param sources,search_fn,max_per_role Retrieval controls used only when
#'   `suggestions` are not supplied.
#' @param session_id Optional existing session id to resume.
#' @param session_root Optional directory for persisted sessions. Defaults to
#'   a package state directory under [tools::R_user_dir()].
#' @param round_size Maximum number of grouped questions to ask in each round.
#'   Default is `3`.
#' @param chat_provider,chat_model,chat_api_key,chat_base_url Optional chat
#'   adapter settings used for shortlist review. When omitted, the function uses
#'   a deterministic fallback over the retrieved shortlist.
#' @param chat_timeout_seconds Timeout for the optional chat adapter call.
#' @param chat_request_fn Advanced/test hook overriding the package-local chat
#'   adapter request function.
#' @param commands Optional scripted replies/actions, mainly for testing.
#'   When supplied, they take precedence over `input_fn`.
#' @param input_fn Function used to read console input. Defaults to
#'   [base::readline()].
#' @param output_fn Function used to print console output. Defaults to a simple
#'   `cat()` wrapper.
#'
#' @return A list with `session_id`, `session_dir`, `approval_status`,
#'   `proposed_patch`, `approved_patch`, `state`, and `transcript`.
#' @export
#'
#' @examples
#' \dontrun{
#' dict <- tibble::tibble(
#'   dataset_id = "demo",
#'   table_id = "main",
#'   column_name = "spawner_count",
#'   column_label = "Spawner count",
#'   column_description = "Estimated natural-origin spawner abundance",
#'   column_role = "measurement",
#'   value_type = "integer",
#'   unit_label = "count",
#'   unit_iri = NA_character_,
#'   term_iri = NA_character_,
#'   property_iri = NA_character_,
#'   entity_iri = NA_character_,
#'   constraint_iri = NA_character_,
#'   method_iri = NA_character_
#' )
#'
#' chat_decomposition(dict, column_name = "spawner_count")
#' }
chat_decomposition <- function(dict,
                               column_name,
                               df = NULL,
                               table_id = NULL,
                               dataset_id = NULL,
                               suggestions = NULL,
                               sources = c("smn", "gcdfo", "ols", "nvs"),
                               search_fn = find_terms,
                               max_per_role = 5L,
                               session_id = NULL,
                               session_root = NULL,
                               round_size = 3L,
                               chat_provider = NULL,
                               chat_model = NULL,
                               chat_api_key = NULL,
                               chat_base_url = NULL,
                               chat_timeout_seconds = 60,
                               chat_request_fn = NULL,
                               commands = NULL,
                               input_fn = readline,
                               output_fn = NULL) {
  dict_row <- .ms_chat_decomposition_find_row(
    dict = dict,
    column_name = column_name,
    table_id = table_id,
    dataset_id = dataset_id
  )

  column_role <- .ms_chat_trim_string(dict_row$column_role %||% NA_character_)
  if (!is.na(column_role) && !identical(column_role, "measurement")) {
    cli::cli_abort(
      c(
        "{.fn chat_decomposition} currently expects a measurement dictionary row.",
        "i" = "Received {.val {column_role}} for column {.val {column_name}}."
      )
    )
  }

  input <- .ms_chat_make_input(commands = commands, input_fn = input_fn)
  output <- .ms_chat_make_output(output_fn = output_fn)

  loaded <- NULL
  if (!is.null(session_id)) {
    loaded <- .ms_chat_load_session(session_id, session_root = session_root)
    state <- loaded$state
    transcript <- loaded$transcript
    session_dir <- loaded$session_dir
  } else {
    candidate_rows <- .ms_chat_decomposition_resolve_candidates(
      df = df,
      dict = dict,
      dict_row = dict_row,
      suggestions = suggestions,
      sources = sources,
      search_fn = search_fn,
      max_per_role = max_per_role
    )
    state <- .ms_chat_decomposition_create_state(dict_row, candidate_rows = candidate_rows)
    state <- .ms_chat_decomposition_recompute_state(
      state,
      chat_provider = chat_provider,
      chat_model = chat_model,
      chat_api_key = chat_api_key,
      chat_base_url = chat_base_url,
      chat_timeout_seconds = chat_timeout_seconds,
      chat_request_fn = chat_request_fn
    )
    transcript <- list()
    session_dir <- .ms_chat_session_paths(state$session_id, session_root = session_root)$session_dir
    transcript <- .ms_chat_append_transcript(
      transcript,
      role = "assistant",
      content = sprintf(
        "Started decomposition session %s for %s.",
        state$session_id,
        state$target$column_name[[1]] %||% column_name
      ),
      turn_index = 0L
    )
    .ms_chat_save_session(state, transcript, session_root = session_root)
  }

  intro <- paste(
    c(
      "metasalmon decomposition chat",
      sprintf("Session: %s", state$session_id),
      sprintf("Mode: %s", state$mode),
      sprintf(
        "Target: %s / %s / %s",
        state$target$dataset_id[[1]] %||% NA_character_,
        state$target$table_id[[1]] %||% NA_character_,
        state$target$column_name[[1]] %||% NA_character_
      ),
      "This flow keeps structured state separate from the transcript and treats the variable as a SKOS concept.",
      "Procedure context is tracked as usedProcedure-style context, not as a native decomposition slot.",
      .ms_chat_decomposition_candidate_preview(state),
      "",
      .ms_chat_decomposition_help_text()
    ),
    collapse = "\n"
  )
  output(intro)
  transcript <- .ms_chat_append_transcript(transcript, role = "assistant", content = intro, turn_index = 0L)
  .ms_chat_save_session(state, transcript, session_root = session_root)

  done <- FALSE
  while (!done) {
    if (identical(state$approval$status, "approved")) {
      break
    }

    round_questions <- .ms_chat_decomposition_next_round(state, round_size = round_size)
    if (length(round_questions) > 0L) {
      round_text <- .ms_chat_decomposition_round_text(round_questions, state)
      output(round_text)
      transcript <- .ms_chat_append_transcript(
        transcript,
        role = "assistant",
        content = round_text,
        turn_index = length(state$turn_summaries) + 1L
      )

      answers <- rep(NA_character_, length(round_questions))
      names(answers) <- vapply(round_questions, function(question) question$slot, character(1))

      for (i in seq_along(round_questions)) {
        question <- round_questions[[i]]
        prompt <- sprintf("%s > ", question$slot)
        answer <- input(prompt)
        if (is.na(answer)) {
          answer <- ""
        }
        answers[[question$slot]] <- .ms_chat_trim_string(answer)
        transcript <- .ms_chat_append_transcript(
          transcript,
          role = "user",
          content = sprintf("%s: %s", question$slot, ifelse(is.na(.ms_chat_trim_string(answer)), "<unknown>", .ms_chat_trim_string(answer))),
          turn_index = length(state$turn_summaries) + 1L
        )
        state <- .ms_chat_decomposition_record_answer(state, question, answer)
      }

      state <- .ms_chat_decomposition_recompute_state(
        state,
        chat_provider = chat_provider,
        chat_model = chat_model,
        chat_api_key = chat_api_key,
        chat_base_url = chat_base_url,
        chat_timeout_seconds = chat_timeout_seconds,
        chat_request_fn = chat_request_fn
      )
      summary <- .ms_chat_decomposition_round_summary(state, round_questions, answers)
      state$turn_summaries[[length(state$turn_summaries) + 1L]] <- summary
      transcript <- .ms_chat_append_transcript(
        transcript,
        role = "assistant",
        content = summary$summary,
        turn_index = summary$round_id
      )
      .ms_chat_save_session(state, transcript, session_root = session_root)
    }

    repeat {
      preview_text <- .ms_chat_decomposition_preview_text(state)
      output(preview_text)
      transcript <- .ms_chat_append_transcript(
        transcript,
        role = "assistant",
        content = preview_text,
        turn_index = length(state$turn_summaries)
      )
      .ms_chat_save_session(state, transcript, session_root = session_root)

      action <- input("Next action > ")
      command <- .ms_chat_decomposition_parse_command(action)
      transcript <- .ms_chat_append_transcript(
        transcript,
        role = "user",
        content = if (is.na(action)) "/quit" else as.character(action),
        turn_index = length(state$turn_summaries)
      )

      if (identical(command$name, "help")) {
        output(.ms_chat_decomposition_help_text())
        next
      }

      if (identical(command$name, "preview")) {
        next
      }

      if (identical(command$name, "choose")) {
        picked <- suppressWarnings(as.integer(command$argument))
        if (is.na(picked) || picked < 1L || picked > nrow(.ms_chat_named_candidate_rows(state$candidate_rows))) {
          output("Choose a valid shortlist index, e.g. /choose 2.")
          next
        }
        state$manual_candidate_index <- picked
        state$force_new_term <- FALSE
        state <- .ms_chat_decomposition_recompute_state(
          state,
          chat_provider = chat_provider,
          chat_model = chat_model,
          chat_api_key = chat_api_key,
          chat_base_url = chat_base_url,
          chat_timeout_seconds = chat_timeout_seconds,
          chat_request_fn = chat_request_fn
        )
        .ms_chat_save_session(state, transcript, session_root = session_root)
        next
      }

      if (identical(command$name, "newterm")) {
        state$force_new_term <- TRUE
        state <- .ms_chat_decomposition_recompute_state(
          state,
          chat_provider = chat_provider,
          chat_model = chat_model,
          chat_api_key = chat_api_key,
          chat_base_url = chat_base_url,
          chat_timeout_seconds = chat_timeout_seconds,
          chat_request_fn = chat_request_fn
        )
        .ms_chat_save_session(state, transcript, session_root = session_root)
        next
      }

      if (identical(command$name, "approve")) {
        state$approval$status <- "approved"
        state$approval$approved_at <- .ms_chat_now()
        state$updated_at <- .ms_chat_now()
        .ms_chat_save_session(state, transcript, session_root = session_root)
        done <- TRUE
        break
      }

      if (identical(command$name, "quit")) {
        .ms_chat_save_session(state, transcript, session_root = session_root)
        done <- TRUE
        break
      }

      if (identical(command$name, "more")) {
        if (length(.ms_chat_decomposition_next_round(state, round_size = round_size)) == 0L) {
          output("No open grouped questions remain. Use /preview, /choose <n>, /approve, /newterm, or /quit.")
          next
        }
        break
      }

      output("Unknown action. Try /help for the available commands.")
    }
  }

  list(
    session_id = state$session_id,
    session_dir = session_dir,
    approval_status = state$approval$status,
    proposed_patch = state$proposed_patch,
    approved_patch = if (identical(state$approval$status, "approved")) state$proposed_patch else NULL,
    state = state,
    transcript = transcript
  )
}
