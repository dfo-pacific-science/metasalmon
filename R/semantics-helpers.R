.ms_semantic_split_role_hints <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(character())
  }
  hints <- trimws(unlist(strsplit(as.character(x), "\\|", fixed = FALSE)))
  hints[nzchar(hints)]
}

.ms_semantic_role_hint_status <- function(role, role_hints) {
  hints <- .ms_semantic_split_role_hints(role_hints)
  if (length(hints) == 0) {
    return("unknown")
  }
  if (role %in% hints) {
    return("match")
  }
  if (identical(role, "variable") && "property" %in% hints) {
    return("mismatch_property")
  }
  if (identical(role, "property") && "variable" %in% hints) {
    return("mismatch_variable")
  }
  "unknown"
}

.ms_semantic_role_hint_bonus <- function(status) {
  switch(status,
    match = 0.35,
    mismatch_property = -0.35,
    mismatch_variable = -0.35,
    0
  )
}

.ms_semantic_role_hint_explanation <- function(status, role) {
  switch(status,
    match = paste0("Candidate carries a ", role, " role hint."),
    mismatch_property = "Candidate carries a property hint; kept lower for variable destination.",
    mismatch_variable = "Candidate carries a variable hint; kept lower for property destination.",
    NA_character_
  )
}

.ms_sources_for_target_role <- function(base_sources, search_role) {
  if (length(base_sources) == 0) {
    return(base_sources)
  }
  if (!identical(search_role, "unit")) {
    return(base_sources)
  }

  unique(c(base_sources, sources_for_role("unit")))
}

.ms_retrieve_semantic_target_candidates <- function(target,
                                                    sources,
                                                    max_per_role,
                                                    search_fn,
                                                    query = NULL,
                                                    retrieval_pass = 1L) {
  target <- tibble::as_tibble(target)
  if (nrow(target) == 0) {
    return(tibble::tibble())
  }

  search_role <- as.character((target$search_role[[1]] %||% target$dictionary_role[[1]]) %||% "")
  if (!nzchar(search_role)) {
    return(tibble::tibble())
  }

  query <- trimws(as.character(query[[1]] %||% target$search_query[[1]] %||% ""))
  if (!nzchar(query)) {
    return(tibble::tibble())
  }

  target_sources <- .ms_sources_for_target_role(sources, search_role)
  res <- search_fn(query, role = search_role, sources = target_sources)
  if (!inherits(res, "data.frame") || nrow(res) == 0) {
    return(tibble::tibble())
  }

  res <- tibble::as_tibble(res)
  res <- res[!duplicated(paste(res$source, res$iri, sep = "::")), , drop = FALSE]
  if (!"role_hints" %in% names(res)) {
    res$role_hints <- NA_character_
  }
  res$role_hint_status <- vapply(
    res$role_hints,
    function(h) .ms_semantic_role_hint_status(search_role, h),
    character(1)
  )
  res$role_hint_bonus <- vapply(res$role_hint_status, .ms_semantic_role_hint_bonus, numeric(1))
  res$role_hint_explanation <- vapply(
    res$role_hint_status,
    function(s) .ms_semantic_role_hint_explanation(s, search_role),
    character(1)
  )
  if ("score" %in% names(res)) {
    res$score <- res$score + res$role_hint_bonus
    res <- res[order(-res$score, res$source, res$ontology, res$label, res$iri), , drop = FALSE]
  } else {
    res <- res[order(-res$role_hint_bonus, res$source, res$ontology, res$label, res$iri), , drop = FALSE]
  }
  res <- utils::head(res, max(1L, as.integer(max_per_role[[1]] %||% 3L)))

  target_cols <- intersect(
    c(
      "dataset_id",
      "table_id",
      "column_name",
      "code_value",
      "dictionary_role",
      "search_role",
      "target_scope",
      "target_sdp_file",
      "target_sdp_field",
      "target_row_key",
      "target_label",
      "target_description",
      "search_query",
      "target_query_basis",
      "target_query_context",
      "column_label",
      "column_description",
      "code_label",
      "code_description"
    ),
    names(target)
  )
  for (nm in target_cols) {
    res[[nm]] <- target[[nm]][[1]]
  }
  res$retrieval_query <- query
  res$retrieval_pass <- as.integer(retrieval_pass[[1]] %||% 1L)
  res
}

.ms_merge_semantic_target_candidates <- function(existing_rows, extra_rows, max_per_role) {
  existing_rows <- tibble::as_tibble(existing_rows)
  extra_rows <- tibble::as_tibble(extra_rows)
  combined <- dplyr::bind_rows(existing_rows, extra_rows)
  if (nrow(combined) == 0) {
    return(combined)
  }

  if ("score" %in% names(combined)) {
    combined <- combined[order(
      -combined$score,
      combined$source,
      combined$ontology,
      combined$label,
      combined$iri,
      combined$retrieval_pass,
      combined$retrieval_query
    ), , drop = FALSE]
  } else {
    combined <- combined[order(
      -combined$role_hint_bonus,
      combined$source,
      combined$ontology,
      combined$label,
      combined$iri,
      combined$retrieval_pass,
      combined$retrieval_query
    ), , drop = FALSE]
  }

  combined <- combined[!duplicated(paste(combined$source, combined$iri, sep = "::")), , drop = FALSE]
  utils::head(combined, max(1L, as.integer(max_per_role[[1]] %||% 3L)))
}

#' Suggest semantic annotations for a dictionary
#'
#' Searches external vocabularies to suggest IRIs for semantic gaps in the
#' dictionary and package metadata. Measurement columns keep full I-ADOPT
#' decomposition (`term_iri`, `property_iri`, `entity_iri`, `unit_iri`,
#' `constraint_iri`), while selected non-measurement columns can receive
#' lighter `term_iri` coverage when they are categorical or controlled
#' low-cardinality attributes.
#'
#' The function uses the column's label or description as the search query and
#' returns suggestions as an attribute on the dictionary tibble. This allows
#' you to review candidates before accepting them into your dictionary.
#'
#' @param df A data frame or tibble containing the data being documented, or a
#'   named list of data frames for multi-table workflows. When a named list is
#'   supplied, `suggest_semantics()` matches each dictionary row to the correct
#'   table via `dict$table_id` and uses that table's data as context.
#' @param dict A dictionary tibble created by `infer_dictionary()` (may have
#'   incomplete semantic fields).
#' @param sources Character vector of vocabulary sources to search. Options are
#'   `"smn"` (Salmon Domain Ontology via content negotiation), `"gcdfo"` (DFO-specific source), `"ols"` (Ontology Lookup Service), `"nvs"` (NERC Vocabulary Server), and
#'   `"bioportal"` (requires `BIOPORTAL_APIKEY` environment variable).
#'   Default is `c("smn", "gcdfo", "ols", "nvs")`.
#' @param include_dwc Logical; if `TRUE`, also attach DwC-DP export mappings
#'   (via `suggest_dwc_mappings()`) as a parallel attribute `dwc_mappings`.
#'   Default is `FALSE` to keep the UI simple for non-DwC users.
#' @param max_per_role Maximum number of suggestions to keep per I-ADOPT role
#'   (variable, property, entity, unit, constraint) per column. Default is 3.
#' @param search_fn Function used to search terms. Defaults to `find_terms()`.
#'   Can be replaced for testing or custom search strategies.
#' @param codes Optional `codes.csv`-like tibble. When provided, suggestions are
#'   also generated for missing `codes.csv$term_iri` targets.
#' @param table_meta Optional `tables.csv`-like tibble. When provided,
#'   suggestions are generated for missing `tables.csv$observation_unit_iri`.
#' @param dataset_meta Optional `dataset.csv`-like tibble. When provided,
#'   suggestions are generated for missing `dataset.csv$keywords` as candidate
#'   semantic keywords (IRIs intended for keyword curation).
#' @param llm_assess Logical; if `TRUE`, assess the top semantic candidates per
#'   target with an LLM after deterministic retrieval. When the first shortlist
#'   looks weak, the LLM may request at most one bounded alternate-query pass
#'   (1--2 plain-text search phrases) before a single reassessment. Default is
#'   `FALSE`.
#' @param llm_provider LLM provider preset. One of `"openai"`,
#'   `"openrouter"`, `"openai_compatible"`, or `"chapi"`.
#' @param llm_model Character model identifier. Required when
#'   `llm_assess = TRUE` unless supplied via `METASALMON_LLM_MODEL`. When
#'   `llm_provider = "openrouter"` and no model is supplied, the package
#'   defaults to `"openrouter/free"`. Any valid OpenRouter model ID may be
#'   supplied here (for example `"openai/gpt-5.4-mini"`). When
#'   `llm_provider = "chapi"` and no model is supplied, the package defaults
#'   to `"ollama2.mistral:7b"` and also checks `CHAPI_MODEL`.
#' @param llm_api_key Optional API key override. If omitted, provider-specific
#'   environment variables are used (`OPENAI_API_KEY`, `OPENROUTER_API_KEY`,
#'   `CHAPI_API_KEY`, or `METASALMON_LLM_API_KEY`).
#' @param llm_base_url Optional base URL override for the OpenAI-compatible
#'   chat endpoint. Required for `llm_provider = "openai_compatible"` when not
#'   set via `METASALMON_LLM_BASE_URL`. For `llm_provider = "chapi"`, the
#'   package defaults to `https://chapi-dev.intra.azure.cloud.dfo-mpo.gc.ca/api`
#'   and also checks `CHAPI_BASE_URL`.
#' @param llm_top_n Maximum number of retrieved candidates to send to the LLM
#'   per target for each assessment round. Default is `5`.
#' @param llm_context_files Optional character vector of local context files
#'   (for example README/markdown notes, CSV dictionaries, HTML exports,
#'   DOCX files, source/notebook files such as `.R`, `.Rmd`, or `.qmd`, Excel
#'   workbooks, or PDF reports) used to provide extra domain context to the
#'   LLM. PDF support uses the optional `pdftools` package; Excel support uses
#'   the optional `readxl` package.
#' @param llm_context_text Optional character vector of extra inline context
#'   snippets passed alongside `llm_context_files`.
#' @param llm_timeout_seconds Timeout for each LLM request in seconds.
#'   `chapi` models matching `gpt-oss` are automatically given at least 120
#'   seconds because the internal endpoint can be slow to warm up.
#' @param llm_request_fn Advanced/test hook overriding the low-level
#'   OpenAI-compatible request function.
#'
#' @return The dictionary tibble (unchanged) with a `semantic_suggestions`
#'   attribute containing a tibble of suggested IRIs. The suggestions tibble
#'   starts with `column_name`, `dictionary_role`, `table_id`, and `dataset_id`
#'   so the original dictionary term is visible before the candidate match.
#'   It also includes `target_scope`, `target_sdp_file`, and
#'   `target_sdp_field` so users can see exactly where each accepted suggestion
#'   would land in the Salmon Data Package. Additional columns include
#'   `search_query`, `target_query_basis`, `target_query_context`,
#'   `column_label`, `column_description`, `label`, `iri`,
#'   `source`, `ontology`, `definition`, `retrieval_query`, and
#'   `retrieval_pass`. If the underlying search results include a `score`
#'   column, it is preserved for downstream filtering.
#'   For non-column targets, the tibble also includes explicit destination
#'   context (`target_row_key`, `target_label`, `target_description`,
#'   `code_value`, `code_label`, `code_description`) so table-, dataset-, and
#'   code-level rows are inspectable without extra joins. When
#'   `llm_assess = TRUE`, the suggestions also include `llm_*` review columns
#'   such as `llm_decision`, `llm_confidence`, `llm_selected`,
#'   `llm_candidate_rank`, and bounded exploration metadata, and the
#'   dictionary gains a parallel `semantic_llm_assessments` attribute with one
#'   row per assessed target.
#'
#' @details
#' Column targets keep full I-ADOPT behavior for
#' `column_role == "measurement"` rows. Non-measurement coverage is lighter:
#' only missing `term_iri` values are considered, focused on categorical rows
#' and controlled low-cardinality attribute rows inferred through `codes.csv`.
#' Identifier and temporal columns are skipped by default. When `codes`,
#' `table_meta`, or `dataset_meta` are supplied, additional target rows are
#' generated for `codes.csv`, `tables.csv`, and `dataset.csv` respectively.
#' Table-level observation-unit queries ignore review placeholders such as
#' `MISSING METADATA:` and fall back to real table metadata context instead.
#'
#' When `llm_assess = TRUE`, the LLM only judges deterministically retrieved
#' candidates; it does not mint new IRIs. If the first shortlist looks weak,
#' the model may suggest at most one bounded alternate-query round (1--2
#' plain-text queries), the package reruns deterministic retrieval, de-dupes
#' the merged shortlist, and reassesses once. Local context files are read on
#' disk, chunked, and lexically trimmed down before prompt assembly so large
#' README/report/workbook files do not get dumped wholesale into the model
#' call.
#'
#' A term can legitimately appear more than once with different
#' `dictionary_role` values (for example as both a variable and a property).
#' In that case, `match_type` still describes lexical match quality, while
#' `target_sdp_field` tells you where that suggestion would be written in the
#' package. The output adds `role_collision` and `role_collision_note` so
#' variable-vs-property collisions stay explicit and destination-aware.
#'
#' After calling this function, access suggestions with:
#' ```
#' suggestions <- attr(result, "semantic_suggestions")
#' ```
#'
#' Suggestions stay separate by default. Review them first, then use
#' [apply_semantic_suggestions()] for an explicit opt-in merge, or copy values
#' manually when you need finer control.
#'
#' @seealso [find_terms()] for direct vocabulary searches, [infer_dictionary()]
#'   for creating starter dictionaries, [apply_semantic_suggestions()] for
#'   explicitly filling selected IRI fields, [validate_dictionary()] for
#'   checking dictionary completeness.
#' @importFrom metasalmon suggest_dwc_mappings
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a starter dictionary
#' dict <- infer_dictionary(my_data, dataset_id = "example", table_id = "main")
#'
#' # Get semantic suggestions for measurement columns
#' dict_with_suggestions <- suggest_semantics(my_data, dict)
#'
#' # View the suggestions
#' suggestions <- attr(dict_with_suggestions, "semantic_suggestions")
#' print(suggestions)
#'
#' # Filter suggestions for a specific column
#' spawner_suggestions <- suggestions[suggestions$column_name == "SPAWNER_COUNT", ]
#'
#' # Explicitly apply the top suggestion for one column without overwriting
#' # any existing IRIs in the dictionary
#' dict <- apply_semantic_suggestions(dict_with_suggestions, columns = "SPAWNER_COUNT")
#' }
suggest_semantics <- function(df,
                              dict,
                              sources = c("smn", "gcdfo", "ols", "nvs"),
                              include_dwc = FALSE,
                              max_per_role = 3,
                              search_fn = find_terms,
                              codes = NULL,
                              table_meta = NULL,
                              dataset_meta = NULL,
                              llm_assess = FALSE,
                              llm_provider = c("openai", "openrouter", "openai_compatible", "chapi"),
                              llm_model = NULL,
                              llm_api_key = NULL,
                              llm_base_url = NULL,
                              llm_top_n = 5L,
                              llm_context_files = NULL,
                              llm_context_text = NULL,
                              llm_timeout_seconds = 60,
                              llm_request_fn = NULL) {
  resource_lookup <- NULL
  default_df <- NULL

  if (is.list(df) && !inherits(df, "data.frame")) {
    if (length(df) == 0) {
      cli::cli_abort("{.arg df} cannot be an empty resource list")
    }
    if (is.null(names(df)) || any(!nzchar(names(df)))) {
      cli::cli_abort("{.arg df} list inputs must be named by table_id")
    }
    if (anyDuplicated(names(df)) > 0) {
      cli::cli_abort("{.arg df} table_id names must be unique")
    }
    bad_resource <- vapply(df, function(x) !inherits(x, "data.frame"), logical(1))
    if (any(bad_resource)) {
      bad <- which(bad_resource)
      cli::cli_abort("All items in {.arg df} must be data frames. Invalid entries at: {.val {bad}}")
    }
    resource_lookup <- df
    default_df <- resource_lookup[[1L]]
  } else if (is.null(df) || inherits(df, "data.frame")) {
    default_df <- df
  } else {
    cli::cli_abort("{.arg df} must be NULL, a data frame, or a named list of data frames")
  }

  dict <- tibble::as_tibble(dict)
  codes <- if (is.null(codes)) tibble::tibble() else tibble::as_tibble(codes)
  table_meta <- if (is.null(table_meta)) tibble::tibble() else tibble::as_tibble(table_meta)
  dataset_meta <- if (is.null(dataset_meta)) tibble::tibble() else tibble::as_tibble(dataset_meta)

  if (nrow(dict) == 0 && nrow(codes) == 0 && nrow(table_meta) == 0 && nrow(dataset_meta) == 0) {
    attr(dict, "semantic_suggestions") <- tibble::tibble()
    if (isTRUE(llm_assess)) {
      attr(dict, "semantic_llm_assessments") <- tibble::tibble()
    }
    if (isTRUE(include_dwc)) {
      attr(dict, "dwc_mappings") <- tibble::tibble()
    }
    return(dict)
  }

  roles <- c(
    term_iri = "variable",
    property_iri = "property",
    entity_iri = "entity",
    unit_iri = "unit",
    constraint_iri = "constraint",
    method_iri = "method"
  )
  suggestion_leading_cols <- c(
    "column_name",
    "dictionary_role",
    "table_id",
    "dataset_id",
    "target_row_key",
    "target_label",
    "target_description",
    "target_scope",
    "target_sdp_file",
    "target_sdp_field",
    "search_query",
    "target_query_basis",
    "target_query_context",
    "column_label",
    "column_description",
    "code_value",
    "code_label",
    "code_description"
  )

  is_missing <- function(x) {
    if (is.null(x) || length(x) == 0) return(TRUE)
    all(is.na(x) | as.character(x) == "")
  }
  is_present <- function(x) !is_missing(x)
  first_non_empty <- function(values) {
    values <- values[!vapply(values, is_missing, logical(1))]
    if (length(values) == 0) "" else values[[1]]
  }
  decamelize_text <- function(x) {
    x <- as.character(x %||% "")
    x[is.na(x)] <- ""
    gsub("([a-z0-9])([A-Z])", "\\1 \\2", x, perl = TRUE)
  }
  clean_query <- function(x) {
    x <- as.character(x %||% "")
    x[is.na(x)] <- ""
    x <- decamelize_text(x)
    x <- gsub("[._]+", " ", x)
    x <- gsub("\\s+", " ", x)
    trimws(x)
  }
  is_review_placeholder <- function(x) {
    if (is_missing(x)) return(FALSE)
    grepl("^\\s*(REVIEW REQUIRED|MISSING DESCRIPTION|MISSING METADATA)\\s*:", as.character(x), ignore.case = TRUE)
  }
  strip_review_placeholder <- function(x) {
    text <- as.character(x %||% "")
    text[is.na(text)] <- ""
    text <- sub("^\\s*(REVIEW REQUIRED|MISSING DESCRIPTION|MISSING METADATA)\\s*:\\s*", "", text, ignore.case = TRUE)
    text <- sub("^\\s*define what\\s+'?", "", text, ignore.case = TRUE)
    text <- sub("'?\\s*means in table.*$", "", text, ignore.case = TRUE)
    clean_query(text)
  }
  table_context <- function(row, dict) {
    if (!all(c("dataset_id", "table_id") %in% names(dict))) {
      return(tibble::tibble())
    }

    same <- dict[dict$dataset_id == row$dataset_id[[1]] & dict$table_id == row$table_id[[1]], , drop = FALSE]
    if (!"column_name" %in% names(same)) {
      return(same)
    }
    same[same$column_name != row$column_name[[1]], , drop = FALSE]
  }
  context_has <- function(ctx, pattern) {
    if (nrow(ctx) == 0) return(FALSE)
    candidates <- c(
      if ("column_name" %in% names(ctx)) as.character(ctx$column_name) else character(),
      if ("column_label" %in% names(ctx)) as.character(ctx$column_label) else character(),
      if ("column_description" %in% names(ctx)) as.character(ctx$column_description) else character()
    )
    candidates <- candidates[!is.na(candidates) & nzchar(trimws(candidates))]
    if (length(candidates) == 0) return(FALSE)
    any(grepl(pattern, candidates, ignore.case = TRUE))
  }
  current_table_df <- function(row) {
    if (is.null(resource_lookup)) {
      return(default_df)
    }

    table_id <- as.character(row$table_id[[1]] %||% "")
    if (nzchar(table_id) && table_id %in% names(resource_lookup)) {
      return(resource_lookup[[table_id]])
    }

    default_df
  }
  normalize_measurement_unit_query <- function(x) {
    text <- tolower(as.character(x %||% ""))
    text[is.na(text)] <- ""
    text <- decamelize_text(text)
    text <- gsub("\u00e2", "", text, fixed = TRUE)
    text <- gsub("\u00b0", " degree ", text, fixed = TRUE)
    text <- gsub("\u00b3", "3", text, fixed = TRUE)
    text <- clean_query(text)
    text <- gsub("[^a-z0-9/ ]+", " ", text)
    text <- clean_query(text)
    if (!nzchar(text)) return("")

    if (grepl("\\b(degree\\s*c|deg\\s*c|celsius)\\b", text)) return("degree celsius")
    if (grepl("^(cms|cumec|cumecs|m3/s|m\\^3/s|m3 s)$", text)) return("cubic meter per second")
    if (grepl("^(km/h|km h|kph)$", text)) return("kilometer per hour")
    if (grepl("^(square\\s+met(er|re)s?|sq\\s*m|m2)$", text)) return("square meter")
    if (grepl("^(mm|millimet(er|re)s?)$", text)) return("millimeter")
    if (grepl("^(cm|centimet(er|re)s?)$", text)) return("centimeter")
    if (grepl("^(m|met(er|re)s?)$", text)) return("meter")
    if (grepl("^(g|gram(me)?s?)$", text)) return("gram")
    if (grepl("^(kg|kilogram(me)?s?)$", text)) return("kilogram")

    ""
  }
  extract_measurement_header_unit <- function(...) {
    texts <- unlist(list(...), use.names = FALSE)
    texts <- as.character(texts)
    texts <- texts[!is.na(texts) & nzchar(trimws(texts))]
    if (length(texts) == 0) return("")

    for (text in texts) {
      matches <- gregexpr("\\(([^)]{1,20})\\)", text, perl = TRUE)
      pieces <- regmatches(text, matches)[[1]]
      if (length(pieces) > 0) {
        pieces <- trimws(gsub("^\\(|\\)$", "", pieces))
        pieces <- pieces[nzchar(pieces)]
        if (length(pieces) > 0) {
          normalized <- normalize_measurement_unit_query(utils::tail(pieces, 1))
          if (nzchar(normalized)) {
            return(normalized)
          }
        }
      }

      normalized_full_text <- normalize_measurement_unit_query(text)
      if (nzchar(normalized_full_text)) {
        return(normalized_full_text)
      }

      suffix_text <- tolower(clean_query(gsub("\\([^)]*\\)", " ", text)))
      suffix_match <- regmatches(
        suffix_text,
        regexpr("\\bin\\s+(square\\s+met(?:er|re)s?|met(?:er|re)s?|centimet(?:er|re)s?|millimet(?:er|re)s?|degree\\s+celsius|celsius|kilomet(?:er|re)\\s+per\\s+hour|km/h|cms|cumecs|m3/s)\\b", suffix_text, perl = TRUE)
      )
      if (length(suffix_match) == 1 && nzchar(suffix_match)) {
        normalized <- normalize_measurement_unit_query(sub("^in\\s+", "", suffix_match))
        if (nzchar(normalized)) {
          return(normalized)
        }
      }
    }

    ""
  }
  paired_unit_query_from_data <- function(row) {
    column_name <- as.character(row$column_name[[1]] %||% "")
    if (!nzchar(column_name) || !grepl("value$", column_name, ignore.case = TRUE)) {
      return("")
    }

    table_df <- current_table_df(row)
    if (is.null(table_df) || !inherits(table_df, "data.frame")) {
      return("")
    }

    stem <- sub("value$", "", column_name, ignore.case = TRUE)
    sibling_hits <- names(table_df)[tolower(names(table_df)) == paste0(tolower(stem), "unit")]
    if (length(sibling_hits) == 0) {
      return("")
    }

    sibling_values <- as.character(table_df[[sibling_hits[[1]]]])
    sibling_values <- trimws(sibling_values[!is.na(sibling_values)])
    sibling_values <- sibling_values[nzchar(sibling_values)]
    if (length(sibling_values) == 0) {
      return("")
    }

    sibling_values <- sort(table(sibling_values), decreasing = TRUE)
    normalized <- normalize_measurement_unit_query(names(sibling_values)[[1]])
    if (!nzchar(normalized)) {
      return("")
    }

    normalized
  }
  normalize_measurement_header_query <- function(x) {
    text <- clean_query(x)
    if (!nzchar(text)) return("")

    text <- gsub("\\([^)]*\\)", " ", text)
    if (grepl("\\s/\\s", text)) {
      text <- strsplit(text, "\\s/\\s", perl = TRUE)[[1]][1]
    }
    text <- tolower(clean_query(text))
    text <- gsub("\\bin\\s+(square\\s+met(?:er|re)s?|met(?:er|re)s?|centimet(?:er|re)s?|millimet(?:er|re)s?|degree\\s+celsius|celsius|kilomet(?:er|re)\\s+per\\s+hour|km/h|cms|cumecs|m3/s)\\b", " ", text, perl = TRUE)
    replacements <- c(
      "\\btemp\\b" = "temperature",
      "\\bspd\\b" = "speed",
      "\\bdir\\b" = "direction",
      "\\bmax\\b" = "maximum",
      "\\bmin\\b" = "minimum",
      "\\bgrnd\\b" = "ground"
    )
    for (pattern in names(replacements)) {
      text <- gsub(pattern, replacements[[pattern]], text, perl = TRUE)
    }

    if (grepl("\\btotal rain\\b", text)) return("rainfall")
    if (grepl("\\btotal snow\\b", text)) return("snowfall")
    if (grepl("\\bwater level\\b", text)) return("water level")
    if (grepl("\\bdischarge\\b", text)) return("discharge")

    clean_query(text)
  }
  is_count_like_measurement <- function(row, base_query) {
    value_type <- tolower(as.character(row$value_type[[1]] %||% ""))
    text <- tolower(clean_query(paste(
      strip_review_placeholder(row$column_name[[1]]),
      strip_review_placeholder(row$column_label[[1]]),
      base_query
    )))
    if (!nzchar(text)) return(FALSE)

    has_explicit_count <- grepl("\\b(count|counts|number|numbers|num|abundance)\\b", text)
    has_total <- grepl("\\btotal\\b", text)
    has_organism <- grepl("\\b(spawner|spawners|fish|salmon|organism|organisms|recruit|recruits|population|populations|adult|adults)\\b", text)
    looks_integer <- value_type %in% c("integer", "int", "number", "numeric", "double")

    has_explicit_count ||
      (has_total && has_organism) ||
      (grepl("\\babundance\\b", text) && (has_organism || looks_integer)) ||
      (looks_integer && has_organism)
  }
  measurement_role_query <- function(row, dict, role_name) {
    desc_query <- if (is_review_placeholder(row$column_description[[1]])) {
      ""
    } else {
      strip_review_placeholder(row$column_description[[1]])
    }
    label_query <- strip_review_placeholder(row$column_label[[1]])
    name_query <- strip_review_placeholder(row$column_name[[1]])
    base_query <- if (nzchar(desc_query)) {
      clean_query(desc_query)
    } else {
      normalize_measurement_header_query(first_non_empty(list(label_query, name_query)))
    }
    if (!nzchar(base_query)) return("")

    base_lower <- tolower(base_query)
    ctx <- table_context(row, dict)

    if (identical(role_name, "unit")) {
      unit_query <- strip_review_placeholder(row$unit_label[[1]])
      if (!nzchar(unit_query)) {
        unit_query <- extract_measurement_header_unit(row$column_label[[1]], row$column_name[[1]])
      }
      if (!nzchar(unit_query)) {
        unit_query <- paired_unit_query_from_data(row)
      }
      if (nzchar(unit_query)) {
        return(unit_query)
      }
      if (is_count_like_measurement(row, base_query)) {
        return("count")
      }
      return("")
    }

    if (identical(role_name, "constraint")) {
      if (grepl("\\bnatural\\b", base_lower)) return("natural origin")
      if (grepl("\\bhatchery\\b", base_lower)) return("hatchery origin")
      return(base_query)
    }

    if (identical(role_name, "method")) {
      if (context_has(ctx, "method")) {
        return("estimate method")
      }
      return(base_query)
    }

    if (identical(role_name, "entity")) {
      if (context_has(ctx, "stock")) return("stock")
      if (context_has(ctx, "population")) return("population")
      if (grepl("spawner", base_lower)) return("population")
      return(base_query)
    }

    if (role_name %in% c("variable", "property")) {
      if (is_count_like_measurement(row, base_query)) {
        if (grepl("spawner", base_lower)) {
          if (identical(role_name, "variable")) {
            if (grepl("adult", base_lower)) return("adult spawner count")
            return("spawner abundance")
          }
          return("spawner abundance")
        }

        if (grepl("\\babundance\\b", base_lower)) {
          return("abundance")
        }

        if (identical(role_name, "variable")) {
          return("count")
        }
        return("count")
      }
    }

    base_query
  }
  expand_attribute_tokens <- function(x) {
    text <- clean_query(x)
    if (!nzchar(text)) return("")

    text <- tolower(text)
    replacements <- c(
      "\\bcu\\b" = "conservation unit",
      "\\bcus\\b" = "conservation units",
      "\\bwaterbody\\b" = "water body",
      "\\bcde\\b" = "code",
      "\\bdtt\\b" = "date time",
      "\\byr\\b" = "year",
      "\\bpfma\\b" = "pacific fisheries management area",
      "\\byn\\b" = "indicator",
      "\\bavg\\b" = "average"
    )

    for (pattern in names(replacements)) {
      text <- gsub(pattern, replacements[[pattern]], text, perl = TRUE)
    }

    clean_query(text)
  }
  extract_taxon_like_phrase <- function(x) {
    text <- expand_attribute_tokens(x)
    if (!nzchar(text)) return("")

    named_patterns <- c(
      "atlantic salmon",
      "chinook salmon",
      "coho salmon",
      "sockeye salmon",
      "chum salmon",
      "pink salmon",
      "steelhead trout",
      "rainbow trout",
      "cutthroat trout",
      "salmo salar"
    )
    for (pattern in named_patterns) {
      if (grepl(paste0("\\b", pattern, "\\b"), text, perl = TRUE)) {
        return(pattern)
      }
    }

    latin_match <- regmatches(text, regexpr("\\boncorhynchus\\s+[a-z]+\\b", text, perl = TRUE))
    if (length(latin_match) == 1 && nzchar(latin_match)) {
      return(latin_match)
    }

    ""
  }
  non_measurement_search_role <- function(row, dict) {
    desc_query <- if (is_review_placeholder(row$column_description[[1]])) {
      ""
    } else {
      strip_review_placeholder(row$column_description[[1]])
    }
    label_query <- strip_review_placeholder(row$column_label[[1]])
    name_query <- strip_review_placeholder(row$column_name[[1]])
    query_text <- expand_attribute_tokens(paste(desc_query, label_query, name_query, collapse = " "))
    if (!nzchar(query_text)) return("variable")

    ctx <- table_context(row, dict)
    taxon_query <- extract_taxon_like_phrase(query_text)

    if (nzchar(taxon_query) && grepl("\\b(confirm|confirmed|identify|identified|species|taxon)\\b", query_text, perl = TRUE)) {
      return("entity")
    }
    if (grepl("\\b(method|protocol|procedure|gear|enumeration)\\b", query_text, perl = TRUE)) {
      return("method")
    }
    if (grepl("\\b(watershed|waterbody|river|stream|location|site|area|conservation unit|management unit)\\b", query_text, perl = TRUE)) {
      return("entity")
    }
    if (grepl("\\b(stage|classification|class|type|status|context|origin|accuracy|precision|reliability|index)\\b", query_text, perl = TRUE)) {
      return("constraint")
    }
    if (grepl("\\b(species|taxon|population|stock)\\b", query_text, perl = TRUE)) {
      return("entity")
    }

    "variable"
  }
  non_measurement_query <- function(row, dict, search_role = non_measurement_search_role(row, dict)) {
    desc_query <- if (is_review_placeholder(row$column_description[[1]])) {
      ""
    } else {
      strip_review_placeholder(row$column_description[[1]])
    }
    label_query <- strip_review_placeholder(row$column_label[[1]])
    name_query <- strip_review_placeholder(row$column_name[[1]])
    base_query <- expand_attribute_tokens(first_non_empty(list(desc_query, label_query, name_query)))
    all_text <- expand_attribute_tokens(paste(desc_query, label_query, name_query, collapse = " "))
    if (!nzchar(base_query)) return("")

    ctx <- table_context(row, dict)

    if (identical(search_role, "method")) {
      if (grepl("\\bestimate\\b", all_text, perl = TRUE) && grepl("\\bmethod\\b", all_text, perl = TRUE)) return("estimate method")
      if (grepl("\\bcount(ing)?\\b", all_text, perl = TRUE) && grepl("\\bmethod\\b", all_text, perl = TRUE)) return("counting method")
      if (grepl("\\bcatch\\b", all_text, perl = TRUE) && grepl("\\bmethod\\b", all_text, perl = TRUE)) return("capture method")
      return(base_query)
    }

    if (identical(search_role, "constraint")) {
      if (grepl("\\brun\\b", all_text, perl = TRUE) && grepl("\\btype\\b", all_text, perl = TRUE)) return("run context")
      if (grepl("\\bestimate\\b", all_text, perl = TRUE) && grepl("\\bstage\\b", all_text, perl = TRUE)) return("spawner stage context")
      if (grepl("\\bestimate\\b", all_text, perl = TRUE) && grepl("\\bclassification\\b", all_text, perl = TRUE)) return("abundance data type")
      if (grepl("\\borigin\\b", all_text, perl = TRUE)) return("origin")
      return(base_query)
    }

    if (identical(search_role, "entity")) {
      taxon_query <- extract_taxon_like_phrase(all_text)
      if (nzchar(taxon_query)) return(taxon_query)
      if (grepl("\\bconservation unit\\b", all_text, perl = TRUE)) return("conservation unit")
      if (grepl("\\baquaculture management unit\\b", all_text, perl = TRUE)) return("aquaculture management unit")
      if (grepl("\\bmanagement unit\\b", all_text, perl = TRUE)) return("management unit")
      if (grepl("\\bspecies\\b|\\btaxon\\b", all_text, perl = TRUE)) return("species")
      if (grepl("\\bpopulation\\b", all_text, perl = TRUE)) return("population")
      if (grepl("\\bwatershed\\b", all_text, perl = TRUE)) return("watershed")
      if (grepl("\\bwaterbody\\b|\\briver\\b|\\bstream\\b", all_text, perl = TRUE)) return("water body")
      if (grepl("\\bsite\\b|\\blocation\\b", all_text, perl = TRUE)) return("site")
      if (grepl("\\barea\\b", all_text, perl = TRUE)) {
        if (context_has(ctx, "waterbody|watershed|river|stream")) return("water body")
        return("area")
      }
    }

    base_query
  }
  table_target_query <- function(row) {
    observation_unit <- if ("observation_unit" %in% names(row) && !is_review_placeholder(row$observation_unit[[1]])) {
      strip_review_placeholder(row$observation_unit[[1]])
    } else {
      ""
    }
    table_description <- if ("description" %in% names(row) && !is_review_placeholder(row$description[[1]])) {
      strip_review_placeholder(row$description[[1]])
    } else {
      ""
    }
    table_label <- if ("table_label" %in% names(row)) strip_review_placeholder(row$table_label[[1]]) else ""
    table_id_query <- if ("table_id" %in% names(row)) strip_review_placeholder(row$table_id[[1]]) else ""

    query_basis <- if (nzchar(observation_unit)) {
      "observation_unit"
    } else if (nzchar(table_description)) {
      "description"
    } else if (nzchar(table_label)) {
      "table_label"
    } else if (nzchar(table_id_query)) {
      "table_id"
    } else {
      ""
    }

    query_context_parts <- c(observation_unit, table_description, table_label, table_id_query)
    query_context_parts <- query_context_parts[nzchar(query_context_parts)]

    tibble::tibble(
      search_query = clean_query(first_non_empty(list(observation_unit, table_description, table_label, table_id_query))),
      target_query_basis = query_basis,
      target_query_context = clean_query(paste(query_context_parts, collapse = " "))
    )
  }
  has_low_card_codes <- function(row, codes) {
    if (nrow(codes) == 0) return(FALSE)
    keep <- rep(TRUE, nrow(codes))
    for (key in intersect(c("dataset_id", "table_id", "column_name"), names(codes))) {
      value <- row[[key]][[1]]
      if (!is.na(value) && nzchar(as.character(value))) {
        keep <- keep & !is.na(codes[[key]]) & as.character(codes[[key]]) == as.character(value)
      }
    }
    any(keep)
  }
  has_location_like_column_signal <- function(row, dict) {
    if (!identical(non_measurement_search_role(row, dict), "entity")) {
      return(FALSE)
    }

    desc_query <- if (is_review_placeholder(row$column_description[[1]])) {
      ""
    } else {
      strip_review_placeholder(row$column_description[[1]])
    }
    label_query <- strip_review_placeholder(row$column_label[[1]])
    name_query <- strip_review_placeholder(row$column_name[[1]])
    query_text <- expand_attribute_tokens(paste(desc_query, label_query, name_query, collapse = " "))
    if (!nzchar(query_text)) {
      return(FALSE)
    }

    grepl("\\b(watershed|waterbody|river|stream|location|site)\\b", query_text, perl = TRUE)
  }
  same_table_column_names <- function(row, dict) {
    if (nrow(dict) == 0 || !all(c("dataset_id", "table_id", "column_name") %in% names(dict))) {
      return(character())
    }

    keep <- rep(TRUE, nrow(dict))
    for (key in intersect(c("dataset_id", "table_id"), names(dict))) {
      value <- row[[key]][[1]]
      if (!is.na(value) && nzchar(as.character(value))) {
        keep <- keep & !is.na(dict[[key]]) & as.character(dict[[key]]) == as.character(value)
      }
    }

    cols <- trimws(tolower(as.character(dict$column_name[keep])))
    unique(cols[nzchar(cols)])
  }
  has_long_format_observation_value_pattern <- function(row, dict) {
    cols <- same_table_column_names(row, dict)
    if (length(cols) == 0) {
      return(FALSE)
    }

    has_value <- "value" %in% cols
    has_variable <- any(cols %in% c("variable_name", "measurement_name", "parameter_name", "parameter", "analyte_name"))
    has_unit <- any(cols %in% c("unit_code", "unit", "unit_label"))

    has_value && has_variable && has_unit
  }
  is_long_format_observation_helper <- function(row, dict) {
    col_name <- tolower(trimws(as.character(row$column_name[[1]] %||% "")))
    if (!nzchar(col_name) || !has_long_format_observation_value_pattern(row, dict)) {
      return(FALSE)
    }

    col_name %in% c(
      "parameter",
      "unit",
      "unit_code",
      "vmv_code",
      "flag",
      "status",
      "grade",
      "method_detect_limit",
      "station_name",
      "location name"
    )
  }
  non_measurement_roles <- function(row, codes, dict) {
    role <- tolower(as.character(row$column_role[[1]] %||% ""))
    if (!nzchar(role) || role %in% c("identifier", "temporal")) return(character())
    if (.ms_is_text_like_field_name(row$column_name[[1]] %||% "")) return(character())
    if (is_long_format_observation_helper(row, dict)) return(character())

    term_missing <- "term_iri" %in% names(row) && is_missing(row$term_iri[[1]])
    if (!term_missing) return(character())

    has_codes <- has_low_card_codes(row, codes)
    location_fallback <- has_location_like_column_signal(row, dict)
    if (!has_codes && !location_fallback) return(character())

    if (role %in% c("categorical", "attribute")) {
      return(c(term_iri = non_measurement_search_role(row, dict)))
    }
    character()
  }
  targets <- tibble::tibble()

  if (nrow(dict) > 0) {
    column_targets <- purrr::map_dfr(seq_len(nrow(dict)), function(i) {
      row <- dict[i, , drop = FALSE]
      role_targets <- if (identical(row$column_role[[1]], "measurement")) {
        roles
      } else {
        non_measurement_roles(row, codes, dict)
      }
      if (length(role_targets) == 0) return(tibble::tibble())

      purrr::imap_dfr(role_targets, function(search_role, col_name) {
        if (!col_name %in% names(row)) return(tibble::tibble())
        if (is_present(row[[col_name]][[1]])) return(tibble::tibble())

        dictionary_role <- roles[[col_name]] %||% search_role
        role_query <- if (identical(row$column_role[[1]], "measurement")) {
          measurement_role_query(row, dict, search_role)
        } else {
          non_measurement_query(row, dict, search_role = search_role)
        }
        if (!nzchar(role_query)) return(tibble::tibble())
        tibble::tibble(
          dataset_id = row$dataset_id[[1]],
          table_id = row$table_id[[1]],
          column_name = row$column_name[[1]],
          code_value = NA_character_,
          dictionary_role = dictionary_role,
          search_role = search_role,
          target_scope = "column",
          target_sdp_file = "column_dictionary.csv",
          target_sdp_field = col_name,
          target_row_key = paste(row$dataset_id[[1]], row$table_id[[1]], row$column_name[[1]], sep = "/"),
          target_label = row$column_label[[1]],
          target_description = row$column_description[[1]],
          search_query = role_query,
          column_label = row$column_label[[1]],
          column_description = row$column_description[[1]],
          code_label = NA_character_,
          code_description = NA_character_
        )
      })
    })
    targets <- dplyr::bind_rows(targets, column_targets)
  }

  if (nrow(codes) > 0) {
    code_targets <- purrr::map_dfr(seq_len(nrow(codes)), function(i) {
      row <- codes[i, , drop = FALSE]
      term_iri <- if ("term_iri" %in% names(row)) row$term_iri[[1]] else NA_character_
      if (is_present(term_iri)) return(tibble::tibble())

      dataset_id <- if ("dataset_id" %in% names(row)) row$dataset_id[[1]] else NA_character_
      table_id <- if ("table_id" %in% names(row)) row$table_id[[1]] else NA_character_
      column_name <- if ("column_name" %in% names(row)) row$column_name[[1]] else NA_character_
      code_value <- if ("code_value" %in% names(row)) row$code_value[[1]] else NA_character_
      code_label <- if ("code_label" %in% names(row)) row$code_label[[1]] else NA_character_
      code_description <- if ("code_description" %in% names(row)) row$code_description[[1]] else NA_character_

      parent_row <- dict[dict$dataset_id == dataset_id & dict$table_id == table_id & dict$column_name == column_name, , drop = FALSE]
      parent_role <- if (nrow(parent_row) > 0 && "column_role" %in% names(parent_row)) parent_row$column_role[[1]] else NA_character_
      parent_label <- if (nrow(parent_row) > 0 && "column_label" %in% names(parent_row)) parent_row$column_label[[1]] else column_name
      parent_description <- if (nrow(parent_row) > 0 && "column_description" %in% names(parent_row)) parent_row$column_description[[1]] else NA_character_

      role_set <- if (identical(parent_role, "measurement")) c("constraint", "entity", "method") else c("entity")
      query <- clean_query(first_non_empty(list(code_description, code_label, code_value, parent_description, parent_label, column_name)))
      if (!nzchar(query)) return(tibble::tibble())

      tibble::tibble(
        dataset_id = dataset_id,
        table_id = table_id,
        column_name = column_name,
        code_value = code_value,
        dictionary_role = role_set,
        search_role = role_set,
        target_scope = "code",
        target_sdp_file = "codes.csv",
        target_sdp_field = "term_iri",
        target_row_key = paste(dataset_id, table_id, column_name, code_value, sep = "/"),
        target_label = first_non_empty(list(code_label, code_value)),
        target_description = code_description,
        search_query = query,
        column_label = parent_label,
        column_description = parent_description,
        code_label = code_label,
        code_description = code_description
      )
    })
    targets <- dplyr::bind_rows(targets, code_targets)
  }

  if (nrow(table_meta) > 0) {
    table_targets <- purrr::map_dfr(seq_len(nrow(table_meta)), function(i) {
      row <- table_meta[i, , drop = FALSE]
      observation_unit_iri <- if ("observation_unit_iri" %in% names(row)) row$observation_unit_iri[[1]] else NA_character_
      if (is_present(observation_unit_iri)) return(tibble::tibble())

      dataset_id <- if ("dataset_id" %in% names(row)) row$dataset_id[[1]] else NA_character_
      table_id <- if ("table_id" %in% names(row)) row$table_id[[1]] else NA_character_
      table_label <- if ("table_label" %in% names(row)) row$table_label[[1]] else table_id
      table_description <- if ("description" %in% names(row)) row$description[[1]] else NA_character_
      query_info <- table_target_query(row)
      query <- query_info$search_query[[1]]
      if (!nzchar(query)) return(tibble::tibble())

      tibble::tibble(
        dataset_id = dataset_id,
        table_id = table_id,
        column_name = NA_character_,
        code_value = NA_character_,
        dictionary_role = "entity",
        search_role = "entity",
        target_scope = "table",
        target_sdp_file = "tables.csv",
        target_sdp_field = "observation_unit_iri",
        target_row_key = paste(dataset_id, table_id, sep = "/"),
        target_label = table_label,
        target_description = table_description,
        search_query = query,
        target_query_basis = query_info$target_query_basis[[1]],
        target_query_context = query_info$target_query_context[[1]],
        column_label = NA_character_,
        column_description = NA_character_,
        code_label = NA_character_,
        code_description = NA_character_
      )
    })
    targets <- dplyr::bind_rows(targets, table_targets)
  }

  if (nrow(dataset_meta) > 0) {
    dataset_targets <- purrr::map_dfr(seq_len(nrow(dataset_meta)), function(i) {
      row <- dataset_meta[i, , drop = FALSE]
      keywords <- if ("keywords" %in% names(row)) row$keywords[[1]] else NA_character_
      if (is_present(keywords)) return(tibble::tibble())

      dataset_id <- if ("dataset_id" %in% names(row)) row$dataset_id[[1]] else NA_character_
      title <- if ("title" %in% names(row)) row$title[[1]] else dataset_id
      description <- if ("description" %in% names(row)) row$description[[1]] else NA_character_
      query <- clean_query(first_non_empty(list(description, title, dataset_id)))
      if (!nzchar(query)) return(tibble::tibble())

      tibble::tibble(
        dataset_id = dataset_id,
        table_id = NA_character_,
        column_name = NA_character_,
        code_value = NA_character_,
        dictionary_role = "entity",
        search_role = "entity",
        target_scope = "dataset",
        target_sdp_file = "dataset.csv",
        target_sdp_field = "keywords",
        target_row_key = dataset_id,
        target_label = title,
        target_description = description,
        search_query = query,
        column_label = NA_character_,
        column_description = NA_character_,
        code_label = NA_character_,
        code_description = NA_character_
      )
    })
    targets <- dplyr::bind_rows(targets, dataset_targets)
  }

  missing_target_cols <- setdiff(suggestion_leading_cols, names(targets))
  if (length(missing_target_cols) > 0) {
    for (nm in missing_target_cols) {
      targets[[nm]] <- NA_character_
    }
  }

  suggestions <- purrr::map_dfr(seq_len(nrow(targets)), function(i) {
    target <- targets[i, , drop = FALSE]
    res <- .ms_retrieve_semantic_target_candidates(
      target = target,
      sources = sources,
      max_per_role = max_per_role,
      search_fn = search_fn,
      retrieval_pass = 1L
    )
    if (nrow(res) == 0) return(tibble::tibble())

    optional_cols <- intersect(
      c("score", "alignment_only", "agreement_sources", "zooma_confidence", "zooma_annotator", "role_hints", "role_hint_status", "role_hint_bonus", "role_hint_explanation"),
      names(res)
    )
    dplyr::select(
      res,
      dplyr::all_of(c(suggestion_leading_cols, "label", "iri", "source", "ontology", "role", "match_type", "definition")),
      dplyr::all_of(optional_cols),
      dplyr::everything()
    )
  })

  if (nrow(suggestions) > 0) {
    suggestions$candidate_label_norm <- tolower(trimws(suggestions$label %||% ""))
    grouped <- suggestions %>%
      dplyr::group_by(
        .data$dataset_id,
        .data$table_id,
        .data$column_name,
        .data$code_value,
        .data$target_scope,
        .data$target_sdp_file,
        .data$candidate_label_norm
      ) %>%
      dplyr::summarise(
        collision_roles = paste(sort(unique(.data$dictionary_role)), collapse = "|"),
        role_collision = all(c("variable", "property") %in% unique(.data$dictionary_role)),
        .groups = "drop"
      )
    suggestions <- suggestions %>%
      dplyr::left_join(
        grouped,
        by = c(
          "dataset_id",
          "table_id",
          "column_name",
          "code_value",
          "target_scope",
          "target_sdp_file",
          "candidate_label_norm"
        )
      ) %>%
      dplyr::mutate(
        role_collision_note = dplyr::case_when(
          .data$role_collision & .data$dictionary_role == "variable" ~ paste0(
            "Label appears for variable and property candidates; this row targets variable semantics for ",
            .data$target_sdp_field,
            "."
          ),
          .data$role_collision & .data$dictionary_role == "property" ~ paste0(
            "Label appears for variable and property candidates; this row targets property semantics for ",
            .data$target_sdp_field,
            "."
          ),
          TRUE ~ NA_character_
        )
      ) %>%
      dplyr::select(-dplyr::any_of("candidate_label_norm"))
  }

  if (isTRUE(llm_assess) && nrow(suggestions) > 0) {
    llm_results <- .ms_assess_semantic_suggestions_llm(
      suggestions,
      provider = llm_provider,
      model = llm_model,
      api_key = llm_api_key,
      base_url = llm_base_url,
      top_n = llm_top_n,
      context_files = llm_context_files,
      context_text = llm_context_text,
      timeout_seconds = llm_timeout_seconds,
      request_fn = llm_request_fn,
      search_fn = search_fn,
      sources = sources,
      max_per_role = max_per_role
    )
    suggestions <- llm_results$suggestions
    attr(dict, "semantic_llm_assessments") <- llm_results$assessments
  } else if (isTRUE(llm_assess)) {
    attr(dict, "semantic_llm_assessments") <- tibble::tibble()
  }

  attr(dict, "semantic_suggestions") <- suggestions

  if (isTRUE(include_dwc)) {
    attr(dict, "dwc_mappings") <- metasalmon::suggest_dwc_mappings(dict) |> attr("dwc_mappings")
  }

  if (nrow(suggestions) > 0) {
    cli::cli_inform("Semantic suggestions stored in attr('semantic_suggestions') for downstream review.")
  } else {
    cli::cli_inform("No semantic suggestions found for missing semantic metadata.")
  }

  dict
}

#' Apply semantic suggestions into a dictionary
#'
#' Copies selected IRIs from a `semantic_suggestions` tibble into the matching
#' dictionary fields. Suggestions remain separate by default; this helper gives
#' you an explicit merge step when you decide the top candidates are good enough.
#'
#' Matching is done by both `column_name` and `dictionary_role`. When the
#' suggestions tibble also includes `dataset_id` and `table_id`, those keys are
#' honored too. Suggestions that target non-column destinations (for example
#' `codes.csv`, `tables.csv`, or `dataset.csv`) are ignored by this helper and
#' remain review-only.
#'
#' @param dict A dictionary tibble, typically returned by [infer_dictionary()] or
#'   [suggest_semantics()].
#' @param suggestions A suggestions tibble, usually
#'   `attr(dict, "semantic_suggestions")`. If omitted, the function reads that
#'   attribute from `dict`.
#' @param strategy Selection strategy per column-role pair. `"top"` keeps the
#'   original lexical ranking; `"llm"` applies only candidates marked with
#'   `llm_selected = TRUE` by `suggest_semantics(..., llm_assess = TRUE)`.
#' @param columns Optional character vector limiting application to specific
#'   `column_name` values.
#' @param roles Optional character vector limiting application to specific
#'   suggestion roles: `"variable"`, `"property"`, `"entity"`, `"unit"`,
#'   `"constraint"`, `"method"`.
#' @param min_score Optional numeric threshold. Only available when
#'   `suggestions` includes a `score` column; otherwise the function errors.
#' @param min_llm_confidence Optional numeric threshold for `strategy = "llm"`.
#'   Requires `llm_confidence` in `suggestions`.
#' @param overwrite Logical; if `FALSE` (default), only missing fields are
#'   filled. Set `TRUE` to intentionally replace existing IRIs.
#' @param verbose Logical; if `TRUE` (default), print a short summary.
#'
#' @return The dictionary tibble with selected semantic IRI fields filled in.
#' @export
#'
#' @examples
#' \dontrun{
#' dict <- infer_dictionary(my_data, dataset_id = "example", table_id = "main")
#' dict <- suggest_semantics(my_data, dict)
#'
#' # Fill only the missing semantic fields for one measurement column
#' dict <- apply_semantic_suggestions(dict, columns = "SPAWNER_COUNT")
#'
#' # Require stronger lexical matches when score is available
#' dict <- apply_semantic_suggestions(dict, min_score = 2)
#' }
apply_semantic_suggestions <- function(dict,
                                       suggestions = attr(dict, "semantic_suggestions"),
                                       strategy = c("top", "llm"),
                                       columns = NULL,
                                       roles = NULL,
                                       min_score = NULL,
                                       min_llm_confidence = NULL,
                                       overwrite = FALSE,
                                       verbose = TRUE) {
  if (!inherits(dict, "data.frame")) {
    cli::cli_abort("{.arg dict} must be a data frame or tibble")
  }

  if (is.null(suggestions)) {
    cli::cli_abort(
      c(
        "No semantic suggestions supplied.",
        "i" = "Pass {.arg suggestions} explicitly or run {.fn suggest_semantics} first."
      )
    )
  }

  suggestions <- tibble::as_tibble(suggestions)
  if (nrow(suggestions) == 0) {
    if (isTRUE(verbose)) {
      cli::cli_inform("No semantic suggestions to apply.")
    }
    return(dict)
  }

  required_cols <- c("column_name", "dictionary_role", "iri")
  missing_cols <- setdiff(required_cols, names(suggestions))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Suggestions are missing required columns: {.field {missing_cols}}"
    )
  }

  role_to_field <- c(
    variable = "term_iri",
    property = "property_iri",
    entity = "entity_iri",
    unit = "unit_iri",
    constraint = "constraint_iri",
    method = "method_iri"
  )

  if (!is.null(roles)) {
    invalid_roles <- setdiff(roles, names(role_to_field))
    if (length(invalid_roles) > 0) {
      cli::cli_abort(
        "Unsupported {.arg roles}: {.val {invalid_roles}}. Valid roles: {.val {names(role_to_field)}}"
      )
    }
  }

  if (!is.null(min_score) && !"score" %in% names(suggestions)) {
    cli::cli_abort(
      c(
        "{.arg min_score} requires scored suggestions.",
        "i" = "Run {.fn suggest_semantics} with the default search results or pass a suggestions tibble that includes a {.field score} column."
      )
    )
  }

  strategy <- match.arg(strategy)

  if (!is.null(min_llm_confidence) && !"llm_confidence" %in% names(suggestions)) {
    cli::cli_abort(
      c(
        "{.arg min_llm_confidence} requires LLM-reviewed suggestions.",
        "i" = "Run {.fn suggest_semantics} with {.code llm_assess = TRUE} or pass a suggestions tibble that includes {.field llm_confidence}."
      )
    )
  }

  infer_term_type <- function(suggestion_row) {
    if ("term_type" %in% names(suggestion_row)) {
      candidate <- as.character(suggestion_row$term_type[[1]] %||% "")
      if (!is.na(candidate) && nzchar(trimws(candidate))) {
        return(trimws(candidate))
      }
    }
    if (identical(suggestion_row$dictionary_role[[1]], "variable")) {
      return("skos_concept")
    }
    NA_character_
  }

  out <- dict
  for (field in unique(unname(role_to_field))) {
    if (!field %in% names(out)) {
      out[[field]] <- NA_character_
    }
  }

  suggestions$.row_id <- seq_len(nrow(suggestions))
  suggestions <- suggestions[!is.na(suggestions$iri) & suggestions$iri != "", , drop = FALSE]

  if ("target_scope" %in% names(suggestions)) {
    keep <- is.na(suggestions$target_scope) | suggestions$target_scope == "column"
    dropped <- sum(!keep)
    suggestions <- suggestions[keep, , drop = FALSE]
    if (isTRUE(verbose) && dropped > 0) {
      cli::cli_inform(
        "{dropped} suggestion{?s} target non-column scopes and were left as review-only metadata."
      )
    }
  }
  if ("target_sdp_file" %in% names(suggestions)) {
    keep <- is.na(suggestions$target_sdp_file) | suggestions$target_sdp_file == "column_dictionary.csv"
    dropped <- sum(!keep)
    suggestions <- suggestions[keep, , drop = FALSE]
    if (isTRUE(verbose) && dropped > 0) {
      cli::cli_inform(
        "{dropped} suggestion{?s} target non-dictionary files and were not auto-applied."
      )
    }
  }

  if (!is.null(columns)) {
    suggestions <- suggestions[suggestions$column_name %in% columns, , drop = FALSE]
  }
  if (!is.null(roles)) {
    suggestions <- suggestions[suggestions$dictionary_role %in% roles, , drop = FALSE]
  }
  if (!is.null(min_score)) {
    suggestions <- suggestions[!is.na(suggestions$score) & suggestions$score >= min_score, , drop = FALSE]
  }
  if (identical(strategy, "llm")) {
    if (!"llm_selected" %in% names(suggestions)) {
      cli::cli_abort(
        c(
          "{.arg strategy = 'llm'} requires LLM-reviewed suggestions.",
          "i" = "Run {.fn suggest_semantics} with {.code llm_assess = TRUE} first."
        )
      )
    }
    suggestions <- suggestions[!is.na(suggestions$llm_selected) & suggestions$llm_selected, , drop = FALSE]
  }
  if (!is.null(min_llm_confidence)) {
    suggestions <- suggestions[!is.na(suggestions$llm_confidence) & suggestions$llm_confidence >= min_llm_confidence, , drop = FALSE]
  }

  suggestions <- .ms_filter_auto_apply_suggestions(out, suggestions)

  unknown_roles <- unique(suggestions$dictionary_role[!is.na(suggestions$dictionary_role) &
    !suggestions$dictionary_role %in% names(role_to_field)])
  if (length(unknown_roles) > 0) {
    cli::cli_warn("Ignoring unsupported suggestion roles: {.val {unknown_roles}}")
  }
  suggestions <- suggestions[!is.na(suggestions$dictionary_role) & suggestions$dictionary_role %in% names(role_to_field), , drop = FALSE]

  if (nrow(suggestions) == 0) {
    if (isTRUE(verbose)) {
      cli::cli_inform("No semantic suggestions met the requested filters.")
    }
    return(out)
  }

  match_keys <- c(intersect(c("dataset_id", "table_id"), names(suggestions)), "column_name", "dictionary_role")
  selected <- suggestions[order(suggestions$.row_id), , drop = FALSE]
  group_id <- do.call(
    paste,
    c(
      lapply(selected[match_keys], function(x) ifelse(is.na(x), "<NA>", as.character(x))),
      sep = "\r"
    )
  )
  selected <- selected[!duplicated(group_id), , drop = FALSE]

  dict_match_keys <- intersect(c("dataset_id", "table_id"), names(out))
  applied <- 0L
  skipped_existing <- 0L
  unmatched <- 0L

  for (i in seq_len(nrow(selected))) {
    suggestion <- selected[i, , drop = FALSE]
    role <- suggestion$dictionary_role[[1]]
    field <- role_to_field[[role]]

    matches <- out$column_name == suggestion$column_name[[1]]
    for (key in dict_match_keys) {
      if (key %in% names(suggestion)) {
        key_value <- suggestion[[key]][[1]]
        if (!is.na(key_value) && key_value != "") {
          matches <- matches & out[[key]] == key_value
        }
      }
    }

    row_ids <- which(matches)
    if (length(row_ids) == 0) {
      unmatched <- unmatched + 1L
      next
    }

    if (isTRUE(overwrite)) {
      out[[field]][row_ids] <- suggestion$iri[[1]]
      if (identical(field, "term_iri") && "term_type" %in% names(out)) {
        term_type_guess <- infer_term_type(suggestion)
        if (!is.na(term_type_guess) && nzchar(term_type_guess)) {
          missing_term_type <- is.na(out$term_type[row_ids]) | out$term_type[row_ids] == ""
          if (any(missing_term_type)) {
            out$term_type[row_ids[missing_term_type]] <- term_type_guess
          }
        }
      }
      if (identical(field, "unit_iri") && "unit_label" %in% names(out) && "label" %in% names(suggestion)) {
        missing_labels <- is.na(out$unit_label[row_ids]) | out$unit_label[row_ids] == ""
        if (any(missing_labels)) {
          out$unit_label[row_ids[missing_labels]] <- suggestion$label[[1]]
        }
      }
      applied <- applied + length(row_ids)
      next
    }

    missing_now <- is.na(out[[field]][row_ids]) | out[[field]][row_ids] == ""
    fill_rows <- row_ids[missing_now]
    if (length(fill_rows) > 0) {
      out[[field]][fill_rows] <- suggestion$iri[[1]]
      if (identical(field, "term_iri") && "term_type" %in% names(out)) {
        term_type_guess <- infer_term_type(suggestion)
        if (!is.na(term_type_guess) && nzchar(term_type_guess)) {
          missing_term_type <- is.na(out$term_type[fill_rows]) | out$term_type[fill_rows] == ""
          if (any(missing_term_type)) {
            out$term_type[fill_rows[missing_term_type]] <- term_type_guess
          }
        }
      }
      if (identical(field, "unit_iri") && "unit_label" %in% names(out) && "label" %in% names(suggestion)) {
        missing_labels <- is.na(out$unit_label[fill_rows]) | out$unit_label[fill_rows] == ""
        if (any(missing_labels)) {
          out$unit_label[fill_rows[missing_labels]] <- suggestion$label[[1]]
        }
      }
      applied <- applied + length(fill_rows)
    }

    if (identical(field, "term_iri") && "term_type" %in% names(out)) {
      term_type_guess <- infer_term_type(suggestion)
      if (!is.na(term_type_guess) && nzchar(term_type_guess)) {
        existing_rows <- row_ids[!missing_now]
        if (length(existing_rows) > 0) {
          missing_term_type <- is.na(out$term_type[existing_rows]) | out$term_type[existing_rows] == ""
          if (any(missing_term_type)) {
            out$term_type[existing_rows[missing_term_type]] <- term_type_guess
          }
        }
      }
    }
    skipped_existing <- skipped_existing + sum(!missing_now)
  }

  if (isTRUE(verbose)) {
    msg <- c(
      "Applied {.val {applied}} semantic suggestion field{?s} using the {.val {strategy}} strategy."
    )
    if (!overwrite && skipped_existing > 0) {
      msg <- c(
        msg,
        "i" = paste0(
          skipped_existing,
          " field",
          if (skipped_existing == 1) " was" else "s were",
          " left alone because the dictionary already had an IRI. Use overwrite = TRUE to replace them."
        )
      )
    }
    if (unmatched > 0) {
      msg <- c(
        msg,
        "i" = paste0(
          unmatched,
          " suggestion",
          if (unmatched == 1) " did" else "s did",
          " not match any dictionary row."
        )
      )
    }
    cli::cli_inform(msg)
  }

  out
}
