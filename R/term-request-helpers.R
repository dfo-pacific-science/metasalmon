 .term_request_default_template <- "https://github.com/dfo-pacific-science/dfo-salmon-ontology/blob/main/.github/ISSUE_TEMPLATE/new-term-request.md"

#' Detect missing semantic terms that are not covered by SMN
#'
#' Given semantic suggestions (typically attached to a dictionary as
#' `semantic_suggestions`), this function summarizes candidate fields that appear to
#' need ontology support but do not have a direct `smn` match.
#'
#' It is designed to support a practical workflow:
#'
#' 1. generate semantic suggestions with `suggest_semantics()`;
#' 2. detect unresolved gaps with `detect_semantic_term_gaps()`;
#' 3. render request payloads with `render_ontology_term_request()`;
#' 4. optionally submit issues with `submit_term_request_issues()`.
#'
#' @param dict A dictionary tibble. Used only when `suggestions` is `NULL`.
#' @param suggestions Optional semantic suggestion table. If omitted, this function
#'   uses `attr(dict, "semantic_suggestions")`.
#' @param include_target_scopes Target scopes to inspect. Defaults to all supported
#'   scopes.
#' @param include_dictionary_roles Optional vector of dictionary roles to restrict
#'   the gap scan (for example `c("variable", "property", "entity")`).
#' @param min_score Optional minimum score filter. Rows with score below this value
#'   are ignored when score is available.
#'
#' @return A tibble with one row per target that has no SMN match.
#'   Key columns:
#'   - `dataset_id`, `table_id`, `column_name`, `target_scope`,
#'     `target_sdp_file`, `target_sdp_field`, `target_row_key`, `dictionary_role`;
#'   - `search_query` text used for lookup;
#'   - `top_non_smn_source`, `top_non_smn_label`, `top_non_smn_iri`,
#'     `top_non_smn_score`;
#'   - `non_smn_sources`, `candidate_count`, `placement_recommendation`,
#'     `placement_confidence`, `placement_rationale`.
#'
#' @seealso [render_ontology_term_request()], [submit_term_request_issues()],
#'   [suggest_semantics()]
#'
#' @export
#'
#' @examples
#' suggestions <- tibble::tibble(
#'   dataset_id = c("d1", "d1"),
#'   table_id = c("t1", "t1"),
#'   column_name = c("run_id", "run_id"),
#'   code_value = NA_character_,
#'   column_label = c("Run ID", "Run ID"),
#'   column_description = "Run identifier from local monitoring pipeline",
#'   dictionary_role = c("variable", "variable"),
#'   target_scope = c("column", "column"),
#'   target_sdp_file = c("column_dictionary.csv", "column_dictionary.csv"),
#'   target_sdp_field = c("term_iri", "term_iri"),
#'   target_row_key = c("run_id", "run_id"),
#'   search_query = c("run_id", "run_id"),
#'   label = c("Run ID", "Run ID"),
#'   iri = c(NA_character_, NA_character_),
#'   source = c("gbif", "worms"),
#'   ontology = c("gbif", "worms"),
#'   match_type = c("label", "label"),
#'   definition = NA_character_,
#'   score = c(0.9, 0.85)
#' )
#' gaps <- detect_semantic_term_gaps(
#'   suggestions = suggestions,
#'   include_dictionary_roles = "variable"
#' )
#' gaps
#'
#'
#' @export
detect_semantic_term_gaps <- function(
    dict = NULL,
    suggestions = NULL,
    include_target_scopes = c("column", "code", "table", "dataset"),
    include_dictionary_roles = NULL,
    min_score = NA_real_
) {
  if (is.null(suggestions)) {
    if (is.null(dict)) {
      cli::cli_abort("Provide either `dict` with `semantic_suggestions` or `suggestions`.")
    }
    suggestions <- attr(dict, "semantic_suggestions")
  }

  if (is.null(suggestions) || length(suggestions) == 0) {
    return(.empty_term_gap_result())
  }

  suggestions <- as.data.frame(suggestions, stringsAsFactors = FALSE)

  required <- c(
    "dataset_id",
    "table_id",
    "column_name",
    "code_value",
    "dictionary_role",
    "target_scope",
    "target_sdp_file",
    "target_sdp_field",
    "target_row_key",
    "search_query",
    "column_label",
    "column_description",
    "source",
    "label",
    "iri",
    "ontology",
    "match_type",
    "definition"
  )

  missing <- setdiff(required, names(suggestions))
  if (length(missing) > 0L) {
    cli::cli_abort("Missing required suggestion columns: {paste(missing, collapse = ', ')}")
  }

  suggestions$target_scope <- tolower(trimws(as.character(suggestions$target_scope)))
  include_target_scopes <- tolower(trimws(as.character(include_target_scopes)))

  suggestions <- suggestions[suggestions$target_scope %in% include_target_scopes, , drop = FALSE]
  if (!is.null(include_dictionary_roles)) {
    suggestions <- suggestions[suggestions$dictionary_role %in% include_dictionary_roles, , drop = FALSE]
  }

  if (nrow(suggestions) == 0L) {
    return(.empty_term_gap_result())
  }

  suggestions$source <- tolower(trimws(as.character(suggestions$source)))
  suggestions$score <- suppressWarnings(as.numeric(suggestions$score))
  if (!is.na(min_score)) {
    keep <- is.na(suggestions$score) | suggestions$score >= min_score
    suggestions <- suggestions[keep, , drop = FALSE]
  }

  if (nrow(suggestions) == 0L) {
    return(.empty_term_gap_result())
  }

  suggestions$is_smn <- vapply(seq_len(nrow(suggestions)), function(i) {
    source <- suggestions$source[[i]]
    iri <- suggestions$iri[[i]]
    if (!is.character(iri) || !nzchar(iri)) {
      return(FALSE)
    }
    source == "smn" || grepl("^https?://w3id\\.org/smn/", iri, ignore.case = TRUE)
  }, logical(1))

  base_cols <- c("dataset_id", "table_id", "column_name", "code_value", "target_scope", "target_sdp_file", "target_sdp_field", "target_row_key", "dictionary_role")

  gaps <- do.call(rbind, lapply(split(suggestions, do.call(paste, c(suggestions[base_cols], sep = "::"))), function(group) {
    group <- as.data.frame(group, stringsAsFactors = FALSE)

    if (any(group$is_smn, na.rm = TRUE)) {
      return(NULL)
    }

    non_smn <- group[!group$is_smn, , drop = FALSE]
    if (nrow(non_smn) == 0L) {
      return(NULL)
    }

    top <- non_smn[order(-non_smn$score, non_smn$source, non_smn$label, na.last = TRUE), , drop = FALSE]
    top <- top[1, , drop = FALSE]

    # Handle missing/empty values robustly
    top_score <- suppressWarnings(as.numeric(top$score[1]))
    if (is.na(top_score)) top_score <- NA_real_

    local_term_hint <- .has_local_term_signals(top$search_query[1], top$dictionary_role[1], top$source)
    source_vec <- .unique_char(.trim_empties(as.character(non_smn$source)))
    recommendation <- .recommend_term_placement(
      search_query = top$search_query[1],
      dictionary_role = top$dictionary_role[1],
      sources = source_vec,
      local_hint = local_term_hint
    )

    key <- group[1, base_cols, drop = FALSE]
    data.frame(
      dataset_id = key$dataset_id,
      table_id = key$table_id,
      column_name = key$column_name,
      code_value = key$code_value,
      target_scope = key$target_scope,
      target_sdp_file = key$target_sdp_file,
      target_sdp_field = key$target_sdp_field,
      target_row_key = key$target_row_key,
      dictionary_role = key$dictionary_role,
      search_query = .first_non_empty(key$search_query),
      column_label = .first_non_empty(key$column_label),
      column_description = .first_non_empty(key$column_description),
      top_non_smn_source = top$source,
      top_non_smn_label = top$label,
      top_non_smn_iri = .first_non_empty(top$iri),
      top_non_smn_ontology = .first_non_empty(top$ontology),
      top_non_smn_match_type = .first_non_empty(top$match_type),
      top_non_smn_score = top_score,
      candidate_count = nrow(non_smn),
      non_smn_sources = paste(source_vec, collapse = ", "),
      placement_recommendation = recommendation$placement,
      placement_confidence = recommendation$confidence,
      placement_rationale = recommendation$rationale,
      stringsAsFactors = FALSE
    )
  }))

  if (is.null(gaps)) {
    return(.empty_term_gap_result())
  }

  gaps <- gaps[order(gaps$placement_confidence, decreasing = TRUE), , drop = FALSE]
  tibble::as_tibble(gaps)
}

#' Render GitHub-ready ontology term request payloads
#'
#' Convert gap candidates into request payload rows (title/body) suitable for
#' creating GitHub issues against the DFO Salmon Ontology repository.
#'
#' For interactive workflows this function can prompt users row-by-row for whether a
#' gap should be requested as a shared SMN term, a profile-specific term, or skipped.
#'
#' @param gaps Output from `detect_semantic_term_gaps()`.
#' @param scope One of `"auto"`, `"smn"`, or `"profile"`.
#'   - `"auto"`: honor `placement_recommendation` and ask for uncertainty
#'   - `"smn"`: route all requests to shared SMN
#'   - `"profile"`: route all requests to a profile
#' @param ask If `TRUE`, unresolved rows are asked interactively.
#' @param profile_name If routing to profiles, provide a default profile name.
#' @param scope_overrides Optional per-row scope overrides (`"smn"`, `"profile"`,
#'   `"skip"`). Useful in non-interactive pipelines.
#' @param issue_labels Optional labels to include on created GitHub issues.
#' @param term_request_template URL for the target issue template.
#' @param ontology_repo Repository slug to target when submitting issues.
#'
#' @return A tibble with one row per rendered request payload. Rows with
#'   `request_scope == "skip"` are retained and can be filtered before
#'   submission.
#'
#' @seealso [detect_semantic_term_gaps()], [submit_term_request_issues()],
#'   [validate_semantics()]
#'
#' @export
#'
#' @examples
#' gap <- dplyr::tibble(
#'   dataset_id = "d1",
#'   table_id = "t1",
#'   column_name = "run_id",
#'   code_value = NA_character_,
#'   target_scope = "column",
#'   target_sdp_file = "column_dictionary.csv",
#'   target_sdp_field = "term_iri",
#'   target_row_key = "run_id",
#'   dictionary_role = "variable",
#'   search_query = "run id",
#'   column_label = "Run ID",
#'   column_description = "Dataset-specific run identifier",
#'   top_non_smn_source = "gbif",
#'   top_non_smn_label = "Run event id",
#'   top_non_smn_iri = NA_character_,
#'   top_non_smn_ontology = NA_character_,
#'   top_non_smn_match_type = "label",
#'   top_non_smn_score = 0.9,
#'   candidate_count = 2,
#'   non_smn_sources = "gbif, worms",
#'   placement_recommendation = "profile",
#'   placement_confidence = 0.82,
#'   placement_rationale = "Contains internal identifier patterns."
#' )
#'
#' render_ontology_term_request(
#'   gap,
#'   scope = "auto",
#'   ask = FALSE,
#'   profile_name = "pacific-monitoring"
#' )
#'

render_ontology_term_request <- function(
    gaps,
    scope = c("auto", "smn", "profile"),
    ask = interactive(),
    profile_name = NULL,
    scope_overrides = NULL,
    issue_labels = NULL,
    term_request_template = .term_request_default_template,
    ontology_repo = "dfo-pacific-science/dfo-salmon-ontology"
) {
  scope <- match.arg(scope)
  gaps <- as.data.frame(gaps, stringsAsFactors = FALSE)

  if (nrow(gaps) == 0L) {
    return(tibble::tibble())
  }

  required <- c(
    "dataset_id", "table_id", "column_name", "target_scope", "target_sdp_file",
    "target_sdp_field", "target_row_key", "dictionary_role", "search_query",
    "column_label", "column_description", "top_non_smn_source",
    "top_non_smn_label", "top_non_smn_iri", "top_non_smn_ontology",
    "placement_recommendation"
  )
  missing <- setdiff(required, names(gaps))
  if (length(missing) > 0L) {
    cli::cli_abort("Missing required gap columns: {paste(missing, collapse = ', ')}")
  }

  gaps <- as.data.frame(gaps, stringsAsFactors = FALSE)
  gaps$request_scope <- if (scope == "auto") {
    tolower(trimws(as.character(gaps$placement_recommendation)))
  } else {
    rep(scope, nrow(gaps))
  }

  if (!is.null(scope_overrides)) {
    scope_overrides <- tolower(trimws(as.character(scope_overrides)))
    if (length(scope_overrides) == 1L) {
      gaps$request_scope <- scope_overrides
    } else {
      if (length(scope_overrides) != nrow(gaps)) {
        cli::cli_abort("`scope_overrides` must be length 1 or nrow(gaps).")
      }
      gaps$request_scope <- scope_overrides
    }
  }

  gaps$request_scope[is.na(gaps$request_scope) | !nzchar(gaps$request_scope)] <- "uncertain"

  # Interactive follow-up only for rows that need judgement.
  if (ask) {
    unresolved <- which(gaps$request_scope %in% c("auto", "uncertain", "") )
    for (i in unresolved) {
      term_label <- .first_non_empty(c(gaps$top_non_smn_label[[i]], gaps$search_query[[i]], gaps$column_name[[i]], "Unnamed term"))
      cli::cli_h2("Term gap review for {.val {term_label}}")
      cli::cat_line(
        sprintf(
          "Target: dataset=%s table=%s scope=%s field=%s role=%s",
          gaps$dataset_id[[i]], gaps$table_id[[i]], gaps$target_scope[[i]],
          gaps$target_sdp_field[[i]], gaps$dictionary_role[[i]]
        )
      )
      if (nzchar(.first_non_empty(gaps$top_non_smn_source[i], ""))) {
        cli::cat_line(
          glue::glue("Candidate: {.val {gaps$top_non_smn_label[i]}} ({gaps$top_non_smn_source[i]})")
        )
      }
      if (nzchar(.first_non_empty(gaps$placement_rationale[i], ""))) {
        cli::cat_line(glue::glue("Why: {.val {gaps$placement_rationale[i]}}"))
      }
      pick <- utils::menu(
        c(
          "Request in shared SMN",
          "Request in local/program/organization profile",
          "Skip for now"
        ),
        title = "How should this term request be routed?"
      )
      gaps$request_scope[i] <- c("smn", "profile", "skip")[pick]
      if (is.na(gaps$request_scope[i])) {
        gaps$request_scope[i] <- "skip"
      }

      if (gaps$request_scope[i] == "profile" && is.null(profile_name)) {
        profile_candidate <- trimws(readline(prompt = "Profile name (example: pacific-monitoring): "))
        if (!nzchar(profile_candidate)) {
          cli::cli_warn("No profile name supplied; skipping this request.")
          gaps$request_scope[i] <- "skip"
        } else {
          profile_name <- profile_candidate
        }
      }
      gaps$profile_name[i] <- if (gaps$request_scope[i] == "profile") profile_name else NA_character_
    }
  } else {
    gaps$profile_name <- NA_character_
    if (scope == "profile" && is.null(profile_name)) {
      gaps$request_scope[gaps$request_scope == "profile"] <- "skip"
      cli::cli_abort("profile_name is required when scope = 'profile' and ask = FALSE.")
    }
    if (is.null(profile_name)) {
      profile_name <- "project-profile"
    }
    gaps$profile_name[gaps$request_scope == "profile"] <- profile_name
  }

  gaps$request_scope <- ifelse(gaps$request_scope %in% c("smn", "profile"), gaps$request_scope, "skip")

  gaps$request_title <- vapply(seq_len(nrow(gaps)), function(i) {
    term_label <- .first_non_empty(c(gaps$top_non_smn_label[[i]], gaps$search_query[[i]], gaps$column_name[[i]], "Unnamed term"))
    if (gaps$request_scope[[i]] == "smn") {
      sprintf("Request new shared SMN term: %s", term_label)
    } else if (gaps$request_scope[[i]] == "profile") {
      profile_label <- .first_non_empty(gaps$profile_name[[i]], "project-profile")
      sprintf("Request new %s profile term: %s", profile_label, term_label)
    } else {
      sprintf("Skip term request: %s", term_label)
    }
  }, character(1), USE.NAMES = FALSE)

  gaps$request_body <- vapply(seq_len(nrow(gaps)), function(i) {
    term_label <- .first_non_empty(c(gaps$top_non_smn_label[[i]], gaps$search_query[[i]], gaps$column_name[[i]], "Unnamed term"))
    query_text <- .first_non_empty(c(gaps$search_query[[i]], gaps$column_name[[i]], term_label))
    source <- .first_non_empty(gaps$top_non_smn_source[[i]], "unknown")
    iri <- .first_non_empty(gaps$top_non_smn_iri[[i]], "Not found")
    ont <- .first_non_empty(gaps$top_non_smn_ontology[[i]], "unknown")
    description <- .first_non_empty(gaps$column_description[[i]], "No additional description captured.")
    rationale <- .first_non_empty(gaps$placement_rationale[[i]], "No rationale computed yet.")
    profile_name <- .first_non_empty(gaps$profile_name[[i]], "")
    target <- paste("dataset", gaps$dataset_id[[i]], "table", gaps$table_id[[i]], "role", gaps$dictionary_role[[i]])

    scope <- gaps$request_scope[[i]]
    if (scope == "profile") {
      scope_block <- sprintf("Profile: `%s` (default location for this domain term)", profile_name)
    } else if (scope == "smn") {
      scope_block <- "Shared vocabulary candidate for `smn` (reusable across salmon programs and organizations)"
    } else {
      scope_block <- "Skipped at this stage"
    }

    glue::glue(
      "## Proposed ontology term request\n\n",
      "**Target term (dataset query):** `{query_text}`\n\n",
      "## Context\n",
      "- Dataset: `{gaps$dataset_id[[i]]}`\n",
      "- Table: `{gaps$table_id[[i]]}`\n",
      "- Target role: `{gaps$dictionary_role[[i]]}`\n",
      "- Target field: `{gaps$target_sdp_field[[i]]}` in `{gaps$target_sdp_file[[i]]}`\n",
      "- Column/table context: `{target}`\n",
      "\n## Why this is currently missing from SMN\n",
      "{rationale}\n",
      "\n## Best matching candidate outside SMN\n",
      "- Label: `{term_label}`\n",
      "- IRI (if any): `{iri}`\n",
      "- Source: `{source}`\n",
      "- Ontology: `{ont}`\n",
      "\n## Suggested definition\n",
      "{description}\n",
      "\n## Placement for governance\n",
      "{scope_block}\n",
      "\n## Helpful links\n",
      "- New term template: {term_request_template}\n",
      "- Ontology repo: https://github.com/dfo-pacific-science/dfo-salmon-ontology\n",
      "- Shared domain conventions: https://github.com/dfo-pacific-science/dfo-salmon-ontology/blob/main/README.md\n"
    )
  }, character(1), USE.NAMES = FALSE)

  if (is.null(issue_labels)) {
    issue_labels <- as.list(rep(list(NULL), nrow(gaps)))
  }
  if (!is.list(issue_labels)) {
    issue_labels <- as.list(rep(list(as.character(issue_labels)), nrow(gaps)))
  } else if (length(issue_labels) == 0L) {
    issue_labels <- as.list(rep(list(NULL), nrow(gaps)))
  }

  # Normalize list of labels
  issue_labels <- lapply(seq_len(nrow(gaps)), function(i) {
    if (length(issue_labels) == 1L) issue_labels[[1L]] else issue_labels[[i]]
  })
  issue_labels <- lapply(issue_labels, function(x) {
    if (is.null(x) || length(x) == 0L) {
      return(NULL)
    }
    x <- .trim_empties(as.character(x))
    if (length(x) == 0L) return(NULL)
    unique(x)
  })

  out <- tibble::as_tibble(gaps)
  out$request_scope <- .trim_empties(out$request_scope)
  out$ontology_repo <- ontology_repo
  out$issue_labels <- issue_labels
  out
}

#' Submit rendered ontology term requests as GitHub issues
#'
#' Push term request payloads generated by
#' [render_ontology_term_request()] to the ontology repository.
#'
#' In normal development, keep `dry_run = TRUE` while reviewing; set it to
#' `FALSE` only after the request payloads look correct.
#'
#' @param requests Output from `render_ontology_term_request()`.
#' @param repo Repository slug, defaulting to the DFO salmon ontology.
#' @param token Optional GitHub PAT. If `NULL`, inferred from `gh` credentials.
#' @param dry_run If `TRUE`, returns a preview payload and does not call GitHub.
#' @param confirm If `TRUE`, prompts before posting each issue.
#'
#' @return A tibble summarizing request outcomes with either a dry-run preview or
#'   GitHub API response fields (`issue_number`, `issue_url`).
#'
#' @seealso [render_ontology_term_request()], [ms_setup_github()]
#'
#' @importFrom utils askYesNo
#' @export
#'
#' @examples
#' sample_gap <- dplyr::tibble(
#'   dataset_id = "d1",
#'   table_id = "t1",
#'   column_name = "run_id",
#'   code_value = NA_character_,
#'   target_scope = "column",
#'   target_sdp_file = "column_dictionary.csv",
#'   target_sdp_field = "term_iri",
#'   target_row_key = "run_id",
#'   dictionary_role = "variable",
#'   search_query = "run id",
#'   column_label = "Run ID",
#'   column_description = "Dataset-specific run identifier",
#'   top_non_smn_source = "gbif",
#'   top_non_smn_label = "Run event id",
#'   top_non_smn_iri = NA_character_,
#'   top_non_smn_ontology = NA_character_,
#'   top_non_smn_match_type = "label",
#'   top_non_smn_score = 0.9,
#'   candidate_count = 1L,
#'   non_smn_sources = "gbif",
#'   placement_recommendation = "profile",
#'   placement_confidence = 0.82,
#'   placement_rationale = "Local identifier-like term for workflow tracking."
#' )
#' reqs <- render_ontology_term_request(
#'   sample_gap,
#'   scope = "auto",
#'   ask = FALSE,
#'   profile_name = "local-program"
#' )
#' submit_term_request_issues(reqs, dry_run = TRUE)
#'
submit_term_request_issues <- function(
    requests,
    repo = "dfo-pacific-science/dfo-salmon-ontology",
    token = NULL,
    dry_run = TRUE,
    confirm = interactive()
) {
  requests <- as.data.frame(requests, stringsAsFactors = FALSE)

  if (nrow(requests) == 0L) {
    return(tibble::tibble())
  }

  required <- c("request_title", "request_body", "request_scope", "ontology_repo")
  missing <- setdiff(required, names(requests))
  if (length(missing) > 0L) {
    cli::cli_abort("Missing required request columns: {paste(missing, collapse = ', ')}")
  }

  repo <- ms_normalize_repo(ifelse("ontology_repo" %in% names(requests), requests$ontology_repo[[1]], repo))

  pending <- requests[requests$request_scope %in% c("smn", "profile"), , drop = FALSE]
  if (nrow(pending) == 0L) {
    return(tibble::tibble())
  }

  if (dry_run) {
    return(tibble::tibble(
      request_title = pending$request_title,
      request_body = pending$request_body,
      request_scope = pending$request_scope,
      issue_number = NA_integer_,
      issue_url = NA_character_,
      status = rep("dry_run", nrow(pending))
    ))
  }

  if (is.null(token)) {
    token <- ms_current_token()
  }
  if (!nzchar(token)) {
    cli::cli_abort("No GitHub token available. Run {.code metasalmon::ms_setup_github()} first or pass `token`.")
  }

  out <- list()
  for (i in seq_len(nrow(pending))) {
    scope <- pending$request_scope[[i]]
    title <- pending$request_title[[i]]
    body <- pending$request_body[[i]]
    lbls <- if ("issue_labels" %in% names(pending)) pending$issue_labels[[i]] else NULL

    if (is.list(lbls) && length(lbls) == 0L) {
      lbls <- NULL
    }

    if (isTRUE(confirm) && isFALSE(askYesNo(sprintf("Submit %s request: %s?", scope, title), default = FALSE))) {
      out[[i]] <- list(
        request_title = title,
        status = "skipped",
        issue_number = NA_integer_,
        issue_url = NA_character_
      )
      next
    }

    resp <- .metasalmon_post_issue(
      repo = repo,
      title = title,
      body = body,
      labels = lbls,
      token = token
    )

    out[[i]] <- list(
      request_title = title,
      status = "submitted",
      issue_number = if (!is.null(resp$number)) resp$number else NA_integer_,
      issue_url = if (!is.null(resp$html_url)) resp$html_url else NA_character_
    )
  }

  dplyr::bind_rows(out)
}


.empty_term_gap_result <- function() {
  tibble::tibble(
    dataset_id = character(),
    table_id = character(),
    column_name = character(),
    code_value = character(),
    target_scope = character(),
    target_sdp_file = character(),
    target_sdp_field = character(),
    target_row_key = character(),
    dictionary_role = character(),
    search_query = character(),
    column_label = character(),
    column_description = character(),
    top_non_smn_source = character(),
    top_non_smn_label = character(),
    top_non_smn_iri = character(),
    top_non_smn_ontology = character(),
    top_non_smn_match_type = character(),
    top_non_smn_score = numeric(),
    candidate_count = integer(),
    non_smn_sources = character(),
    placement_recommendation = character(),
    placement_confidence = numeric(),
    placement_rationale = character()
  )
}

# Internal helper functions (not exported)

.has_local_term_signals <- function(query, dictionary_role, sources) {
  if (is.null(query)) return(FALSE)
  q <- tolower(paste(query, collapse = " "))
  if (!nzchar(q)) return(FALSE)

  local_patterns <- c(
    "id", "ids", "code", "codes", "flag", "status", "project", "program",
    "site", "station", "trip", "haul", "vessel", "fleet", "qc", "qaqc",
    "sample", "event", "group", "run", "permit", "operator", "file"
  )
  local_hits <- vapply(local_patterns, function(p) {
    grepl(paste0("\\b", p, "\\b"), q)
  }, logical(1))

  if (any(local_hits, na.rm = TRUE)) return(TRUE)
  if (tolower(dictionary_role) %in% c("unit", "constraint", "method") && length(sources) > 0L) return(TRUE)
  FALSE
}

.recommend_term_placement <- function(search_query, dictionary_role, sources, local_hint = FALSE) {
  sources <- tolower(.trim_empties(as.character(sources)))
  score_smn <- 0
  score_profile <- 0

  if (any(sources %in% c("smn", "gcdfo"))) score_smn <- score_smn + 2
  if (any(sources %in% c("ols", "nvs", "qudt"))) score_smn <- score_smn + 0.7
  if (any(sources %in% c("gbif", "worms", "bioportal", "zooma"))) score_profile <- score_profile + 0.6

  if (local_hint) score_profile <- score_profile + 1

  # Mildly conservative: variable/measurement semantics that match broad roles trend SMN
  if (dictionary_role %in% c("variable", "property", "entity", "constraint") && length(sources) > 0L) {
    score_smn <- score_smn + 0.4
  }

  if (score_profile >= score_smn + 0.8) {
    placement <- "profile"
  } else if (score_smn >= score_profile + 0.8) {
    placement <- "smn"
  } else {
    placement <- "uncertain"
  }

  gap <- max(abs(score_smn - score_profile), 0)
  confidence <- min(0.95, 0.35 + (gap / 4))

  rationale <- sprintf(
    "Signals: sources={%s}, local_pattern=%s, role=%s -> suggest '%s'",
    paste(sources, collapse = ","),
    ifelse(local_hint, "TRUE", "FALSE"),
    dictionary_role,
    placement
  )

  list(placement = placement, confidence = confidence, rationale = rationale)
}

.trim_empties <- function(x) {
  x <- as.character(x)
  x[!nzchar(x)] <- NA_character_
  x
}

.first_non_empty <- function(x, default = NA_character_) {
  for (i in seq_along(x)) {
    if (nzchar(x[[i]])) {
      return(x[[i]])
    }
  }  
  default
}

.unique_char <- function(x) {
  unique(x[!is.na(x) & nzchar(x)])
}

.metasalmon_post_issue <- function(repo, title, body, labels = NULL, token) {
  endpoint <- sprintf("/repos/%s/issues", repo)
  payload <- list(title = title, body = body)
  if (!is.null(labels) && length(labels) > 0L) {
    payload$labels <- labels
  }
  do.call(gh::gh, c(list(endpoint, .token = token), payload))
}
