#' Suggest Darwin Core Data Package mappings for dictionary columns
#'
#' Uses DwC Conceptual Model + DwC-DP table schemas (cached locally) to suggest
#' likely table/field mappings for column dictionary entries, and returns the
#' associated Darwin Core property IRIs for review.
#'
#' @param dict A dictionary tibble with `column_name`, and optionally
#'   `column_label` and `column_description`.
#' @param max_per_column Maximum number of mapping suggestions per column.
#'
#' @return The dictionary tibble with a `dwc_mappings` attribute containing
#'   suggestions (columns: `column_name`, `table_id`, `field_name`, `field_label`,
#'   `term_iri`, `match_score`, `match_basis`).
#' @export
#'
#' @examples
#' \dontrun{
#' dict <- infer_dictionary(mtcars)
#' dict <- suggest_dwc_mappings(dict)
#' attr(dict, "dwc_mappings") %>% head()
#' }
suggest_dwc_mappings <- function(dict, max_per_column = 3) {
  if (!inherits(dict, "data.frame")) {
    cli::cli_abort("{.arg dict} must be a data frame or tibble")
  }
  if (!"column_name" %in% names(dict)) {
    cli::cli_abort("{.arg dict} must contain {.field column_name}")
  }

  fields <- .dwc_dp_fields()
  if (nrow(fields) == 0 || nrow(dict) == 0) {
    attr(dict, "dwc_mappings") <- tibble::tibble()
    return(dict)
  }

  first_non_empty <- function(values) {
    values <- values[!vapply(values, function(x) is.null(x) || is.na(x) || x == "", logical(1))]
    if (length(values) == 0) "" else values[[1]]
  }

  suggestions <- purrr::map_dfr(seq_len(nrow(dict)), function(i) {
    row <- dict[i, , drop = TRUE]
    query <- first_non_empty(list(row$column_description, row$column_label, row$column_name))
    query <- .dwc_clean_text(query)
    if (!nzchar(query)) {
      return(tibble::tibble())
    }

    scored <- .dwc_score_fields(query, fields)
    if (nrow(scored) == 0) {
      return(tibble::tibble())
    }

    scored <- scored %>%
      dplyr::arrange(dplyr::desc(.data$match_score), .data$table_id, .data$field_name) %>%
      dplyr::slice_head(n = max_per_column) %>%
      dplyr::mutate(column_name = row$column_name) %>%
      dplyr::select(
        column_name,
        table_id,
        field_name,
        field_label,
        term_iri,
        match_score,
        match_basis
      )

    scored
  })

  attr(dict, "dwc_mappings") <- suggestions

  if (nrow(suggestions) > 0) {
    cli::cli_inform("DwC-DP mapping suggestions added (attr 'dwc_mappings'); no fields were auto-filled.")
  } else {
    cli::cli_inform("No DwC-DP mapping suggestions found.")
  }

  dict
}

.dwc_dp_fields <- function() {
  path <- system.file("extdata", "dwc-dp-fields.csv", package = "metasalmon", mustWork = FALSE)
  if (!file.exists(path) || path == "") {
    return(tibble::tibble(
      table_id = character(),
      table_label = character(),
      table_iri = character(),
      field_name = character(),
      field_label = character(),
      field_description = character(),
      term_iri = character(),
      term_namespace = character()
    ))
  }
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

.dwc_clean_text <- function(x) {
  x <- gsub("[._]+", " ", tolower(x %||% ""))
  x <- gsub("[^a-z0-9\\s]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

.dwc_score_fields <- function(query, fields) {
  if (!nzchar(query)) {
    return(tibble::tibble())
  }
  query_tokens <- unique(strsplit(query, "\\s+")[[1]])
  query_tokens <- query_tokens[nzchar(query_tokens)]
  if (length(query_tokens) == 0) {
    return(tibble::tibble())
  }

  name_clean <- .dwc_clean_text(fields$field_name)
  label_clean <- .dwc_clean_text(fields$field_label)

  exact_name <- name_clean == query
  exact_label <- label_clean == query
  substring <- grepl(query, name_clean, fixed = TRUE) |
    grepl(query, label_clean, fixed = TRUE)

  token_overlap <- vapply(seq_len(nrow(fields)), function(i) {
    tokens <- unique(strsplit(label_clean[i], "\\s+")[[1]])
    tokens <- tokens[nzchar(tokens)]
    if (length(tokens) == 0) return(0)
    length(intersect(query_tokens, tokens)) / length(unique(c(query_tokens, tokens)))
  }, numeric(1))

  distance_name <- as.numeric(utils::adist(query, name_clean))
  distance_label <- as.numeric(utils::adist(query, label_clean))
  fuzzy <- pmin(distance_name, distance_label, na.rm = TRUE)

  score <- (exact_name * 3) + (exact_label * 2) + (substring * 1.5) + (token_overlap * 1.2)
  score <- score + ifelse(fuzzy <= 2, 1, ifelse(fuzzy <= 4, 0.5, 0))

  basis <- vapply(seq_len(nrow(fields)), function(i) {
    tags <- character()
    if (exact_name[i]) tags <- c(tags, "exact_name")
    if (exact_label[i]) tags <- c(tags, "exact_label")
    if (substring[i]) tags <- c(tags, "substring")
    if (token_overlap[i] > 0) tags <- c(tags, "token_overlap")
    if (fuzzy[i] <= 4) tags <- c(tags, "fuzzy")
    if (length(tags) == 0) "none" else paste(tags, collapse = "|")
  }, character(1))

  fields %>%
    dplyr::mutate(
      match_score = score,
      match_basis = basis
    ) %>%
    dplyr::filter(.data$match_score > 0)
}

utils::globalVariables(c(
  "table_id",
  "field_name",
  "field_label",
  "term_iri",
  "match_score",
  "match_basis"
))
