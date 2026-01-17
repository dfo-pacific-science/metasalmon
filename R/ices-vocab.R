#' ICES controlled vocabularies (code lists)
#'
#' ICES publishes controlled vocabularies (also called **code lists**: tables of
#' allowed values like "Gear" codes) via a public REST API.
#'
#' These are not OWL ontologies; use them for categorical fields and reporting
#' codes, not for ontology IRIs.
#'
#' @seealso ICES vocab API docs: <https://vocab.ices.dk/services/api/swagger/index.html>
#'
#' @name ices_vocab
NULL

.ices_base_url <- "https://vocab.ices.dk/services/api"

.ices_empty <- function() {
  tibble::tibble()
}

#' List ICES code types
#'
#' @param code_type Optional code type key or GUID to filter the API response.
#' @param code_type_id Optional numeric code type id to filter the API response.
#' @param modified Optional date string (`"YYYY-MM-DD"`) to return code types
#'   modified after that date.
#'
#' @return Tibble of ICES code types (includes `key`, `description`, `guid`, etc.).
#' @export
ices_code_types <- function(code_type = "",
                            code_type_id = 0L,
                            modified = "") {
  url <- paste0(.ices_base_url, "/CodeType")
  query <- list()
  if (!is.null(code_type) && nzchar(code_type)) query$codeType <- code_type
  if (!is.null(code_type_id) && !is.na(code_type_id) && code_type_id != 0) query$codeTypeID <- code_type_id
  if (!is.null(modified) && nzchar(modified)) query$modified <- modified
  if (length(query) > 0) url <- httr::modify_url(url, query = query)

  data <- .safe_json(url, headers = c(Accept = "application/json"))
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) return(.ices_empty())
  tibble::as_tibble(data)
}

#' List ICES codes for a code type
#'
#' @param code_type ICES code type key or GUID (e.g., `"Gear"`).
#' @param code Optional code key or GUID to filter the API response.
#' @param modified Optional date string (`"YYYY-MM-DD"`) to return codes modified
#'   after that date.
#'
#' @return Tibble of ICES codes for the requested code type. Adds a `code_type`
#'   column and a `url` column pointing at the corresponding `CodeDetail` API endpoint.
#' @export
ices_codes <- function(code_type,
                       code = "",
                       modified = "") {
  if (is.null(code_type) || !nzchar(code_type)) {
    cli::cli_abort("{.arg code_type} must be a non-empty ICES code type key (e.g., {.code Gear}).")
  }
  url <- paste0(.ices_base_url, "/Code/", utils::URLencode(code_type, reserved = TRUE))
  query <- list()
  if (!is.null(code) && nzchar(code)) query$code <- code
  if (!is.null(modified) && nzchar(modified)) query$modified <- modified
  if (length(query) > 0) url <- httr::modify_url(url, query = query)

  data <- .safe_json(url, headers = c(Accept = "application/json"))
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) return(.ices_empty())

  tibble::as_tibble(data) %>%
    dplyr::mutate(
      code_type = code_type,
      url = paste0(.ices_base_url, "/CodeDetail/", utils::URLencode(code_type, reserved = TRUE), "/", .data$key)
    )
}

#' Find ICES code types by text match
#'
#' @param query Search string matched against `key`, `description`, and `longDescription`.
#' @param max_results Maximum number of rows to return (default 20).
#'
#' @return Filtered tibble of code types.
#' @export
ices_find_code_types <- function(query, max_results = 20) {
  if (is.null(query) || is.na(query) || !nzchar(query)) return(.ices_empty())
  q <- tolower(query)
  ices_code_types() %>%
    dplyr::filter(
      grepl(q, tolower(.data$key %||% ""), fixed = TRUE) |
        grepl(q, tolower(.data$description %||% ""), fixed = TRUE) |
        grepl(q, tolower(.data$longDescription %||% ""), fixed = TRUE)
    ) %>%
    utils::head(max_results)
}

#' Find ICES codes within a code type by text match
#'
#' @param query Search string matched against `key`, `description`, and `longDescription`.
#' @param code_type ICES code type key (e.g., `"Gear"`).
#' @param max_results Maximum number of rows to return (default 50).
#'
#' @return Filtered tibble of codes for the given code type.
#' @export
ices_find_codes <- function(query, code_type, max_results = 50) {
  if (is.null(query) || is.na(query) || !nzchar(query)) return(.ices_empty())
  q <- tolower(query)
  ices_codes(code_type) %>%
    dplyr::filter(
      grepl(q, tolower(.data$key %||% ""), fixed = TRUE) |
        grepl(q, tolower(.data$description %||% ""), fixed = TRUE) |
        grepl(q, tolower(.data$longDescription %||% ""), fixed = TRUE)
    ) %>%
    utils::head(max_results)
}

