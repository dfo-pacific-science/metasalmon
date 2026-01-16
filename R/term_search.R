#' Find candidate terms across external vocabularies
#'
#' Searches multiple vocabulary services to find IRIs that match a query string.
#' Useful for discovering existing terms to link your data to standard
#' scientific definitions.
#'
#' **Supported sources:**
#' - **OLS** (Ontology Lookup Service): Broad cross-ontology search, no API key needed
#' - **NVS** (NERC Vocabulary Server): Marine and oceanographic terms (P01/P06)
#' - **BioPortal**: Requires API key via `BIOPORTAL_APIKEY` environment variable
#'
#' Results are scored using I-ADOPT vocabulary hints and ranked by relevance.
#' Network calls are best-effort and return an empty tibble on failure.
#'
#' @param query Character search string (e.g., `"spawner count"`, `"temperature"`).
#' @param role Optional I-ADOPT role hint for ranking results. One of:
#'   `"variable"` (compound term), `"property"` (characteristic),
#'   `"entity"` (thing measured), `"constraint"` (qualifier), or `"unit"`.
#'   Results matching the specified role are ranked higher.
#' @param sources Character vector of vocabulary sources to query. Options:
#'   `"ols"`, `"nvs"`, `"bioportal"`. Default is `c("ols", "nvs")`.
#'
#' @return Tibble with columns: `label`, `iri`, `source`, `ontology`, `role`,
#'   `match_type`, `definition`. Returns empty tibble if no matches found.
#'
#' @seealso [suggest_semantics()] for automated suggestions based on your dictionary.
#'
#' @export
#' @import httr
#' @importFrom rlang %||%
#'
#' @examples
#' \dontrun{
#' # Search for terms matching "spawner count"
#' results <- find_terms("spawner count")
#' head(results)
#'
#' # Search specifically for property terms
#' property_terms <- find_terms("temperature", role = "property")
#'
#' # Search a specific source
#' ols_results <- find_terms("salmon", sources = "ols")
#'
#' # Search multiple sources
#' all_results <- find_terms("escapement", sources = c("ols", "nvs"))
#' }
find_terms <- function(query,
                       role = NA_character_,
                       sources = c("ols", "nvs")) {
  if (length(sources) == 0 || is.na(query) || query == "") {
    return(.empty_terms(role))
  }

  cache_key <- paste(query, role, paste(sort(sources), collapse = ","), sep = "::")
  if (.metasalmon_cache_enabled && exists(cache_key, envir = .metasalmon_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .metasalmon_cache))
  }

  results <- purrr::map(sources, function(src) {
    if (src == "ols") {
      .search_ols(query, role)
    } else if (src == "nvs") {
      .search_nvs(query, role)
    } else if (src == "bioportal") {
      .search_bioportal(query, role)
    } else {
      .empty_terms(role)
    }
  })

  combined <- dplyr::bind_rows(results)
  ranked <- .score_and_rank_terms(combined, role, .iadopt_vocab())
  ranked <- dplyr::select(ranked, label, iri, source, ontology, role, match_type, definition)
  if (.metasalmon_cache_enabled) {
    assign(cache_key, ranked, envir = .metasalmon_cache)
  }
  ranked
}

.empty_terms <- function(role) {
  tibble::tibble(
    label = character(),
    iri = character(),
    source = character(),
    ontology = character(),
    role = if (is.null(role)) NA_character_ else role,
    match_type = character(),
    definition = character()
  )
}

.metasalmon_cache <- new.env(parent = emptyenv())
.metasalmon_cache_enabled <- tolower(Sys.getenv("METASALMON_CACHE", unset = "")) %in% c("1", "true", "yes")

.metasalmon_cache <- new.env(parent = emptyenv())

.safe_json <- function(url, headers = NULL) {
  tryCatch(
    {
      res <- httr::GET(url, httr::add_headers(.headers = headers %||% list()))
      if (httr::status_code(res) >= 300) {
        return(NULL)
      }
      jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
    },
    error = function(...) NULL
  )
}

.search_ols <- function(query, role) {
  encoded <- utils::URLencode(query, reserved = TRUE)
  url <- paste0("https://www.ebi.ac.uk/ols4/api/search?q=", encoded)
  data <- .safe_json(url)
  if (is.null(data) || is.null(data$response$docs)) {
    return(.empty_terms(role))
  }

  docs <- data$response$docs
  tibble::tibble(
    label = docs$label %||% "",
    iri = docs$iri %||% "",
    source = "ols",
    ontology = docs$ontology_name %||% "",
    role = role,
    match_type = docs$type %||% "",
    definition = purrr::map_chr(docs$description %||% list(), ~ if (length(.x) > 0) .x[[1]] else "")
  )
}

.search_nvs <- function(query, role) {
  encoded <- utils::URLencode(query, reserved = TRUE)
  # P01 (observables) and P06 (units); we query both
  urls <- c(
    paste0("https://vocab.nerc.ac.uk/search_nvs/P01/?q=", encoded, "&format=json"),
    paste0("https://vocab.nerc.ac.uk/search_nvs/P06/?q=", encoded, "&format=json")
  )

  rows <- purrr::map(urls, function(u) {
    data <- .safe_json(u)
    if (is.null(data) || is.null(data$results)) {
      return(.empty_terms(role))
    }
    tibble::tibble(
      label = data$results$prefLabel %||% "",
      iri = data$results$uri %||% "",
      source = "nvs",
      ontology = data$results$collection %||% "",
      role = role,
      match_type = data$results$type %||% "",
      definition = data$results$definition %||% ""
    )
  })

  dplyr::bind_rows(rows)
}

.search_bioportal <- function(query, role) {
  apikey <- Sys.getenv("BIOPORTAL_APIKEY", unset = "")
  if (apikey == "") {
    if (isFALSE(getOption("metasalmon.warned_bioportal_missing", FALSE))) {
      warning(
        "BioPortal API key missing; set BIOPORTAL_APIKEY in your env and restart. ",
        "Example (bash/zsh): export BIOPORTAL_APIKEY=your_key_here. ",
        "Persist it by adding BIOPORTAL_APIKEY=your_key_here to ~/.Renviron or ~/.zshrc. ",
        "Get a key at https://bioportal.bioontology.org/register. ",
        call. = FALSE
      )
      options(metasalmon.warned_bioportal_missing = TRUE)
    }
    return(.empty_terms(role))
  }
  encoded <- utils::URLencode(query, reserved = TRUE)
  url <- paste0("https://data.bioontology.org/search?q=", encoded, "&apikey=", apikey)
  data <- .safe_json(url)
  if (is.null(data) || is.null(data$collection)) {
    return(.empty_terms(role))
  }

  coll <- data$collection
  tibble::tibble(
    label = coll$prefLabel %||% "",
    iri = coll$`@id` %||% "",
    source = "bioportal",
    ontology = coll$links$ontology %||% "",
    role = role,
    match_type = coll$matchType %||% "",
    definition = purrr::map_chr(coll$definition %||% list(), ~ if (length(.x) > 0) .x[[1]] else "")
  )
}

.iadopt_vocab <- function() {
  path <- system.file("extdata", "iadopt-terminologies.csv", package = "metasalmon", mustWork = TRUE)
  if (!file.exists(path)) {
    return(tibble::tibble())
  }
  tib <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  tib %>%
    dplyr::mutate(
      host = purrr::map_chr(ttl_url, ~ httr::parse_url(.x)$hostname %||% ""),
      slug = tools::file_path_sans_ext(basename(ttl_url)),
      label_tokens = gsub("[^a-z0-9]+", " ", tolower(label))
    )
}

.score_and_rank_terms <- function(df, role, vocab_tbl) {
  if (nrow(df) == 0) {
    return(df)
  }

  base_source_weight <- c(ols = 0.3, nvs = 0.6, bioportal = 0.2)
  role_boost <- list(
    unit = c(nvs = 1.2, ols = 0.4),
    property = c(nvs = 1.0, ols = 0.4),
    variable = c(ols = 0.4, bioportal = 0.4),
    entity = c(ols = 0.4, bioportal = 0.4),
    constraint = c(ols = 0.4, bioportal = 0.4),
    method = c(ols = 0.4, bioportal = 0.4)
  )

  role_key <- if (is.null(role) || is.na(role)) NA_character_ else role
  role_vocabs <- if (!is.na(role_key)) dplyr::filter(vocab_tbl, role == role_key) else vocab_tbl[0, ]

  host_pattern <- if (nrow(role_vocabs) > 0) paste(unique(role_vocabs$host), collapse = "|") else ""
  slug_pattern <- if (nrow(role_vocabs) > 0) paste(unique(role_vocabs$slug), collapse = "|") else ""
  label_pattern <- if (nrow(role_vocabs) > 0) paste(unique(role_vocabs$label_tokens), collapse = "|") else ""

  df$score <- vapply(df$source, function(src) base_source_weight[[src]] %||% 0, numeric(1))

  role_map <- role_boost[[role_key]] %||% numeric(0)
  if (length(role_map) > 0) {
    df$score <- df$score + vapply(df$source, function(src) role_map[[src]] %||% 0, numeric(1))
  }

  if (host_pattern != "") {
    df$score <- df$score + ifelse(grepl(host_pattern, df$iri, ignore.case = TRUE), 1, 0)
  }
  if (slug_pattern != "") {
    df$score <- df$score + ifelse(
      grepl(slug_pattern, df$iri, ignore.case = TRUE) | grepl(slug_pattern, df$ontology, ignore.case = TRUE),
      1, 0
    )
  }
  if (label_pattern != "") {
    df$score <- df$score + ifelse(grepl(label_pattern, df$ontology, ignore.case = TRUE), 0.5, 0)
  }

  df[order(-df$score, df$source, df$ontology, df$label, df$iri), ]
}

utils::globalVariables(c("ttl_url", "label", "iri", "ontology", "match_type", "definition"))
