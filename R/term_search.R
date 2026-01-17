#' Find candidate terms across external vocabularies
#'
#' Lightweight meta-search helper for IRIs. Uses public APIs when available:
#' - OLS (no key): broad cross-ontology search
#' - NERC NVS P01/P06 (via SPARQL endpoint)
#' - ZOOMA (no key): EBI text-to-term annotations (resolves to OLS term metadata)
#' - BioPortal (optional; requires API key via env `BIOPORTAL_APIKEY`)
#'
#' Results are scored using role-specific I-ADOPT vocabulary hints (from
#' `inst/extdata/iadopt-terminologies.csv`) and deterministic tie-breakers so
#' the ordering is stable.
#'
#' Network calls are best-effort and will return an empty tibble on failure.
#'
#' @param query Character search string.
#' @param role Optional I-ADOPT role hint (`"variable"`, `"property"`, `"entity"`,
#'   `"constraint"`, `"method"`, `"unit"`); used only for metadata tagging.
#' @param sources Character vector of sources to query (`"ols"`, `"nvs"`, `"zooma"`, `"bioportal"`).
#' @return Tibble with columns `label`, `iri`, `source`, `ontology`, `role`, `match_type`, `definition`.
#' @export
#' @import httr
#' @importFrom rlang %||%
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
    } else if (src == "zooma") {
      .search_zooma(query, role)
    } else if (src == "bioportal") {
      .search_bioportal(query, role)
    } else {
      .empty_terms(role)
    }
  })

  combined <- dplyr::bind_rows(results)
  ranked <- .score_and_rank_terms(combined, role, .iadopt_vocab(), query)
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
.metasalmon_user_agent <- httr::user_agent(
  sprintf("metasalmon/%s", utils::packageVersion("metasalmon"))
)

.safe_json <- function(url, headers = NULL, timeout_secs = 30) {
  tryCatch(
    {
      ua <- .metasalmon_user_agent
      res <- if (!is.null(headers) && length(headers) > 0) {
        httr::GET(url, ua, httr::timeout(timeout_secs), httr::add_headers(.headers = headers))
      } else {
        httr::GET(url, ua, httr::timeout(timeout_secs))
      }
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
  url <- paste0("https://www.ebi.ac.uk/ols4/api/search?q=", encoded, "&rows=50")
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
  tokens <- unique(strsplit(gsub("[^a-z0-9]+", " ", tolower(query)), "\\s+")[[1]])
  tokens <- tokens[nzchar(tokens)]
  if (length(tokens) == 0) {
    return(.empty_terms(role))
  }

  # NVS search_nvs endpoints are not reliable; use the SPARQL endpoint instead.

  # Restrict to P01 (observables) and P06 (units).
  # Use simple REGEX on prefLabel for speed (REGEX + OPTIONAL is too slow on P01).
  pattern <- paste(tokens, collapse = ".*")
  pattern <- gsub("\\\\", "\\\\\\\\", pattern)
  pattern <- gsub("\"", "\\\\\"", pattern)

  sparql <- paste0(
    "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>\n",
    "SELECT DISTINCT ?uri ?label ?definition WHERE {\n",
    "  ?uri skos:prefLabel ?label .\n",
    "  OPTIONAL { ?uri skos:definition ?definition . }\n",
    "  FILTER(\n",
    "    STRSTARTS(STR(?uri), \"http://vocab.nerc.ac.uk/collection/P01/\") ||\n",
    "    STRSTARTS(STR(?uri), \"http://vocab.nerc.ac.uk/collection/P06/\")\n",
    "  )\n",
    "  FILTER(REGEX(LCASE(STR(?label)), \"", pattern, "\"))\n",
    "}\n",
    "LIMIT 50\n"
  )

  url <- paste0("https://vocab.nerc.ac.uk/sparql/?query=", utils::URLencode(sparql, reserved = TRUE))
  data <- .safe_json(url, headers = c(Accept = "application/sparql-results+json"), timeout_secs = 60)
  bindings <- data$results$bindings %||% NULL
  if (is.null(bindings) || !is.data.frame(bindings) || nrow(bindings) == 0) {
    return(.empty_terms(role))
  }

  sparql_value <- function(var) {
    flat <- paste0(var, ".value")
    if (flat %in% names(bindings)) {
      return(bindings[[flat]])
    }
    if (var %in% names(bindings) && is.data.frame(bindings[[var]]) && "value" %in% names(bindings[[var]])) {
      return(bindings[[var]]$value)
    }
    rep("", nrow(bindings))
  }

  iri <- sparql_value("uri")
  label <- sparql_value("label")
  definition <- sparql_value("definition")
  definition <- ifelse(is.na(definition), "", definition)

  ontology <- gsub("^http://vocab\\.nerc\\.ac\\.uk/collection/([^/]+)/.*$", "\\1", iri)
  ontology <- ifelse(grepl("^http://vocab\\.nerc\\.ac\\.uk/collection/[^/]+/", iri), ontology, "")

  tibble::tibble(
    label = label,
    iri = iri,
    source = "nvs",
    ontology = ontology,
    role = role,
    match_type = "concept",
    definition = definition
  ) %>%
    dplyr::distinct(iri, .keep_all = TRUE)
}

.search_zooma <- function(query, role) {
  encoded <- utils::URLencode(query, reserved = TRUE)
  url <- paste0("https://www.ebi.ac.uk/spot/zooma/v2/api/services/annotate?propertyValue=", encoded)
  data <- .safe_json(url, headers = c(Accept = "application/json"), timeout_secs = 60)

  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    return(.empty_terms(role))
  }

  olslinks_list <- data$`_links`$olslinks %||% list()
  if (length(olslinks_list) == 0) {
    return(.empty_terms(role))
  }

  links_df <- purrr::map2_dfr(olslinks_list, data$confidence %||% rep(NA_character_, nrow(data)), function(links, conf) {
    if (is.null(links) || !is.data.frame(links) || nrow(links) == 0) return(tibble::tibble())
    tibble::as_tibble(links) %>%
      dplyr::mutate(confidence = conf)
  })

  if (!all(c("href", "semanticTag") %in% names(links_df)) || nrow(links_df) == 0) {
    return(.empty_terms(role))
  }

  hrefs <- unique(links_df$href)
  hrefs <- hrefs[nzchar(hrefs)]
  hrefs <- utils::head(hrefs, 25)

  terms <- purrr::map_dfr(hrefs, function(href) {
    term_data <- .safe_json(href)
    terms_df <- term_data$`_embedded`$terms %||% NULL
    if (is.null(terms_df) || !is.data.frame(terms_df) || nrow(terms_df) == 0) return(tibble::tibble())
    defn <- terms_df$description[[1]] %||% character()
    tibble::tibble(
      label = terms_df$label[[1]] %||% "",
      iri = terms_df$iri[[1]] %||% "",
      source = "zooma",
      ontology = terms_df$ontology_name[[1]] %||% "",
      role = role,
      match_type = "",
      definition = if (length(defn) > 0) defn[[1]] else ""
    )
  })

  if (nrow(terms) == 0) {
    return(.empty_terms(role))
  }

  match_tbl <- links_df %>%
    dplyr::mutate(
      iri = semanticTag,
      match_type = paste0(
        "zooma_",
        tolower(dplyr::if_else(is.na(confidence) | confidence == "", "unknown", confidence))
      )
    ) %>%
    dplyr::select(iri, match_type) %>%
    dplyr::group_by(iri) %>%
    dplyr::summarise(match_type = match_type[[1]], .groups = "drop")

  terms %>%
    dplyr::left_join(match_tbl, by = "iri", suffix = c("", ".zooma")) %>%
    dplyr::mutate(match_type = match_type.zooma %||% match_type) %>%
    dplyr::select(-match_type.zooma) %>%
    dplyr::distinct(iri, .keep_all = TRUE)
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

.score_and_rank_terms <- function(df, role, vocab_tbl, query = NULL) {
  if (nrow(df) == 0) {
    return(df)
  }

  base_source_weight <- c(ols = 0.3, nvs = 0.6, zooma = 0.5, bioportal = 0.2)
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

  query_tokens <- character()
  if (!is.null(query) && !is.na(query) && nzchar(query)) {
    query_tokens <- unique(strsplit(gsub("[^a-z0-9]+", " ", tolower(query)), "\\s+")[[1]])
    query_tokens <- query_tokens[nzchar(query_tokens)]
  }

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

  if (length(query_tokens) > 0) {
    df$score <- df$score + vapply(df$label, function(lbl) {
      lbl_tokens <- unique(strsplit(gsub("[^a-z0-9]+", " ", tolower(lbl %||% "")), "\\s+")[[1]])
      lbl_tokens <- lbl_tokens[nzchar(lbl_tokens)]
      overlaps <- intersect(query_tokens, lbl_tokens)
      length(overlaps) * 0.2
    }, numeric(1))
  }

  df[order(-df$score, df$source, df$ontology, df$label, df$iri), ]
}

utils::globalVariables(c(
  "ttl_url",
  "label",
  "iri",
  "ontology",
  "match_type",
  "definition",
  "confidence",
  "href",
  "semanticTag",
  "match_type.zooma"
))
