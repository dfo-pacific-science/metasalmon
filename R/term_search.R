#' Expand query based on role context (Phase 4)
#'
#' Generates additional query variants based on role-specific patterns.
#' For example, unit queries get "unit" suffix, entity queries get species
#' name variants.
#'
#' @param query Original query string
#' @param role I-ADOPT role hint
#' @return Character vector of query variants (original always first)
#' @keywords internal
.expand_query <- function(query, role) {
  if (is.null(query) || is.na(query) || !nzchar(query)) {
    return(character())
  }

  queries <- query  # Original always first

  if (is.null(role) || is.na(role)) {
    return(queries)
  }

  role <- tolower(role)

  # Role-specific expansions
  if (role == "unit") {
    # Add "unit" suffix for unit searches if not already present
    if (!grepl("unit", query, ignore.case = TRUE)) {
      queries <- c(queries, paste(query, "unit"))
    }
    # Common unit abbreviation expansions
    abbrevs <- list(
      "kg" = "kilogram",
      "m" = "meter",
      "cm" = "centimeter",
      "mm" = "millimeter",
      "g" = "gram",
      "l" = "liter",
      "ml" = "milliliter",
      "s" = "second",
      "min" = "minute",
      "h" = "hour",
      "d" = "day"
    )
    q_lower <- tolower(trimws(query))
    if (q_lower %in% names(abbrevs)) {
      queries <- c(queries, abbrevs[[q_lower]])
    }
  } else if (role == "method") {
    # Add method-related context terms
    if (!grepl("method|protocol|procedure|technique", query, ignore.case = TRUE)) {
      queries <- c(queries, paste(query, "method"))
    }
  } else if (role == "entity") {
    # For species-like queries, try both exact and with common suffixes
    # Check if it looks like a binomial (two capitalized words)
    if (grepl("^[A-Z][a-z]+ [a-z]+$", query)) {
      # Already looks like a species name, add genus-only variant
      genus <- sub(" .*", "", query)
      queries <- c(queries, genus)
    }
  } else if (role == "property") {
    # Add "measurement" or "observation" context if not present
    if (!grepl("measurement|observation|count|abundance|length|weight|size", query, ignore.case = TRUE)) {
      queries <- c(queries, paste(query, "measurement"))
    }
  }

  unique(queries)
}

#' Find candidate terms across external vocabularies
#'
#' Lightweight meta-search helper for IRIs. Uses public APIs when available.
#' Implements role-aware ontology preferences per dfo-salmon-ontology CONVENTIONS.
#'
#' **Supported sources:**
#' - **OLS** (Ontology Lookup Service): Broad cross-ontology search, no API key needed
#' - **NVS** (NERC Vocabulary Server): Marine and oceanographic terms (P01/P06)
#' - **ZOOMA** (EBI text-to-term annotations): Resolves to OLS term metadata
#' - **QUDT** (Quantities, Units, Dimensions and Types): Preferred for unit role
#' - **GBIF** (Global Biodiversity Information Facility): Taxon backbone for entity role
#' - **WoRMS** (World Register of Marine Species): Marine taxa for entity role
#' - **BioPortal**: Requires API key via `BIOPORTAL_APIKEY` environment variable
#'
#' **Role-based ontology preferences (Phase 2):**
#' - `unit`: QUDT preferred, then NVS P06
#' - `property`: STATO/OBA measurement ontologies, NVS P01
#' - `entity`: gcdfo + NCEAS Salmon (ODO), GBIF/WoRMS for taxa
#' - `method`: gcdfo: SKOS + SOSA/PROV patterns, plus AGROVOC
#' - Wikidata is alignment-only (lower ranking for crosswalks/reconciliation)
#'
#' Results are scored using I-ADOPT vocabulary hints and role-based ontology
#' preferences, then ranked by relevance. Network calls are best-effort and
#' return an empty tibble on failure.
#'
#' @param query Character search string (e.g., `"spawner count"`, `"temperature"`).
#' @param role Optional I-ADOPT role hint for ranking and source selection. One of:
#'   `"variable"` (compound term), `"property"` (characteristic),
#'   `"entity"` (thing measured), `"constraint"` (qualifier), `"method"`, or `"unit"`.
#'   When specified, sources are optimized for the role and results are ranked higher
#'   when they match preferred ontologies for that role.
#' @param sources Character vector of vocabulary sources to query. Options:
#'   `"ols"`, `"nvs"`, `"zooma"`, `"qudt"`, `"gbif"`, `"worms"`, `"bioportal"`.
#'   Default is `c("ols", "nvs")`. Use [sources_for_role()] to get role-optimized sources.
#' @param expand_query Logical. If `TRUE` (default), applies role-aware query expansion
#'   (Phase 4) to generate additional query variants based on the role context.
#'   For example, unit queries get abbreviation expansions, method queries get
#'   "method" suffix added. Set to `FALSE` to search only the exact query.
#'
#' @return Tibble with columns: `label`, `iri`, `source`, `ontology`, `role`,
#'   `match_type`, `definition`, `score`, `alignment_only`, `agreement_sources`,
#'   `zooma_confidence`, `zooma_annotator`. The `score` column shows the computed
#'   ranking score. The `alignment_only` column indicates terms from Wikidata
#'   (useful for crosswalks but not canonical modeling). The `agreement_sources`
#'   column indicates how many sources returned the same IRI or label (Phase 4
#'   cross-source agreement). Returns empty tibble if no matches found.
#'
#'   The result has a `"diagnostics"` attribute (access via `attr(result, "diagnostics")`)
#'   containing per-source/query diagnostic information: source, query, status
#'   (success/error), count, elapsed_secs, and error message if applicable. This
#'   helps explain empty results or slow queries.
#'
#' @seealso [suggest_semantics()] for automated suggestions based on your dictionary.
#' @seealso [sources_for_role()] for role-optimized source selection.
#'
#' @export
#' @import httr
#' @importFrom rlang %||% .data
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
#' # Search for units with QUDT preference
#' unit_terms <- find_terms("kilogram", role = "unit", sources = sources_for_role("unit"))
#'
#' # Search for taxa using taxon resolvers
#' taxa <- find_terms("Oncorhynchus kisutch", role = "entity", sources = c("gbif", "worms"))
#'
#' # Search a specific source
#' ols_results <- find_terms("salmon", sources = "ols")
#'
#' # Search multiple sources
#' all_results <- find_terms("escapement", sources = c("ols", "nvs"))
#' }
find_terms <- function(query,
                       role = NA_character_,
                       sources = c("ols", "nvs"),
                       expand_query = TRUE) {
  if (length(sources) == 0 || is.na(query) || query == "") {
    return(.empty_terms(role))
  }

  # Apply role-aware query expansion (Phase 4)
  queries <- if (expand_query) .expand_query(query, role) else query

  cache_key <- paste(paste(queries, collapse = "|"), role, paste(sort(sources), collapse = ","), sep = "::")
  if (.metasalmon_cache_enabled && exists(cache_key, envir = .metasalmon_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .metasalmon_cache))
  }

  # Run searches for all expanded queries with diagnostic tracking (Phase 4)
  diagnostics <- list()

  results <- purrr::map(queries, function(q) {
    purrr::map(sources, function(src) {
      start_time <- Sys.time()
      result <- tryCatch(
        {
          res <- if (src == "ols") {
            .search_ols(q, role)
          } else if (src == "nvs") {
            .search_nvs(q, role)
          } else if (src == "zooma") {
            .search_zooma(q, role)
          } else if (src == "bioportal") {
            .search_bioportal(q, role)
          } else if (src == "qudt") {
            .search_qudt(q, role)
          } else if (src == "gbif") {
            .search_gbif(q, role)
          } else if (src == "worms") {
            .search_worms(q, role)
          } else {
            .empty_terms(role)
          }
          elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
          diagnostics[[length(diagnostics) + 1]] <<- list(
            source = src,
            query = q,
            status = "success",
            count = nrow(res),
            elapsed_secs = round(elapsed, 2),
            error = NA_character_
          )
          res
        },
        error = function(e) {
          elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
          diagnostics[[length(diagnostics) + 1]] <<- list(
            source = src,
            query = q,
            status = "error",
            count = 0L,
            elapsed_secs = round(elapsed, 2),
            error = conditionMessage(e)
          )
          .empty_terms(role)
        }
      )
      result
    })
  })

  # Flatten nested results and combine
  results <- purrr::flatten(results)
  combined <- dplyr::bind_rows(results)

  # Deduplicate by IRI (keep first occurrence, which has original query priority)
  combined <- combined[!duplicated(combined$iri), ]

  combined <- dplyr::bind_rows(combined)
  if (!"zooma_confidence" %in% names(combined)) {
    combined$zooma_confidence <- NA_character_
  }
  if (!"zooma_annotator" %in% names(combined)) {
    combined$zooma_annotator <- NA_character_
  }
  # Add alignment_only column if missing (for sources that don't set it)
  if (!"alignment_only" %in% names(combined)) {
    combined$alignment_only <- FALSE
  }
  ranked <- .score_and_rank_terms(combined, role, .iadopt_vocab(), query)
  ranked <- dplyr::select(
    ranked,
    dplyr::all_of(c(
      "label",
      "iri",
      "source",
      "ontology",
      "role",
      "match_type",
      "definition",
      "score",
      "alignment_only",
      "agreement_sources",
      "zooma_confidence",
      "zooma_annotator"
    ))
  )

  # Attach diagnostics as attribute (Phase 4)
  diag_df <- dplyr::bind_rows(lapply(diagnostics, tibble::as_tibble))
  attr(ranked, "diagnostics") <- diag_df

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
    definition = character(),
    score = numeric(),
    alignment_only = logical(),
    agreement_sources = integer()
  )
}

.metasalmon_cache <- new.env(parent = emptyenv())
.metasalmon_cache_enabled <- tolower(Sys.getenv("METASALMON_CACHE", unset = "")) %in% c("1", "true", "yes")
.metasalmon_user_agent <- httr::user_agent(
  sprintf("metasalmon/%s", utils::packageVersion("metasalmon"))
)

# Bindings for NSE columns used in dplyr pipelines
alignment_only <- zooma_confidence <- zooma_annotator <- match_type.zooma <- NULL

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

  links_df <- purrr::imap_dfr(olslinks_list, function(links, idx) {
    conf <- data$confidence[[idx]] %||% NA_character_
    annotator <- data$annotator[[idx]] %||% NA_character_
    if (is.null(links) || !is.data.frame(links) || nrow(links) == 0) return(tibble::tibble())
    tibble::as_tibble(links) %>%
      dplyr::mutate(confidence = conf, annotator = annotator)
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
      iri = .data$semanticTag,
      zooma_confidence = .data$confidence,
      zooma_annotator = .data$annotator,
      match_type = paste0(
        "zooma_",
        tolower(dplyr::if_else(is.na(.data$confidence) | .data$confidence == "", "unknown", .data$confidence))
      )
    ) %>%
    dplyr::select(iri, match_type, zooma_confidence, zooma_annotator) %>%
    dplyr::group_by(iri) %>%
    dplyr::summarise(
      match_type = .data$match_type[[1]],
      zooma_confidence = .data$zooma_confidence[[1]],
      zooma_annotator = .data$zooma_annotator[[1]],
      .groups = "drop"
    )

  terms %>%
    dplyr::left_join(match_tbl, by = "iri", suffix = c("", ".zooma")) %>%
    dplyr::mutate(
      match_type = dplyr::coalesce(.data$match_type.zooma, .data$match_type),
      zooma_confidence = dplyr::coalesce(.data$zooma_confidence, NA_character_),
      zooma_annotator = dplyr::coalesce(.data$zooma_annotator, NA_character_)
    ) %>%
    dplyr::select(-dplyr::any_of("match_type.zooma")) %>%
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

#' Search QUDT for unit terms
#'
#' Preferred source for unit role (per dfo-salmon-ontology CONVENTIONS).
#' Uses the QUDT SPARQL endpoint to find matching unit terms.
#'
#' @param query Search query string
#' @param role I-ADOPT role (typically "unit")
#' @return Tibble of matching terms
#' @keywords internal
.search_qudt <- function(query, role) {
  tokens <- unique(strsplit(gsub("[^a-z0-9]+", " ", tolower(query)), "\\s+")[[1]])
  tokens <- tokens[nzchar(tokens)]
  if (length(tokens) == 0) {
    return(.empty_terms(role))
  }

  # Build regex pattern for SPARQL FILTER

pattern <- paste(tokens, collapse = ".*")
  pattern <- gsub("\\\\", "\\\\\\\\", pattern)
  pattern <- gsub("\"", "\\\\\"", pattern)

  sparql <- paste0(
    "PREFIX qudt: <http://qudt.org/schema/qudt/>\n",
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n",
    "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>\n",
    "SELECT DISTINCT ?uri ?label ?definition WHERE {\n",
    "  ?uri a qudt:Unit .\n",
    "  ?uri rdfs:label ?label .\n",
    "  OPTIONAL { ?uri skos:definition ?definition . }\n",
    "  OPTIONAL { ?uri qudt:description ?definition . }\n",
    "  FILTER(REGEX(LCASE(STR(?label)), \"", pattern, "\", \"i\"))\n",
    "}\n",
    "LIMIT 50\n"
  )

  url <- paste0("https://www.qudt.org/fuseki/qudt/sparql?query=", utils::URLencode(sparql, reserved = TRUE))
  data <- .safe_json(url, headers = c(Accept = "application/sparql-results+json"), timeout_secs = 60)
  bindings <- data$results$bindings %||% NULL
  if (is.null(bindings) || length(bindings) == 0) {
    return(.empty_terms(role))
  }

  # Handle both list-of-lists and data.frame binding formats
  if (is.data.frame(bindings)) {
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
  } else {
    # bindings is a list
    iri <- vapply(bindings, function(b) b$uri$value %||% "", character(1))
    label <- vapply(bindings, function(b) b$label$value %||% "", character(1))
    definition <- vapply(bindings, function(b) b$definition$value %||% "", character(1))
  }

  definition <- ifelse(is.na(definition), "", definition)

  tibble::tibble(
    label = label,
    iri = iri,
    source = "qudt",
    ontology = "qudt",
    role = role,
    match_type = "unit",
    definition = definition
  ) %>%
    dplyr::distinct(iri, .keep_all = TRUE)
}

#' Search GBIF Backbone Taxonomy for taxon entities
#'
#' Useful for entity role when the entity is a species/taxon.
#' Uses GBIF Species API to match taxon names.
#'
#' @param query Search query string (taxon name)
#' @param role I-ADOPT role (typically "entity")
#' @return Tibble of matching taxa
#' @keywords internal
.search_gbif <- function(query, role) {
  encoded <- utils::URLencode(query, reserved = TRUE)
  # Use GBIF species match for exact-ish matches
  url <- paste0("https://api.gbif.org/v1/species/match?name=", encoded, "&verbose=true")
  data <- .safe_json(url, timeout_secs = 30)

  if (is.null(data) || is.null(data$usageKey)) {
    # Fallback to species search for broader matches
    url <- paste0("https://api.gbif.org/v1/species/search?q=", encoded, "&limit=20")
    data <- .safe_json(url, timeout_secs = 30)
    if (is.null(data) || is.null(data$results) || length(data$results) == 0) {
      return(.empty_terms(role))
    }
    results <- data$results
    tibble::tibble(
      label = vapply(results, function(r) r$scientificName %||% r$canonicalName %||% "", character(1)),
      iri = vapply(results, function(r) paste0("https://www.gbif.org/species/", r$key), character(1)),
      source = "gbif",
      ontology = "gbif_backbone",
      role = role,
      match_type = vapply(results, function(r) tolower(r$rank %||% "taxon"), character(1)),
      definition = vapply(results, function(r) {
        parts <- c(
          if (!is.null(r$kingdom)) paste("Kingdom:", r$kingdom) else NULL,
          if (!is.null(r$phylum)) paste("Phylum:", r$phylum) else NULL,
          if (!is.null(r$class)) paste("Class:", r$class) else NULL,
          if (!is.null(r$order)) paste("Order:", r$order) else NULL,
          if (!is.null(r$family)) paste("Family:", r$family) else NULL
        )
        paste(parts, collapse = "; ")
      }, character(1))
    ) %>%
      dplyr::distinct(iri, .keep_all = TRUE)
  } else {
    # Single match result
    tibble::tibble(
      label = data$scientificName %||% data$canonicalName %||% "",
      iri = paste0("https://www.gbif.org/species/", data$usageKey),
      source = "gbif",
      ontology = "gbif_backbone",
      role = role,
      match_type = tolower(data$rank %||% "taxon"),
      definition = paste(
        if (!is.null(data$kingdom)) paste("Kingdom:", data$kingdom) else "",
        if (!is.null(data$phylum)) paste("Phylum:", data$phylum) else "",
        if (!is.null(data$class)) paste("Class:", data$class) else "",
        if (!is.null(data$order)) paste("Order:", data$order) else "",
        if (!is.null(data$family)) paste("Family:", data$family) else "",
        sep = "; "
      )
    )
  }
}

#' Search WoRMS for marine species entities
#'
#' World Register of Marine Species - authoritative for marine taxa.
#' Useful for entity role when dealing with marine species (salmon, etc.).
#'
#' @param query Search query string (taxon name)
#' @param role I-ADOPT role (typically "entity")
#' @return Tibble of matching marine species
#' @keywords internal
.search_worms <- function(query, role) {
  encoded <- utils::URLencode(query, reserved = TRUE)
  url <- paste0(
    "https://www.marinespecies.org/rest/AphiaRecordsByName/",
    encoded,
    "?like=true&marine_only=false&offset=1"
  )
  data <- .safe_json(url, timeout_secs = 30)

  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    # Try fuzzy match endpoint
    url <- paste0("https://www.marinespecies.org/rest/AphiaRecordsByMatchNames?scientificnames%5B%5D=", encoded)
    data <- .safe_json(url, timeout_secs = 30)
    if (is.null(data) || length(data) == 0 || is.null(data[[1]])) {
      return(.empty_terms(role))
    }
    # Flatten the nested list result
    data <- data[[1]]
    if (!is.data.frame(data) && is.list(data)) {
      if (length(data) == 0) return(.empty_terms(role))
      data <- dplyr::bind_rows(data)
    }
    if (!is.data.frame(data) || nrow(data) == 0) {
      return(.empty_terms(role))
    }
  }

  tibble::tibble(
    label = data$scientificname %||% "",
    iri = paste0("urn:lsid:marinespecies.org:taxname:", data$AphiaID),
    source = "worms",
    ontology = "worms",
    role = role,
    match_type = tolower(data$rank %||% "taxon"),
    definition = vapply(seq_len(nrow(data)), function(i) {
      r <- data[i, ]
      parts <- c(
        if (!is.null(r$kingdom) && !is.na(r$kingdom)) paste("Kingdom:", r$kingdom) else NULL,
        if (!is.null(r$phylum) && !is.na(r$phylum)) paste("Phylum:", r$phylum) else NULL,
        if (!is.null(r$class) && !is.na(r$class)) paste("Class:", r$class) else NULL,
        if (!is.null(r$order) && !is.na(r$order)) paste("Order:", r$order) else NULL,
        if (!is.null(r$family) && !is.na(r$family)) paste("Family:", r$family) else NULL
      )
      paste(parts, collapse = "; ")
    }, character(1))
  ) %>%
    dplyr::distinct(iri, .keep_all = TRUE)
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

#' Load role-based ontology preferences
#'
#' Returns the ranked allowlist of preferred ontologies per I-ADOPT role.
#' Based on dfo-salmon-ontology CONVENTIONS.md:
#' - unit: QUDT + NVS P06 preferred
#' - method: gcdfo: SKOS + SOSA/PROV patterns
#' - entity: gcdfo salmon domain + taxa resolvers (GBIF/WoRMS)
#' - property: STATO/OBA measurement ontologies
#' - Wikidata is alignment-only
#'
#' @return Tibble with role preferences and priority rankings
#' @keywords internal
.role_preferences <- function() {
  path <- system.file("extdata", "ontology-preferences.csv", package = "metasalmon", mustWork = FALSE)
  if (!file.exists(path) || path == "") {
    # Return default preferences if file not found
    return(tibble::tibble(
      role = character(),
      ontology = character(),
      priority = integer(),
      source_hint = character(),
      iri_pattern = character(),
      alignment_only = logical(),
      notes = character()
    ))
  }
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

#' Get recommended sources for a given role
#'
#' Returns the optimal set of sources to query based on role.
#' Implements Phase 2 role-aware source selection.
#'
#' @param role I-ADOPT role (unit, property, entity, method, variable, constraint)
#' @return Character vector of recommended sources
#' @export
#' @examples
#' sources_for_role("unit")
#' # Returns: c("qudt", "nvs", "ols")
#'
#' sources_for_role("entity")
#' # Returns: c("gbif", "worms", "ols")
sources_for_role <- function(role) {
  if (is.null(role) || is.na(role) || role == "") {
    return(c("ols", "nvs"))
  }
  role <- tolower(role)
  switch(role,
    unit = c("qudt", "nvs", "ols"),
    property = c("nvs", "ols", "zooma"),
    entity = c("gbif", "worms", "bioportal", "ols"),
    method = c("bioportal", "ols", "zooma"),
    variable = c("nvs", "ols", "zooma"),
    constraint = c("ols"),
    c("ols", "nvs")
  )
}

#' Embedding-based reranking (Phase 4 placeholder)
#'
#' Optional reranking of term candidates using sentence embeddings.
#' When enabled via `METASALMON_EMBEDDING_RERANK=1`, applies cosine similarity
#' reranking over the top lexical candidates.
#'
#' Current status: placeholder infrastructure. Full implementation requires:
#' - Local embedding model (e.g., sentence-transformers via reticulate)
#' - Embedding cache to avoid repeated computation
#' - Configurable top-k for reranking (default: top 50 lexical candidates)
#'
#' @param df Data frame of term results with score column

#' @param query Original search query
#' @param top_k Number of top candidates to rerank (default 50)
#' @return Data frame with optional embedding_score column
#' @keywords internal
.apply_embedding_rerank <- function(df, query, top_k = 50L) {
  # Check if embedding rerank is enabled
  if (!.embedding_rerank_enabled()) {
    return(df)
  }

  if (nrow(df) == 0 || is.null(query) || is.na(query)) {
    return(df)
  }

  # Placeholder: log that embedding rerank was requested but not yet implemented

  # Full implementation would:
  # 1. Take top-k candidates by lexical score
  # 2. Embed query and candidate (label + definition) using sentence model
  # 3. Compute cosine similarity
  # 4. Add embedding_score column and optionally rerank

  # For now, just add a placeholder column
  df$embedding_score <- NA_real_

  # Issue a one-time message if debug mode is on
  if (tolower(Sys.getenv("METASALMON_DEBUG", unset = "")) %in% c("1", "true")) {
    message("metasalmon: embedding rerank requested but not yet implemented (Phase 4 placeholder)")
  }

  df
}

#' Check if embedding rerank is enabled
#' @keywords internal
.embedding_rerank_enabled <- function() {
  tolower(Sys.getenv("METASALMON_EMBEDDING_RERANK", unset = "")) %in% c("1", "true", "yes")
}

#' Apply cross-source agreement boosting (Phase 4)
#'
#' Boosts terms that appear from multiple sources, indicating higher confidence.
#' IRI agreement (same IRI from different sources) gets higher boost than
#' label-only agreement (same label, different IRIs).
#'
#' @param df Data frame of term results with score column
#' @return Data frame with agreement boosts applied and agreement_sources column
#' @keywords internal
.apply_cross_source_agreement <- function(df) {
  if (nrow(df) < 2) {
    df$agreement_sources <- 1L
    return(df)
  }

  # Normalize IRIs and labels for comparison
  df$iri_norm <- tolower(trimws(df$iri))
  df$label_norm <- tolower(trimws(df$label))

  # Count sources per IRI (strong agreement)
  iri_counts <- stats::aggregate(source ~ iri_norm, data = df, FUN = function(x) length(unique(x)))
  names(iri_counts)[2] <- "iri_source_count"

  # Count sources per label (weaker agreement - same label, possibly different IRIs)
  label_counts <- stats::aggregate(source ~ label_norm, data = df, FUN = function(x) length(unique(x)))
  names(label_counts)[2] <- "label_source_count"

  # Merge counts back

  df <- merge(df, iri_counts, by = "iri_norm", all.x = TRUE)
  df <- merge(df, label_counts, by = "label_norm", all.x = TRUE)

  # Apply boosts:
  # - IRI agreement (2+ sources): +0.5 per additional source (strong signal)
  # - Label-only agreement (2+ sources, no IRI match): +0.2 per additional source
  df$score <- df$score + ifelse(
    df$iri_source_count > 1,
    (df$iri_source_count - 1) * 0.5,  # IRI agreement boost
    ifelse(
      df$label_source_count > 1,
      (df$label_source_count - 1) * 0.2,  # Label-only agreement boost
      0
    )
  )

  # Record agreement for explainability

  df$agreement_sources <- pmax(df$iri_source_count, df$label_source_count)

  # Clean up temp columns
  df$iri_norm <- NULL
  df$label_norm <- NULL
  df$iri_source_count <- NULL
  df$label_source_count <- NULL

  df
}

.score_and_rank_terms <- function(df, role, vocab_tbl, query = NULL) {
if (nrow(df) == 0) {
    return(df)
  }

  # Load role-based ontology preferences (Phase 2)
  role_prefs <- .role_preferences()

  # Base source weights - updated for new sources
  base_source_weight <- c(
    ols = 0.3,
    nvs = 0.6,
    zooma = 0.5,
    bioportal = 0.2,
    qudt = 0.7,  # High weight for QUDT (preferred for units)
    gbif = 0.6,  # High weight for GBIF (preferred for entities)
    worms = 0.6  # High weight for WoRMS (preferred for entities)
  )

  # Role-specific source boosts (Phase 2: ontology preferences by role)
  role_boost <- list(
    unit = c(qudt = 1.5, nvs = 1.2, ols = 0.3),
    property = c(nvs = 1.0, ols = 0.5, zooma = 0.4),
    variable = c(nvs = 1.0, ols = 0.4, zooma = 0.4),
    entity = c(gbif = 1.3, worms = 1.3, bioportal = 0.4, ols = 0.4),
    constraint = c(ols = 0.5),
    method = c(bioportal = 0.4, ols = 0.5, zooma = 0.4)
  )

  role_key <- if (is.null(role) || is.na(role)) NA_character_ else role
  role_vocabs <- if (!is.na(role_key)) dplyr::filter(vocab_tbl, .data$role == role_key) else vocab_tbl[0, ]

  host_pattern <- if (nrow(role_vocabs) > 0) paste(unique(role_vocabs$host), collapse = "|") else ""
  slug_pattern <- if (nrow(role_vocabs) > 0) paste(unique(role_vocabs$slug), collapse = "|") else ""
  label_pattern <- if (nrow(role_vocabs) > 0) paste(unique(role_vocabs$label_tokens), collapse = "|") else ""

  df$score <- vapply(df$source, function(src) base_source_weight[[src]] %||% 0.1, numeric(1))

  query_tokens <- character()
  if (!is.null(query) && !is.na(query) && nzchar(query)) {
    query_tokens <- unique(strsplit(gsub("[^a-z0-9]+", " ", tolower(query)), "\\s+")[[1]])
    query_tokens <- query_tokens[nzchar(query_tokens)]
  }

  role_map <- role_boost[[role_key]] %||% numeric(0)
  if (length(role_map) > 0) {
    df$score <- df$score + vapply(df$source, function(src) role_map[[src]] %||% 0, numeric(1))
  }

  # Apply ontology preference boosts based on IRI patterns (Phase 2)
  if (nrow(role_prefs) > 0 && !is.na(role_key)) {
    role_specific_prefs <- dplyr::filter(role_prefs, .data$role == role_key | .data$role == "wikidata")
    if (nrow(role_specific_prefs) > 0) {
      # Apply priority-based boost: higher priority (lower number) = bigger boost
      df$score <- df$score + vapply(seq_len(nrow(df)), function(i) {
        iri <- df$iri[i]
        boost <- 0

        for (j in seq_len(nrow(role_specific_prefs))) {
          pref <- role_specific_prefs[j, ]
          pattern <- pref$iri_pattern

          # Check if IRI matches this ontology preference
          if (!is.na(pattern) && nzchar(pattern) && grepl(pattern, iri, ignore.case = TRUE)) {
            # Wikidata is alignment-only: penalize instead of boost
            if (isTRUE(pref$alignment_only)) {
              boost <- boost - 0.5
            } else {
              # Priority 1 = +2.0, Priority 2 = +1.5, Priority 3 = +1.0, etc.
              priority_boost <- max(0, 2.5 - (pref$priority * 0.5))
              boost <- boost + priority_boost
            }
            break  # Use first matching preference
          }
        }
        boost
      }, numeric(1))
    }
  }

  # I-ADOPT vocabulary matching (legacy, still useful for broad coverage)
  if (host_pattern != "") {
    df$score <- df$score + ifelse(grepl(host_pattern, df$iri, ignore.case = TRUE), 0.8, 0)
  }
  if (slug_pattern != "") {
    df$score <- df$score + ifelse(
      grepl(slug_pattern, df$iri, ignore.case = TRUE) | grepl(slug_pattern, df$ontology, ignore.case = TRUE),
      0.8, 0
    )
  }
  if (label_pattern != "") {
    df$score <- df$score + ifelse(grepl(label_pattern, df$ontology, ignore.case = TRUE), 0.4, 0)
  }

  # Query token overlap scoring
  if (length(query_tokens) > 0) {
    df$score <- df$score + vapply(df$label, function(lbl) {
      lbl_tokens <- unique(strsplit(gsub("[^a-z0-9]+", " ", tolower(lbl %||% "")), "\\s+")[[1]])
      lbl_tokens <- lbl_tokens[nzchar(lbl_tokens)]
      overlaps <- intersect(query_tokens, lbl_tokens)
      length(overlaps) * 0.2
    }, numeric(1))
  }

  # ZOOMA confidence weighting (Phase 4 prework)
  if ("zooma_confidence" %in% names(df)) {
    conf <- tolower(df$zooma_confidence %||% NA_character_) # nolint
    annot <- tolower(df$zooma_annotator %||% NA_character_)
    is_curated <- !is.na(annot) & grepl("curated|manual", annot)
    is_automatic <- !is.na(annot) & !is_curated # nolint

    df$score <- df$score + dplyr::case_when(
      is_curated & conf %in% c("high", "good") ~ 0.75,
      is_curated & conf %in% c("medium") ~ 0.35,
      is_automatic & conf %in% c("low") ~ -0.25,
      TRUE ~ 0
    )
  }

  # Cross-source agreement boosting (Phase 4)
  # Boost terms that appear from multiple sources (same IRI or same label)
  df <- .apply_cross_source_agreement(df)

  # Optional embedding-based reranking (Phase 4)
  # Enabled via METASALMON_EMBEDDING_RERANK=1 environment variable
  df <- .apply_embedding_rerank(df, query)

  # Add alignment_only flag for downstream filtering
  df$alignment_only <- vapply(df$iri, function(iri) {
    grepl("wikidata\\.org", iri, ignore.case = TRUE)
  }, logical(1))

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
  "annotator",
  "zooma_confidence",
  "zooma_annotator",
  "href",
  "semanticTag",
  "match_type.zooma",
  "alignment_only",
  "priority",
  "iri_pattern",
  "score",
  "agreement_sources",
  "iri_norm",
  "label_norm",
  "iri_source_count",
  "label_source_count",
  "embedding_score"
))
