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
#' - **SMN** (Salmon Domain Ontology): shared salmon-domain search from `https://w3id.org/smn/` with canonical shared IRIs (e.g. `https://w3id.org/smn/Stock`)
#' - **GCDFO** (DFO Salmon Ontology): DFO-specific search from `https://w3id.org/gcdfo/salmon#`
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
#' - `entity`: smn first, then gcdfo + NCEAS Salmon (ODO), GBIF/WoRMS for taxa
#' - `method`: smn first, then gcdfo: SKOS + SOSA/PROV patterns, plus AGROVOC
#' - Wikidata is alignment-only (lower ranking for crosswalks/reconciliation)
#'
#' Results are scored using I-ADOPT vocabulary hints and role-based ontology
#' preferences, then ranked by relevance. When `"smn"` is included in
#' `sources`, shared salmon-domain ontology search runs first; `"gcdfo"` is used
#' as a deterministic DFO-specific source before external sources. External fallback
#' sources are skipped when SMN or GCDFO returns a good label match. Network calls are
#' best-effort and return an empty tibble on failure.
#'
#' @param query Character search string (e.g., `"spawner count"`, `"temperature"`).
#' @param role Optional I-ADOPT role hint for ranking and source selection. One of:
#'   `"variable"` (compound term), `"property"` (characteristic),
#'   `"entity"` (thing measured), `"constraint"` (qualifier), `"method"`, or `"unit"`.
#'   When specified, sources are optimized for the role and results are ranked higher
#'   when they match preferred ontologies for that role.
#' @param sources Character vector of vocabulary sources to query. Options:
#'   `"smn"`, `"gcdfo"`, `"ols"`, `"nvs"`, `"zooma"`, `"qudt"`, `"gbif"`, `"worms"`, `"bioportal"`.
#'   Default is `c("smn", "gcdfo", "ols", "nvs")`. Use [sources_for_role()] to get role-optimized sources.
#' @param expand_query Logical. If `TRUE` (default), applies role-aware query expansion
#'   (Phase 4) to generate additional query variants based on the role context.
#'   For example, unit queries get abbreviation expansions, method queries get
#'   "method" suffix added. Set to `FALSE` to search only the exact query.
#'
#' @return Tibble with columns: `label`, `iri`, `source`, `ontology`, `role`,
#'   `match_type`, `definition`, `score`, `alignment_only`, `agreement_sources`,
#'   `role_hints`,
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
#' all_results <- find_terms("escapement", sources = c("smn", "gcdfo", "ols", "nvs"))
#' }
find_terms <- function(query,
                       role = NA_character_,
                       sources = c("smn", "gcdfo", "ols", "nvs"),
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
    run_source <- function(src) {
      start_time <- Sys.time()
      tryCatch(
        {
          res <- if (src == "smn") {
            .search_smn(q, role)
          } else if (src == "gcdfo") {
            .search_gcdfo(q, role)
          } else if (src == "ols") {
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
    }

    query_results <- list()

    local_pref_sources <- intersect(c("smn", "gcdfo"), sources)
    if (length(local_pref_sources) > 0) {
      for (local_src in local_pref_sources) {
        local_res <- run_source(local_src)
        query_results[[length(query_results) + 1]] <- local_res
        local_good_hit <- nrow(local_res) > 0 && any(local_res$match_type %in% c("label_exact", "label_partial"))
        if (local_good_hit) {
          return(query_results)
        }
      }
    }

    for (src in setdiff(sources, c("smn", "gcdfo"))) {
      query_results[[length(query_results) + 1]] <- run_source(src)
    }

    query_results
  })

  # Flatten nested results and combine
  results <- purrr::flatten(results)
  combined <- dplyr::bind_rows(results)

  # Keep one row per source+IRI while preserving source provenance.
  combined <- dplyr::distinct(combined, .data$source, .data$iri, .keep_all = TRUE)
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
  if (!"role_hints" %in% names(ranked)) {
    ranked$role_hints <- NA_character_
  }
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
      "role_hints",
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
    agreement_sources = integer(),
    role_hints = character()
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

.gcdfo_index_cache <- new.env(parent = emptyenv())
.smn_index_cache <- new.env(parent = emptyenv())

.local_name <- function(x) {
  ifelse(is.na(x) | !nzchar(x), "", sub("^.*[#/]", "", x))
}


.camel_words <- function(x) {
  x <- gsub("([a-z0-9])([A-Z])", "\\1 \\2", x)
  trimws(gsub("[_-]+", " ", x))
}

.query_tokens <- function(x) {
  tokens <- unique(strsplit(gsub("[^a-z0-9]+", " ", tolower(x)), "\\s+")[[1]])
  tokens[nzchar(tokens)]
}

.first_non_empty_chr <- function(x) {
  x <- x[!is.na(x)]
  x <- trimws(x)
  x <- x[nzchar(x)]
  if (length(x) == 0) "" else x[[1]]
}

.text_similarity_score <- function(query, label, definition) {
  q <- tolower(trimws(query %||% ""))
  if (!nzchar(q)) {
    return(0)
  }

  label_text <- tolower(trimws(paste(label %||% "", definition %||% "")))
  if (!nzchar(label_text)) {
    return(0)
  }

  query_tokens <- .query_tokens(q)
  candidate_tokens <- .query_tokens(label_text)
  if (length(query_tokens) == 0 || length(candidate_tokens) == 0) {
    return(0)
  }

  overlap <- intersect(query_tokens, candidate_tokens)
  token_ratio <- length(overlap) / length(query_tokens)
  phrase_bonus <- if (grepl(q, label_text, fixed = TRUE)) 0.35 else 0
  exact_bonus <- if (identical(label_text, q)) 0.35 else 0
  min(1, token_ratio + phrase_bonus + exact_bonus)
}

.match_type_score <- function(match_type) {
  mt <- tolower(trimws(match_type %||% ""))
  if (!nzchar(mt)) {
    return(0)
  }

  if (mt == "label_exact") {
    return(1.0)
  }
  if (grepl("^label", mt)) {
    return(0.45)
  }
  if (grepl("^zooma", mt)) {
    return(0.3)
  }
  if (mt %in% c("definition", "concept")) {
    return(0.15)
  }
  0.05
}

.xml_text_values <- function(node, xpath, ns) {
  vals <- xml2::xml_text(xml2::xml_find_all(node, xpath, ns = ns))
  vals <- trimws(vals)
  vals[nzchar(vals)]
}

.xml_resource_values <- function(node, xpath, ns) {
  vals <- xml2::xml_attr(xml2::xml_find_all(node, xpath, ns = ns), "resource")
  vals <- vals[!is.na(vals)]
  vals[nzchar(vals)]
}

.parse_salmon_rdfxml <- function(doc, iri_pattern = "^https?://w3id\\.org/smn(#|/|$)") {
  ns <- xml2::xml_ns(doc)
  nodes <- xml2::xml_find_all(doc, "/*/*[@rdf:about][not(self::owl:Ontology)]", ns = ns)
  if (length(nodes) == 0) {
    return(tibble::tibble())
  }

  rows <- purrr::map_dfr(nodes, function(node) {
    iri <- xml2::xml_attr(node, "about") %||% ""
    label <- .first_non_empty_chr(c(
      .xml_text_values(node, "./skos:prefLabel", ns),
      .xml_text_values(node, "./rdfs:label", ns)
    ))
    definition <- .first_non_empty_chr(c(
      .xml_text_values(node, "./skos:definition", ns),
      .xml_text_values(node, "./obo:IAO_0000115", ns),
      .xml_text_values(node, "./rdfs:comment", ns),
      .xml_text_values(node, "./dcterms:description", ns)
    ))
    alt_labels <- .xml_text_values(node, "./skos:altLabel", ns)
    in_scheme <- .xml_resource_values(node, "./skos:inScheme", ns)
    rdf_types <- .xml_resource_values(node, "./rdf:type", ns)
    parents <- unique(c(
      .xml_resource_values(node, "./skos:broader", ns),
      .xml_resource_values(node, "./rdfs:subClassOf", ns)
    ))
    iadopt_property <- .xml_resource_values(node, "./*[local-name()='iadoptProperty']", ns)
    iadopt_entity <- .xml_resource_values(node, "./*[local-name()='iadoptEntity']", ns)
    iadopt_constraint <- .xml_resource_values(node, "./*[local-name()='iadoptConstraint']", ns)
    used_procedure <- .xml_resource_values(node, "./*[local-name()='usedProcedure']", ns)
    iri_local <- .local_name(iri)
    label_fallback <- if (nzchar(label)) label else .camel_words(iri_local)
    search_text <- paste(
      c(
        label_fallback,
        alt_labels,
        .camel_words(iri_local),
        .camel_words(.local_name(in_scheme)),
        .camel_words(.local_name(rdf_types)),
        definition
      ),
      collapse = " "
    )

    tibble::tibble(
      iri = iri,
      label = label_fallback,
      alt_labels = paste(alt_labels, collapse = " | "),
      definition = definition,
      resource_kind = xml2::xml_name(node),
      in_scheme = paste(in_scheme, collapse = " | "),
      parent_iris = paste(parents, collapse = " | "),
      type_iris = paste(rdf_types, collapse = " | "),
      search_text = tolower(search_text),
      is_variable = length(c(iadopt_property, iadopt_entity, iadopt_constraint)) > 0,
      iadopt_property_targets = list(iadopt_property),
      iadopt_entity_targets = list(iadopt_entity),
      iadopt_constraint_targets = list(iadopt_constraint),
      used_procedure_targets = list(used_procedure)
    )
  })

  rows <- dplyr::filter(rows, grepl(iri_pattern, .data$iri))

  property_targets <- unique(unlist(rows$iadopt_property_targets, use.names = FALSE))
  entity_targets <- unique(unlist(rows$iadopt_entity_targets, use.names = FALSE))
  constraint_targets <- unique(unlist(rows$iadopt_constraint_targets, use.names = FALSE))
  method_targets <- unique(unlist(rows$used_procedure_targets, use.names = FALSE))

  rows %>%
    dplyr::mutate(
      is_property = .data$iri %in% property_targets,
      is_entity = .data$iri %in% entity_targets,
      is_constraint = .data$iri %in% constraint_targets,
      is_method = .data$iri %in% method_targets | grepl("method|procedure|enumeration", tolower(.data$in_scheme)),
      role_hints = purrr::pmap_chr(
        list(.data$is_variable, .data$is_property, .data$is_entity, .data$is_constraint, .data$is_method),
        function(variable, property, entity, constraint, method) {
          hints <- c(
            if (isTRUE(variable)) "variable",
            if (isTRUE(property)) "property",
            if (isTRUE(entity)) "entity",
            if (isTRUE(constraint)) "constraint",
            if (isTRUE(method)) "method"
          )
          paste(hints, collapse = "|")
        }
      )
    ) %>%
    dplyr::select(-dplyr::ends_with("_targets"))
}

.smn_term_index <- function(refresh = FALSE) {
  cache_dir <- file.path(tempdir(), "metasalmon-ontology-rdf-cache", "smn")
  paths <- vapply(.smn_module_urls(), .smn_fetch_module_path, character(1), cache_dir = cache_dir)

  info <- file.info(paths)
  stamp <- paste(
    paste(paths, collapse = "|"),
    paste(info$mtime, collapse = "|"),
    paste(info$size, collapse = "|"),
    sep = "||"
  )
  if (!refresh && exists("stamp", envir = .smn_index_cache, inherits = FALSE) &&
      exists("index", envir = .smn_index_cache, inherits = FALSE) &&
      identical(get("stamp", envir = .smn_index_cache), stamp)) {
    return(get("index", envir = .smn_index_cache))
  }

  index <- .parse_smn_ttl_modules(paths)
  assign("stamp", stamp, envir = .smn_index_cache)
  assign("index", index, envir = .smn_index_cache)
  index
}

.gcdfo_term_index <- function(refresh = FALSE) {
  cache_dir <- file.path(tempdir(), "metasalmon-ontology-rdf-cache", "gcdfo")
  path <- fetch_salmon_ontology(
    url = "https://w3id.org/gcdfo/salmon",
    accept = "application/rdf+xml",
    cache_dir = cache_dir,
    fallback_urls = c(
      "https://w3id.org/gcdfo/salmon/",
      "https://dfo-pacific-science.github.io/dfo-salmon-ontology/gcdfo.owl"
    )
  )

  stamp <- paste(path, file.info(path)$mtime, file.info(path)$size)
  if (!refresh && exists("stamp", envir = .gcdfo_index_cache, inherits = FALSE) &&
      exists("index", envir = .gcdfo_index_cache, inherits = FALSE) &&
      identical(get("stamp", envir = .gcdfo_index_cache), stamp)) {
    return(get("index", envir = .gcdfo_index_cache))
  }

  doc <- xml2::read_xml(path)
  index <- .parse_salmon_rdfxml(doc, iri_pattern = "^https?://w3id\\.org/gcdfo/salmon(#|$)")
  assign("stamp", stamp, envir = .gcdfo_index_cache)
  assign("index", index, envir = .gcdfo_index_cache)
  index
}

.gcdfo_filter_for_role <- function(index, role) {
  if (nrow(index) == 0 || is.null(role) || is.na(role) || role == "") {
    return(index)
  }

  role <- tolower(role)
  scheme_text <- tolower(paste(index$in_scheme, index$label, index$role_hints))
  keep <- switch(role,
    unit = rep(FALSE, nrow(index)),
    variable = index$is_variable | (
      grepl("count|rate|abundance|estimate|escapement|spawner|recruit|run", index$search_text) &
        !grepl("context|scheme|method|procedure", scheme_text)
    ),
    property = index$is_property | (
      grepl("abundance|count|rate|length|weight|size|status|confidence|level|phase", index$search_text) &
        !grepl("context|scheme|method|procedure", scheme_text)
    ),
    entity = index$is_entity | (
      tolower(index$resource_kind) %in% c("class", "namedindividual") &
        !grepl("theme|scheme|measurement|assessment|benchmark|reference point|procedure|method|property|characteristic|context",
               tolower(paste(index$label, index$iri, index$in_scheme)))
    ),
    constraint = index$is_constraint | grepl("criteria|context|origin|phase|zone|basis|dimension|notation|framework|confidence|level", scheme_text),
    method = index$is_method | grepl("method|procedure|enumeration", scheme_text),
    rep(TRUE, nrow(index))
  )

  index[keep, , drop = FALSE]
}

.gcdfo_match_terms <- function(index, query) {
  if (nrow(index) == 0) {
    return(index[0, , drop = FALSE])
  }

  tokens <- .query_tokens(query)
  if (length(tokens) == 0) {
    return(index[0, , drop = FALSE])
  }

  q_lower <- tolower(trimws(query))
  primary_label <- trimws(tolower(index$label))
  label_text <- trimws(tolower(paste(index$label, index$alt_labels)))
  exact_label <- primary_label == q_lower
  phrase_label <- grepl(q_lower, primary_label, fixed = TRUE) | grepl(q_lower, label_text, fixed = TRUE)
  phrase_text <- grepl(q_lower, index$search_text, fixed = TRUE)
  token_hits <- vapply(index$search_text, function(txt) {
    sum(vapply(tokens, function(tok) grepl(tok, txt, fixed = TRUE), logical(1)))
  }, numeric(1))
  all_tokens <- vapply(index$search_text, function(txt) {
    all(vapply(tokens, function(tok) grepl(tok, txt, fixed = TRUE), logical(1)))
  }, logical(1))

  score <- token_hits +
    ifelse(phrase_text, 1.5, 0) +
    ifelse(phrase_label, 2.0, 0) +
    ifelse(exact_label, 3.0, 0) +
    ifelse(all_tokens, 1.0, 0)

  keep <- score > 0
  if (!any(keep)) {
    return(index[0, , drop = FALSE])
  }

  index <- index[keep, , drop = FALSE]
  score <- score[keep]
  label_text <- label_text[keep]
  phrase_label <- phrase_label[keep]
  exact_label <- exact_label[keep]
  phrase_text <- phrase_text[keep]

  match_type <- ifelse(
    exact_label, "label_exact",
    ifelse(phrase_label, "label_partial", ifelse(phrase_text, "definition", tolower(index$resource_kind)))
  )

  index$backend_score <- score
  index$match_type <- match_type
  index[order(-index$backend_score, index$label, index$iri), , drop = FALSE]
}

.search_smn <- function(query, role) {
  index <- .smn_term_index()
  index <- .gcdfo_filter_for_role(index, role)
  index <- .gcdfo_match_terms(index, query)
  if (nrow(index) == 0) {
    return(.empty_terms(role))
  }

  tibble::tibble(
    label = index$label,
    iri = index$iri,
    source = "smn",
    ontology = "smn",
    role = role,
    match_type = index$match_type,
    definition = index$definition,
    backend_score = index$backend_score,
    role_hints = index$role_hints
  ) %>%
    dplyr::distinct(iri, .keep_all = TRUE)
}

.search_gcdfo <- function(query, role) {
  index <- .gcdfo_term_index()
  index <- .gcdfo_filter_for_role(index, role)
  index <- .gcdfo_match_terms(index, query)
  if (nrow(index) == 0) {
    return(.empty_terms(role))
  }

  tibble::tibble(
    label = index$label,
    iri = index$iri,
    source = "gcdfo",
    ontology = "gcdfo",
    role = role,
    match_type = index$match_type,
    definition = index$definition,
    backend_score = index$backend_score,
    role_hints = index$role_hints
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
#' - method: smn first, then gcdfo: SKOS + SOSA/PROV patterns
#' - entity: smn first, then gcdfo salmon domain + taxa resolvers (GBIF/WoRMS)
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
#' # Returns: c("smn", "gcdfo", "gbif", "worms", "bioportal", "ols")
sources_for_role <- function(role) {
  if (is.null(role) || is.na(role) || role == "") {
    return(c("smn", "gcdfo", "ols", "nvs"))
  }
  role <- tolower(role)
  switch(role,
    unit = c("qudt", "nvs", "ols"),
    property = c("smn", "gcdfo", "nvs", "ols", "zooma"),
    entity = c("smn", "gcdfo", "gbif", "worms", "bioportal", "ols"),
    method = c("smn", "gcdfo", "bioportal", "ols", "zooma"),
    variable = c("smn", "gcdfo", "nvs", "ols", "zooma"),
    constraint = c("smn", "gcdfo", "ols"),
    c("smn", "gcdfo", "ols", "nvs")
  )
}

#' Embedding/ranking re-rank utility (Phase 4)
#'
#' Optional semantic reranking stage that uses lightweight text-similarity when
#' `METASALMON_EMBEDDING_RERANK=1` is set. This is deterministic and
#' dependency-light (no Python model required), and it adds a reusable
#' `embedding_score` field that can later be replaced with true vector
#' embeddings without changing callers.
#'
#' @param df Data frame of term results with score column
#'
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

  if (!("label" %in% names(df)) || !("definition" %in% names(df))) {
    return(df)
  }

  # Run ranking on the top-k lexical rows. Keep rest in their existing order.
  top_n <- min(as.integer(top_k), nrow(df))
  if (top_n < 1) {
    return(df)
  }
  ranking_idx <- seq_len(nrow(df))
  if (nrow(df) > top_n) {
    ranking_idx <- ranking_idx[order(-df$score)][seq_len(top_n)]
  }

  sim_scores <- vapply(ranking_idx, function(i) {
    .text_similarity_score(query, df$label[[i]], df$definition[[i]])
  }, numeric(1))

  # Normalize similarity scores to [0, 1] for stable weighting.
  sim_range <- range(sim_scores, na.rm = TRUE)
  if (!is.finite(sim_range[1]) || !is.finite(sim_range[2]) || sim_range[1] == sim_range[2]) {
    sim_scores <- rep(0, length(sim_scores))
  } else {
    sim_scores <- (sim_scores - sim_range[1]) / (sim_range[2] - sim_range[1])
  }

  # Blend with existing lexical ranking score. Weight defaults to 1.0.
  weight <- suppressWarnings(as.numeric(Sys.getenv("METASALMON_EMBEDDING_WEIGHT", unset = "1")))
  if (is.na(weight) || !is.finite(weight)) {
    weight <- 1
  }

  score <- rep(NA_real_, nrow(df))
  score[ranking_idx] <- df$score[ranking_idx] + (weight * sim_scores)
  score[is.na(score)] <- df$score[is.na(score)]
  df$embedding_score <- rep(NA_real_, nrow(df))
  df$embedding_score[ranking_idx] <- sim_scores
  df$score <- score

  if (tolower(Sys.getenv("METASALMON_DEBUG", unset = "")) %in% c("1", "true")) {
    message("metasalmon: embedding-style rerank applied to top candidates using text similarity")
  }

  df
}

#' Check if embedding rerank is enabled
#' @keywords internal
.embedding_rerank_enabled <- function() {
  tolower(Sys.getenv("METASALMON_EMBEDDING_RERANK", unset = "")) %in% c("1", "true", "yes")
}

.ranking_profile_defaults <- function() {
  list(
    base_source_weight = c(
      smn = 1.0,
      gcdfo = 0.9,
      ols = 0.3,
      nvs = 0.6,
      zooma = 0.5,
      bioportal = 0.2,
      qudt = 0.7,
      gbif = 0.6,
      worms = 0.6
    ),
    role_boost = list(
      unit = c(qudt = 1.5, nvs = 1.2, ols = 0.3),
      property = c(smn = 1.6, gcdfo = 1.0, nvs = 1.0, ols = 0.5, zooma = 0.4),
      variable = c(smn = 1.7, gcdfo = 1.3, nvs = 1.0, ols = 0.4, zooma = 0.4),
      entity = c(smn = 1.7, gcdfo = 1.3, gbif = 1.3, worms = 1.3, bioportal = 0.4, ols = 0.4),
      constraint = c(smn = 1.4, gcdfo = 1.0, ols = 0.5),
      method = c(smn = 1.7, gcdfo = 1.3, bioportal = 0.4, ols = 0.5, zooma = 0.4)
    ),
    ontology_preferences = list(
      host_bonus = 0.8,
      slug_bonus = 0.8,
      label_pattern_bonus = 0.4,
      wikidata_penalty = -0.5,
      role_priority_base = 2.5,
      role_priority_step = 0.5
    ),
    match_type_weights = list(
      label_exact = 1.0,
      label = 0.45,
      label_partial = 0.45,
      zooma_high = 0.3,
      zooma = 0.3,
      definition = 0.15,
      concept = 0.15,
      other = 0.05
    ),
    lexical_weights = list(
      label_overlap = 0.2,
      definition_overlap = 0.4
    ),
    zooma_weights = list(
      curated_high = 0.75,
      curated_medium = 0.35,
      automatic_low = -0.25
    ),
    cross_source_agreement = list(
      iri_boost = 0.5,
      label_boost = 0.2
    ),
    role_preferences_enabled = TRUE,
    match_type_enabled = TRUE,
    lexical_enabled = TRUE,
    zooma_enabled = TRUE,
    cross_source_enabled = TRUE,
    backend_score_weight = 1
  )
}

.merge_ranking_profile <- function(base, update) {
  if (is.null(update) || length(update) == 0) {
    return(base)
  }

  if (!is.list(base) || !is.list(update)) {
    return(update)
  }

  out <- base
  for (n in names(update)) {
    if (!is.null(update[[n]]) && is.list(update[[n]]) && !is.null(base[[n]]) && is.list(base[[n]])) {
      out[[n]] <- .merge_ranking_profile(base[[n]], update[[n]])
    } else if (!is.null(update[[n]])) {
      out[[n]] <- update[[n]]
    }
  }

  out
}

.match_type_score_profiled <- function(match_type, weights) {
  mt <- tolower(trimws(match_type %||% ""))
  if (!nzchar(mt)) {
    return(weights$other %||% 0)
  }

  if (mt == "label_exact") {
    return(weights$label_exact %||% 1.0)
  }

  if (grepl("^label", mt)) {
    return(weights$label %||% 0.45)
  }

  if (grepl("^zooma", mt)) {
    return(weights$zooma %||% 0.3)
  }

  if (mt %in% c("definition", "concept")) {
    return(weights$definition %||% 0.15)
  }

  weights$other %||% 0.05
}

.apply_match_type_score <- function(match_type, match_type_weights) {
  .match_type_score_profiled(match_type, match_type_weights)
}

#' Apply cross-source agreement boosting (Phase 4)
#'
#' Boosts terms that appear from multiple sources, indicating higher confidence.
#' IRI agreement (same IRI from different sources) gets higher boost than
#' label-only agreement (same label, different IRIs).
#'
#' @param df Data frame of term results with score column
#' @param iri_boost Per-additional-source boost when IRI matches
#' @param label_boost Per-additional-source boost when only label matches
#' @return Data frame with agreement boosts applied and agreement_sources column
#' @keywords internal
.apply_cross_source_agreement <- function(df, iri_boost = 0.5, label_boost = 0.2) {
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

  iri_boost <- suppressWarnings(as.numeric(iri_boost))
  label_boost <- suppressWarnings(as.numeric(label_boost))
  if (!is.finite(iri_boost)) {
    iri_boost <- 0.5
  }
  if (!is.finite(label_boost)) {
    label_boost <- 0.2
  }

  # Apply boosts:
  df$score <- df$score + ifelse(
    df$iri_source_count > 1,
    (df$iri_source_count - 1L) * iri_boost,
    ifelse(
      df$label_source_count > 1,
      (df$label_source_count - 1L) * label_boost,
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



.score_and_rank_terms <- function(df, role, vocab_tbl, query = NULL, ranking_profile = NULL) {
  if (nrow(df) == 0) {
    return(df)
  }

  profile <- .merge_ranking_profile(.ranking_profile_defaults(), ranking_profile)
  base_source_weight <- as.list(profile$base_source_weight)
  role_boost_map <- profile$role_boost
  ontology_prefs_cfg <- profile$ontology_preferences
  match_type_weights <- profile$match_type_weights
  lexical_weights <- profile$lexical_weights
  zooma_weights <- profile$zooma_weights
  cross_source_cfg <- profile$cross_source_agreement

  role_prefs <- if (isTRUE(profile$role_preferences_enabled)) {
    .role_preferences()
  } else {
    NULL
  }

  role_prefs <- role_prefs %||% tibble::tibble(
    role = character(),
    ontology = character(),
    priority = integer(),
    source_hint = character(),
    iri_pattern = character(),
    alignment_only = logical(),
    notes = character()
  )

  role_key <- if (is.null(role) || is.na(role)) NA_character_ else role
  role_vocabs <- if (!is.na(role_key)) dplyr::filter(vocab_tbl, .data$role == role_key) else vocab_tbl[0, ]

  host_pattern <- if (nrow(role_vocabs) > 0) paste(unique(role_vocabs$host), collapse = "|") else ""
  slug_pattern <- if (nrow(role_vocabs) > 0) paste(unique(role_vocabs$slug), collapse = "|") else ""
  label_pattern <- if (nrow(role_vocabs) > 0) paste(unique(role_vocabs$label_tokens), collapse = "|") else ""

  df$score <- vapply(df$source, function(src) {
    as.numeric(base_source_weight[[src]] %||% 0.1)
  }, numeric(1))

  if ("backend_score" %in% names(df)) {
    df$score <- df$score + (dplyr::coalesce(df$backend_score, 0) * as.numeric(profile$backend_score_weight %||% 1))
  }

  query_tokens <- character()
  if (!is.null(query) && !is.na(query) && nzchar(query)) {
    query_tokens <- unique(strsplit(gsub("[^a-z0-9]+", " ", tolower(query)), "\\s+")[[1]])
    query_tokens <- query_tokens[nzchar(query_tokens)]
  }

  role_map <- as.list(role_boost_map[[role_key]] %||% numeric(0))
  if (length(role_map) > 0) {
    df$score <- df$score + vapply(df$source, function(src) as.numeric(role_map[[src]] %||% 0), numeric(1))
  }

  # Apply ontology preference boosts based on IRI patterns (Phase 2)
  if (nrow(role_prefs) > 0 && !is.na(role_key)) {
    role_specific_prefs <- dplyr::filter(role_prefs, .data$role == role_key | .data$role == "wikidata")
    if (nrow(role_specific_prefs) > 0) {
      host_boost <- as.numeric(ontology_prefs_cfg$host_bonus %||% 0)
      slug_boost <- as.numeric(ontology_prefs_cfg$slug_bonus %||% 0)
      label_pat_boost <- as.numeric(ontology_prefs_cfg$label_pattern_bonus %||% 0)
      wikidata_penalty <- as.numeric(ontology_prefs_cfg$wikidata_penalty %||% 0)
      role_priority_base <- as.numeric(ontology_prefs_cfg$role_priority_base %||% 2.5)
      role_priority_step <- as.numeric(ontology_prefs_cfg$role_priority_step %||% 0.5)

      if (!is.finite(host_boost)) {
        host_boost <- 0
      }
      if (!is.finite(slug_boost)) {
        slug_boost <- 0
      }
      if (!is.finite(label_pat_boost)) {
        label_pat_boost <- 0
      }
      if (!is.finite(wikidata_penalty)) {
        wikidata_penalty <- -0.5
      }
      if (!is.finite(role_priority_base)) {
        role_priority_base <- 2.5
      }
      if (!is.finite(role_priority_step)) {
        role_priority_step <- 0.5
      }

      if (host_pattern != "") {
        df$score <- df$score + ifelse(grepl(host_pattern, df$iri, ignore.case = TRUE), host_boost, 0)
      }
      if (slug_pattern != "") {
        df$score <- df$score + ifelse(
          grepl(slug_pattern, df$iri, ignore.case = TRUE) | grepl(slug_pattern, df$ontology, ignore.case = TRUE),
          slug_boost,
          0
        )
      }
      if (label_pattern != "") {
        df$score <- df$score + ifelse(grepl(label_pattern, df$ontology, ignore.case = TRUE), label_pat_boost, 0)
      }

      df$score <- df$score + vapply(seq_len(nrow(df)), function(i) {
        iri <- df$iri[i]
        boost <- 0

        for (j in seq_len(nrow(role_specific_prefs))) {
          pref <- role_specific_prefs[j, ]
          pattern <- pref$iri_pattern

          # Check if IRI matches this ontology preference
          if (!is.na(pattern) && nzchar(pattern) && grepl(pattern, iri, ignore.case = TRUE)) {
            if (isTRUE(pref$alignment_only)) {
              boost <- wikidata_penalty
            } else {
              # Priority 1 = +2.0, Priority 2 = +1.5, Priority 3 = +1.0, etc.
              priority_boost <- max(0, role_priority_base - (pref$priority * role_priority_step))
              boost <- boost + priority_boost
            }
            break
          }
        }
        boost
      }, numeric(1))
    }
  }

  # Match-type and lexical overlap scoring
  if (length(query_tokens) > 0) {
    if (isTRUE(profile$match_type_enabled)) {
      df$score <- df$score + vapply(df$match_type, function(mt) .apply_match_type_score(mt, match_type_weights), numeric(1))
    }

    if (isTRUE(profile$lexical_enabled)) {
      label_overlap_weight <- as.numeric(lexical_weights$label_overlap %||% 0)
      definition_overlap_weight <- as.numeric(lexical_weights$definition_overlap %||% 0)
      if (!is.finite(label_overlap_weight)) {
        label_overlap_weight <- 0
      }
      if (!is.finite(definition_overlap_weight)) {
        definition_overlap_weight <- 0
      }

      # Label overlap remains useful for compact strings.
      df$score <- df$score + vapply(df$label, function(lbl) {
        lbl_tokens <- unique(strsplit(gsub("[^a-z0-9]+", " ", tolower(lbl %||% "")), "\\s+")[[1]])
        lbl_tokens <- lbl_tokens[nzchar(lbl_tokens)]
        overlaps <- intersect(query_tokens, lbl_tokens)
        length(overlaps) * label_overlap_weight
      }, numeric(1))

      # Definition overlap helps when labels are terse but definitions informative.
      df$score <- df$score + vapply(seq_len(nrow(df)), function(i) {
        q <- paste(query_tokens, collapse = " ")
        if (!nzchar(q)) {
          0
        } else {
          txt_tokens <- .query_tokens(df$definition[[i]] %||% "")
          def_overlap <- intersect(query_tokens, txt_tokens)
          definition_overlap_weight * (length(def_overlap) / length(query_tokens))
        }
      }, numeric(1))
    }
  }

  # ZOOMA confidence weighting
  if (isTRUE(profile$zooma_enabled) && "zooma_confidence" %in% names(df)) {
    conf <- tolower(df$zooma_confidence %||% NA_character_)
    annot <- tolower(df$zooma_annotator %||% NA_character_)
    is_curated <- !is.na(annot) & grepl("curated|manual", annot)
    is_automatic <- !is.na(annot) & !is_curated

    curated_high <- as.numeric(zooma_weights$curated_high %||% 0.75)
    curated_medium <- as.numeric(zooma_weights$curated_medium %||% 0.35)
    automatic_low <- as.numeric(zooma_weights$automatic_low %||% -0.25)
    if (!is.finite(curated_high)) {
      curated_high <- 0.75
    }
    if (!is.finite(curated_medium)) {
      curated_medium <- 0.35
    }
    if (!is.finite(automatic_low)) {
      automatic_low <- -0.25
    }

    df$score <- df$score + dplyr::case_when(
      is_curated & conf %in% c("high", "good") ~ curated_high,
      is_curated & conf %in% c("medium") ~ curated_medium,
      is_automatic & conf %in% c("low") ~ automatic_low,
      TRUE ~ 0
    )
  }

  # Cross-source agreement boosting (Phase 4)
  if (isTRUE(profile$cross_source_enabled)) {
    iri_boost <- as.numeric(cross_source_cfg$iri_boost %||% 0.5)
    label_boost <- as.numeric(cross_source_cfg$label_boost %||% 0.2)
    if (!is.finite(iri_boost)) {
      iri_boost <- 0.5
    }
    if (!is.finite(label_boost)) {
      label_boost <- 0.2
    }
    df <- .apply_cross_source_agreement(df, iri_boost = iri_boost, label_boost = label_boost)
  } else {
    df$agreement_sources <- 1L
  }

  # Optional embedding-based reranking (Phase 4)
  # Enabled via METASALMON_EMBEDDING_RERANK=1 environment variable
  df <- .apply_embedding_rerank(df, query)

  # Add alignment_only flag for downstream filtering
  df$alignment_only <- vapply(df$iri, function(iri) {
    grepl("wikidata\\.org", iri, ignore.case = TRUE)
  }, logical(1))

  df[order(-df$score, df$source, df$ontology, df$label, df$iri), ]
}


#' Benchmark semantic term ranking against fixture cases
#'
#' Evaluate ranking quality across a curated fixture dataset to support profile tuning.
#'
#' @param fixture_path Path to semantic ranking fixture JSON.
#'   The file should contain a list of case objects with `query`, `role`,
#'   `expected`, and `candidates` fields.
#' @param profiles Named list of ranking profiles. Each element should be a list of
#'   overrides merged into the default profile used by
#'   `.score_and_rank_terms()`. If `NULL`, a single `baseline` profile is run.
#' @param top_k Optional top-k cutoff for top-k accuracy and hit position checks.
#' @param include_details Return per-case diagnostics table (TRUE by default).
#' @param fixture_path_override Optional preloaded fixture object. If provided,
#'   `fixture_path` is ignored and this value is used as the fixture list.
#' @return A list with `summary`, `per_case`, and `profiles`.
#' @export
benchmark_term_ranking_fixtures <- function(fixture_path = NULL, profiles = NULL, top_k = 3L, include_details = TRUE, fixture_path_override = NULL) {
  if (is.null(fixture_path) && is.null(fixture_path_override)) {
    stop("fixture_path is required unless fixture_path_override is provided")
  }

  fixtures <- if (!is.null(fixture_path_override)) {
    fixture_path_override
  } else {
    jsonlite::fromJSON(fixture_path, simplifyDataFrame = FALSE)
  }

  if (!is.list(fixtures) || length(fixtures) == 0) {
    stop("fixture must be a non-empty list")
  }

  if (is.null(profiles) || length(profiles) == 0) {
    profiles <- list(baseline = NULL)
  }
  if (is.null(names(profiles)) || any(!nzchar(names(profiles)))) {
    names(profiles) <- if (is.null(names(profiles)) || any(!nzchar(names(profiles)))) {
      vapply(seq_along(profiles), function(i) paste0("profile_", i), character(1))
    } else names(profiles)
  }

  top_k <- as.integer(top_k)
  if (!is.finite(top_k) || top_k < 1) {
    top_k <- 3L
  }

  vocab_tbl <- .iadopt_vocab()

  eval_case <- function(case, profile) {
    candidate_df <- .build_fixture_candidates(case)
    ranked <- .score_and_rank_terms(
      candidate_df,
      case$role,
      vocab_tbl,
      case$query,
      ranking_profile = profile
    )

    top <- ranked[1, , drop = FALSE]
    expected <- case$expected
    expected_top <- expected$top %||% list()

    top1_position <- .fixture_expected_position(ranked, expected_top)
    expected_order <- .fixture_expected_order_check(ranked, expected)
    top1_match <- !is.na(top1_position) && top1_position == 1L

    disallow <- .fixture_disallowed_top_violation(top, expected)

    top2 <- if (nrow(ranked) >= 2L) ranked$score[[2L]] else NA_real_
    top1_margin <- if (nrow(ranked) >= 2L) top$score[[1L]] - top2 else NA_real_

    if (include_details) {
      tibble::tibble(
        case_id = case$case_id %||% NA_character_,
        query = case$query,
        role = case$role %||% NA_character_,
        n_candidates = nrow(ranked),
        top1_candidate_id = top$candidate_id[[1]],
        top1_source = top$source[[1]],
        top1_match_type = top$match_type[[1]],
        top1_score = top$score[[1]],
        top1_margin = top1_margin,
        top1_ok = top1_match,
        top1_position = top1_position,
        top_k_ok = !is.na(top1_position) && top1_position <= top_k,
        expected_order_ok = expected_order,
        disallowed_top_source = disallow$disallowed_source,
        disallowed_top_match = disallow$disallowed_match,
        mrr = if (is.na(top1_position)) NA_real_ else 1 / top1_position
      )
    } else {
      NULL
    }
  }

  profile_results <- list()
  per_case <- list()
  for (name in names(profiles)) {
    prof <- profiles[[name]]
    case_rows <- purrr::map(fixtures, function(case) eval_case(case, prof))

    if (include_details) {
      case_tbl <- dplyr::bind_rows(case_rows)
      case_tbl$profile <- name
    } else {
      case_tbl <- tibble::tibble()
      case_tbl$profile <- name
    }

    summary_tbl <- if (nrow(case_tbl) > 0) {
      tibble::tibble(
        profile = name,
        n_cases = nrow(case_tbl),
        top1_accuracy = mean(case_tbl$top1_ok, na.rm = TRUE),
        top_k_accuracy = mean(case_tbl$top_k_ok, na.rm = TRUE),
        expected_order_accuracy = mean(case_tbl$expected_order_ok, na.rm = TRUE),
        no_disallow_rate = 1 - mean(case_tbl$disallowed_top_source | case_tbl$disallowed_top_match, na.rm = TRUE),
        mean_top1_margin = mean(case_tbl$top1_margin, na.rm = TRUE),
        mrr = mean(case_tbl$mrr, na.rm = TRUE)
      )
    } else {
      tibble::tibble(
        profile = name,
        n_cases = 0L,
        top1_accuracy = NA_real_,
        top_k_accuracy = NA_real_,
        expected_order_accuracy = NA_real_,
        no_disallow_rate = NA_real_,
        mean_top1_margin = NA_real_,
        mrr = NA_real_
      )
    }

    profile_results[[name]] <- list(per_case = case_tbl, summary = summary_tbl)
    per_case[[name]] <- case_tbl
  }

  all_case_tbl <- if (include_details) {
    dplyr::bind_rows(per_case)
  } else {
    tibble::tibble(profile = names(profiles))
  }

  all_summary <- dplyr::bind_rows(lapply(profile_results, function(x) x$summary))

  structure(
    list(
      summary = all_summary,
      per_case = all_case_tbl,
      profiles = names(profiles)
    ),
    class = "metasalmon_ranking_benchmark"
  )
}

.fixture_expected_position <- function(ranked_df, expected_top) {
  if (length(expected_top) == 0) {
    return(NA_integer_)
  }
  expected_id <- expected_top$candidate_id %||% NULL
  expected_source <- expected_top$source %||% NULL
  expected_match_type <- expected_top$match_type %||% NULL
  expected_iri_contains <- expected_top$iri_contains %||% NULL

  for (i in seq_len(nrow(ranked_df))) {
    candidate <- ranked_df[i, ]
    if (!is.null(expected_id) && nzchar(expected_id) && candidate$candidate_id[[1]] == expected_id) {
      return(i)
    }
    if (!is.null(expected_iri_contains) && nzchar(expected_iri_contains) && grepl(expected_iri_contains, candidate$iri[[1]], fixed = TRUE)) {
      return(i)
    }
    if (!is.null(expected_source) && nzchar(expected_source) && candidate$source[[1]] == expected_source) {
      if (!is.null(expected_match_type) && nzchar(expected_match_type)) {
        if (!is.null(candidate$match_type) && candidate$match_type[[1]] == expected_match_type) {
          return(i)
        }
      } else {
        return(i)
      }
    }
  }
  NA_integer_
}

.fixture_expected_order_check <- function(ranked_df, expected) {
  expected_order <- expected$expected_order %||% NULL
  if (is.null(expected_order) || length(expected_order) == 0) {
    return(TRUE)
  }

  if (nrow(ranked_df) < length(expected_order)) {
    return(FALSE)
  }

  candidate_ids <- ranked_df$candidate_id %||% as.character(seq_len(nrow(ranked_df)))
  for (i in seq_along(expected_order)) {
    if (candidate_ids[[i]] != expected_order[[i]]) {
      return(FALSE)
    }
  }
  TRUE
}

.fixture_disallowed_top_violation <- function(top_row, expected) {
  if (nrow(top_row) == 0) {
    return(list(disallowed_source = FALSE, disallowed_match = FALSE))
  }

  top <- top_row[1, , drop = FALSE]
  disallowed_source <- FALSE
  disallowed_match <- FALSE

  if (!is.null(expected$disallow_top_sources)) {
    bad_sources <- unlist(expected$disallow_top_sources)
    disallowed_source <- top$source[[1]] %in% bad_sources
  }

  if (!is.null(expected$disallow_top_matches)) {
    bad_matches <- unlist(expected$disallow_top_matches)
    disallowed_match <- any(vapply(bad_matches, function(x) {
      is.character(top$iri[[1]]) && grepl(x, top$iri[[1]], fixed = TRUE)
    }, logical(1)))
  }

  list(disallowed_source = disallowed_source, disallowed_match = disallowed_match)
}

.build_fixture_candidates <- function(case) {
  if (!"candidates" %in% names(case)) {
    stop("fixture case missing candidates")
  }

  candidate_df <- dplyr::bind_rows(case$candidates)
  for (col in c("candidate_id", "role_hints", "zooma_confidence", "zooma_annotator", "alignment_only", "agreement_sources", "backend_score")) {
    if (!col %in% names(candidate_df)) {
      candidate_df[[col]] <- switch(
        col,
        candidate_id = as.character(seq_len(nrow(candidate_df))),
        role_hints = NA_character_,
        zooma_confidence = NA_character_,
        zooma_annotator = NA_character_,
        alignment_only = FALSE,
        agreement_sources = as.integer(1L),
        backend_score = 0,
        candidate_df[[col]]
      )
    }
  }

  candidate_df$alignment_only <- as.logical(candidate_df$alignment_only)
  candidate_df$agreement_sources <- as.integer(candidate_df$agreement_sources)
  candidate_df$backend_score <- as.numeric(candidate_df$backend_score)
  candidate_df$candidate_id <- as.character(candidate_df$candidate_id)
  candidate_df$zooma_confidence <- as.character(candidate_df$zooma_confidence)
  candidate_df$zooma_annotator <- as.character(candidate_df$zooma_annotator)

  candidate_df
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
