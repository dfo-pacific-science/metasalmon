#' Fetch the DFO Salmon Ontology with caching
#'
#' Downloads the DFO Salmon Ontology using HTTP content negotiation and caches
#' the response using ETag / Last-Modified headers when available.
#'
#' @param url Ontology URL. Default is the published TTL on GitHub Pages.
#' @param accept Accept header; defaults to turtle with RDF/XML fallback.
#' @param cache_dir Directory to store cached ontology and headers.
#' @return Path to the cached ontology file (character string).
#' @export
fetch_salmon_ontology <- function(
    url = "https://dfo-pacific-science.github.io/dfo-salmon-ontology/ontology/dfo-salmon.ttl",
    accept = "text/turtle, application/rdf+xml;q=0.8",
    cache_dir = file.path(tempdir(), "metasalmon-ontology-cache"),
    fallback_urls = c("https://w3id.org/gcdfo/salmon")) {

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  ttl_file <- file.path(cache_dir, "dfo-salmon.ttl")
  etag_file <- file.path(cache_dir, "etag.txt")
  lastmod_file <- file.path(cache_dir, "last_modified.txt")

  headers <- c(Accept = accept)
  if (file.exists(etag_file)) {
    headers <- c(headers, `If-None-Match` = readLines(etag_file, warn = FALSE))
  }
  if (file.exists(lastmod_file)) {
    headers <- c(headers, `If-Modified-Since` = readLines(lastmod_file, warn = FALSE))
  }

  urls <- c(url, fallback_urls)
  res <- NULL
  last_error <- NULL

  for (u in urls) {
    res <- try(httr::GET(u, httr::add_headers(.headers = headers)), silent = TRUE)
    if (inherits(res, "try-error")) {
      last_error <- res
      next
    }
    if (httr::status_code(res) %in% c(200, 304)) {
      break
    } else {
      last_error <- res
      res <- NULL
    }
  }

  if (is.null(res)) {
    stop("Failed to fetch ontology from provided URLs: ", paste(urls, collapse = ", "),
         "; last error: ", last_error)
  }

  if (httr::status_code(res) == 304 && file.exists(ttl_file)) {
    return(ttl_file)
  }

  httr::stop_for_status(res)
  content <- httr::content(res, as = "text", encoding = "UTF-8")
  writeLines(content, ttl_file)

  etag <- httr::headers(res)[["etag"]]
  if (!is.null(etag)) writeLines(etag, etag_file)
  lastmod <- httr::headers(res)[["last-modified"]]
  if (!is.null(lastmod)) writeLines(lastmod, lastmod_file)

  ttl_file
}

#' Parse ontology terms from a Turtle (TTL) file
#'
#' Extracts term IRIs, labels, definitions, and types from a Turtle-format
#' ontology file. Designed for the DFO Salmon Ontology but works with any TTL
#' file that uses `rdfs:label`, `skos:prefLabel`, `skos:definition`, and/or
#' `iao:0000115` (IAO definition) annotation properties.
#'
#' The parser is regex-based and does **not** require an RDF library. It
#' resolves `gcdfo:` and `:` prefixed local names to full IRIs using the
#' `@prefix` declarations found in the file.
#'
#' @param ttl_path Path to a Turtle (.ttl) file.
#' @return A tibble with columns:
#'   \describe{
#'     \item{iri}{Full IRI of the term (character).}
#'     \item{local_name}{Local name without namespace prefix (character).}
#'     \item{label}{Human-readable label from `rdfs:label` or `skos:prefLabel` (character).}
#'     \item{definition}{Definition from `skos:definition` or `iao:0000115` (character).}
#'     \item{term_type}{OWL/SKOS type: `"owl:Class"`, `"skos:Concept"`,
#'       `"skos:ConceptScheme"`, `"owl:ObjectProperty"`,
#'       `"owl:DatatypeProperty"`, `"owl:AnnotationProperty"`, or
#'       `"other"` (character).}
#'   }
#' @export
#' @examples
#' \dontrun{
#' ttl_path <- fetch_salmon_ontology()
#' terms <- parse_ontology_terms(ttl_path)
#' head(terms)
#' }
parse_ontology_terms <- function(ttl_path) {
  if (!file.exists(ttl_path)) {
    cli::cli_abort("TTL file not found: {.path {ttl_path}}")
  }
  lines <- readLines(ttl_path, warn = FALSE)
  full_text <- paste(lines, collapse = "\n")

  # ── Resolve prefixes ──
  prefix_pattern <- "@prefix\\s+(\\S+:?)\\s+<([^>]+)>\\s*\\."
  prefix_matches <- regmatches(full_text, gregexpr(prefix_pattern, full_text, perl = TRUE))[[1]]
  prefixes <- list()
  for (pm in prefix_matches) {
    parts <- regmatches(pm, regexec(prefix_pattern, pm, perl = TRUE))[[1]]
    prefixes[[parts[2]]] <- parts[3]
  }

  # Expand a prefixed name to a full IRI
  expand_iri <- function(curie) {
    curie <- trimws(curie)
    if (grepl("^<.*>$", curie)) {
      return(sub("^<", "", sub(">$", "", curie)))
    }
    parts <- regmatches(curie, regexec("^([^:]*:)(.+)$", curie))[[1]]
    if (length(parts) == 3 && parts[2] %in% names(prefixes)) {
      return(paste0(prefixes[[parts[2]]], parts[3]))
    }
    curie
  }

  # ── Split into subject blocks ──
  # A subject block starts with a non-whitespace token at column 1 and may span

  # multiple lines until the next subject or end of file.  The trailing "." in
 # Turtle delimits statements.
  gcdfo_ns <- prefixes[["gcdfo:"]] %||% "https://w3id.org/gcdfo/salmon#"
  default_ns <- prefixes[[":"]] %||% gcdfo_ns

  # We only care about terms in the gcdfo/default namespace
  target_ns <- unique(c(gcdfo_ns, default_ns))

  # Split on subject declarations (lines starting with a non-space token that
 # has a " a " type assertion or property)
  subject_re <- "(?:^|\\n)((?:gcdfo:|:)[A-Za-z][^\\s]*)"
  subject_positions <- gregexpr(subject_re, full_text, perl = TRUE)[[1]]
  subject_curies <- regmatches(full_text, gregexpr(subject_re, full_text, perl = TRUE))[[1]]
  subject_curies <- trimws(subject_curies)

  if (length(subject_curies) == 0) {
    return(tibble::tibble(
      iri = character(), local_name = character(),
      label = character(), definition = character(),
      term_type = character()
    ))
  }

  # For each subject, extract the block of text from that subject to the next
  block_starts <- as.integer(subject_positions)
  block_ends <- c(block_starts[-1] - 1L, nchar(full_text))

  results <- vector("list", length(subject_curies))
  for (idx in seq_along(subject_curies)) {
    curie <- subject_curies[idx]
    block <- substr(full_text, block_starts[idx], block_ends[idx])

    iri <- expand_iri(curie)
    local_name <- sub("^.*[#/]", "", iri)

    # ── type ──
    type_val <- "other"
    if (grepl("\\ba\\s+owl:Class\\b", block)) type_val <- "owl:Class"
    else if (grepl("\\ba\\s+owl:ObjectProperty\\b", block)) type_val <- "owl:ObjectProperty"
    else if (grepl("\\ba\\s+owl:DatatypeProperty\\b", block)) type_val <- "owl:DatatypeProperty"
    else if (grepl("\\ba\\s+owl:AnnotationProperty\\b", block)) type_val <- "owl:AnnotationProperty"
    else if (grepl("\\ba\\s+skos:ConceptScheme\\b", block)) type_val <- "skos:ConceptScheme"
    else if (grepl("\\ba\\s+skos:Concept\\b", block) ||
             grepl("\\ba\\s+gcdfo:\\w+\\s*,\\s*skos:Concept\\b", block)) type_val <- "skos:Concept"

    # ── label (prefer skos:prefLabel, fallback to rdfs:label) ──
    label_val <- .ttl_extract_literal(block, "skos:prefLabel")
    if (is.na(label_val)) label_val <- .ttl_extract_literal(block, "rdfs:label")
    if (is.na(label_val)) label_val <- NA_character_

    # ── definition (prefer skos:definition, fallback to iao:0000115) ──
    def_val <- .ttl_extract_literal(block, "skos:definition")
    if (is.na(def_val)) def_val <- .ttl_extract_literal(block, "iao:0000115")
    if (is.na(def_val)) def_val <- NA_character_

    results[[idx]] <- list(
      iri = iri, local_name = local_name,
      label = label_val, definition = def_val,
      term_type = type_val
    )
  }

  tibble::as_tibble(do.call(rbind.data.frame, c(results, stringsAsFactors = FALSE)))
}

#' Extract a string literal value from a Turtle property in a block
#'
#' @param block Character; a chunk of TTL text for one subject.
#' @param property Character; the predicate to search for (e.g. `"rdfs:label"`).
#' @return The first literal value (character), or `NA_character_` if not found.
#' @keywords internal
.ttl_extract_literal <- function(block, property) {
  # Match patterns like:  property "value"@en  or  property "value"
  escaped_prop <- gsub(":", "\\:", property, fixed = TRUE)
  pattern <- paste0(
    "(?:^|\\s)", escaped_prop, "\\s+\"((?:[^\"\\\\]|\\\\.)*)\"(?:@\\w+)?"
  )
  m <- regmatches(block, regexec(pattern, block, perl = TRUE))[[1]]
  if (length(m) >= 2 && nzchar(m[2])) {
    return(m[2])
  }
  NA_character_
}
