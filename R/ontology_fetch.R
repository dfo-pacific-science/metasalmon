#' Fetch the Salmon Domain Ontology with caching
#'
#' Downloads the Salmon Domain Ontology using HTTP content negotiation and caches
#' the response using ETag / Last-Modified headers when available.
#'
#' @param url Ontology URL. Default is the canonical SMN namespace root.
#' @param accept Accept header; defaults to turtle with RDF/XML fallback.
#' @param cache_dir Directory to store cached ontology and headers. Defaults to
#'   a persistent user cache path.
#' @param fallback_urls Optional fallback ontology URLs tried if the primary `url` fails.
#' @param timeout_seconds Numeric timeout in seconds for each HTTP request.
#' @return Path to the cached ontology file (character string).
#' @export
fetch_salmon_ontology <- function(
    url = "https://w3id.org/smn/",
    accept = "text/turtle, application/rdf+xml;q=0.8",
    cache_dir = file.path(tools::R_user_dir("metasalmon", which = "cache"), "ontology"),
    timeout_seconds = 30,
    fallback_urls = c(
      "https://w3id.org/smn"
    )) {

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  ttl_file <- file.path(cache_dir, "salmon-ontology.ttl")
  etag_file <- file.path(cache_dir, "etag.txt")
  lastmod_file <- file.path(cache_dir, "last_modified.txt")

  headers <- c(Accept = accept)
  if (file.exists(etag_file)) {
    etag <- .ms_read_cached_header(etag_file)
    if (!is.na(etag)) {
      headers <- c(headers, `If-None-Match` = etag)
    }
  }
  if (file.exists(lastmod_file)) {
    last_modified <- .ms_read_cached_header(lastmod_file)
    if (!is.na(last_modified)) {
      headers <- c(headers, `If-Modified-Since` = last_modified)
    }
  }

  urls <- c(url, fallback_urls)
  res <- NULL
  last_error <- NULL

  for (u in urls) {
    res <- try(
      httr::GET(
        u,
        httr::add_headers(.headers = headers),
        httr::timeout(timeout_seconds),
        httr::config(connecttimeout = timeout_seconds)
      ),
      silent = TRUE
    )
    if (inherits(res, "try-error")) {
      last_error <- res
      res <- NULL
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
    if (file.exists(ttl_file)) {
      cli::cli_warn(c(
        "Failed to refresh Salmon ontology; using cached copy at {.path {ttl_file}}.",
        "i" = "Last fetch error: {last_error}"
      ))
      return(ttl_file)
    }
    stop("Failed to fetch ontology from provided URLs: ", paste(urls, collapse = ", "),
         "; last error: ", last_error)
  }

  if (httr::status_code(res) == 304 && file.exists(ttl_file)) {
    return(ttl_file)
  }

  httr::stop_for_status(res)
  content <- httr::content(res, as = "text", encoding = "UTF-8")
  temp_ttl <- tempfile(tmpdir = cache_dir, fileext = ".ttl")
  on.exit(unlink(temp_ttl, force = TRUE), add = TRUE)
  writeLines(content, temp_ttl, useBytes = TRUE)
  if (!file.rename(temp_ttl, ttl_file)) {
    cli::cli_abort("Failed to update cached ontology file at {.path {ttl_file}}.")
  }

  etag <- httr::headers(res)[["etag"]]
  if (!is.null(etag) && nzchar(etag)) writeLines(etag, etag_file, useBytes = TRUE)
  lastmod <- httr::headers(res)[["last-modified"]]
  if (!is.null(lastmod) && nzchar(lastmod)) writeLines(lastmod, lastmod_file, useBytes = TRUE)

  ttl_file
}

.ms_read_cached_header <- function(path) {
  value <- readLines(path, warn = FALSE, n = 1)
  if (length(value) == 0) {
    return(NA_character_)
  }
  value <- trimws(value[[1]])
  if (!nzchar(value)) {
    return(NA_character_)
  }
  value
}
