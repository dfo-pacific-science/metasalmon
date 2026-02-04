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
