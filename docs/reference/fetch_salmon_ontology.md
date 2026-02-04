# Fetch the DFO Salmon Ontology with caching

Downloads the DFO Salmon Ontology using HTTP content negotiation and
caches the response using ETag / Last-Modified headers when available.

## Usage

``` r
fetch_salmon_ontology(
  url =
    "https://dfo-pacific-science.github.io/dfo-salmon-ontology/ontology/dfo-salmon.ttl",
  accept = "text/turtle, application/rdf+xml;q=0.8",
  cache_dir = file.path(tempdir(), "metasalmon-ontology-cache"),
  fallback_urls = c("https://w3id.org/gcdfo/salmon")
)
```

## Arguments

- url:

  Ontology URL. Default is the published TTL on GitHub Pages.

- accept:

  Accept header; defaults to turtle with RDF/XML fallback.

- cache_dir:

  Directory to store cached ontology and headers.

## Value

Path to the cached ontology file (character string).
