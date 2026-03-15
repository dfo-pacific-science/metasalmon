# Fetch the Salmon Domain Ontology with caching

Downloads the Salmon Domain Ontology using HTTP content negotiation and
caches the response using ETag / Last-Modified headers when available.

## Usage

``` r
fetch_salmon_ontology(
  url = "https://w3id.org/smn/",
  accept = "text/turtle, application/rdf+xml;q=0.8",
  cache_dir = file.path(tempdir(), "metasalmon-ontology-cache"),
  fallback_urls = c("https://w3id.org/smn",
    "https://dfo-pacific-science.github.io/salmon-domain-ontology/smn.ttl")
)
```

## Arguments

- url:

  Ontology URL. Default is the canonical SMN namespace root.

- accept:

  Accept header; defaults to turtle with RDF/XML fallback.

- cache_dir:

  Directory to store cached ontology and headers.

- fallback_urls:

  Optional fallback ontology URLs tried if the primary `url` fails.

## Value

Path to the cached ontology file (character string).
