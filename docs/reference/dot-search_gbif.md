# Search GBIF Backbone Taxonomy for taxon entities

Useful for entity role when the entity is a species/taxon. Uses GBIF
Species API to match taxon names.

## Usage

``` r
.search_gbif(query, role)
```

## Arguments

- query:

  Search query string (taxon name)

- role:

  I-ADOPT role (typically "entity")

## Value

Tibble of matching taxa
