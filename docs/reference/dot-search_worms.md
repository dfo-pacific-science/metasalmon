# Search WoRMS for marine species entities

World Register of Marine Species - authoritative for marine taxa. Useful
for entity role when dealing with marine species (salmon, etc.).

## Usage

``` r
.search_worms(query, role)
```

## Arguments

- query:

  Search query string (taxon name)

- role:

  I-ADOPT role (typically "entity")

## Value

Tibble of matching marine species
