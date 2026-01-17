# Find ICES codes within a code type by text match

Find ICES codes within a code type by text match

## Usage

``` r
ices_find_codes(query, code_type, max_results = 50)
```

## Arguments

- query:

  Search string matched against `key`, `description`, and
  `longDescription`.

- code_type:

  ICES code type key (e.g., `"Gear"`).

- max_results:

  Maximum number of rows to return (default 50).

## Value

Filtered tibble of codes for the given code type.
