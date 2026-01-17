# Find ICES code types by text match

Find ICES code types by text match

## Usage

``` r
ices_find_code_types(query, max_results = 20)
```

## Arguments

- query:

  Search string matched against `key`, `description`, and
  `longDescription`.

- max_results:

  Maximum number of rows to return (default 20).

## Value

Filtered tibble of code types.
