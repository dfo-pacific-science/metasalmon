# List ICES codes for a code type

List ICES codes for a code type

## Usage

``` r
ices_codes(code_type, code = "", modified = "")
```

## Arguments

- code_type:

  ICES code type key or GUID (e.g., `"Gear"`).

- code:

  Optional code key or GUID to filter the API response.

- modified:

  Optional date string (`"YYYY-MM-DD"`) to return codes modified after
  that date.

## Value

Tibble of ICES codes for the requested code type. Adds a `code_type`
column and a `url` column pointing at the corresponding `CodeDetail` API
endpoint.
