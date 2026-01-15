# Validate a salmon data dictionary

Validates a dictionary tibble against the salmon data package schema.
Checks required columns, value types, required flags, and optionally
validates IRIs. Reports issues using `cli` messaging.

## Usage

``` r
validate_dictionary(dict, require_iris = FALSE)
```

## Arguments

- dict:

  A tibble or data.frame with dictionary schema columns

- require_iris:

  Logical; if `TRUE`, requires non-empty IRIs for semantic fields
  (default: `FALSE`). Measurement columns always require `term_iri`,
  `property_iri`, `entity_iri`, and `unit_iri`.

## Value

Invisibly returns the normalized dictionary if valid; otherwise raises
errors with clear messages

## Examples

``` r
if (FALSE) { # \dontrun{
dict <- infer_dictionary(mtcars)
validate_dictionary(dict)
} # }
```
