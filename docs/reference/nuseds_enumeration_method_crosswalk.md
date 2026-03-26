# NuSEDS enumeration method crosswalk

Return a static crosswalk of NuSEDS `ENUMERATION_METHODS` values to the
canonical enumeration method-family labels used by the Type 1–6
guidance.

## Usage

``` r
nuseds_enumeration_method_crosswalk()
```

## Value

A tibble with columns `nuseds_value`, `method_family`, `ontology_term`,
and `notes`.

## Details

The returned table tracks the legacy NuSEDS term, the canonical family
code, and linked ontology identifiers used in the current
implementation.

## See also

[`nuseds_estimate_method_crosswalk()`](https://dfo-pacific-science.github.io/metasalmon/reference/nuseds_estimate_method_crosswalk.md)

## Examples

``` r
nuseds_enumeration_method_crosswalk()
```
