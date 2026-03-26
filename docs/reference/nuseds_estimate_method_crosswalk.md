# NuSEDS estimate method crosswalk

Return a static crosswalk of NuSEDS `ESTIMATE_METHOD` values to the
canonical estimate-method families used by the Type 1–6 guidance.

## Usage

``` r
nuseds_estimate_method_crosswalk()
```

## Value

A tibble with columns `nuseds_value`, `method_family`,
`guidance_interpretation`, `ontology_term`, and `notes`.

## Details

The returned table tracks the legacy NuSEDS term, the canonical family
label, and linked ontology identifiers used in the current
implementation.

## Examples

``` r
nuseds_estimate_method_crosswalk()
```
