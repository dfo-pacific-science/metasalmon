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
#> # A tibble: 27 × 5
#>    nuseds_value         method_family guidance_interpretat…¹ ontology_term notes
#>    <chr>                <chr>         <chr>                  <chr>         <chr>
#>  1 Fixed Site Census    FS            Enumeration device/mo… gcdfo:FixedS… ""   
#>  2 Resistivity Counter  FS            Enumeration device/mo… gcdfo:FixedS… "Enu…
#>  3 Video Counter        FS            Enumeration device/mo… gcdfo:FixedS… "Enu…
#>  4 Mark & Recapture: B… M             Mark-recapture estima… gcdfo:MarkRe… ""   
#>  5 Mark & Recapture: J… M             Mark-recapture estima… gcdfo:MarkRe… ""   
#>  6 Mark & Recapture: O… M             Mark-recapture estima… gcdfo:MarkRe… ""   
#>  7 Mark & Recapture: P… M             Mark-recapture estima… gcdfo:MarkRe… ""   
#>  8 Cumulative CPUE      P             CPUE index             gcdfo:Estima… "No …
#>  9 Redd Count           R             Redd-based estimation… gcdfo:ReddEx… ""   
#> 10 Sonar-ARIS           S             Hydroacoustic modelli… gcdfo:Hydroa… ""   
#> # ℹ 17 more rows
#> # ℹ abbreviated name: ¹​guidance_interpretation
```
