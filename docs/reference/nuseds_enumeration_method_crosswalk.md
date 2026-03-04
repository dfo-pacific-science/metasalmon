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
#> # A tibble: 25 × 4
#>    nuseds_value            method_family ontology_term                   notes  
#>    <chr>                   <chr>         <chr>                           <chr>  
#>  1 Fixed Wing Aircraft     A             gcdfo:AerialSurveyCount         ""     
#>  2 Helicopter              A             gcdfo:AerialSurveyCount         ""     
#>  3 Broodstock Removal      FS            gcdfo:FixedSiteCensusManual     ""     
#>  4 Electronic Counters     FS            gcdfo:FixedSiteCensusElectronic ""     
#>  5 Enumeration by Hatchery FS            gcdfo:FixedSiteCensusManual     ""     
#>  6 Fence                   FS            gcdfo:FixedSiteCensusManual     ""     
#>  7 Tag Recovery            M             gcdfo:MarkRecaptureFieldProgram ""     
#>  8 Based on Angling Catch  P             gcdfo:EnumerationMethod         "Catch…
#>  9 Electroshocking         P             gcdfo:ElectrofishingCount       ""     
#> 10 Redd Counts             R             gcdfo:ReddCount                 ""     
#> # ℹ 15 more rows
```
