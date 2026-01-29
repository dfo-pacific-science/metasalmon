# Get recommended sources for a given role

Returns the optimal set of sources to query based on role. Implements
Phase 2 role-aware source selection.

## Usage

``` r
sources_for_role(role)
```

## Arguments

- role:

  I-ADOPT role (unit, property, entity, method, variable, constraint)

## Value

Character vector of recommended sources

## Examples

``` r
sources_for_role("unit")
#> [1] "qudt" "nvs"  "ols" 
# Returns: c("qudt", "nvs", "ols")

sources_for_role("entity")
#> [1] "gbif"      "worms"     "bioportal" "ols"      
# Returns: c("gbif", "worms", "ols")
```
