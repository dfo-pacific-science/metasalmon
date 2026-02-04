# Suggest facet schemes for proposed terms

Analyzes a proposed terms dataframe and suggests which facet schemes
(AgeClassScheme, LifePhaseScheme, etc.) should be created instead of
individual terms.

## Usage

``` r
suggest_facet_schemes(proposed_terms)
```

## Arguments

- proposed_terms:

  A data frame with term_label column

## Value

A tibble with suggested facet schemes and their member concepts

## Examples

``` r
if (FALSE) { # \dontrun{
proposed <- readr::read_csv("work/semantics/gpt_proposed_terms.csv")
facets <- suggest_facet_schemes(proposed)
print(facets)
} # }
```
