# Apply cross-source agreement boosting (Phase 4)

Boosts terms that appear from multiple sources, indicating higher
confidence. IRI agreement (same IRI from different sources) gets higher
boost than label-only agreement (same label, different IRIs).

## Usage

``` r
.apply_cross_source_agreement(df)
```

## Arguments

- df:

  Data frame of term results with score column

## Value

Data frame with agreement boosts applied and agreement_sources column
