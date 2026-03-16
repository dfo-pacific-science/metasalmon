# Apply cross-source agreement boosting (Phase 4)

Boosts terms that appear from multiple sources, indicating higher
confidence. IRI agreement (same IRI from different sources) gets higher
boost than label-only agreement (same label, different IRIs).

## Usage

``` r
.apply_cross_source_agreement(df, iri_boost = 0.5, label_boost = 0.2)
```

## Arguments

- df:

  Data frame of term results with score column

- iri_boost:

  Per-additional-source boost when IRI matches

- label_boost:

  Per-additional-source boost when only label matches

## Value

Data frame with agreement boosts applied and agreement_sources column
