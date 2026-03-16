# Benchmark semantic term ranking against fixture cases

Evaluate ranking quality across a curated fixture dataset to support
profile tuning.

## Usage

``` r
benchmark_term_ranking_fixtures(
  fixture_path = NULL,
  profiles = NULL,
  top_k = 3L,
  include_details = TRUE,
  fixture_path_override = NULL
)
```

## Arguments

- fixture_path:

  Path to semantic ranking fixture JSON. The file should contain a list
  of case objects with `query`, `role`, `expected`, and `candidates`
  fields.

- profiles:

  Named list of ranking profiles. Each element should be a list of
  overrides merged into the default profile used by
  `.score_and_rank_terms()`. If `NULL`, a single `baseline` profile is
  run.

- top_k:

  Optional top-k cutoff for top-k accuracy and hit position checks.

- include_details:

  Return per-case diagnostics table (TRUE by default).

- fixture_path_override:

  Optional preloaded fixture object. If provided, `fixture_path` is
  ignored and this value is used as the fixture list.

## Value

A list with `summary`, `per_case`, and `profiles`.
