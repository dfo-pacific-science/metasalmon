# Embedding/ranking re-rank utility (Phase 4)

Optional semantic reranking stage that uses lightweight text-similarity
when `METASALMON_EMBEDDING_RERANK=1` is set. This is deterministic and
dependency-light (no Python model required), and it adds a reusable
`embedding_score` field that can later be replaced with true vector
embeddings without changing callers.

## Usage

``` r
.apply_embedding_rerank(df, query, top_k = 50L)
```

## Arguments

- df:

  Data frame of term results with score column

- query:

  Original search query

- top_k:

  Number of top candidates to rerank (default 50)

## Value

Data frame with optional embedding_score column
