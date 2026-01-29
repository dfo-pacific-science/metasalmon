# Embedding-based reranking (Phase 4 placeholder)

Optional reranking of term candidates using sentence embeddings. When
enabled via `METASALMON_EMBEDDING_RERANK=1`, applies cosine similarity
reranking over the top lexical candidates.

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

## Details

Current status: placeholder infrastructure. Full implementation
requires:

- Local embedding model (e.g., sentence-transformers via reticulate)

- Embedding cache to avoid repeated computation

- Configurable top-k for reranking (default: top 50 lexical candidates)
