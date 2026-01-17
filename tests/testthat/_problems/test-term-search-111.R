# Extracted from test-term-search.R:111

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "metasalmon", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
df <- tibble::tibble(
    label = c("Spawner count", "Natural killer cell"),
    iri = c("http://example.org/a", "http://example.org/b"),
    source = c("ols", "ols"),
    ontology = c("o1", "o1"),
    role = NA_character_,
    match_type = "",
    definition = ""
  )
ranked <- metasalmon:::`.score_and_rank_terms`(df, NA_character_, tibble::tibble(), "spawner count")
