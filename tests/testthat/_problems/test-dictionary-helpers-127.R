# Extracted from test-dictionary-helpers.R:127

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "metasalmon", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
queries <- list()
fake_search <- function(query, role, sources = NULL) {
    queries <<- append(queries, list(list(query = query, role = role)))
    tibble::tibble(
      label = "x",
      iri = "y",
      source = "ols",
      ontology = "",
      role = role,
      match_type = "",
      definition = ""
    )
  }
dict <- tibble::tibble(
    dataset_id = "d",
    table_id = "t",
    column_name = "MEAS",
    column_label = "Spawner count",
    column_description = "Spawner count estimate",
    column_role = "measurement",
    value_type = "number",
    unit_label = "fish",
    unit_iri = NA_character_,
    term_iri = NA_character_,
    property_iri = NA_character_,
    entity_iri = NA_character_,
    constraint_iri = NA_character_,
    method_iri = NA_character_
  )
suggest_semantics(NULL, dict, sources = "ols", max_per_role = 1, search_fn = fake_search)
unit_queries <- purrr::map_chr(queries, "query")
expect_true(any(unit_queries == "fish"))
