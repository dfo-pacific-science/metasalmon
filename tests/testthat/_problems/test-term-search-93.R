# Extracted from test-term-search.R:93

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "metasalmon", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
urls <- character()
links <- data.frame(
    olslinks = I(list(data.frame(
      href = "https://www.ebi.ac.uk/ols4/api/terms?iri=http%3A%2F%2Fexample.org%2Fterm",
      semanticTag = "http://example.org/term",
      stringsAsFactors = FALSE
    ))),
    stringsAsFactors = FALSE
  )
fake_zooma <- data.frame(confidence = "MEDIUM", stringsAsFactors = FALSE)
fake_zooma$`_links` <- links
term_df <- data.frame(
    iri = "http://example.org/term",
    label = "Spawner count",
    ontology_name = "demo",
    description = I(list(c("A demo definition"))),
    stringsAsFactors = FALSE
  )
fake_ols_term <- list()
fake_ols_term$`_embedded` <- list(terms = term_df)
res <- with_mocked_bindings(
    .safe_json = function(url, headers = NULL, timeout_secs = 30) {
      urls <<- c(urls, url)
      if (grepl("zooma", url, fixed = TRUE)) return(fake_zooma)
      if (grepl("ols4/api/terms", url, fixed = TRUE)) return(fake_ols_term)
      NULL
    },
    find_terms("spawner count", sources = "zooma")
  )
expect_true(any(grepl("zooma", urls, fixed = TRUE)))
