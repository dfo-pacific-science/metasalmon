# Extracted from test-term-search.R:54

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "metasalmon", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
url_called <- NULL
headers_called <- NULL
bindings <- data.frame(row = 1, stringsAsFactors = FALSE)
bindings$row <- NULL
bindings$uri <- data.frame(type = "uri", value = "http://vocab.nerc.ac.uk/collection/P06/current/XXXX/")
bindings$label <- data.frame(type = "literal", value = "fish")
bindings$definition <- data.frame(type = "literal", value = "A unit-like placeholder")
fake <- list(results = list(bindings = bindings))
res <- with_mocked_bindings(
    .safe_json = function(url, headers = NULL, timeout_secs = 30) {
      url_called <<- url
      headers_called <<- headers
      fake
    },
    find_terms("fish", sources = "nvs")
  )
expect_match(url_called, "vocab\\.nerc\\.ac\\.uk/sparql/")
expect_true(grepl("P01", url_called, fixed = TRUE))
expect_true(grepl("P06", url_called, fixed = TRUE))
expect_true(is.character(headers_called))
expect_equal(headers_called[["Accept"]], "application/sparql-results+json")
