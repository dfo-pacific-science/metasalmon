test_that("find_terms returns empty tibble when sources empty", {
  res <- find_terms("escapement", sources = character(0))
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0)
  expect_true(all(c("label", "iri", "source", "ontology", "role", "match_type", "definition") %in% names(res)))
})

test_that("find_terms includes OLS rows hint and uses User-Agent", {
  url_called <- NULL
  fake <- list(response = list(docs = list(
    label = "Spawner count",
    iri = "http://example.org/count",
    ontology_name = "test",
    description = list("desc"),
    type = "class"
  )))

  res <- with_mocked_bindings(
    .safe_json = function(url, headers = NULL, timeout_secs = 30) {
      url_called <<- url
      fake
    },
    find_terms("spawner count", sources = "ols")
  )

  expect_true(grepl("rows=50", url_called, fixed = TRUE))
  expect_s3_class(res, "tbl_df")
  expect_gte(nrow(res), 1)
})

test_that("find_terms uses NVS SPARQL endpoint", {
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
  expect_gte(nrow(res), 1)
  expect_equal(res$source[[1]], "nvs")
})

test_that("find_terms uses ZOOMA annotations and resolves OLS term metadata", {
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
  expect_true(any(grepl("ols4/api/terms", urls, fixed = TRUE)))
  expect_equal(res$source[[1]], "zooma")
  expect_equal(res$iri[[1]], "http://example.org/term")
  expect_equal(res$match_type[[1]], "zooma_medium")
})

test_that("score_and_rank_terms boosts label overlap with query tokens", {
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
  expect_equal(ranked$label[[1]], "Spawner count")
})

test_that("score_and_rank_terms boosts I-ADOPT vocab matches for role", {
  vocab <- metasalmon:::`.iadopt_vocab`()
  df <- tibble::tibble(
    label = c("Generic unit", "BODC unit", "Another unit"),
    iri = c(
      "http://example.org/unit",
      "http://vocab.nerc.ac.uk/collection/P06/current/UPID/",
      "http://example.org/bodc_units"
    ),
    source = c("ols", "nvs", "ols"),
    ontology = c("generic", "bodc_units", "bodc_units"),
    role = NA_character_,
    match_type = "",
    definition = ""
  )

  ranked <- metasalmon:::`.score_and_rank_terms`(df, "unit", vocab)
  expect_equal(ranked$source[[1]], "nvs")
  expect_match(ranked$iri[[1]], "vocab\\.nerc\\.ac\\.uk")
})

test_that("score_and_rank_terms is deterministic on ties", {
  df <- tibble::tibble(
    label = c("B label", "A label"),
    iri = c("http://example.org/b", "http://example.org/a"),
    source = c("ols", "ols"),
    ontology = c("obs", "obs"),
    role = NA_character_,
    match_type = "",
    definition = ""
  )

  ranked <- metasalmon:::`.score_and_rank_terms`(df, NA_character_, tibble::tibble())
  expect_equal(ranked$label, c("A label", "B label"))
})
