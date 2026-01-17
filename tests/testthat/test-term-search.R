test_that("find_terms returns empty tibble when sources empty", {
  res <- find_terms("escapement", sources = character(0))
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0)
  expect_true(all(c("label", "iri", "source", "ontology", "role", "match_type", "definition", "alignment_only") %in% names(res)))
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

# ============================================================================
# Phase 2 Tests: Ontology Preferences by Role
# ============================================================================

test_that("sources_for_role returns appropriate sources for each role", {
  expect_equal(sources_for_role("unit"), c("qudt", "nvs", "ols"))
  expect_equal(sources_for_role("entity"), c("gbif", "worms", "ols"))
  expect_equal(sources_for_role("property"), c("nvs", "ols", "zooma"))
  expect_equal(sources_for_role("method"), c("ols", "zooma"))
  expect_equal(sources_for_role("variable"), c("nvs", "ols", "zooma"))
  expect_equal(sources_for_role("constraint"), c("ols"))
  # Default fallback
  expect_equal(sources_for_role(NA), c("ols", "nvs"))
  expect_equal(sources_for_role(""), c("ols", "nvs"))
})

test_that("find_terms uses QUDT SPARQL endpoint for units", {
  url_called <- NULL
  bindings <- list(
    list(uri = list(value = "http://qudt.org/vocab/unit/KiloGM"),
         label = list(value = "Kilogram"),
         definition = list(value = "SI unit of mass"))
  )
  fake <- list(results = list(bindings = bindings))

  res <- with_mocked_bindings(
    .safe_json = function(url, headers = NULL, timeout_secs = 30) {
      url_called <<- url
      fake
    },
    find_terms("kilogram", sources = "qudt")
  )

  expect_match(url_called, "qudt\\.org")
  expect_match(url_called, "sparql")
  expect_gte(nrow(res), 1)
  expect_equal(res$source[[1]], "qudt")
  expect_match(res$iri[[1]], "qudt\\.org")
})

test_that("find_terms uses GBIF API for entity taxa", {
  url_called <- NULL
  fake <- list(
    usageKey = 5206141,
    scientificName = "Oncorhynchus kisutch (Walbaum, 1792)",
    canonicalName = "Oncorhynchus kisutch",
    rank = "SPECIES",
    kingdom = "Animalia",
    phylum = "Chordata",
    class = "Actinopterygii",
    order = "Salmoniformes",
    family = "Salmonidae"
  )

  res <- with_mocked_bindings(
    .safe_json = function(url, headers = NULL, timeout_secs = 30) {
      url_called <<- url
      fake
    },
    find_terms("Oncorhynchus kisutch", sources = "gbif")
  )

  expect_match(url_called, "api\\.gbif\\.org")
  expect_gte(nrow(res), 1)
  expect_equal(res$source[[1]], "gbif")
  expect_match(res$iri[[1]], "gbif\\.org/species")
  expect_match(res$definition[[1]], "Salmonidae")
})

test_that("find_terms uses WoRMS API for marine species", {
  url_called <- NULL
  fake <- data.frame(
    AphiaID = 291984,
    scientificname = "Oncorhynchus kisutch",
    rank = "Species",
    kingdom = "Animalia",
    phylum = "Chordata",
    class = "Actinopteri",
    order = "Salmoniformes",
    family = "Salmonidae",
    stringsAsFactors = FALSE
  )

  res <- with_mocked_bindings(
    .safe_json = function(url, headers = NULL, timeout_secs = 30) {
      url_called <<- url
      fake
    },
    find_terms("Oncorhynchus kisutch", sources = "worms")
  )

  expect_match(url_called, "marinespecies\\.org")
  expect_gte(nrow(res), 1)
  expect_equal(res$source[[1]], "worms")
  expect_match(res$iri[[1]], "marinespecies\\.org")
})

test_that("score_and_rank_terms applies role-based ontology preferences", {
  vocab <- metasalmon:::`.iadopt_vocab`()
  df <- tibble::tibble(
    label = c("Generic unit", "QUDT kilogram", "NVS unit"),
    iri = c(
      "http://example.org/unit",
      "http://qudt.org/vocab/unit/KiloGM",
      "http://vocab.nerc.ac.uk/collection/P06/current/UPID/"
    ),
    source = c("ols", "qudt", "nvs"),
    ontology = c("generic", "qudt", "nvs"),
    role = "unit",
    match_type = "",
    definition = ""
  )

  ranked <- metasalmon:::`.score_and_rank_terms`(df, "unit", vocab)
  # QUDT should rank highest for unit role
  expect_equal(ranked$source[[1]], "qudt")
  expect_match(ranked$iri[[1]], "qudt\\.org")
})

test_that("score_and_rank_terms penalizes Wikidata as alignment-only", {
  vocab <- metasalmon:::`.iadopt_vocab`()
  df <- tibble::tibble(
    label = c("Salmon (Wikidata)", "Salmon (OLS)"),
    iri = c(
      "http://www.wikidata.org/entity/Q34134",
      "http://purl.obolibrary.org/obo/NCBITaxon_8030"
    ),
    source = c("ols", "ols"),
    ontology = c("wikidata", "ncbitaxon"),
    role = "entity",
    match_type = "",
    definition = ""
  )

  ranked <- metasalmon:::`.score_and_rank_terms`(df, "entity", vocab)
  # Wikidata should have alignment_only = TRUE
  wikidata_row <- which(grepl("wikidata", ranked$iri))
  expect_true(ranked$alignment_only[[wikidata_row]])
  # Non-Wikidata should rank higher
  expect_false(grepl("wikidata", ranked$iri[[1]]))
})

test_that("role_preferences loads ontology-preferences.csv", {
  prefs <- metasalmon:::`.role_preferences`()
  expect_s3_class(prefs, "tbl_df")
  expect_true("role" %in% names(prefs))
  expect_true("ontology" %in% names(prefs))
  expect_true("priority" %in% names(prefs))
  expect_true("alignment_only" %in% names(prefs))
  # Should have entries for key roles
  expect_true("unit" %in% prefs$role)
  expect_true("entity" %in% prefs$role)
  expect_true("property" %in% prefs$role)
})

test_that("QUDT is preferred for unit role", {
  prefs <- metasalmon:::`.role_preferences`()
  unit_prefs <- dplyr::filter(prefs, role == "unit")
  expect_true(nrow(unit_prefs) > 0)
  # QUDT should have priority 1
  qudt_pref <- dplyr::filter(unit_prefs, ontology == "qudt")
  expect_equal(qudt_pref$priority[[1]], 1)
})

test_that("GBIF and WoRMS are preferred for entity role", {
  prefs <- metasalmon:::`.role_preferences`()
  entity_prefs <- dplyr::filter(prefs, role == "entity")
  expect_true("gbif" %in% entity_prefs$ontology)
  expect_true("worms" %in% entity_prefs$ontology)
})
