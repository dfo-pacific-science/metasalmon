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
  expect_equal(sources_for_role("entity"), c("gbif", "worms", "bioportal", "ols"))
  expect_equal(sources_for_role("property"), c("nvs", "ols", "zooma"))
  expect_equal(sources_for_role("method"), c("bioportal", "ols", "zooma"))
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

test_that("Entity role preferences include ODO and taxon resolvers", {
  prefs <- metasalmon:::`.role_preferences`()
  entity_prefs <- dplyr::filter(prefs, role == "entity")
  gcdfo_pref <- dplyr::filter(entity_prefs, ontology == "gcdfo")
  expect_true(nrow(gcdfo_pref) > 0)
  expect_equal(gcdfo_pref$priority[[1]], 1)
  odo_pref <- dplyr::filter(entity_prefs, ontology == "odo")
  expect_true(nrow(odo_pref) > 0)
  expect_true("gbif" %in% entity_prefs$ontology)
  expect_true("worms" %in% entity_prefs$ontology)
})

# ============================================================================
# Phase 4 Tests: Matching Quality Features
# ============================================================================

test_that("cross-source agreement boosts identical IRIs", {
  # Create mock results with same IRI from different sources
  mock_df <- tibble::tibble(
    label = c("temperature", "temperature", "salinity"),
    iri = c("http://example.org/temp", "http://example.org/temp", "http://example.org/sal"),
    source = c("ols", "nvs", "ols"),
    ontology = c("envo", "p01", "envo"),
    role = NA_character_,
    match_type = c("label", "label", "label"),
    definition = c("def1", "def2", "def3"),
    score = c(1.0, 1.0, 1.0)
  )

  result <- metasalmon:::.apply_cross_source_agreement(mock_df)

  # The IRI that appears in 2 sources should have higher agreement_sources
  temp_rows <- result[result$iri == "http://example.org/temp", ]
  sal_rows <- result[result$iri == "http://example.org/sal", ]

  expect_equal(temp_rows$agreement_sources[[1]], 2L)
  expect_equal(sal_rows$agreement_sources[[1]], 1L)

  # Score boost should be applied for IRI agreement (+0.5 per additional source)
  expect_gt(temp_rows$score[[1]], sal_rows$score[[1]])
})

test_that("cross-source agreement handles label-only matches", {
  # Create mock results with same label but different IRIs
  mock_df <- tibble::tibble(
    label = c("count", "count", "abundance"),
    iri = c("http://a.org/count", "http://b.org/count", "http://a.org/abund"),
    source = c("ols", "nvs", "ols"),
    ontology = c("ont1", "ont2", "ont1"),
    role = NA_character_,
    match_type = c("label", "label", "label"),
    definition = c("def1", "def2", "def3"),
    score = c(1.0, 1.0, 1.0)
  )

  result <- metasalmon:::.apply_cross_source_agreement(mock_df)

  # Both "count" rows should have label agreement = 2
  count_rows <- result[result$label == "count", ]
  expect_equal(count_rows$agreement_sources[[1]], 2L)
})

test_that("query expansion adds role-specific variants for units", {
  # Unit role should expand abbreviations
  expanded <- metasalmon:::.expand_query("kg", "unit")
  expect_true(length(expanded) >= 2)
  expect_true("kg" %in% expanded)
  expect_true("kilogram" %in% expanded)

  # Should add "unit" suffix if not present
  expanded2 <- metasalmon:::.expand_query("meter", "unit")
  expect_true("meter unit" %in% expanded2)
})

test_that("query expansion adds method suffix for method role", {
  expanded <- metasalmon:::.expand_query("visual survey", "method")
  expect_true("visual survey" %in% expanded)
  expect_true("visual survey method" %in% expanded)
})

test_that("query expansion extracts genus for entity role", {
  # Species name should also search genus

  expanded <- metasalmon:::.expand_query("Oncorhynchus kisutch", "entity")
  expect_true("Oncorhynchus kisutch" %in% expanded)
  expect_true("Oncorhynchus" %in% expanded)
})

test_that("query expansion returns original when role is NA", {
  expanded <- metasalmon:::.expand_query("salmon", NA)
  expect_equal(expanded, "salmon")
})

test_that("find_terms output includes score and agreement_sources columns", {
  # Test that empty results still have the expected columns
  result <- find_terms("", sources = "ols", expand_query = FALSE)
  expect_true("score" %in% names(result))
  expect_true("agreement_sources" %in% names(result))
})

test_that("score_and_rank_terms adds score and agreement columns", {
  # Create a mock result dataframe that simulates raw search output
  mock_df <- tibble::tibble(
    label = c("temperature", "temperature measurement"),
    iri = c("http://example.org/temp1", "http://example.org/temp2"),
    source = c("ols", "ols"),
    ontology = c("envo", "stato"),
    role = c(NA_character_, NA_character_),
    match_type = c("label", "label"),
    definition = c("def1", "def2"),
    zooma_confidence = c(NA_character_, NA_character_),
    zooma_annotator = c(NA_character_, NA_character_)
  )

  # Load vocab table
  vocab_tbl <- metasalmon:::.iadopt_vocab()

  # Run scoring
  result <- metasalmon:::.score_and_rank_terms(mock_df, NA_character_, vocab_tbl, "temperature")

  expect_true("score" %in% names(result))
  expect_true("agreement_sources" %in% names(result))
  expect_true(all(is.numeric(result$score)))
  expect_true(all(is.integer(result$agreement_sources)))
})

test_that("embedding rerank placeholder works when disabled", {
  mock_df <- tibble::tibble(
    label = "test",
    iri = "http://example.org/test",
    score = 1.0
  )

  # Should return unchanged when not enabled (no embedding_score column added)
  result <- metasalmon:::.apply_embedding_rerank(mock_df, "test query")
  expect_equal(nrow(result), 1)
  # When disabled, no column added
  expect_false("embedding_score" %in% names(result))
})

test_that("embedding_rerank_enabled checks env var", {
  # Default should be FALSE
  old_val <- Sys.getenv("METASALMON_EMBEDDING_RERANK", unset = NA)
  on.exit({
    if (is.na(old_val)) {
      Sys.unsetenv("METASALMON_EMBEDDING_RERANK")
    } else {
      Sys.setenv(METASALMON_EMBEDDING_RERANK = old_val)
    }
  })

  Sys.unsetenv("METASALMON_EMBEDDING_RERANK")
  expect_false(metasalmon:::.embedding_rerank_enabled())

  Sys.setenv(METASALMON_EMBEDDING_RERANK = "1")
  expect_true(metasalmon:::.embedding_rerank_enabled())
})

test_that("expand_query returns original for disabled expansion", {
  # Test the expand_query function directly
  expanded <- metasalmon:::.expand_query("test", NA)
  expect_equal(expanded, "test")
  expect_equal(length(expanded), 1)
})
