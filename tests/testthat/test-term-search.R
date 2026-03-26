test_that("find_terms returns empty tibble when sources empty", {
  res <- find_terms("escapement", sources = character(0))
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0)
  expect_true(all(c("label", "iri", "source", "ontology", "role", "match_type", "definition", "alignment_only") %in% names(res)))
})

test_that("find_terms surfaces timeout errors for online lookup", {
  timed_out <- FALSE
  res <- with_mocked_bindings(
    .safe_json = function(url, headers = NULL, timeout_secs = 30) {
      stop("Error in curl::curl_fetch_memory: Timeout was reached: Operation timed out")
    },
    withCallingHandlers(
      find_terms("water temperature", sources = "ols", expand_query = FALSE),
      warning = function(w) {
        timed_out <<- grepl("timed out", conditionMessage(w), ignore.case = TRUE)
      }
    )
  )

  expect_true(timed_out)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0L)
  expect_true(all(c("label", "iri", "source", "ontology", "role", "match_type", "definition", "alignment_only") %in% names(res)))
})

test_that("find_terms surfaces timeout errors from source-level failures", {
  timed_out <- FALSE
  res <- with_mocked_bindings(
    .search_ols = function(query, role) {
      stop("Request timed out while resolving source")
    },
    withCallingHandlers(
      find_terms("water temperature", sources = "ols", expand_query = FALSE),
      warning = function(w) {
        timed_out <<- grepl("timed out", conditionMessage(w), ignore.case = TRUE)
      }
    )
  )

  expect_true(timed_out)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0L)
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

test_that("find_terms uses gcdfo ontology backend", {
  mock_index <- tibble::tibble(
    iri = c(
      "https://w3id.org/gcdfo/salmon#NaturalSpawnerCount",
      "https://w3id.org/gcdfo/salmon#Stock"
    ),
    label = c("Natural spawner count", "Stock"),
    alt_labels = c("Spawner count", ""),
    definition = c("Count of natural spawners.", "A salmon stock entity."),
    resource_kind = c("NamedIndividual", "Class"),
    in_scheme = c("https://w3id.org/gcdfo/salmon#EstimateTypeScheme", ""),
    parent_iris = c("", ""),
    type_iris = c("http://www.w3.org/2004/02/skos/core#Concept", "http://www.w3.org/2002/07/owl#Class"),
    search_text = c(
      "natural spawner count spawner count estimate type count of natural spawners",
      "stock salmon stock entity"
    ),
    is_variable = c(TRUE, FALSE),
    is_property = c(FALSE, FALSE),
    is_entity = c(FALSE, TRUE),
    is_constraint = c(FALSE, FALSE),
    is_method = c(FALSE, FALSE),
    role_hints = c("variable", "entity")
  )

  res <- with_mocked_bindings(
    .gcdfo_term_index = function(refresh = FALSE) mock_index,
    find_terms("spawner count", role = "variable", sources = "gcdfo", expand_query = FALSE)
  )

  expect_gte(nrow(res), 1)
  expect_equal(res$source[[1]], "gcdfo")
  expect_match(res$iri[[1]], "NaturalSpawnerCount")
})

test_that("find_terms uses smn ontology backend", {
  mock_index <- tibble::tibble(
    iri = c(
      "https://w3id.org/smn/NaturalSpawnerCount",
      "https://w3id.org/smn/Stock"
    ),
    label = c("Natural spawner count", "Stock"),
    alt_labels = c("Spawner count", ""),
    definition = c("Count of natural spawners.", "A salmon stock entity."),
    resource_kind = c("NamedIndividual", "Class"),
    in_scheme = c("https://w3id.org/smn/EstimateTypeScheme", ""),
    parent_iris = c("", ""),
    type_iris = c("http://www.w3.org/2004/02/skos/core#Concept", "http://www.w3.org/2002/07/owl#Class"),
    search_text = c(
      "natural spawner count spawner count estimate type count of natural spawners",
      "stock salmon stock entity"
    ),
    is_variable = c(TRUE, FALSE),
    is_property = c(FALSE, FALSE),
    is_entity = c(FALSE, TRUE),
    is_constraint = c(FALSE, FALSE),
    is_method = c(FALSE, FALSE),
    role_hints = c("variable", "entity")
  )

  res <- with_mocked_bindings(
    .smn_term_index = function(refresh = FALSE) mock_index,
    find_terms("spawner count", role = "variable", sources = "smn", expand_query = FALSE)
  )

  expect_gte(nrow(res), 1)
  expect_equal(res$source[[1]], "smn")
  expect_match(res$iri[[1]], "NaturalSpawnerCount")
})

test_that("find_terms keeps distinct source IRIs without cross-source collapsing", {
  smn_rows <- tibble::tibble(
    label = "Stock",
    iri = "https://w3id.org/smn/Stock",
    source = "smn",
    ontology = "smn",
    role = "entity",
    match_type = "definition",
    definition = "Shared stock concept"
  )
  gcdfo_rows <- tibble::tibble(
    label = "Stock",
    iri = "https://w3id.org/gcdfo/salmon#Stock",
    source = "gcdfo",
    ontology = "gcdfo",
    role = "entity",
    match_type = "label_exact",
    definition = "DFO stock concept"
  )

  res <- with_mocked_bindings(
    .search_smn = function(query, role) smn_rows,
    .search_gcdfo = function(query, role) gcdfo_rows,
    .search_ols = function(query, role) .empty_terms(role),
    .search_nvs = function(query, role) .empty_terms(role),
    find_terms("stock", role = "entity", sources = c("smn", "gcdfo"), expand_query = FALSE)
  )

  expect_equal(nrow(res), 2)
  expect_true(all(c("https://w3id.org/smn/Stock", "https://w3id.org/gcdfo/salmon#Stock") %in% res$iri))
})

test_that("find_terms short-circuits fallback when smn has a good hit", {
  mock_index <- tibble::tibble(
    iri = "https://w3id.org/smn/Stock",
    label = "Stock",
    alt_labels = "",
    definition = "A salmon stock entity.",
    resource_kind = "Class",
    in_scheme = "",
    parent_iris = "",
    type_iris = "http://www.w3.org/2002/07/owl#Class",
    search_text = "stock salmon stock entity",
    is_variable = FALSE,
    is_property = FALSE,
    is_entity = TRUE,
    is_constraint = FALSE,
    is_method = FALSE,
    role_hints = "entity"
  )

  res <- with_mocked_bindings(
    .smn_term_index = function(refresh = FALSE) mock_index,
    .safe_json = function(url, headers = NULL, timeout_secs = 30) {
      stop("fallback should not run when smn already matched")
    },
    find_terms("stock", role = "entity", sources = c("smn", "gcdfo", "ols", "nvs"), expand_query = FALSE)
  )

  expect_equal(res$source[[1]], "smn")
  expect_equal(res$label[[1]], "Stock")
})

test_that("find_terms falls back to gcdfo when smn has no good hit", {
  smn_index <- tibble::tibble(
    iri = character(),
    label = character(),
    alt_labels = character(),
    definition = character(),
    resource_kind = character(),
    in_scheme = character(),
    parent_iris = character(),
    type_iris = character(),
    search_text = character(),
    is_variable = logical(),
    is_property = logical(),
    is_entity = logical(),
    is_constraint = logical(),
    is_method = logical(),
    role_hints = character()
  )
  gcdfo_index <- tibble::tibble(
    iri = "https://w3id.org/gcdfo/salmon#Stock",
    label = "Stock",
    alt_labels = "",
    definition = "A salmon stock entity.",
    resource_kind = "Class",
    in_scheme = "",
    parent_iris = "",
    type_iris = "http://www.w3.org/2002/07/owl#Class",
    search_text = "stock salmon stock entity",
    is_variable = FALSE,
    is_property = FALSE,
    is_entity = TRUE,
    is_constraint = FALSE,
    is_method = FALSE,
    role_hints = "entity"
  )

  res <- with_mocked_bindings(
    .smn_term_index = function(refresh = FALSE) smn_index,
    .gcdfo_term_index = function(refresh = FALSE) gcdfo_index,
    .safe_json = function(url, headers = NULL, timeout_secs = 30) {
      stop("external fallback should not run when gcdfo already matched")
    },
    find_terms("stock", role = "entity", sources = c("smn", "gcdfo", "ols", "nvs"), expand_query = FALSE)
  )

  expect_equal(res$source[[1]], "gcdfo")
  expect_equal(res$label[[1]], "Stock")
})

test_that("find_terms does not short-circuit on lexical-poor local count hits", {
  smn_rows <- tibble::tibble(
    label = "Observed rate or abundance",
    iri = "https://w3id.org/smn/ObservedRateOrAbundance",
    source = "smn",
    ontology = "smn",
    role = "property",
    match_type = "definition",
    definition = "Observed count or abundance metric."
  )
  gcdfo_rows <- tibble::tibble(
    label = "Spawner abundance",
    iri = "https://w3id.org/gcdfo/salmon#SpawnerAbundance",
    source = "gcdfo",
    ontology = "gcdfo",
    role = "property",
    match_type = "label_partial",
    definition = "Spawner abundance estimate."
  )
  ols_rows <- tibble::tibble(
    label = "count",
    iri = "http://purl.obolibrary.org/obo/STATO_0000047",
    source = "ols",
    ontology = "stato",
    role = "property",
    match_type = "label_exact",
    definition = "count"
  )

  res <- with_mocked_bindings(
    .search_smn = function(query, role) smn_rows,
    .search_gcdfo = function(query, role) gcdfo_rows,
    .search_ols = function(query, role) ols_rows,
    .search_nvs = function(query, role) .empty_terms(role),
    find_terms("count", role = "property", sources = c("smn", "gcdfo", "ols", "nvs"), expand_query = FALSE)
  )

  expect_true("ols" %in% res$source)
  expect_equal(res$label[[1]], "count")
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

test_that("score_and_rank_terms demotes generic entity drift and boosts trusted generic entity matches", {
  vocab <- metasalmon:::`.iadopt_vocab`()

  species_df <- tibble::tibble(
    label = c("Population", "species", "species"),
    iri = c(
      "https://w3id.org/smn/Population",
      "http://purl.obolibrary.org/obo/APOLLO_SV_00000121",
      "http://purl.obolibrary.org/obo/NCBITaxon_species"
    ),
    source = c("smn", "ols", "ols"),
    ontology = c("smn", "apollo_sv", "genepio"),
    role = "entity",
    match_type = c("definition", "class", "class"),
    definition = c(
      "A group of organisms of the same species occupying a defined area that interbreed and share a gene pool.",
      "Species concept.",
      "NCBI taxonomy species rank."
    )
  )

  species_ranked <- metasalmon:::`.score_and_rank_terms`(species_df, "entity", vocab, "species")
  expect_equal(species_ranked$iri[[1]], "http://purl.obolibrary.org/obo/NCBITaxon_species")
  expect_false(identical(species_ranked$iri[[1]], "https://w3id.org/smn/Population"))

  spatial_df <- tibble::tibble(
    label = c("Body shape", "Escapement", "water body"),
    iri = c(
      "https://w3id.org/smn/BodyShape",
      "https://w3id.org/smn/Escapement",
      "http://purl.obolibrary.org/obo/ENVO_00000063"
    ),
    source = c("smn", "smn", "ols"),
    ontology = c("smn", "smn", "envo"),
    role = "entity",
    match_type = c("class", "class", "class"),
    definition = c("Fish body shape.", "Escapement counts.", "A body of water.")
  )

  spatial_ranked <- metasalmon:::`.score_and_rank_terms`(spatial_df, "entity", vocab, "water body")
  expect_equal(spatial_ranked$iri[[1]], "http://purl.obolibrary.org/obo/ENVO_00000063")

  local_exact_df <- tibble::tibble(
    label = c("Conservation Unit", "watershed"),
    iri = c(
      "https://w3id.org/gcdfo/salmon#ConservationUnit",
      "http://purl.obolibrary.org/obo/ENVO_00000292"
    ),
    source = c("gcdfo", "ols"),
    ontology = c("gcdfo", "envo"),
    role = "entity",
    match_type = c("label_exact", "class"),
    definition = c("A group of fish sufficiently isolated from other groups...", "A watershed.")
  )

  local_exact_ranked <- metasalmon:::`.score_and_rank_terms`(local_exact_df, "entity", vocab, "conservation unit")
  expect_equal(local_exact_ranked$iri[[1]], "https://w3id.org/gcdfo/salmon#ConservationUnit")
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

test_that("score_and_rank_terms demotes local salmon drift for physical/environmental queries", {
  vocab <- metasalmon:::`.iadopt_vocab`()

  variable_df <- tibble::tibble(
    label = c("Escapement", "Water temperature"),
    iri = c(
      "https://w3id.org/smn/Escapement",
      "https://vocab.nerc.ac.uk/collection/P01/TEMP01"
    ),
    source = c("smn", "nvs"),
    ontology = c("smn", "P01"),
    role = c("variable", "variable"),
    match_type = c("class", "label_exact"),
    definition = c("Count of salmon returning to spawn.", "Temperature of water body."),
    backend_score = c(3.0, 1.8)
  )

  variable_ranked <- metasalmon:::`.score_and_rank_terms`(variable_df, "variable", vocab, "water temperature")
  expect_equal(variable_ranked$iri[[1]], "https://vocab.nerc.ac.uk/collection/P01/TEMP01")

  entity_df <- tibble::tibble(
    label = c("Body shape", "fresh water body"),
    iri = c(
      "https://w3id.org/smn/BodyShape",
      "http://purl.obolibrary.org/obo/ENVO_01001320"
    ),
    source = c("smn", "ols"),
    ontology = c("smn", "envo"),
    role = c("entity", "entity"),
    match_type = c("class", "class"),
    definition = c("Fish body shape.", "A body of fresh water."),
    backend_score = c(3.0, 1.5)
  )

  entity_ranked <- metasalmon:::`.score_and_rank_terms`(entity_df, "entity", vocab, "freshwater body")
  expect_equal(entity_ranked$iri[[1]], "http://purl.obolibrary.org/obo/ENVO_01001320")

  method_df <- tibble::tibble(
    label = c("Electrofishing Count", "Catch method", "Sampling protocol"),
    iri = c(
      "https://w3id.org/gcdfo/salmon#ElectrofishingCount",
      "https://w3id.org/smn/CatchMethod",
      "https://w3id.org/smn/SamplingProtocol"
    ),
    source = c("gcdfo", "smn", "smn"),
    ontology = c("gcdfo", "smn", "smn"),
    role = c("method", "method", "method"),
    match_type = c("namedindividual", "class", "class"),
    definition = c("Electrofishing count metric.", "Method used to catch fish.", "Protocol used to sample fish."),
    backend_score = c(2.8, 1.6, 1.4)
  )

  method_ranked <- metasalmon:::`.score_and_rank_terms`(method_df, "method", vocab, "catch method")
  expect_true(grepl("method|protocol", tolower(method_ranked$label[[1]])))
  expect_false(grepl("count", tolower(method_ranked$label[[1]])))
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

test_that("score_and_rank_terms tolerates role sources missing from role boost map", {
  vocab <- metasalmon:::`.iadopt_vocab`()
  df <- tibble::tibble(
    label = c("Spawner count", "Fallback count"),
    iri = c("https://example.org/spawner-count", "https://example.org/fallback-count"),
    source = c("gbif", "nvs"),
    ontology = c("gbif", "bodc"),
    role = NA_character_,
    match_type = c("label", "label"),
    definition = c("Count of spawners", "Fallback count term")
  )

  expect_no_error({
    ranked <- metasalmon:::`.score_and_rank_terms`(df, "variable", vocab, "spawner count")
    expect_equal(nrow(ranked), 2)
  })
})

# ============================================================================
# Phase 2 Tests: Ontology Preferences by Role
# ============================================================================

test_that("sources_for_role returns appropriate sources for each role", {
  expect_equal(sources_for_role("unit"), c("qudt", "nvs", "ols"))
  expect_equal(sources_for_role("entity"), c("smn", "gcdfo", "gbif", "worms", "bioportal", "ols"))
  expect_equal(sources_for_role("property"), c("smn", "gcdfo", "nvs", "ols", "zooma"))
  expect_equal(sources_for_role("method"), c("smn", "gcdfo", "bioportal", "ols", "zooma"))
  expect_equal(sources_for_role("variable"), c("smn", "gcdfo", "nvs", "ols", "zooma"))
  expect_equal(sources_for_role("constraint"), c("smn", "gcdfo", "ols"))
  # Default fallback
  expect_equal(sources_for_role(NA), c("smn", "gcdfo", "ols", "nvs"))
  expect_equal(sources_for_role(""), c("smn", "gcdfo", "ols", "nvs"))
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
  smn_pref <- dplyr::filter(entity_prefs, ontology == "smn")
  expect_true(nrow(smn_pref) > 0)
  expect_equal(smn_pref$priority[[1]], 1)
  gcdfo_pref <- dplyr::filter(entity_prefs, ontology == "gcdfo")
  expect_true(nrow(gcdfo_pref) > 0)
  expect_equal(gcdfo_pref$priority[[1]], 2)
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

test_that("query expansion adds hydrometric variants for variable/property roles", {
  level_expanded <- metasalmon:::.expand_query("water level", "variable")
  expect_true("stage height" %in% level_expanded)
  expect_true("gauge height" %in% level_expanded)
  expect_true("surface elevation" %in% level_expanded)

  discharge_expanded <- metasalmon:::.expand_query("water discharge", "variable")
  expect_true("discharge" %in% discharge_expanded)
  expect_true("riverine discharge" %in% discharge_expanded)
  expect_true("streamflow" %in% discharge_expanded)

  property_expanded <- metasalmon:::.expand_query("water discharge", "property")
  expect_true("water discharge measurement" %in% property_expanded)
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

# ==========================================================================
# Ranking fixture pack for regression safety
# ==========================================================================

.build_fixture_rank_df <- function(case) {
  candidate_df <- dplyr::bind_rows(case$candidates)

  for (col in c("candidate_id", "role_hints", "zooma_confidence", "zooma_annotator", "alignment_only", "agreement_sources")) {
    if (!col %in% names(candidate_df)) {
      candidate_df[[col]] <- switch(
        col,
        candidate_id = as.character(seq_len(nrow(candidate_df))),
        role_hints = NA_character_,
        zooma_confidence = NA_character_,
        zooma_annotator = NA_character_,
        alignment_only = FALSE,
        agreement_sources = as.integer(1L),
        candidate_df[[col]]
      )
    }
  }

  candidate_df$alignment_only <- as.logical(candidate_df$alignment_only)
  candidate_df$agreement_sources <- as.integer(candidate_df$agreement_sources)
  if (!"backend_score" %in% names(candidate_df)) {
    candidate_df$backend_score <- 0
  }
  candidate_df$backend_score <- as.numeric(candidate_df$backend_score)
  candidate_df$zooma_confidence <- as.character(candidate_df$zooma_confidence)
  candidate_df$zooma_annotator <- as.character(candidate_df$zooma_annotator)

  candidate_df
}

.test_expected_top <- function(ranked, expected, case_id) {
  expect_true(nrow(ranked) >= 1, info = paste("empty ranking", case_id))

  top <- ranked[1, , drop = FALSE]
  top_expected <- expected$top

  if (!is.null(top_expected$candidate_id)) {
    expect_equal(
      top$candidate_id[[1]],
      top_expected$candidate_id,
      info = paste("unexpected top candidate", case_id)
    )
  }
  if (!is.null(top_expected$source)) {
    expect_equal(top$source[[1]], top_expected$source, info = paste("unexpected top source", case_id))
  }
  if (!is.null(top_expected$match_type)) {
    expect_equal(top$match_type[[1]], top_expected$match_type, info = paste("unexpected top match type", case_id))
  }
  if (!is.null(top_expected$iri_contains)) {
    expect_true(
      grepl(top_expected$iri_contains, top$iri[[1]], fixed = TRUE),
      info = paste("unexpected top iri", case_id)
    )
  }

  if (!is.null(top_expected$disallow_top_sources)) {
    expect_false(
      top$source[[1]] %in% top_expected$disallow_top_sources,
      info = paste("disallowed top source present", case_id)
    )
  }

  if (!is.null(top_expected$disallow_top_matches)) {
    for (bad in top_expected$disallow_top_matches) {
      expect_false(
        grepl(bad, top$iri[[1]], fixed = TRUE),
        info = paste("disallowed top match present", case_id)
      )
    }
  }
}

.test_expected_order <- function(ranked, expected, case_id) {
  expected_order <- expected$expected_order
  if (is.null(expected_order)) {
    return(invisible(NULL))
  }

  ranked_ids <- ranked$candidate_id
  expect_true(length(ranked_ids) >= length(expected_order))

  for (i in seq_along(expected_order)) {
    expect_equal(
      ranked_ids[[i]],
      expected_order[[i]],
      info = paste("expected order mismatch", case_id, "position", i)
    )
  }
}

test_that("ranking fixtures keep top results deterministic", {
  fixtures <- jsonlite::fromJSON(
    testthat::test_path("fixtures", "semantic-ranking-fixtures.json"),
    simplifyDataFrame = FALSE
  )

  expect_true(length(fixtures) > 0)
  vocab <- metasalmon:::.iadopt_vocab()

  for (case in fixtures) {
    candidate_df <- .build_fixture_rank_df(case)

    ranked <- metasalmon:::.score_and_rank_terms(
      candidate_df,
      case$role,
      vocab,
      case$query
    )

    .test_expected_top(ranked, case$expected, case$case_id)
    .test_expected_order(ranked, case$expected, case$case_id)

    # Ensure deterministic top ordering and score numeric ordering behavior.
    if (!is.null(case$expected$min_margin) && nrow(ranked) > 1) {
      expect_gte(ranked$score[[1]] - ranked$score[[2]], case$expected$min_margin - 1e-8)
    }
  }
})

test_that("text-similarity rerank updates score when enabled", {
  old_enabled <- Sys.getenv("METASALMON_EMBEDDING_RERANK", unset = NA_character_)
  old_weight <- Sys.getenv("METASALMON_EMBEDDING_WEIGHT", unset = NA_character_)
  on.exit({
    if (is.na(old_enabled)) {
      Sys.unsetenv("METASALMON_EMBEDDING_RERANK")
    } else {
      Sys.setenv(METASALMON_EMBEDDING_RERANK = old_enabled)
    }
    if (is.na(old_weight)) {
      Sys.unsetenv("METASALMON_EMBEDDING_WEIGHT")
    } else {
      Sys.setenv(METASALMON_EMBEDDING_WEIGHT = old_weight)
    }
  }, add = TRUE)

  Sys.setenv(METASALMON_EMBEDDING_RERANK = "1", METASALMON_EMBEDDING_WEIGHT = "1.5")

  candidate_df <- tibble::tibble(
    candidate_id = c("salinity", "measurement"),
    label = c("Water salinity", "Random fish measurement"),
    iri = c("https://example.org/temp", "https://example.org/filler"),
    source = c("ols", "ols"),
    ontology = c("envo", "misc"),
    role = c(NA_character_, NA_character_),
    match_type = c("label", "label"),
    definition = c("Concentration of salt in water", "Unrelated fish-related text"),
    backend_score = c(1.0, 1.0),
    alignment_only = c(FALSE, FALSE),
    agreement_sources = c(1L, 1L)
  )
  vocab <- metasalmon:::.iadopt_vocab()

  ranked <- metasalmon:::.score_and_rank_terms(
    candidate_df,
    NA_character_,
    vocab,
    "water salinity"
  )

  expect_true("embedding_score" %in% names(ranked))
  expect_false(is.na(ranked$embedding_score[[1]]))
  expect_equal(ranked$label[[1]], "Water salinity")
})

test_that("embedding rerank only updates top_k candidates", {
  old_enabled <- Sys.getenv("METASALMON_EMBEDDING_RERANK", unset = NA_character_)
  on.exit({
    if (is.na(old_enabled)) {
      Sys.unsetenv("METASALMON_EMBEDDING_RERANK")
    } else {
      Sys.setenv(METASALMON_EMBEDDING_RERANK = old_enabled)
    }
  }, add = TRUE)

  Sys.setenv(METASALMON_EMBEDDING_RERANK = "1")

  n <- 200L
  candidate_df <- tibble::tibble(
    candidate_id = as.character(seq_len(n)),
    label = paste("candidate", seq_len(n)),
    iri = paste0("https://example.org/candidate-", seq_len(n)),
    source = rep(c("ols", "nvs", "gcdfo", "smn"), length.out = n),
    ontology = rep(c("ont1", "ont2", "ont3", "ont4"), length.out = n),
    role = rep(NA_character_, n),
    match_type = rep("label", n),
    definition = rep("This is a synthetic candidate for performance testing", n),
    backend_score = seq(0.1, 2, length.out = n),
    alignment_only = rep(FALSE, n),
    agreement_sources = rep(1L, n)
  )

  candidate_df$score <- candidate_df$backend_score
  res <- metasalmon:::.apply_embedding_rerank(candidate_df, "candidate 57", top_k = 20)

  expect_equal(nrow(res), n)
  expect_equal(sum(!is.na(res$embedding_score)), 20)
})

test_that("text-similarity score helpers are bounded and stable", {
  expect_gte(metasalmon:::.text_similarity_score("water temperature", "Water temperature", ""), 1)
  expect_equal(metasalmon:::.text_similarity_score("water temperature", "Fish", ""), 0)
  expect_true(
    metasalmon:::.text_similarity_score("water temperature", "Water temperature", "") <= 1,
    "similarity score should be normalized to 1"
  )

  expect_equal(metasalmon:::.match_type_score("label_exact"), 1.0)
  expect_equal(metasalmon:::.match_type_score("label_partial"), 0.45)
  expect_equal(metasalmon:::.match_type_score("zooma_high"), 0.3)
  expect_equal(metasalmon:::.match_type_score("definition"), 0.15)
  expect_equal(metasalmon:::.match_type_score(""), 0)
})

test_that("benchmark helper returns profile-level and per-case ranking metrics", {
  fixture_path <- testthat::test_path("fixtures", "semantic-ranking-fixtures.json")
  bench <- benchmark_term_ranking_fixtures(fixture_path = fixture_path)

  expect_true(is.list(bench))
  expect_true(all(c("summary", "per_case", "profiles") %in% names(bench)))
  expect_equal(class(bench), "metasalmon_ranking_benchmark")
  expect_equal(nrow(bench$summary), 1L)
  expect_gte(nrow(bench$per_case), 1L)
  expect_true(all(c("profile", "top1_accuracy") %in% names(bench$summary)))
  expect_true(all(c("top1_ok", "top_k_ok", "mrr", "top1_position") %in% names(bench$per_case)))
  expect_true(bench$summary$top1_accuracy[1] >= 0.6)
})

test_that("benchmark helper can compare ranking profiles and detect sensitivity", {
  fixture_path <- testthat::test_path("fixtures", "semantic-ranking-fixtures.json")

  profiles <- list(
    baseline = NULL,
    no_smn = list(base_source_weight = c(smn = -100), role_preferences_enabled = TRUE)
  )

  bench <- benchmark_term_ranking_fixtures(
    fixture_path = fixture_path,
    profiles = profiles,
    top_k = 2L
  )

  expect_equal(nrow(bench$summary), 2L)
  expect_setequal(bench$summary$profile, c("baseline", "no_smn"))

  baseline <- bench$summary$top1_accuracy[bench$summary$profile == "baseline"]
  no_smn <- bench$summary$top1_accuracy[bench$summary$profile == "no_smn"]
  expect_true(length(baseline) == 1L)
  expect_true(length(no_smn) == 1L)
  expect_true(no_smn <= baseline)

  no_smn_cases <- subset(bench$per_case, profile == "no_smn")
  expect_true(any(!no_smn_cases$top1_ok))
})
