# ──────────────────────────────────────────────────────────────────────────────
# Tests for ontology fetch, parse, and validation workflow
# ──────────────────────────────────────────────────────────────────────────────

# ── Minimal inline TTL for unit testing (no network required) ────────────────

minimal_ttl <- function() {
  tmp <- tempfile(fileext = ".ttl")
  writeLines(c(
    '@prefix gcdfo:    <https://w3id.org/gcdfo/salmon#> .',
    '@prefix :         <https://w3id.org/gcdfo/salmon#> .',
    '@prefix owl:      <http://www.w3.org/2002/07/owl#> .',
    '@prefix rdfs:     <http://www.w3.org/2000/01/rdf-schema#> .',
    '@prefix skos:     <http://www.w3.org/2004/02/skos/core#> .',
    '@prefix iao:      <http://purl.obolibrary.org/obo/IAO_> .',
    '@prefix dcterms:  <http://purl.org/dc/terms/> .',
    '',
    'gcdfo:ConservationUnit a owl:Class ;',
    '  rdfs:label "Conservation Unit"@en ;',
    '  iao:0000115 "A group of fish sufficiently isolated from other groups."@en .',
    '',
    'gcdfo:EscapementMeasurement a owl:Class ;',
    '  rdfs:label "Escapement measurement"@en ;',
    '  iao:0000115 "A measurement or estimate of escapement."@en .',
    '',
    'gcdfo:WSPBiologicalStatusZone a skos:Concept ;',
    '  skos:prefLabel "WSP biological status zone"@en ;',
    '  skos:definition "The biological status of a Conservation Unit."@en .',
    '',
    ':GreenZone a gcdfo:WSPBiologicalStatusZone , skos:Concept ;',
    '  skos:prefLabel "Green zone"@en ;',
    '  skos:definition "CU in good status."@en .',
    '',
    ':AmberZone a gcdfo:WSPBiologicalStatusZone , skos:Concept ;',
    '  skos:prefLabel "Amber zone"@en ;',
    '  skos:definition "CU in intermediate status."@en .',
    '',
    ':RedZone a gcdfo:WSPBiologicalStatusZone , skos:Concept ;',
    '  skos:prefLabel "Red zone"@en ;',
    '  skos:definition "CU in poor status."@en .',
    '',
    'gcdfo:Stock a owl:Class ;',
    '  rdfs:label "Stock"@en ;',
    '  iao:0000115 "A salmon demographic unit."@en .',
    '',
    ':EnumerationMethodScheme a skos:ConceptScheme ;',
    '  skos:prefLabel "Enumeration Method Scheme"@en ;',
    '  skos:definition "Methods for counting salmon."@en .'
  ), tmp)
  tmp
}

# ── parse_ontology_terms ─────────────────────────────────────────────────────

test_that("parse_ontology_terms extracts terms from minimal TTL", {
  ttl <- minimal_ttl()
  on.exit(unlink(ttl))

  terms <- parse_ontology_terms(ttl)

  expect_s3_class(terms, "tbl_df")
  expect_true(all(c("iri", "local_name", "label", "definition", "term_type") %in% names(terms)))

  # Should find all terms we defined
  expect_true("https://w3id.org/gcdfo/salmon#ConservationUnit" %in% terms$iri)
  expect_true("https://w3id.org/gcdfo/salmon#EscapementMeasurement" %in% terms$iri)
  expect_true("https://w3id.org/gcdfo/salmon#WSPBiologicalStatusZone" %in% terms$iri)
  expect_true("https://w3id.org/gcdfo/salmon#GreenZone" %in% terms$iri)
  expect_true("https://w3id.org/gcdfo/salmon#Stock" %in% terms$iri)
  expect_true("https://w3id.org/gcdfo/salmon#EnumerationMethodScheme" %in% terms$iri)
})

test_that("parse_ontology_terms resolves types correctly", {
  ttl <- minimal_ttl()
  on.exit(unlink(ttl))

  terms <- parse_ontology_terms(ttl)

  cu <- terms[terms$local_name == "ConservationUnit", ]
  expect_equal(cu$term_type, "owl:Class")

  zone <- terms[terms$local_name == "WSPBiologicalStatusZone", ]
  expect_equal(zone$term_type, "skos:Concept")

  scheme <- terms[terms$local_name == "EnumerationMethodScheme", ]
  expect_equal(scheme$term_type, "skos:ConceptScheme")
})

test_that("parse_ontology_terms extracts labels and definitions", {
  ttl <- minimal_ttl()
  on.exit(unlink(ttl))

  terms <- parse_ontology_terms(ttl)

  cu <- terms[terms$local_name == "ConservationUnit", ]
  expect_equal(cu$label, "Conservation Unit")
  expect_match(cu$definition, "group of fish")

  green <- terms[terms$local_name == "GreenZone", ]
  expect_equal(green$label, "Green zone")
  expect_match(green$definition, "good status")
})

test_that("parse_ontology_terms errors on missing file", {
  expect_error(parse_ontology_terms("/nonexistent/path.ttl"), "not found")
})

# ── validate_ontology_terms ──────────────────────────────────────────────────

test_that("validate_ontology_terms passes for valid gcdfo: IRIs", {
  ttl <- minimal_ttl()
  on.exit(unlink(ttl))
  terms <- parse_ontology_terms(ttl)

  dict <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = c("cu_id", "status_zone"),
    column_label = c("CU ID", "Status Zone"),
    column_description = c("Conservation unit ID", "WSP status zone"),
    column_role = c("identifier", "categorical"),
    value_type = c("string", "string"),
    required = c(TRUE, FALSE),
    term_iri = c(NA_character_,
                 "https://w3id.org/gcdfo/salmon#WSPBiologicalStatusZone"),
    property_iri = c(NA_character_, NA_character_),
    entity_iri = c("https://w3id.org/gcdfo/salmon#ConservationUnit",
                   "https://w3id.org/gcdfo/salmon#ConservationUnit"),
    unit_iri = c(NA_character_, NA_character_),
    constraint_iri = c(NA_character_, NA_character_),
    method_iri = c(NA_character_, NA_character_)
  )

  report <- validate_ontology_terms(dict, terms)

  expect_s3_class(report, "ms_validation_report")
  expect_equal(report$status, "pass")
  expect_equal(report$n_fail, 0L)

  # The valid gcdfo: IRIs should be pass
  gcdfo_results <- report$results[grepl("gcdfo", report$results$iri), ]
  expect_true(all(gcdfo_results$status == "pass"))
})

test_that("validate_ontology_terms fails for invalid gcdfo: IRI", {
  ttl <- minimal_ttl()
  on.exit(unlink(ttl))
  terms <- parse_ontology_terms(ttl)

  dict <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "bad_col",
    column_label = "Bad Column",
    column_description = "Has a bogus IRI",
    column_role = "attribute",
    value_type = "string",
    required = FALSE,
    term_iri = "https://w3id.org/gcdfo/salmon#BogusTermThatDoesNotExist",
    property_iri = NA_character_,
    entity_iri = NA_character_,
    unit_iri = NA_character_,
    constraint_iri = NA_character_,
    method_iri = NA_character_
  )

  report <- validate_ontology_terms(dict, terms)

  expect_equal(report$status, "fail")
  expect_equal(report$n_fail, 1L)
  expect_match(report$results$message[report$results$status == "fail"],
               "not found in ontology")
})

test_that("validate_ontology_terms warns for empty measurement IRIs", {
  ttl <- minimal_ttl()
  on.exit(unlink(ttl))
  terms <- parse_ontology_terms(ttl)

  dict <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "count",
    column_label = "Count",
    column_description = "Fish count",
    column_role = "measurement",
    value_type = "integer",
    required = FALSE,
    term_iri = NA_character_,
    property_iri = NA_character_,
    entity_iri = NA_character_,
    unit_iri = NA_character_,
    constraint_iri = NA_character_,
    method_iri = NA_character_
  )

  report <- validate_ontology_terms(dict, terms)

  expect_equal(report$status, "warn")
  expect_gt(report$n_warn, 0L)
})

test_that("validate_ontology_terms accepts external namespace IRIs", {
  ttl <- minimal_ttl()
  on.exit(unlink(ttl))
  terms <- parse_ontology_terms(ttl)

  dict <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = "date_col",
    column_label = "Date",
    column_description = "Observation date",
    column_role = "temporal",
    value_type = "date",
    required = FALSE,
    term_iri = "http://purl.org/dc/terms/date",
    property_iri = NA_character_,
    entity_iri = NA_character_,
    unit_iri = NA_character_,
    constraint_iri = NA_character_,
    method_iri = NA_character_
  )

  report <- validate_ontology_terms(dict, terms)

  # External IRIs are pass (not verified against gcdfo)
  expect_equal(report$status, "pass")
  expect_equal(report$n_fail, 0L)
})

test_that("validate_ontology_terms print method works", {
  ttl <- minimal_ttl()
  on.exit(unlink(ttl))
  terms <- parse_ontology_terms(ttl)

  dict <- tibble::tibble(
    dataset_id = "d1",
    table_id = "t1",
    column_name = c("good", "bad"),
    column_label = c("Good", "Bad"),
    column_description = c("Good col", "Bad col"),
    column_role = c("attribute", "attribute"),
    value_type = c("string", "string"),
    required = c(FALSE, FALSE),
    term_iri = c("https://w3id.org/gcdfo/salmon#Stock",
                 "https://w3id.org/gcdfo/salmon#FakeTermXYZ"),
    property_iri = c(NA_character_, NA_character_),
    entity_iri = c(NA_character_, NA_character_),
    unit_iri = c(NA_character_, NA_character_),
    constraint_iri = c(NA_character_, NA_character_),
    method_iri = c(NA_character_, NA_character_)
  )

  report <- validate_ontology_terms(dict, terms)
  expect_output(print(report), "fail")
})

# ── WSP mapping integration test ─────────────────────────────────────────────

test_that("validate_ontology_terms handles WSP-style mapping dict", {
  ttl <- minimal_ttl()
  on.exit(unlink(ttl))
  terms <- parse_ontology_terms(ttl)

  # Simulate a WSP mapping dictionary (like smn-data-gpt produces)
  dict <- tibble::tibble(
    dataset_id = rep("wsp", 3),
    table_id = rep("rapid_status", 3),
    column_name = c("FULL_CU_IN", "WSP_RAPID_STATUS", "ESCAPEMENT_TOTAL"),
    column_label = c("CU ID", "Status zone", "Escapement total"),
    column_description = c("CU identifier", "WSP status", "Total spawners"),
    column_role = c("identifier", "categorical", "measurement"),
    value_type = c("string", "string", "number"),
    required = c(TRUE, FALSE, FALSE),
    term_iri = c(NA_character_,
                 "https://w3id.org/gcdfo/salmon#WSPBiologicalStatusZone",
                 "https://w3id.org/gcdfo/salmon#EscapementMeasurement"),
    property_iri = c(NA_character_, NA_character_,
                     "http://qudt.org/vocab/quantitykind/Count"),
    entity_iri = c("https://w3id.org/gcdfo/salmon#ConservationUnit",
                   "https://w3id.org/gcdfo/salmon#ConservationUnit",
                   "https://w3id.org/gcdfo/salmon#ConservationUnit"),
    unit_iri = c(NA_character_, NA_character_,
                 "http://qudt.org/vocab/unit/NUM"),
    constraint_iri = c(NA_character_, NA_character_, NA_character_),
    method_iri = c(NA_character_, NA_character_, NA_character_)
  )

  report <- validate_ontology_terms(dict, terms)

  # All gcdfo: IRIs should pass since they're in our minimal ontology
  gcdfo_rows <- report$results[grepl("gcdfo", report$results$iri), ]
  expect_true(all(gcdfo_rows$status == "pass"))
  expect_equal(report$n_fail, 0L)
})

# ── Integration: fetch + parse + validate (requires network) ─────────────────

test_that("full fetch/parse/validate workflow succeeds", {

  testthat::skip_if_offline("dfo-pacific-science.github.io")

  # Fetch
  ttl_path <- fetch_salmon_ontology()
  expect_true(file.exists(ttl_path))

  # Parse
  terms <- parse_ontology_terms(ttl_path)
  expect_gt(nrow(terms), 30)  # The released ontology has 40+ gcdfo: terms

  # Verify well-known terms
  expect_true("https://w3id.org/gcdfo/salmon#ConservationUnit" %in% terms$iri)
  expect_true("https://w3id.org/gcdfo/salmon#Escapement" %in% terms$iri)
  expect_true("https://w3id.org/gcdfo/salmon#Stock" %in% terms$iri)

  # Validate a simple dictionary against the real ontology
  dict <- tibble::tibble(
    dataset_id = "test",
    table_id = "test",
    column_name = c("cu_id", "escapement"),
    column_label = c("CU", "Escapement"),
    column_description = c("id", "esc"),
    column_role = c("identifier", "measurement"),
    value_type = c("string", "number"),
    required = c(TRUE, FALSE),
    term_iri = c(NA_character_,
                 "https://w3id.org/gcdfo/salmon#EscapementMeasurement"),
    property_iri = c(NA_character_,
                     "http://qudt.org/vocab/quantitykind/Count"),
    entity_iri = c("https://w3id.org/gcdfo/salmon#ConservationUnit",
                   "https://w3id.org/gcdfo/salmon#ConservationUnit"),
    unit_iri = c(NA_character_,
                 "http://qudt.org/vocab/unit/NUM"),
    constraint_iri = c(NA_character_, NA_character_),
    method_iri = c(NA_character_, NA_character_)
  )

  report <- validate_ontology_terms(dict, terms)
  expect_equal(report$status, "pass")
  expect_equal(report$n_fail, 0L)
})
