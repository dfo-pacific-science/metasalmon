test_that("deduplicate_proposed_terms handles underscore and PascalCase age variants", {
  proposed <- tibble::tibble(
    term_label = c("Spawner_Age1", "SpawnerAge2", "SpawnerAge3"),
    term_definition = c("x", "x", "x"),
    term_type = c("skos_concept", "skos_concept", "skos_concept"),
    suggested_parent_iri = c(
      "https://example.org/parent",
      "https://example.org/parent",
      "https://example.org/parent"
    )
  )

  deduped <- deduplicate_proposed_terms(proposed)

  expect_equal(nrow(deduped), 1)
  expect_equal(deduped$collapsed_from, 3)
  expect_true(deduped$needs_age_facet)
  expect_match(deduped$dedup_notes, "propose one if missing")
})

test_that("deduplicate_proposed_terms collapses PascalCase phase variants", {
  proposed <- tibble::tibble(
    term_label = c("OceanPhaseCount", "TerminalPhaseCount"),
    term_definition = c("x", "x"),
    term_type = c("skos_concept", "skos_concept"),
    suggested_parent_iri = c("https://example.org/parent", "https://example.org/parent")
  )

  deduped <- deduplicate_proposed_terms(proposed)

  expect_equal(nrow(deduped), 1)
  expect_equal(deduped$collapsed_from, 2)
  expect_true(deduped$needs_phase_facet)
  expect_match(deduped$dedup_notes, "propose one if missing")
})

test_that("suggest_facet_schemes detects age, phase, and benchmark facets from common label styles", {
  proposed <- tibble::tibble(
    term_label = c(
      "Spawner_Age1",
      "SpawnerAge2",
      "SpawnerAge3",
      "OceanPhaseCount",
      "TerminalPhaseCount",
      "CULowerBenchmark",
      "CUUpperBenchmark"
    )
  )

  facets <- suggest_facet_schemes(proposed)

  expect_setequal(
    facets$scheme_name,
    c("AgeClassScheme", "LifePhaseScheme", "BenchmarkLevelScheme")
  )

  age_concepts <- facets$suggested_concepts[[match("AgeClassScheme", facets$scheme_name)]]
  phase_concepts <- facets$suggested_concepts[[match("LifePhaseScheme", facets$scheme_name)]]
  benchmark_concepts <- facets$suggested_concepts[[match("BenchmarkLevelScheme", facets$scheme_name)]]

  expect_setequal(age_concepts, c("Age1Class", "Age2Class", "Age3Class"))
  expect_setequal(phase_concepts, c("OceanPhase", "TerminalPhase"))
  expect_setequal(benchmark_concepts, c("LowerBenchmark", "UpperBenchmark"))
})
