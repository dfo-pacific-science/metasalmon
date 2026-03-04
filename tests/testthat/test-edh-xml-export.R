test_that("edh_build_iso19139_xml writes expected core content", {
  dataset_meta <- tibble::tibble(
    dataset_id = "fraser-coho-2024",
    title = "Fraser River Coho Escapement Data",
    description = "Sample escapement monitoring data for coho salmon in PFMA 29",
    creator = "DFO Pacific Science",
    contact_name = "Your Name",
    contact_email = "your.email@dfo-mpo.gc.ca",
    contact_org = "Fisheries and Oceans Canada",
    contact_position = "Fishery and Assessment Data Section",
    license = "Open Government Licence - Canada",
    temporal_start = "2001",
    temporal_end = "2024",
    spatial_extent = "PFMA 29, Fraser River watershed",
    topic_categories = "biota;inlandWaters",
    keywords = "coho;escapement;Fraser River",
    update_frequency = "annually",
    security_classification = "unclassified",
    source_citation = "NuSEDS export"
  )

  out <- tempfile(fileext = ".xml")
  result <- edh_build_iso19139_xml(
    dataset_meta,
    output_path = out,
    date_stamp = as.Date("2026-03-03")
  )

  expect_true(file.exists(out))
  expect_type(result, "list")
  expect_true("xml" %in% names(result))

  xml_text <- paste(readLines(out, warn = FALSE), collapse = "\n")

  expect_match(xml_text, "<gmd:MD_Metadata", fixed = TRUE)
  expect_match(xml_text, "fraser-coho-2024", fixed = TRUE)
  expect_match(xml_text, "Fraser River Coho Escapement Data", fixed = TRUE)
  expect_match(xml_text, "pointOfContact", fixed = TRUE)
  expect_match(xml_text, "Open Government Licence - Canada", fixed = TRUE)
  expect_match(xml_text, "gml:TimePeriod", fixed = TRUE)
  expect_match(xml_text, "biota", fixed = TRUE)
  expect_match(xml_text, "inlandWaters", fixed = TRUE)
  expect_match(xml_text, "coho", fixed = TRUE)
  expect_match(xml_text, "security_classification=unclassified", fixed = TRUE)
})

test_that("edh_build_iso19139_xml validates required columns", {
  bad_meta <- tibble::tibble(
    dataset_id = "x",
    title = "Missing description"
  )

  expect_error(
    edh_build_iso19139_xml(bad_meta),
    "missing required"
  )
})
