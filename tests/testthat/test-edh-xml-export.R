test_that("edh_build_hnap_xml writes the HNAP-aware EDH export", {
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
  result <- edh_build_hnap_xml(
    dataset_meta,
    output_path = out,
    date_stamp = as.Date("2026-03-03")
  )

  expect_true(file.exists(out))
  expect_type(result, "list")
  expect_true("xml" %in% names(result))

  xml <- xml2::read_xml(out)
  ns <- xml2::xml_ns(xml)

  expect_match(
    xml2::xml_text(xml2::xml_find_first(xml, ".//gmd:fileIdentifier/gco:CharacterString", ns)),
    "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
  )
  expect_equal(
    xml2::xml_text(xml2::xml_find_first(xml, ".//gmd:dataSetURI/gco:CharacterString", ns)),
    "fraser-coho-2024"
  )
  expect_equal(
    xml2::xml_text(xml2::xml_find_first(xml, ".//gmd:CI_Citation/gmd:title/gco:CharacterString", ns)),
    "Fraser River Coho Escapement Data"
  )
  expect_equal(
    xml2::xml_text(xml2::xml_find_first(xml, ".//gmd:abstract/gco:CharacterString", ns)),
    "Sample escapement monitoring data for coho salmon in PFMA 29"
  )
  expect_equal(
    xml2::xml_text(xml2::xml_find_first(xml, ".//gmd:hierarchyLevel/*", ns)),
    "nonGeographicDataset"
  )
  expect_true(length(xml2::xml_find_all(xml, ".//gmd:topicCategory", ns)) == 2)
  expect_true(length(xml2::xml_find_all(xml, ".//gmd:locale/gmd:PT_Locale", ns)) == 2)
  expect_true(length(xml2::xml_find_all(xml, ".//gmd:EX_GeographicDescription", ns)) == 0)
  expect_true(length(xml2::xml_find_all(xml, ".//gml:TimePeriod", ns)) == 1)
  expect_true(length(xml2::xml_find_all(xml, ".//gmd:resourceMaintenance", ns)) == 1)
  expect_true(length(xml2::xml_find_all(xml, ".//gmd:classification", ns)) == 1)
})

test_that("edh_build_hnap_xml defaults to HNAP-aware EDH structure", {
  dataset_meta <- tibble::tibble(
    dataset_id = "pacific-marine-habitat-classes",
    title = "Pacific Marine Habitat Classes",
    title_fr = "Catégories d'habitat marin du Pacifique",
    description = "Marine habitat class polygons for Pacific waters.",
    description_fr = "Polygones des catégories d'habitat marin pour les eaux du Pacifique.",
    creator = "Government of Canada; Fisheries and Oceans Canada; Pacific Science; Marine Spatial Planning",
    contact_name = "Marine Spatial Planning Team",
    contact_org = "Fisheries and Oceans Canada; Pacific Science; Marine Spatial Planning",
    contact_position = "Research Scientist",
    contact_position_fr = "Chercheur scientifique",
    contact_email = "habitat@dfo-mpo.gc.ca",
    contact_phone = "250-363-3001",
    contact_address = "9860 W Saanich Rd",
    contact_city = "Sidney",
    contact_admin_area = "British Columbia",
    contact_country = "Canada",
    contact_postal_code = "V8L 5T5",
    contact_url = "https://www.gis-hub.ca/dataset/marine-habitat-classes",
    license = "Open Government Licence - Canada",
    source_citation = "Pacific Marine Habitat Classes source package",
    created = "2020-03-31",
    modified = "2022-11-10T12:34:56",
    temporal_start = "2010-01-01",
    temporal_end = "2022-12-31",
    spatial_extent = "Pacific Region marine waters",
    bbox_west = -137.4,
    bbox_east = -122.1,
    bbox_south = 48.1,
    bbox_north = 54.9,
    dataset_type = "habitat",
    topic_categories = "oceans;biota;oceans",
    keywords = "habitat;marine;Pacific;marine",
    update_frequency = "annual",
    security_classification = "public",
    provenance_note = "Compiled from marine habitat interpretation workflows.",
    reference_system = "EPSG:3005",
    distribution_url = "https://www.gis-hub.ca/dataset/marine-habitat-classes/download",
    distribution_name = "GeoPackage download",
    distribution_description = "Primary downloadable distribution",
    status = "completed"
  )

  out <- tempfile(fileext = ".xml")
  edh_build_hnap_xml(
    dataset_meta,
    output_path = out
  )

  xml <- xml2::read_xml(out)
  ns <- xml2::xml_ns(xml)

  file_identifier <- xml2::xml_text(
    xml2::xml_find_first(xml, ".//gmd:fileIdentifier/gco:CharacterString", ns)
  )
  expect_match(
    file_identifier,
    "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
  )

  expect_equal(
    xml2::xml_text(xml2::xml_find_first(xml, ".//gmd:dataSetURI/gco:CharacterString", ns)),
    "pacific-marine-habitat-classes"
  )
  expect_equal(
    xml2::xml_text(xml2::xml_find_first(xml, ".//gmd:metadataStandardVersion/gco:CharacterString", ns)),
    "CAN/CGSB-171.100-2009"
  )
  expect_equal(
    xml2::xml_text(xml2::xml_find_first(xml, ".//gmd:hierarchyLevel/*", ns)),
    "nonGeographicDataset"
  )
  expect_true(length(xml2::xml_find_all(xml, ".//gmd:locale/gmd:PT_Locale", ns)) == 2)
  expect_true(length(xml2::xml_find_all(xml, ".//gmd:PT_FreeText", ns)) >= 3)
  expect_equal(
    xml2::xml_text(xml2::xml_find_first(xml, ".//gmd:status/*", ns)),
    "completed"
  )
  expect_equal(
    xml2::xml_text(xml2::xml_find_first(xml, ".//gmd:maintenanceAndUpdateFrequency/*", ns)),
    "annually"
  )
  expect_equal(
    xml2::xml_text(xml2::xml_find_first(xml, ".//gmd:classification/*", ns)),
    "unclassified"
  )
  expect_true(length(xml2::xml_find_all(xml, ".//gmd:EX_GeographicBoundingBox", ns)) == 1)
  expect_true(length(xml2::xml_find_all(xml, ".//gmd:referenceSystemInfo", ns)) == 1)
  expect_true(length(xml2::xml_find_all(xml, ".//gmd:distributionInfo//gmd:CI_OnlineResource", ns)) == 1)
  expect_true(length(xml2::xml_find_all(xml, ".//gmd:citedResponsibleParty", ns)) == 1)

  keyword_values <- xml2::xml_text(xml2::xml_find_all(xml, ".//gmd:descriptiveKeywords//gmd:keyword/gco:CharacterString", ns))
  expect_setequal(keyword_values, c("habitat", "marine", "Pacific"))
  expect_equal(
    xml2::xml_attr(xml2::xml_find_first(xml, ".//gmd:descriptiveKeywords//gmd:type/gmd:MD_KeywordTypeCode", ns), "codeListValue"),
    "theme"
  )
  expect_equal(
    xml2::xml_attr(xml2::xml_find_first(xml, ".//gmd:citedResponsibleParty//gmd:electronicMailAddress", ns), "nilReason"),
    "missing"
  )

  expect_match(
    xml2::xml_text(xml2::xml_find_first(xml, ".//gmd:supplementalInformation/gco:CharacterString", ns)),
    "spatial_extent=Pacific Region marine waters",
    fixed = TRUE
  )
})

test_that("edh_build_hnap_xml emits comprehensive placeholders for GeoNetwork editing", {
  dataset_meta <- tibble::tibble(
    dataset_id = "coho-placeholder-test",
    title = "Coho Placeholder Test",
    description = "Checks GeoNetwork-friendly placeholder nodes.",
    creator = "DFO Pacific Science",
    keywords = "coho;escapement",
    keyword_thesaurus_title = "Local free keywords",
    keyword_thesaurus_date = "2026-03-26"
  )

  out <- tempfile(fileext = ".xml")
  edh_build_hnap_xml(dataset_meta, output_path = out)

  xml <- xml2::read_xml(out)
  ns <- xml2::xml_ns(xml)

  expect_equal(
    xml2::xml_attr(xml2::xml_find_first(xml, ".//gmd:topicCategory", ns), "nilReason"),
    "missing"
  )
  expect_equal(
    xml2::xml_text(xml2::xml_find_first(xml, ".//gmd:descriptiveKeywords//gmd:thesaurusName//gmd:title/gco:CharacterString", ns)),
    "Local free keywords"
  )
  expect_equal(
    xml2::xml_text(xml2::xml_find_first(xml, ".//gmd:descriptiveKeywords//gmd:thesaurusName//gmd:date/gmd:CI_Date/gmd:date/gco:Date", ns)),
    "2026-03-26"
  )
})

test_that("edh_build_hnap_xml validates required columns", {
  bad_meta <- tibble::tibble(
    dataset_id = "x",
    title = "Missing description"
  )

  expect_error(
    edh_build_hnap_xml(bad_meta),
    "missing required"
  )
})

test_that("edh_build_iso19139_xml remains a deprecated compatibility alias", {
  dataset_meta <- tibble::tibble(
    dataset_id = "alias-check",
    title = "Alias Check",
    description = "Deprecated helper should still write XML."
  )

  out <- tempfile(fileext = ".xml")
  expect_warning(
    edh_build_iso19139_xml(dataset_meta, output_path = out),
    "deprecated"
  )
  expect_true(file.exists(out))
})

test_that("write_edh_xml_from_sdp rebuilds XML from edited dataset metadata", {
  pkg_dir <- withr::local_tempdir()
  resources <- list(main = tibble::tibble(species = c("Coho"), count = c(1L, 2L)))
  pkg_path <- create_sdp(
    resources,
    path = file.path(pkg_dir, "pkg"),
    dataset_id = "edh-rebuild-demo",
    table_id = "main",
    seed_semantics = FALSE,
    check_updates = FALSE,
    overwrite = TRUE
  )

  dataset_path <- file.path(pkg_path, "metadata", "dataset.csv")
  dataset_meta <- readr::read_csv(dataset_path, show_col_types = FALSE)
  dataset_meta$title <- "Reviewed Fraser Coho Metadata"
  dataset_meta$description <- "Reviewed description from Excel workflow"
  readr::write_csv(dataset_meta, dataset_path, na = "")

  xml_path <- file.path(pkg_path, "metadata", "metadata-edh-hnap.xml")
  expect_false(file.exists(xml_path))

  result <- write_edh_xml_from_sdp(pkg_path)

  expect_true(file.exists(xml_path))
  expect_true(is.list(result))
  xml <- xml2::read_xml(xml_path)
  ns <- xml2::xml_ns(xml)
  expect_equal(
    xml2::xml_text(xml2::xml_find_first(xml, ".//gmd:CI_Citation/gmd:title/gco:CharacterString", ns)),
    "Reviewed Fraser Coho Metadata"
  )
  expect_equal(
    xml2::xml_text(xml2::xml_find_first(xml, ".//gmd:abstract/gco:CharacterString", ns)),
    "Reviewed description from Excel workflow"
  )
})

test_that("write_edh_xml_from_sdp respects overwrite flag", {
  pkg_dir <- withr::local_tempdir()
  resources <- list(main = tibble::tibble(species = c("Coho"), count = c(1L)))
  pkg_path <- create_sdp(
    resources,
    path = file.path(pkg_dir, "pkg-overwrite"),
    dataset_id = "edh-rebuild-overwrite",
    table_id = "main",
    seed_semantics = FALSE,
    check_updates = FALSE,
    overwrite = TRUE
  )

  xml_path <- file.path(pkg_path, "metadata", "metadata-edh-hnap.xml")
  write_edh_xml_from_sdp(pkg_path)
  expect_true(file.exists(xml_path))

  expect_error(
    write_edh_xml_from_sdp(pkg_path, overwrite = FALSE),
    "overwrite = TRUE"
  )
})
