#' Build ISO 19139 metadata XML for DFO Enterprise Data Hub export
#'
#' Generates a compact ISO 19139-style metadata XML document from
#' `dataset_meta`. This is intended as a starter export for DFO Enterprise
#' Data Hub / GeoNetwork workflows.
#'
#' The produced XML should be validated and enriched against your local EDH
#' profile before production upload.
#'
#' @param dataset_meta Data frame/tibble with exactly one row of
#'   dataset-level metadata.
#'   Required columns: `dataset_id`, `title`, `description`.
#'   Optional columns include: `creator`, `contact_name`, `contact_email`,
#'   `contact_org`, `contact_position`, `license`, `source_citation`,
#'   `temporal_start`, `temporal_end`, `spatial_extent`, `update_frequency`,
#'   `topic_categories`, `keywords`, and `security_classification`.
#' @param output_path Optional file path to write XML.
#' @param file_identifier Optional metadata file identifier.
#'   Defaults to `dataset_id`.
#' @param language ISO 639-2/T language code (default: `"eng"`).
#' @param date_stamp Metadata date stamp (default: `Sys.Date()`).
#'
#' @return Invisible list with elements `xml` (string) and `path`.
#' @export
#'
#' @examples
#' dataset_meta <- tibble::tibble(
#'   dataset_id = "fraser-coho-2024",
#'   title = "Fraser River Coho Escapement Data",
#'   description = "Sample escapement monitoring data for coho salmon in PFMA 29",
#'   contact_name = "Your Name",
#'   contact_email = "your.email@dfo-mpo.gc.ca",
#'   topic_categories = "biota;oceans",
#'   keywords = "coho;escapement;Fraser River",
#'   temporal_start = "2001",
#'   temporal_end = "2024"
#' )
#'
#' out <- tempfile(fileext = ".xml")
#' edh_build_iso19139_xml(dataset_meta, output_path = out)
edh_build_iso19139_xml <- function(dataset_meta,
                                   output_path = NULL,
                                   file_identifier = NULL,
                                   language = "eng",
                                   date_stamp = Sys.Date()) {
  if (!inherits(dataset_meta, "data.frame") || nrow(dataset_meta) != 1) {
    cli::cli_abort("{.arg dataset_meta} must be a single-row data frame/tibble")
  }

  required <- c("dataset_id", "title", "description")
  missing_required <- setdiff(required, names(dataset_meta))
  if (length(missing_required) > 0) {
    cli::cli_abort(
      "{.arg dataset_meta} is missing required column{?s}: {.val {missing_required}}"
    )
  }

  meta <- function(name, default = NA_character_) {
    if (!name %in% names(dataset_meta)) {
      return(default)
    }
    value <- dataset_meta[[name]][1]
    if (length(value) == 0 || is.na(value) || identical(value, "")) {
      return(default)
    }
    as.character(value)
  }

  split_multi <- function(x) {
    if (is.na(x) || identical(x, "")) {
      return(character(0))
    }
    values <- trimws(unlist(strsplit(x, "[;,]")))
    values <- values[nzchar(values)]
    unique(values)
  }

  add_text <- function(parent, node_name, value) {
    if (is.na(value) || identical(value, "")) {
      return(invisible(NULL))
    }
    node <- xml2::xml_add_child(parent, node_name)
    xml2::xml_add_child(node, "gco:CharacterString", value)
    invisible(node)
  }

  add_contact <- function(parent, role = "pointOfContact") {
    contact <- xml2::xml_add_child(parent, "gmd:contact")
    rp <- xml2::xml_add_child(contact, "gmd:CI_ResponsibleParty")

    org <- meta("contact_org", meta("creator", meta("contact_name", meta("dataset_id"))))
    add_text(rp, "gmd:organisationName", org)
    add_text(rp, "gmd:positionName", meta("contact_position"))
    add_text(rp, "gmd:individualName", meta("contact_name"))

    email <- meta("contact_email")
    if (!is.na(email) && nzchar(email)) {
      ci_contact <- xml2::xml_add_child(rp, "gmd:contactInfo")
      ci <- xml2::xml_add_child(ci_contact, "gmd:CI_Contact")
      address <- xml2::xml_add_child(ci, "gmd:address")
      ci_address <- xml2::xml_add_child(address, "gmd:CI_Address")
      email_node <- xml2::xml_add_child(ci_address, "gmd:electronicMailAddress")
      xml2::xml_add_child(email_node, "gco:CharacterString", email)
    }

    role_node <- xml2::xml_add_child(rp, "gmd:role")
    xml2::xml_add_child(
      role_node,
      "gmd:CI_RoleCode",
      role,
      codeList = "http://www.isotc211.org/2005/resources/codeList.xml#CI_RoleCode",
      codeListValue = role
    )

    invisible(contact)
  }

  fid <- file_identifier
  if (is.null(fid) || is.na(fid) || identical(fid, "")) {
    fid <- meta("dataset_id")
  }

  root <- xml2::xml_new_root(
    "gmd:MD_Metadata",
    "xmlns:gmd" = "http://www.isotc211.org/2005/gmd",
    "xmlns:gco" = "http://www.isotc211.org/2005/gco",
    "xmlns:gml" = "http://www.opengis.net/gml"
  )

  file_id <- xml2::xml_add_child(root, "gmd:fileIdentifier")
  xml2::xml_add_child(file_id, "gco:CharacterString", fid)

  lang <- xml2::xml_add_child(root, "gmd:language")
  xml2::xml_add_child(
    lang,
    "gmd:LanguageCode",
    language,
    codeList = "http://www.loc.gov/standards/iso639-2/",
    codeListValue = language
  )

  charset <- xml2::xml_add_child(root, "gmd:characterSet")
  xml2::xml_add_child(
    charset,
    "gmd:MD_CharacterSetCode",
    "utf8",
    codeList = "http://www.isotc211.org/2005/resources/codeList.xml#MD_CharacterSetCode",
    codeListValue = "utf8"
  )

  scope <- xml2::xml_add_child(root, "gmd:hierarchyLevel")
  xml2::xml_add_child(
    scope,
    "gmd:MD_ScopeCode",
    "dataset",
    codeList = "http://www.isotc211.org/2005/resources/codeList.xml#MD_ScopeCode",
    codeListValue = "dataset"
  )

  add_contact(root, role = "pointOfContact")

  date_node <- xml2::xml_add_child(root, "gmd:dateStamp")
  xml2::xml_add_child(date_node, "gco:Date", as.character(date_stamp))

  add_text(root, "gmd:metadataStandardName", "ISO 19115:2003/19139")
  add_text(root, "gmd:metadataStandardVersion", "ISO 19139")

  identification_info <- xml2::xml_add_child(root, "gmd:identificationInfo")
  data_ident <- xml2::xml_add_child(identification_info, "gmd:MD_DataIdentification")

  citation <- xml2::xml_add_child(data_ident, "gmd:citation")
  ci_citation <- xml2::xml_add_child(citation, "gmd:CI_Citation")
  add_text(ci_citation, "gmd:title", meta("title"))

  citation_date <- xml2::xml_add_child(ci_citation, "gmd:date")
  ci_date <- xml2::xml_add_child(citation_date, "gmd:CI_Date")
  date_val <- xml2::xml_add_child(ci_date, "gmd:date")
  xml2::xml_add_child(date_val, "gco:Date", as.character(date_stamp))
  date_type <- xml2::xml_add_child(ci_date, "gmd:dateType")
  xml2::xml_add_child(
    date_type,
    "gmd:CI_DateTypeCode",
    "publication",
    codeList = "http://www.isotc211.org/2005/resources/codeList.xml#CI_DateTypeCode",
    codeListValue = "publication"
  )

  add_text(data_ident, "gmd:abstract", meta("description"))

  poc <- xml2::xml_add_child(data_ident, "gmd:pointOfContact")
  poc_rp <- xml2::xml_add_child(poc, "gmd:CI_ResponsibleParty")
  add_text(poc_rp, "gmd:organisationName", meta("contact_org", meta("creator", meta("dataset_id"))))
  add_text(poc_rp, "gmd:individualName", meta("contact_name"))
  add_text(poc_rp, "gmd:positionName", meta("contact_position"))

  poc_email <- meta("contact_email")
  if (!is.na(poc_email) && nzchar(poc_email)) {
    poc_ci_contact <- xml2::xml_add_child(poc_rp, "gmd:contactInfo")
    poc_ci <- xml2::xml_add_child(poc_ci_contact, "gmd:CI_Contact")
    poc_address <- xml2::xml_add_child(poc_ci, "gmd:address")
    poc_ci_address <- xml2::xml_add_child(poc_address, "gmd:CI_Address")
    poc_email_node <- xml2::xml_add_child(poc_ci_address, "gmd:electronicMailAddress")
    xml2::xml_add_child(poc_email_node, "gco:CharacterString", poc_email)
  }

  poc_role <- xml2::xml_add_child(poc_rp, "gmd:role")
  xml2::xml_add_child(
    poc_role,
    "gmd:CI_RoleCode",
    "pointOfContact",
    codeList = "http://www.isotc211.org/2005/resources/codeList.xml#CI_RoleCode",
    codeListValue = "pointOfContact"
  )

  keywords <- split_multi(meta("keywords"))
  if (length(keywords) > 0) {
    desc_keywords <- xml2::xml_add_child(data_ident, "gmd:descriptiveKeywords")
    md_keywords <- xml2::xml_add_child(desc_keywords, "gmd:MD_Keywords")
    for (kw in keywords) {
      kw_node <- xml2::xml_add_child(md_keywords, "gmd:keyword")
      xml2::xml_add_child(kw_node, "gco:CharacterString", kw)
    }
  }

  topic_categories <- split_multi(meta("topic_categories"))
  if (length(topic_categories) > 0) {
    for (topic in topic_categories) {
      topic_node <- xml2::xml_add_child(data_ident, "gmd:topicCategory")
      xml2::xml_add_child(topic_node, "gmd:MD_TopicCategoryCode", topic)
    }
  }

  license <- meta("license")
  if (!is.na(license) && nzchar(license)) {
    constraints <- xml2::xml_add_child(data_ident, "gmd:resourceConstraints")
    legal <- xml2::xml_add_child(constraints, "gmd:MD_LegalConstraints")
    limitation <- xml2::xml_add_child(legal, "gmd:useLimitation")
    xml2::xml_add_child(limitation, "gco:CharacterString", license)
  }

  extent <- xml2::xml_add_child(data_ident, "gmd:extent")
  ex_extent <- xml2::xml_add_child(extent, "gmd:EX_Extent")

  spatial_extent <- meta("spatial_extent")
  if (!is.na(spatial_extent) && nzchar(spatial_extent)) {
    geo_el <- xml2::xml_add_child(ex_extent, "gmd:geographicElement")
    geo_desc <- xml2::xml_add_child(geo_el, "gmd:EX_GeographicDescription")
    geo_id <- xml2::xml_add_child(geo_desc, "gmd:geographicIdentifier")
    md_id <- xml2::xml_add_child(geo_id, "gmd:MD_Identifier")
    code <- xml2::xml_add_child(md_id, "gmd:code")
    xml2::xml_add_child(code, "gco:CharacterString", spatial_extent)
  }

  temporal_start <- meta("temporal_start")
  temporal_end <- meta("temporal_end")
  if ((!is.na(temporal_start) && nzchar(temporal_start)) ||
      (!is.na(temporal_end) && nzchar(temporal_end))) {
    temporal_el <- xml2::xml_add_child(ex_extent, "gmd:temporalElement")
    ex_temporal <- xml2::xml_add_child(temporal_el, "gmd:EX_TemporalExtent")
    temporal_extent <- xml2::xml_add_child(ex_temporal, "gmd:extent")

    period_id <- gsub("[^A-Za-z0-9]", "", fid)
    if (identical(period_id, "")) {
      period_id <- "dataset"
    }

    period <- xml2::xml_add_child(temporal_extent, "gml:TimePeriod")
    xml2::xml_set_attr(period, "gml:id", paste0("tp-", period_id))

    if (!is.na(temporal_start) && nzchar(temporal_start)) {
      xml2::xml_add_child(period, "gml:beginPosition", temporal_start)
    }
    if (!is.na(temporal_end) && nzchar(temporal_end)) {
      xml2::xml_add_child(period, "gml:endPosition", temporal_end)
    }
  }

  supplemental <- c(
    if (!is.na(meta("source_citation"))) sprintf("source_citation=%s", meta("source_citation")) else NULL,
    if (!is.na(meta("update_frequency"))) sprintf("update_frequency=%s", meta("update_frequency")) else NULL,
    if (!is.na(meta("security_classification"))) sprintf("security_classification=%s", meta("security_classification")) else NULL
  )

  if (length(supplemental) > 0) {
    add_text(data_ident, "gmd:supplementalInformation", paste(supplemental, collapse = "; "))
  }

  xml_text <- as.character(root)

  if (!is.null(output_path)) {
    xml2::write_xml(root, output_path, options = "format")
  }

  invisible(list(xml = xml_text, path = output_path))
}
