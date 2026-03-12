#' Build ISO 19139 or HNAP-aware metadata XML for DFO Enterprise Data Hub export
#'
#' Generates metadata XML from `dataset_meta` for DFO Enterprise Data Hub /
#' GeoNetwork workflows. The default `"dfo_edh_hnap"` profile now emits the
#' more practical North American Profile / EDH-oriented structure with
#' deterministic file identifiers, maintenance/status, legal constraints,
#' optional distribution info, reference system support, bilingual locale
#' scaffolding, and richer contact metadata when available. The legacy
#' `"iso19139"` profile remains available as a compact fallback export.
#'
#' The produced XML should still be validated and enriched against your local EDH
#' profile before production upload.
#'
#' @param dataset_meta Data frame/tibble with exactly one row of
#'   dataset-level metadata.
#'   Required columns: `dataset_id`, `title`, `description`.
#'   Common optional columns include: `creator`, `contact_name`, `contact_email`,
#'   `contact_org`, `contact_position`, `license`, `source_citation`,
#'   `temporal_start`, `temporal_end`, `spatial_extent`, `update_frequency`,
#'   `topic_categories`, `keywords`, `security_classification`, `created`,
#'   `modified`, `provenance_note`, `status`, `distribution_url`, `download_url`,
#'   `reference_system`, `bbox_west`, `bbox_east`, `bbox_south`, `bbox_north`,
#'   plus optional French-localized fields such as `title_fr` and
#'   `description_fr`.
#' @param output_path Optional file path to write XML.
#' @param file_identifier Optional metadata file identifier.
#'   For the default `profile = "dfo_edh_hnap"`, non-UUID identifiers are
#'   converted to a deterministic UUID-like value and the original `dataset_id`
#'   is preserved in `gmd:dataSetURI` / citation identifiers. For
#'   `profile = "iso19139"`, this defaults to `dataset_id`.
#' @param language ISO 639-2/T language code for the primary metadata language
#'   (default: `"eng"`).
#' @param date_stamp Metadata date stamp (default: `Sys.Date()`). When
#'   `dataset_meta$modified` is present, that value is preferred.
#' @param profile Metadata export profile. `"dfo_edh_hnap"` (the default)
#'   returns the richer HNAP/EDH-oriented structure; `"iso19139"` keeps the
#'   original lightweight fallback export.
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
#'
#' out_iso <- tempfile(fileext = ".xml")
#' edh_build_iso19139_xml(
#'   dataset_meta,
#'   output_path = out_iso,
#'   profile = "iso19139"
#' )
edh_build_iso19139_xml <- function(dataset_meta,
                                   output_path = NULL,
                                   file_identifier = NULL,
                                   language = "eng",
                                   date_stamp = Sys.Date(),
                                   profile = c("dfo_edh_hnap", "iso19139")) {
  profile <- match.arg(profile)

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

  meta <- function(name,
                   default = NA_character_,
                   aliases = character(),
                   allow_blank = FALSE) {
    candidates <- unique(c(name, aliases))
    for (candidate in candidates) {
      if (!candidate %in% names(dataset_meta)) {
        next
      }
      value <- dataset_meta[[candidate]][1]
      if (length(value) == 0 || is.null(value) || is.na(value)) {
        next
      }
      if (inherits(value, c("POSIXct", "POSIXt", "Date"))) {
        value <- as.character(value)
      } else {
        value <- as.character(value)
      }
      if (!allow_blank && !nzchar(trimws(value))) {
        next
      }
      return(value)
    }
    default
  }

  meta_fr <- function(name, default = NA_character_) {
    meta(
      paste0(name, "_fr"),
      default = default,
      aliases = c(paste0(name, "_fra"), paste0(name, "_fr_ca"))
    )
  }

  split_multi <- function(x) {
    if (is.na(x) || identical(trimws(x), "")) {
      return(character(0))
    }
    values <- trimws(unlist(strsplit(x, "[;,]")))
    values <- values[nzchar(values)]
    values[!duplicated(tolower(values))]
  }

  normalize_token <- function(x) {
    x <- tolower(trimws(x))
    x <- gsub("[_-]+", " ", x)
    x <- gsub("\\s+", " ", x)
    x
  }

  normalize_codelist_value <- function(x, mapping, field, fallback) {
    if (is.null(x) || length(x) == 0 || is.na(x)) {
      return(fallback)
    }
    token <- normalize_token(x)
    if (is.na(token) || !nzchar(token)) {
      return(fallback)
    }
    idx <- match(token, names(mapping))
    if (!is.na(idx)) {
      return(unname(mapping[[idx]]))
    }
    cli::cli_warn(
      "Unrecognized {.field {field}} value {.val {x}}; using {.val {fallback}}."
    )
    fallback
  }

  looks_like_uuid <- function(x) {
    grepl(
      "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$",
      x
    )
  }

  deterministic_uuid <- function(x) {
    tmp <- tempfile(fileext = ".txt")
    on.exit(unlink(tmp), add = TRUE)
    writeLines(enc2utf8(x), tmp, useBytes = TRUE)
    hash <- unname(tools::md5sum(tmp))
    substr(hash, 13, 13) <- "5"
    variant_hex <- c("8", "9", "a", "b")
    variant_idx <- (strtoi(substr(hash, 17, 17), base = 16L) %% 4L) + 1L
    substr(hash, 17, 17) <- variant_hex[[variant_idx]]
    paste(
      substr(hash, 1, 8),
      substr(hash, 9, 12),
      substr(hash, 13, 16),
      substr(hash, 17, 20),
      substr(hash, 21, 32),
      sep = "-"
    )
  }

  looks_organizational <- function(x) {
    if (is.na(x) || !nzchar(trimws(x))) {
      return(FALSE)
    }
    grepl(
      "section|branch|team|office|program|division|unit|committee|ministry|department|fisheries and oceans|government of canada|science",
      x,
      ignore.case = TRUE
    )
  }

  non_empty <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return(FALSE)
    }
    !is.na(x[[1]]) && nzchar(trimws(as.character(x[[1]])))
  }

  parse_numeric <- function(...) {
    candidates <- list(...)
    for (value in candidates) {
      if (is.null(value) || length(value) == 0 || is.na(value)) {
        next
      }
      out <- suppressWarnings(as.numeric(value))
      if (!is.na(out)) {
        return(out)
      }
    }
    NA_real_
  }

  date_value_node <- function(parent, value) {
    if (!non_empty(value)) {
      return(invisible(NULL))
    }
    node_name <- if (grepl("T", value, fixed = TRUE)) "gco:DateTime" else "gco:Date"
    xml2::xml_add_child(parent, node_name, value)
    invisible(parent)
  }

  add_text <- function(parent, node_name, value) {
    if (!non_empty(value)) {
      return(invisible(NULL))
    }
    node <- xml2::xml_add_child(parent, node_name)
    xml2::xml_add_child(node, "gco:CharacterString", value)
    invisible(node)
  }

  add_localized_text <- function(parent,
                                 node_name,
                                 value,
                                 localized_value = NA_character_,
                                 locale_id = "fra",
                                 include_locale = FALSE) {
    if (!non_empty(value)) {
      return(invisible(NULL))
    }

    node <- xml2::xml_add_child(parent, node_name)
    xml2::xml_add_child(node, "gco:CharacterString", value)

    if (include_locale) {
      if (!non_empty(localized_value)) {
        localized_value <- value
      }
      xml2::xml_set_attr(node, "xsi:type", "gmd:PT_FreeText_PropertyType")
      pt_free_text <- xml2::xml_add_child(node, "gmd:PT_FreeText")
      text_group <- xml2::xml_add_child(pt_free_text, "gmd:textGroup")
      loc <- xml2::xml_add_child(text_group, "gmd:LocalisedCharacterString", localized_value)
      xml2::xml_set_attr(loc, "locale", paste0("#", locale_id))
    }

    invisible(node)
  }

  add_code <- function(parent,
                       node_name,
                       child_name,
                       value,
                       code_list,
                       text = value) {
    if (!non_empty(value)) {
      return(invisible(NULL))
    }
    node <- xml2::xml_add_child(parent, node_name)
    xml2::xml_add_child(
      node,
      child_name,
      text,
      codeList = code_list,
      codeListValue = value
    )
    invisible(node)
  }

  add_citation_date <- function(parent,
                                value,
                                date_type,
                                code_list,
                                text = date_type) {
    if (!non_empty(value)) {
      return(invisible(NULL))
    }
    citation_date <- xml2::xml_add_child(parent, "gmd:date")
    ci_date <- xml2::xml_add_child(citation_date, "gmd:CI_Date")
    date_node <- xml2::xml_add_child(ci_date, "gmd:date")
    date_value_node(date_node, value)
    add_code(
      ci_date,
      "gmd:dateType",
      "gmd:CI_DateTypeCode",
      value = date_type,
      code_list = code_list,
      text = text
    )
    invisible(citation_date)
  }

  add_online_resource <- function(parent,
                                  url,
                                  name = NA_character_,
                                  description = NA_character_,
                                  protocol = NA_character_,
                                  function_code = NA_character_,
                                  function_code_list = NA_character_) {
    if (!non_empty(url)) {
      return(invisible(NULL))
    }

    online <- xml2::xml_add_child(parent, "gmd:CI_OnlineResource")
    linkage <- xml2::xml_add_child(online, "gmd:linkage")
    xml2::xml_add_child(linkage, "gmd:URL", url)
    add_text(online, "gmd:protocol", protocol)
    add_text(online, "gmd:name", name)
    add_text(online, "gmd:description", description)

    if (non_empty(function_code) && non_empty(function_code_list)) {
      add_code(
        online,
        "gmd:function",
        "gmd:CI_OnLineFunctionCode",
        value = function_code,
        code_list = function_code_list,
        text = function_code
      )
    }

    invisible(online)
  }

  add_reference_system <- function(root, code_value, include_locale = FALSE) {
    if (!non_empty(code_value)) {
      return(invisible(NULL))
    }

    ref_info <- xml2::xml_add_child(root, "gmd:referenceSystemInfo")
    md_ref <- xml2::xml_add_child(ref_info, "gmd:MD_ReferenceSystem")
    rs_id_parent <- xml2::xml_add_child(md_ref, "gmd:referenceSystemIdentifier")
    rs_id <- xml2::xml_add_child(rs_id_parent, "gmd:RS_Identifier")
    add_localized_text(rs_id, "gmd:code", code_value, include_locale = include_locale)
    add_text(rs_id, "gmd:codeSpace", meta("reference_system_codespace", aliases = c("crs_codespace")))
    add_text(rs_id, "gmd:version", meta("reference_system_version", aliases = c("crs_version")))
    invisible(ref_info)
  }

  add_bounding_box <- function(ex_extent) {
    west <- parse_numeric(
      meta("bbox_west", aliases = c("west_bound_longitude", "westBoundLongitude"))
    )
    east <- parse_numeric(
      meta("bbox_east", aliases = c("east_bound_longitude", "eastBoundLongitude"))
    )
    south <- parse_numeric(
      meta("bbox_south", aliases = c("south_bound_latitude", "southBoundLatitude"))
    )
    north <- parse_numeric(
      meta("bbox_north", aliases = c("north_bound_latitude", "northBoundLatitude"))
    )

    if (any(is.na(c(west, east, south, north)))) {
      return(invisible(NULL))
    }

    geo_el <- xml2::xml_add_child(ex_extent, "gmd:geographicElement")
    bbox <- xml2::xml_add_child(geo_el, "gmd:EX_GeographicBoundingBox")

    west_node <- xml2::xml_add_child(bbox, "gmd:westBoundLongitude")
    xml2::xml_add_child(west_node, "gco:Decimal", format(west, scientific = FALSE, trim = TRUE))
    east_node <- xml2::xml_add_child(bbox, "gmd:eastBoundLongitude")
    xml2::xml_add_child(east_node, "gco:Decimal", format(east, scientific = FALSE, trim = TRUE))
    south_node <- xml2::xml_add_child(bbox, "gmd:southBoundLatitude")
    xml2::xml_add_child(south_node, "gco:Decimal", format(south, scientific = FALSE, trim = TRUE))
    north_node <- xml2::xml_add_child(bbox, "gmd:northBoundLatitude")
    xml2::xml_add_child(north_node, "gco:Decimal", format(north, scientific = FALSE, trim = TRUE))

    invisible(bbox)
  }

  add_temporal_extent <- function(ex_extent, identifier_seed) {
    temporal_start <- meta("temporal_start")
    temporal_end <- meta("temporal_end")
    if (!non_empty(temporal_start) && !non_empty(temporal_end)) {
      return(invisible(NULL))
    }

    temporal_el <- xml2::xml_add_child(ex_extent, "gmd:temporalElement")
    ex_temporal <- xml2::xml_add_child(temporal_el, "gmd:EX_TemporalExtent")
    temporal_extent <- xml2::xml_add_child(ex_temporal, "gmd:extent")

    period_id <- gsub("[^A-Za-z0-9]", "", identifier_seed)
    if (identical(period_id, "")) {
      period_id <- "dataset"
    }

    period <- xml2::xml_add_child(temporal_extent, "gml:TimePeriod")
    xml2::xml_set_attr(period, "gml:id", paste0("tp-", period_id))

    if (non_empty(temporal_start)) {
      xml2::xml_add_child(period, "gml:beginPosition", temporal_start)
    }
    if (non_empty(temporal_end)) {
      xml2::xml_add_child(period, "gml:endPosition", temporal_end)
    }

    invisible(period)
  }

  add_contact_party <- function(parent,
                                wrapper_name,
                                role,
                                code_list_role,
                                include_locale = FALSE,
                                use_creator = FALSE) {
    if (!non_empty(role)) {
      role <- "pointOfContact"
    }

    prefix <- if (use_creator) "creator" else "contact"
    org <- meta(
      paste0(prefix, "_org"),
      aliases = if (use_creator) c("creator") else c("creator")
    )
    individual <- meta(paste0(prefix, "_name"), aliases = if (use_creator) c("creator_name") else character())
    position <- meta(paste0(prefix, "_position"), aliases = if (use_creator) c("creator_position") else character())
    email <- meta(paste0(prefix, "_email"), aliases = if (use_creator) c("creator_email") else character())
    phone <- meta(paste0(prefix, "_phone"), aliases = if (use_creator) c("creator_phone") else character())
    delivery <- meta(paste0(prefix, "_address"), aliases = if (use_creator) c("creator_delivery_point") else c("contact_delivery_point"))
    city <- meta(paste0(prefix, "_city"))
    admin <- meta(paste0(prefix, "_admin_area"), aliases = c(paste0(prefix, "_province"), paste0(prefix, "_administrative_area")))
    postal <- meta(paste0(prefix, "_postal_code"))
    country <- meta(paste0(prefix, "_country"), default = if (profile == "dfo_edh_hnap") "Canada" else NA_character_)
    url <- meta(paste0(prefix, "_url"), aliases = if (use_creator) c("creator_online_resource") else c("contact_online_resource"))

    if (looks_organizational(individual) && !non_empty(org)) {
      org <- individual
      individual <- NA_character_
    }

    if (!non_empty(org) && !non_empty(individual) && !non_empty(email)) {
      return(invisible(NULL))
    }

    wrapper <- xml2::xml_add_child(parent, wrapper_name)
    rp <- xml2::xml_add_child(wrapper, "gmd:CI_ResponsibleParty")

    add_localized_text(
      rp,
      "gmd:individualName",
      individual,
      localized_value = meta_fr(paste0(prefix, "_name")),
      include_locale = include_locale
    )
    add_localized_text(
      rp,
      "gmd:organisationName",
      org,
      localized_value = meta_fr(paste0(prefix, "_org")),
      include_locale = include_locale
    )
    add_localized_text(
      rp,
      "gmd:positionName",
      position,
      localized_value = meta_fr(paste0(prefix, "_position")),
      include_locale = include_locale
    )

    if (non_empty(email) || non_empty(phone) || non_empty(delivery) || non_empty(city) ||
        non_empty(admin) || non_empty(postal) || non_empty(country) || non_empty(url)) {
      ci_contact_parent <- xml2::xml_add_child(rp, "gmd:contactInfo")
      ci_contact <- xml2::xml_add_child(ci_contact_parent, "gmd:CI_Contact")

      if (non_empty(phone)) {
        phone_parent <- xml2::xml_add_child(ci_contact, "gmd:phone")
        telephone <- xml2::xml_add_child(phone_parent, "gmd:CI_Telephone")
        add_localized_text(
          telephone,
          "gmd:voice",
          phone,
          localized_value = meta_fr(paste0(prefix, "_phone")),
          include_locale = include_locale
        )
      }

      if (non_empty(delivery) || non_empty(city) || non_empty(admin) || non_empty(postal) ||
          non_empty(country) || non_empty(email)) {
        address_parent <- xml2::xml_add_child(ci_contact, "gmd:address")
        ci_address <- xml2::xml_add_child(address_parent, "gmd:CI_Address")
        add_localized_text(
          ci_address,
          "gmd:deliveryPoint",
          delivery,
          localized_value = meta_fr(paste0(prefix, "_address")),
          include_locale = include_locale
        )
        add_localized_text(
          ci_address,
          "gmd:city",
          city,
          localized_value = meta_fr(paste0(prefix, "_city")),
          include_locale = include_locale
        )
        add_localized_text(
          ci_address,
          "gmd:administrativeArea",
          admin,
          localized_value = meta_fr(paste0(prefix, "_admin_area")),
          include_locale = include_locale
        )
        add_text(ci_address, "gmd:postalCode", postal)
        add_localized_text(
          ci_address,
          "gmd:country",
          country,
          localized_value = meta_fr(paste0(prefix, "_country")),
          include_locale = include_locale
        )
        add_localized_text(
          ci_address,
          "gmd:electronicMailAddress",
          email,
          localized_value = email,
          include_locale = include_locale
        )
      }

      if (non_empty(url)) {
        online_parent <- xml2::xml_add_child(ci_contact, "gmd:onlineResource")
        add_online_resource(
          online_parent,
          url = url,
          name = meta(paste0(prefix, "_url_name")),
          description = meta(paste0(prefix, "_url_description")),
          protocol = meta(paste0(prefix, "_url_protocol"), default = "http")
        )
      }
    }

    add_code(
      rp,
      "gmd:role",
      "gmd:CI_RoleCode",
      value = role,
      code_list = code_list_role,
      text = role
    )

    invisible(wrapper)
  }

  update_frequency_map <- c(
    "continual" = "continual",
    "continuous" = "continual",
    "daily" = "daily",
    "weekly" = "weekly",
    "fortnightly" = "fortnightly",
    "biweekly" = "fortnightly",
    "monthly" = "monthly",
    "quarterly" = "quarterly",
    "biannually" = "biannually",
    "semiannual" = "biannually",
    "semiannually" = "biannually",
    "annually" = "annually",
    "annual" = "annually",
    "yearly" = "annually",
    "asneeded" = "asNeeded",
    "as needed" = "asNeeded",
    "ad hoc" = "asNeeded",
    "adhoc" = "asNeeded",
    "on demand" = "asNeeded",
    "irregular" = "irregular",
    "notplanned" = "notPlanned",
    "not planned" = "notPlanned",
    "none" = "notPlanned",
    "unknown" = "unknown"
  )

  classification_map <- c(
    "unclassified" = "unclassified",
    "public" = "unclassified",
    "open" = "unclassified",
    "restricted" = "restricted",
    "confidential" = "confidential",
    "protected" = "confidential",
    "protected a" = "confidential",
    "protected b" = "confidential",
    "protected c" = "confidential",
    "secret" = "secret",
    "top secret" = "topSecret",
    "topsecret" = "topSecret",
    "unknown" = "unclassified"
  )

  status_map <- c(
    "completed" = "completed",
    "complete" = "completed",
    "historical archive" = "historicalArchive",
    "historicalarchive" = "historicalArchive",
    "obsolete" = "obsolete",
    "ongoing" = "onGoing",
    "on going" = "onGoing",
    "planned" = "planned",
    "required" = "required",
    "under development" = "underDevelopment",
    "underdevelopment" = "underDevelopment"
  )

  include_locale <- identical(profile, "dfo_edh_hnap")
  locale_id <- "fra"

  iso_code_list <- function(code) {
    paste0("http://www.isotc211.org/2005/resources/codeList.xml#", code)
  }

  hnap_code_list <- function(code) {
    paste0("http://nap.geogratis.gc.ca/metadata/register/napMetadataRegister.xml#", code)
  }

  code_list_role <- if (include_locale) hnap_code_list("CI_RoleCode") else iso_code_list("CI_RoleCode")
  code_list_date_type <- if (include_locale) hnap_code_list("CI_DateTypeCode") else iso_code_list("CI_DateTypeCode")
  code_list_charset <- if (include_locale) hnap_code_list("MD_CharacterSetCode") else iso_code_list("MD_CharacterSetCode")
  code_list_scope <- if (include_locale) hnap_code_list("MD_ScopeCode") else iso_code_list("MD_ScopeCode")
  code_list_maintenance <- if (include_locale) hnap_code_list("MD_MaintenanceFrequencyCode") else iso_code_list("MD_MaintenanceFrequencyCode")
  code_list_status <- if (include_locale) hnap_code_list("MD_ProgressCode") else iso_code_list("MD_ProgressCode")
  code_list_restriction <- if (include_locale) hnap_code_list("MD_RestrictionCode") else iso_code_list("MD_RestrictionCode")
  code_list_classification <- if (include_locale) hnap_code_list("MD_ClassificationCode") else iso_code_list("MD_ClassificationCode")
  code_list_online_function <- if (include_locale) hnap_code_list("CI_OnLineFunctionCode") else iso_code_list("CI_OnLineFunctionCode")

  dataset_id <- meta("dataset_id")
  title <- meta("title")
  title_fr <- meta_fr("title")
  description <- meta("description")
  description_fr <- meta_fr("description")
  original_identifier <- dataset_id

  fid <- file_identifier
  if (!non_empty(fid)) {
    if (profile == "dfo_edh_hnap" && !looks_like_uuid(dataset_id)) {
      fid <- deterministic_uuid(dataset_id)
    } else {
      fid <- dataset_id
    }
  }

  effective_date_stamp <- meta("modified", default = as.character(date_stamp))
  update_frequency <- normalize_codelist_value(
    meta("update_frequency", default = if (profile == "dfo_edh_hnap") "unknown" else NA_character_),
    update_frequency_map,
    field = "update_frequency",
    fallback = if (profile == "dfo_edh_hnap") "unknown" else NA_character_
  )
  security_classification <- normalize_codelist_value(
    meta(
      "security_classification",
      default = if (profile == "dfo_edh_hnap") "unclassified" else NA_character_
    ),
    classification_map,
    field = "security_classification",
    fallback = if (profile == "dfo_edh_hnap") "unclassified" else NA_character_
  )
  status_value <- normalize_codelist_value(
    meta("status", default = if (profile == "dfo_edh_hnap") "completed" else NA_character_),
    status_map,
    field = "status",
    fallback = if (profile == "dfo_edh_hnap") "completed" else NA_character_
  )

  root <- xml2::xml_new_root(
    "gmd:MD_Metadata",
    "xmlns:gmd" = "http://www.isotc211.org/2005/gmd",
    "xmlns:gco" = "http://www.isotc211.org/2005/gco",
    "xmlns:gml" = if (include_locale) "http://www.opengis.net/gml/3.2" else "http://www.opengis.net/gml",
    "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance"
  )

  if (include_locale) {
    xml2::xml_set_attr(
      root,
      "xsi:schemaLocation",
      paste(
        "http://www.isotc211.org/2005/gmd",
        "http://nap.geogratis.gc.ca/metadata/tools/schemas/metadata/can-cgsb-171.100-2009-a/gmd/gmd.xsd",
        "http://www.isotc211.org/2005/srv",
        "http://nap.geogratis.gc.ca/metadata/tools/schemas/metadata/can-cgsb-171.100-2009-a/srv/srv.xsd",
        "http://www.geconnections.org/nap/napMetadataTools/napXsd/napm",
        "http://nap.geogratis.gc.ca/metadata/tools/schemas/metadata/can-cgsb-171.100-2009-a/napm/napm.xsd"
      )
    )
  }

  file_id <- xml2::xml_add_child(root, "gmd:fileIdentifier")
  xml2::xml_add_child(file_id, "gco:CharacterString", fid)

  language_node <- xml2::xml_add_child(root, "gmd:language")
  if (include_locale) {
    xml2::xml_add_child(language_node, "gco:CharacterString", paste0(language, "; CAN"))
  } else {
    xml2::xml_add_child(
      language_node,
      "gmd:LanguageCode",
      language,
      codeList = "http://www.loc.gov/standards/iso639-2/",
      codeListValue = language
    )
  }

  add_code(
    root,
    "gmd:characterSet",
    "gmd:MD_CharacterSetCode",
    value = "utf8",
    code_list = code_list_charset,
    text = if (include_locale) "utf8; utf8" else "utf8"
  )

  hierarchy_value <- if (profile == "dfo_edh_hnap") {
    meta("hierarchy_level", default = "nonGeographicDataset")
  } else {
    meta("hierarchy_level", default = "dataset")
  }
  add_code(
    root,
    "gmd:hierarchyLevel",
    "gmd:MD_ScopeCode",
    value = hierarchy_value,
    code_list = code_list_scope,
    text = hierarchy_value
  )

  add_contact_party(
    root,
    wrapper_name = "gmd:contact",
    role = meta("contact_role", default = "pointOfContact"),
    code_list_role = code_list_role,
    include_locale = include_locale,
    use_creator = FALSE
  )

  date_node <- xml2::xml_add_child(root, "gmd:dateStamp")
  date_value_node(date_node, effective_date_stamp)

  if (include_locale) {
    add_localized_text(
      root,
      "gmd:metadataStandardName",
      "North American Profile of ISO 19115:2003 - Geographic information - Metadata",
      localized_value = "Profil nord-am\u00e9ricain de la norme ISO 19115:2003 - Information g\u00e9ographique - M\u00e9tadonn\u00e9es",
      include_locale = TRUE
    )
    add_text(root, "gmd:metadataStandardVersion", "CAN/CGSB-171.100-2009")

    data_set_uri <- meta(
      "dataset_uri",
      aliases = c("data_set_uri", "landing_page", "dataset_url"),
      default = if (!identical(fid, original_identifier)) original_identifier else NA_character_
    )
    uri_node <- xml2::xml_add_child(root, "gmd:dataSetURI")
    if (non_empty(data_set_uri)) {
      xml2::xml_add_child(uri_node, "gco:CharacterString", data_set_uri)
    } else {
      xml2::xml_set_attr(uri_node, "gco:nilReason", "missing")
      xml2::xml_add_child(uri_node, "gco:CharacterString", "")
    }

    locale_node <- xml2::xml_add_child(root, "gmd:locale")
    pt_locale <- xml2::xml_add_child(locale_node, "gmd:PT_Locale")
    xml2::xml_set_attr(pt_locale, "id", locale_id)
    lang_code <- xml2::xml_add_child(pt_locale, "gmd:languageCode")
    xml2::xml_add_child(
      lang_code,
      "gmd:LanguageCode",
      "French; Fran\u00e7ais",
      codeList = hnap_code_list("LanguageCode"),
      codeListValue = locale_id
    )
    country_node <- xml2::xml_add_child(pt_locale, "gmd:country")
    xml2::xml_add_child(
      country_node,
      "gmd:Country",
      "Canada; Canada",
      codeList = hnap_code_list("Country"),
      codeListValue = "CAN"
    )
    add_code(
      pt_locale,
      "gmd:characterEncoding",
      "gmd:MD_CharacterSetCode",
      value = "utf8",
      code_list = code_list_charset,
      text = "utf8; utf8"
    )
  } else {
    add_text(root, "gmd:metadataStandardName", "ISO 19115:2003/19139")
    add_text(root, "gmd:metadataStandardVersion", "ISO 19139")
  }

  add_reference_system(
    root,
    meta("reference_system", aliases = c("crs", "epsg_code")),
    include_locale = include_locale
  )

  identification_info <- xml2::xml_add_child(root, "gmd:identificationInfo")
  data_ident <- xml2::xml_add_child(identification_info, "gmd:MD_DataIdentification")

  citation <- xml2::xml_add_child(data_ident, "gmd:citation")
  ci_citation <- xml2::xml_add_child(citation, "gmd:CI_Citation")
  add_localized_text(
    ci_citation,
    "gmd:title",
    title,
    localized_value = title_fr,
    include_locale = include_locale
  )

  created_value <- meta("created")
  add_citation_date(
    ci_citation,
    value = if (non_empty(meta("publication_date"))) meta("publication_date") else effective_date_stamp,
    date_type = "publication",
    code_list = code_list_date_type,
    text = if (include_locale) "publication" else "publication"
  )
  add_citation_date(
    ci_citation,
    value = created_value,
    date_type = "creation",
    code_list = code_list_date_type,
    text = if (include_locale) "creation" else "creation"
  )

  if (!identical(fid, original_identifier)) {
    identifier_node <- xml2::xml_add_child(ci_citation, "gmd:identifier")
    md_identifier <- xml2::xml_add_child(identifier_node, "gmd:MD_Identifier")
    add_text(md_identifier, "gmd:code", original_identifier)
  }

  if (non_empty(meta("source_citation"))) {
    add_localized_text(
      ci_citation,
      "gmd:otherCitationDetails",
      meta("source_citation"),
      localized_value = meta_fr("source_citation"),
      include_locale = include_locale
    )
  }

  add_contact_party(
    ci_citation,
    wrapper_name = "gmd:citedResponsibleParty",
    role = meta("creator_role", default = "principalInvestigator"),
    code_list_role = code_list_role,
    include_locale = include_locale,
    use_creator = TRUE
  )

  add_localized_text(
    data_ident,
    "gmd:abstract",
    description,
    localized_value = description_fr,
    include_locale = include_locale
  )

  add_contact_party(
    data_ident,
    wrapper_name = "gmd:pointOfContact",
    role = meta("point_of_contact_role", default = "pointOfContact"),
    code_list_role = code_list_role,
    include_locale = include_locale,
    use_creator = FALSE
  )

  keywords <- split_multi(meta("keywords"))
  dataset_type <- meta("dataset_type")
  if (non_empty(dataset_type)) {
    keywords <- c(keywords, dataset_type)
    keywords <- keywords[!duplicated(tolower(keywords))]
  }
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

  if (non_empty(status_value)) {
    add_code(
      data_ident,
      "gmd:status",
      "gmd:MD_ProgressCode",
      value = status_value,
      code_list = code_list_status,
      text = status_value
    )
  }

  if (non_empty(update_frequency)) {
    maintenance <- xml2::xml_add_child(data_ident, "gmd:resourceMaintenance")
    md_maintenance <- xml2::xml_add_child(maintenance, "gmd:MD_MaintenanceInformation")
    add_code(
      md_maintenance,
      "gmd:maintenanceAndUpdateFrequency",
      "gmd:MD_MaintenanceFrequencyCode",
      value = update_frequency,
      code_list = code_list_maintenance,
      text = update_frequency
    )
  }

  license <- meta("license")
  access_constraint <- meta(
    "access_constraints",
    default = if (non_empty(license)) "license" else NA_character_
  )
  use_constraint <- meta(
    "use_constraints",
    default = if (non_empty(license)) "license" else NA_character_
  )

  if (non_empty(license) || non_empty(access_constraint) || non_empty(use_constraint) ||
      non_empty(security_classification)) {
    constraints <- xml2::xml_add_child(data_ident, "gmd:resourceConstraints")
    legal <- xml2::xml_add_child(constraints, "gmd:MD_LegalConstraints")

    if (non_empty(license)) {
      limitation <- xml2::xml_add_child(legal, "gmd:useLimitation")
      xml2::xml_add_child(limitation, "gco:CharacterString", license)
    }

    if (non_empty(access_constraint)) {
      add_code(
        legal,
        "gmd:accessConstraints",
        "gmd:MD_RestrictionCode",
        value = access_constraint,
        code_list = code_list_restriction,
        text = access_constraint
      )
    }

    if (non_empty(use_constraint)) {
      add_code(
        legal,
        "gmd:useConstraints",
        "gmd:MD_RestrictionCode",
        value = use_constraint,
        code_list = code_list_restriction,
        text = use_constraint
      )
    }

    if (non_empty(security_classification)) {
      add_code(
        legal,
        "gmd:classification",
        "gmd:MD_ClassificationCode",
        value = security_classification,
        code_list = code_list_classification,
        text = security_classification
      )
    }
  }

  has_extent <- non_empty(meta("spatial_extent")) ||
    non_empty(meta("temporal_start")) ||
    non_empty(meta("temporal_end")) ||
    all(!is.na(c(
      parse_numeric(meta("bbox_west", aliases = c("west_bound_longitude", "westBoundLongitude"))),
      parse_numeric(meta("bbox_east", aliases = c("east_bound_longitude", "eastBoundLongitude"))),
      parse_numeric(meta("bbox_south", aliases = c("south_bound_latitude", "southBoundLatitude"))),
      parse_numeric(meta("bbox_north", aliases = c("north_bound_latitude", "northBoundLatitude")))
    )))

  if (has_extent) {
    extent <- xml2::xml_add_child(data_ident, "gmd:extent")
    ex_extent <- xml2::xml_add_child(extent, "gmd:EX_Extent")

    add_bounding_box(ex_extent)

    spatial_extent <- meta("spatial_extent")
    if (non_empty(spatial_extent) && profile == "iso19139") {
      geo_el <- xml2::xml_add_child(ex_extent, "gmd:geographicElement")
      geo_desc <- xml2::xml_add_child(geo_el, "gmd:EX_GeographicDescription")
      geo_id <- xml2::xml_add_child(geo_desc, "gmd:geographicIdentifier")
      md_id <- xml2::xml_add_child(geo_id, "gmd:MD_Identifier")
      add_text(md_id, "gmd:code", spatial_extent)
    }

    add_temporal_extent(ex_extent, fid)
  }

  supplemental_parts <- c(
    if (profile == "dfo_edh_hnap" && non_empty(meta("spatial_extent"))) {
      sprintf("spatial_extent=%s", meta("spatial_extent"))
    } else {
      NULL
    },
    if (non_empty(meta("provenance_note"))) sprintf("provenance_note=%s", meta("provenance_note")) else NULL,
    if (non_empty(meta("spec_version"))) sprintf("spec_version=%s", meta("spec_version")) else NULL
  )

  if (length(supplemental_parts) > 0) {
    add_localized_text(
      data_ident,
      "gmd:supplementalInformation",
      paste(supplemental_parts, collapse = "; "),
      localized_value = paste(supplemental_parts, collapse = "; "),
      include_locale = include_locale
    )
  }

  distribution_url <- meta(
    "distribution_url",
    aliases = c("download_url", "data_url", "access_url")
  )
  if (non_empty(distribution_url)) {
    dist_info <- xml2::xml_add_child(root, "gmd:distributionInfo")
    md_dist <- xml2::xml_add_child(dist_info, "gmd:MD_Distribution")
    transfer <- xml2::xml_add_child(md_dist, "gmd:transferOptions")
    digital <- xml2::xml_add_child(transfer, "gmd:MD_DigitalTransferOptions")
    online_parent <- xml2::xml_add_child(digital, "gmd:onLine")
    add_online_resource(
      online_parent,
      url = distribution_url,
      name = meta("distribution_name", aliases = c("download_name")),
      description = meta("distribution_description", aliases = c("download_description")),
      protocol = meta("distribution_protocol", default = "WWW:LINK-1.0-http--link"),
      function_code = meta("distribution_function", aliases = c("download_function"), default = "download"),
      function_code_list = code_list_online_function
    )
  }

  xml_text <- as.character(root)

  if (!is.null(output_path)) {
    xml2::write_xml(root, output_path, options = "format")
  }

  invisible(list(xml = xml_text, path = output_path))
}
