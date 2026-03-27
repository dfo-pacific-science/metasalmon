# Deprecated alias for [`edh_build_hnap_xml()`](https://dfo-pacific-science.github.io/metasalmon/reference/edh_build_hnap_xml.md)

Deprecated alias for
[`edh_build_hnap_xml()`](https://dfo-pacific-science.github.io/metasalmon/reference/edh_build_hnap_xml.md)

## Usage

``` r
edh_build_iso19139_xml(
  dataset_meta,
  output_path = NULL,
  file_identifier = NULL,
  language = "eng",
  date_stamp = Sys.Date()
)
```

## Arguments

- dataset_meta:

  Data frame/tibble with exactly one row of dataset-level metadata.
  Required columns: `dataset_id`, `title`, `description`. Common
  optional columns include: `creator`, `contact_name`, `contact_email`,
  `contact_org`, `contact_position`, `license`, `source_citation`,
  `temporal_start`, `temporal_end`, `spatial_extent`,
  `update_frequency`, `topic_categories`, `keywords`, `keyword_type`,
  `keyword_thesaurus_title`, `keyword_thesaurus_date`,
  `keyword_thesaurus_date_type`, `security_classification`, `created`,
  `modified`, `provenance_note`, `status`, `distribution_url`,
  `download_url`, `reference_system`, `bbox_west`, `bbox_east`,
  `bbox_south`, `bbox_north`, plus optional French-localized fields such
  as `title_fr`, `description_fr`, and `keyword_thesaurus_title_fr`.

- output_path:

  Optional file path to write XML. Parent directories are created
  automatically when needed.

- file_identifier:

  Optional metadata file identifier. Non-UUID identifiers are converted
  to a deterministic UUID-like value and the original `dataset_id` is
  preserved in `gmd:dataSetURI` / citation identifiers.

- language:

  ISO 639-2/T language code for the primary metadata language (default:
  `"eng"`).

- date_stamp:

  Metadata date stamp (default:
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)). When
  `dataset_meta$modified` is present, that value is preferred.

## Value

Invisible list with elements `xml` (string) and `path`.
