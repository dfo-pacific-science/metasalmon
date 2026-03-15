# Build ISO 19139 or HNAP-aware metadata XML for DFO Enterprise Data Hub export

Generates metadata XML from `dataset_meta` for DFO Enterprise Data Hub /
GeoNetwork workflows. The default `"dfo_edh_hnap"` profile now emits the
more practical North American Profile / EDH-oriented structure with
deterministic file identifiers, maintenance/status, legal constraints,
optional distribution info, reference system support, bilingual locale
scaffolding, and richer contact metadata when available. The legacy
`"iso19139"` profile remains available as a compact fallback export.

## Usage

``` r
edh_build_iso19139_xml(
  dataset_meta,
  output_path = NULL,
  file_identifier = NULL,
  language = "eng",
  date_stamp = Sys.Date(),
  profile = c("dfo_edh_hnap", "iso19139")
)
```

## Arguments

- dataset_meta:

  Data frame/tibble with exactly one row of dataset-level metadata.
  Required columns: `dataset_id`, `title`, `description`. Common
  optional columns include: `creator`, `contact_name`, `contact_email`,
  `contact_org`, `contact_position`, `license`, `source_citation`,
  `temporal_start`, `temporal_end`, `spatial_extent`,
  `update_frequency`, `topic_categories`, `keywords`,
  `security_classification`, `created`, `modified`, `provenance_note`,
  `status`, `distribution_url`, `download_url`, `reference_system`,
  `bbox_west`, `bbox_east`, `bbox_south`, `bbox_north`, plus optional
  French-localized fields such as `title_fr` and `description_fr`.

- output_path:

  Optional file path to write XML.

- file_identifier:

  Optional metadata file identifier. For the default
  `profile = "dfo_edh_hnap"`, non-UUID identifiers are converted to a
  deterministic UUID-like value and the original `dataset_id` is
  preserved in `gmd:dataSetURI` / citation identifiers. For
  `profile = "iso19139"`, this defaults to `dataset_id`.

- language:

  ISO 639-2/T language code for the primary metadata language (default:
  `"eng"`).

- date_stamp:

  Metadata date stamp (default:
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)). When
  `dataset_meta$modified` is present, that value is preferred.

- profile:

  Metadata export profile. `"dfo_edh_hnap"` (the default) returns the
  richer HNAP/EDH-oriented structure; `"iso19139"` keeps the original
  lightweight fallback export.

## Value

Invisible list with elements `xml` (string) and `path`.

## Details

The produced XML should still be validated and enriched against your
local EDH profile before production upload.

## Examples

``` r
dataset_meta <- tibble::tibble(
  dataset_id = "fraser-coho-2024",
  title = "Fraser River Coho Escapement Data",
  description = "Sample escapement monitoring data for coho salmon in PFMA 29",
  contact_name = "Your Name",
  contact_email = "your.email@dfo-mpo.gc.ca",
  topic_categories = "biota;oceans",
  keywords = "coho;escapement;Fraser River",
  temporal_start = "2001",
  temporal_end = "2024"
)

out <- tempfile(fileext = ".xml")
edh_build_iso19139_xml(dataset_meta, output_path = out)

out_iso <- tempfile(fileext = ".xml")
edh_build_iso19139_xml(
  dataset_meta,
  output_path = out_iso,
  profile = "iso19139"
)
```
