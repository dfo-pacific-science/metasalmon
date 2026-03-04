# Build ISO 19139 metadata XML for DFO Enterprise Data Hub export

Generates a compact ISO 19139-style metadata XML document from
`dataset_meta`. This is intended as a starter export for DFO Enterprise
Data Hub / GeoNetwork workflows.

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
  Required columns: `dataset_id`, `title`, `description`. Optional
  columns include: `creator`, `contact_name`, `contact_email`,
  `contact_org`, `contact_position`, `license`, `source_citation`,
  `temporal_start`, `temporal_end`, `spatial_extent`,
  `update_frequency`, `topic_categories`, `keywords`, and
  `security_classification`.

- output_path:

  Optional file path to write XML.

- file_identifier:

  Optional metadata file identifier. Defaults to `dataset_id`.

- language:

  ISO 639-2/T language code (default: `"eng"`).

- date_stamp:

  Metadata date stamp (default:
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)).

## Value

Invisible list with elements `xml` (string) and `path`.

## Details

The produced XML should be validated and enriched against your local EDH
profile before production upload.

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
```
