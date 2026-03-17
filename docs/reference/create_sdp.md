# Create a Salmon Data Package directly from raw tables

Primary one-shot wrapper: infer dictionary/table metadata/codes/dataset
metadata from raw data tables and immediately write a review-ready
Salmon Data Package.

## Usage

``` r
create_sdp(
  resources,
  path = NULL,
  dataset_id = "dataset-1",
  table_id = "table_1",
  guess_types = TRUE,
  seed_semantics = TRUE,
  semantic_sources = c("smn", "gcdfo", "ols", "nvs"),
  semantic_max_per_role = 1,
  seed_verbose = TRUE,
  seed_codes = NULL,
  seed_table_meta = NULL,
  seed_dataset_meta = NULL,
  format = "csv",
  overwrite = FALSE,
  include_edh_xml = FALSE,
  edh_profile = c("dfo_edh_hnap", "iso19139"),
  edh_xml_path = NULL
)
```

## Arguments

- resources:

  Either a named list of data frames (one per resource table) or a
  single data frame (converted internally to a one-table list).

- path:

  Character; directory path where package will be written. If omitted,
  defaults to `file.path(getwd(), paste0(<dataset_id>-sdp))` using a
  filesystem-safe dataset id slug.

- dataset_id:

  Dataset identifier applied to all inferred metadata rows.

- table_id:

  Fallback table identifier when `resources` is a single data frame.

- guess_types:

  Logical; if `TRUE` (default), infer `value_type` for each dictionary
  column.

- seed_semantics:

  Logical; if `TRUE` (default), seed semantic suggestions during
  inference.

- semantic_sources:

  Vector of vocabulary sources passed to
  [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md).

- semantic_max_per_role:

  Maximum number of suggestions retained per I-ADOPT role.

- seed_verbose:

  Logical; if TRUE, emit progress messages while seeding semantic
  suggestions.

- seed_codes:

  Optional `codes.csv`-style seed metadata.

- seed_table_meta:

  Optional `tables.csv`-style seed metadata.

- seed_dataset_meta:

  Optional `dataset.csv`-style seed metadata.

- format:

  Character; resource format: `"csv"` (default, only format supported)

- overwrite:

  Logical; if `FALSE` (default), errors if path exists

- include_edh_xml:

  Logical; when `TRUE`, writes an EDH XML metadata file into the output
  package path using
  [`edh_build_iso19139_xml()`](https://dfo-pacific-science.github.io/metasalmon/reference/edh_build_iso19139_xml.md).

- edh_profile:

  One of "dfo_edh_hnap" (default) or "iso19139". Determines whether the
  richer HNAP-aware profile or compact fallback profile is written when
  `include_edh_xml = TRUE`.

- edh_xml_path:

  Optional file path for the EDH output when `include_edh_xml = TRUE`.
  If `NULL`, defaults to `metadata-edh-hnap.xml` for
  `edh_profile = "dfo_edh_hnap"` and `metadata-iso19139.xml` for
  `edh_profile = "iso19139"`.

## Value

Invisibly returns the package path.

## Details

This one-shot helper creates a review-ready package by default: semantic
suggestions are seeded and the top-ranked column-level suggestions are
auto-applied only into missing dictionary IRI fields. The output package
also includes `semantic_suggestions.csv` (when available) plus
`README-review.txt` with clear Excel-based review instructions, plus
required-field review placeholders in the inferred metadata files.

## Examples

``` r
if (FALSE) { # \dontrun{
data_path <- system.file("extdata", "nuseds-fraser-coho-sample.csv", package = "metasalmon")
fraser_coho <- readr::read_csv(data_path, show_col_types = FALSE)

pkg <- create_sdp(
  fraser_coho,
  dataset_id = "fraser-coho-2024",
  table_id = "escapement",
  overwrite = TRUE
)
} # }
```
