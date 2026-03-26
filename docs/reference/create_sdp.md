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
  seed_table_meta = TRUE,
  seed_dataset_meta = NULL,
  semantic_code_scope = c("factor", "all", "none"),
  check_updates = interactive(),
  format = "csv",
  overwrite = FALSE,
  include_edh_xml = FALSE,
  ...
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

- semantic_code_scope:

  Character string controlling which `codes.csv` rows are sent through
  [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md)
  during one-shot seeding. `"factor"` (default) analyzes codes sourced
  from factor columns and low-cardinality character columns in the
  original data frame(s); `"all"` analyzes all inferred or supplied code
  rows; `"none"` skips code-level semantic suggestions.

- check_updates:

  Logical; if `TRUE`, run a short, non-fatal
  [`check_for_updates()`](https://dfo-pacific-science.github.io/metasalmon/reference/check_for_updates.md)
  call after writing the package and mention newer releases only when
  one is available. Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

- format:

  Character; resource format: `"csv"` (default, only format supported)

- overwrite:

  Logical; if `FALSE` (default), errors if path exists. If `TRUE`,
  replacement is only allowed for empty directories or directories
  previously written by `metasalmon`.

- include_edh_xml:

  Logical; when `TRUE`, writes an HNAP-aware EDH XML metadata file to
  `metadata/metadata-edh-hnap.xml` using
  [`edh_build_hnap_xml()`](https://dfo-pacific-science.github.io/metasalmon/reference/edh_build_hnap_xml.md).
  The default is `FALSE`.

- ...:

  Deprecated legacy EDH arguments accepted for backwards compatibility:
  `edh_profile`, `EDH_Profile`, and `EDH_profile` all enable EDH XML
  export and must be `"dfo_edh_hnap"` when supplied. `edh_xml_path` is
  ignored with a warning because XML now always writes to the default
  metadata path. Any other extra arguments error.

## Value

Invisibly returns the package path.

## Details

This one-shot helper creates a review-ready package by default: semantic
suggestions are seeded and the top-ranked column-level suggestions are
auto-applied only into missing dictionary IRI fields. Table-level
observation-unit suggestions stay enabled, but `create_sdp()` only
auto-applies them into missing `tables.csv$observation_unit_iri` values
when they are backed by non-placeholder table metadata and still look
lexically compatible with that context; compatible suggestions can also
backfill `tables.csv$observation_unit` labels when missing. To reduce
review noise conservatively, code-level suggestions default to factor
and low-cardinality character source columns only; set
`semantic_code_scope = "all"` to broaden that or `"none"` to disable it.
The package root contains `README-review.txt`,
`semantic_suggestions.csv` (when available), `datapackage.json`,
`metadata/`, and `data/`. To keep review files usable,
`semantic_suggestions.csv` trims code-level suggestions that do not have
enough human-readable context to review safely. Required-field review
placeholders are also inserted into the inferred metadata files. In
interactive use, `create_sdp()` can also mention an available package
update; set `check_updates = FALSE` to skip that network check. The
package bundles two Fraser coho examples:
`nuseds-fraser-coho-sample.csv` (30 rows across 1996-2024) for the
quickest demo, and `nuseds-fraser-coho-2023-2024.csv` (173 rows from the
official Open Government Canada Fraser and BC Interior workbook) for a
fuller multi-year example. The bundled
`system.file("extdata", "example-data-README.md", package = "metasalmon")`
note points to the upstream record/resource URLs, licensing, and the
repository `data-raw/` script used to derive the fuller example.

## Examples

``` r
if (FALSE) { # \dontrun{
data_path <- system.file("extdata", "nuseds-fraser-coho-sample.csv", package = "metasalmon")
fraser_coho <- readr::read_csv(data_path, show_col_types = FALSE)

pkg <- create_sdp(
  fraser_coho,
  dataset_id = "fraser-coho-2024",
  table_id = "escapement",
  overwrite = FALSE
)
} # }
```
