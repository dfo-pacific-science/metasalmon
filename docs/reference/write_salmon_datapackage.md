# Write a Salmon Data Package from preassembled metadata

Advanced/manual writer for cases where you already have the canonical
Salmon Data Package (SDP) metadata tables assembled. It writes the SDP
CSV metadata files under `metadata/` (`dataset.csv`, `tables.csv`,
`column_dictionary.csv`, and optional `codes.csv`) plus the data
resource files themselves under `data/`. For interoperability with
Frictionless-style tooling, the function also emits a derived
`datapackage.json` descriptor at the package root.

## Usage

``` r
write_salmon_datapackage(
  resources,
  dataset_meta,
  table_meta,
  dict,
  codes = NULL,
  path,
  format = "csv",
  overwrite = FALSE
)
```

## Arguments

- resources:

  Named list of data frames/tibbles (one per resource)

- dataset_meta:

  Tibble with dataset-level metadata (one row)

- table_meta:

  Tibble with table-level metadata (one row per table)

- dict:

  Dictionary tibble with column definitions

- codes:

  Optional tibble with code lists

- path:

  Character; directory path where package will be written

- format:

  Character; resource format: `"csv"` (default, only format supported)

- overwrite:

  Logical; if `FALSE` (default), errors if path exists. If `TRUE`,
  replacement is only allowed for empty directories or directories
  previously written by `metasalmon`.

## Value

Invisibly returns the path to the created package

## Details

The SDP CSV files remain the canonical package metadata.
`datapackage.json` is a convenience export, not the source of truth.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a simple package
resources <- list(main_table = mtcars)
dataset_meta <- tibble::tibble(
  dataset_id = "test-1",
  title = "Test Dataset",
  description = "A test dataset"
)
table_meta <- tibble::tibble(
  dataset_id = "test-1",
  table_id = "main_table",
  file_name = "data/main_table.csv",
  table_label = "Main Table"
)
dict <- infer_dictionary(mtcars, dataset_id = "test-1", table_id = "main_table")
write_salmon_datapackage(
  resources, dataset_meta, table_meta, dict,
  path = tempdir()
)
} # }
```
