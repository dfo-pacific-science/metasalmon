# Create a Salmon Data Package

Writes the canonical Salmon Data Package (SDP) CSV metadata files
(`dataset.csv`, `tables.csv`, `column_dictionary.csv`, and optional
`codes.csv`) plus the data resource files themselves. For
interoperability with Frictionless-style tooling, the function also
emits a derived `datapackage.json` descriptor.

## Usage

``` r
create_salmon_datapackage(
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

  Logical; if `FALSE` (default), errors if path exists

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
create_salmon_datapackage(
  resources, dataset_meta, table_meta, dict,
  path = tempdir()
)
} # }
```
