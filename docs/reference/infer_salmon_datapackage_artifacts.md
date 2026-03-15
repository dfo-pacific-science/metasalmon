# Infer Salmon Data Package artifacts from resource tables

Infers package-building metadata artifacts from one or more input tables
in a single step.

## Usage

``` r
infer_salmon_datapackage_artifacts(
  resources,
  dataset_id = "dataset-1",
  table_id = "table-1",
  guess_types = TRUE,
  seed_semantics = TRUE,
  semantic_sources = c("smn", "gcdfo", "ols", "nvs"),
  semantic_max_per_role = 1,
  seed_verbose = TRUE,
  seed_codes = NULL,
  seed_table_meta = NULL,
  seed_dataset_meta = NULL
)
```

## Arguments

- resources:

  Either a single data frame or a named list of data frames.

- dataset_id:

  Dataset identifier applied to all inferred metadata rows.

- table_id:

  Fallback table identifier when `resources` is a single data frame.

- guess_types:

  Logical; if TRUE (default), infer `value_type` for each column in the
  dictionary.

- seed_semantics:

  Logical; if TRUE, run
  [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md)
  and attach semantic suggestions.

- semantic_sources:

  Vector of vocabulary sources forwarded to
  [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md).

- semantic_max_per_role:

  Maximum number of suggestions retained per I-ADOPT role.

- seed_verbose:

  Logical; if TRUE, report inference and seeding progress.

- seed_codes:

  Optional prebuilt codes metadata used to seed semantic suggestion
  target generation.

- seed_table_meta:

  Optional prebuilt tables metadata used to seed semantic suggestion
  target generation.

- seed_dataset_meta:

  Optional prebuilt dataset metadata used to seed semantic suggestion
  target generation.

## Value

A list with elements:

- `resources`:

  Named list of input tables.

- `dataset_id`:

  Dataset identifier used during inference.

- `dict`:

  Inferred dictionary tibble.

- `table_meta`:

  Inferred table metadata tibble.

- `codes`:

  Inferred candidate codes tibble.

- `dataset_meta`:

  Inferred dataset metadata tibble.

- `semantic_suggestions`:

  The semantic suggestion tibble attached during seeding, or `NULL` when
  `seed_semantics = FALSE`.

## Details

The return object is intentionally shaped for common biologist
workflows: use it with
[`create_salmon_datapackage()`](https://dfo-pacific-science.github.io/metasalmon/reference/create_salmon_datapackage.md)
once you are happy with the inferred metadata.

## See also

[`infer_dictionary`](https://dfo-pacific-science.github.io/metasalmon/reference/infer_dictionary.md),
[`create_salmon_datapackage`](https://dfo-pacific-science.github.io/metasalmon/reference/create_salmon_datapackage.md),
[`suggest_semantics`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md)

## Examples

``` r
if (FALSE) { # \dontrun{
resources <- list(
  catches = data.frame(
    station_id = c("A", "B"),
    species = c("Coho", "Chinook"),
    count = c(10L, 20L),
    sample_date = as.Date(c("2024-01-01", "2024-01-02"))
  ),
  stations = data.frame(
    station_id = c("A", "B"),
    latitude = c(49.8, 49.9),
    longitude = c(-124.4, -124.5)
  )
)

artifacts <- infer_salmon_datapackage_artifacts(
  resources,
  dataset_id = "demo-1",
  seed_semantics = TRUE,
  seed_verbose = TRUE
)

artifacts$dict
artifacts$table_meta
artifacts$codes
artifacts$dataset_meta
} # }
```
