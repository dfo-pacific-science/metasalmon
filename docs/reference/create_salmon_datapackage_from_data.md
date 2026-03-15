# Create a Salmon Data Package from raw tables

Convenience one-shot helper that infers package artifacts from one or
more raw tables and immediately writes a Salmon Data Package.

## Usage

``` r
create_salmon_datapackage_from_data(
  resources,
  path,
  dataset_id = "dataset-1",
  table_id = "table-1",
  guess_types = TRUE,
  seed_semantics = TRUE,
  semantic_sources = c("smn", "gcdfo", "ols", "nvs"),
  semantic_max_per_role = 1,
  seed_verbose = TRUE,
  seed_codes = NULL,
  seed_table_meta = NULL,
  seed_dataset_meta = NULL,
  format = "csv",
  overwrite = FALSE
)
```

## Arguments

- resources:

  Either a single data frame or a named list of data frames.

- path:

  Character; directory path where the package will be written.

- dataset_id:

  Dataset identifier applied to all inferred metadata rows.

- table_id:

  Fallback table identifier when `resources` is a single data frame.

- guess_types:

  Logical; if TRUE (default), infer `value_type` for each dictionary
  column.

- seed_semantics:

  Logical; if TRUE, run
  [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md)
  during inference and attach semantic suggestions to the returned
  dictionary (review-only, by design).

- semantic_sources:

  Vector of vocabulary sources passed to
  [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md).

- semantic_max_per_role:

  Maximum number of suggestions retained per I-ADOPT role.

- seed_verbose:

  Logical; if TRUE, emit progress messages while seeding semantic
  suggestions.

- seed_codes:

  Optional prebuilt `codes.csv`-style seed metadata used to seed
  semantic suggestion target generation.

- seed_table_meta:

  Optional prebuilt `tables.csv`-style seed metadata used to seed
  semantic suggestion target generation.

- seed_dataset_meta:

  Optional prebuilt `dataset.csv`-style seed metadata used to seed
  semantic suggestion target generation.

- format:

  Character; resource format. Only `"csv"` is supported.

- overwrite:

  Logical; if FALSE (default), errors when the target path exists.

## Value

Invisibly returns the path to the created package.

## Details

This is a one-shot bootstrap flow intended for fast first-pass package
creation, not automatic publication-ready review.

Use this function for demos and initial assembly, then run an explicit
iterative loop:

1.  Inspect inferred artifacts with
    \`infer_salmon_datapackage_artifacts(..., seed_semantics = TRUE)\`.

2.  Validate and clean with \`validate_dictionary()\` and
    \`validate_semantics()\`.

3.  Use \`suggest_semantics()\` and \`apply_semantic_suggestions()\`
    deliberately (review-first by design).

4.  Rebuild with \`create_salmon_datapackage()\` using reviewed
    metadata.

See
<https://dfo-pacific-science.github.io/metasalmon/articles/data-dictionary-publication.html>
and
<https://dfo-pacific-science.github.io/metasalmon/articles/reusing-standards-salmon-data-terms.html>
for full guidance.

## See also

[`infer_salmon_datapackage_artifacts`](https://dfo-pacific-science.github.io/metasalmon/reference/infer_salmon_datapackage_artifacts.md),
[`create_salmon_datapackage`](https://dfo-pacific-science.github.io/metasalmon/reference/create_salmon_datapackage.md),
[`suggest_semantics`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md)

## Examples

``` r
if (FALSE) { # \dontrun{
resources <- list(
  catches = data.frame(
    station_id = c("A", "B"),
    species = c("Coho", "Chinook"),
    count = c(10L, 20L)
  ),
  stations = data.frame(
    station_id = c("A", "B"),
    lat = c(49.8, 49.9),
    lon = c(-124.4, -124.5)
  )
)

pkg_path <- create_salmon_datapackage_from_data(
  resources,
  path = "my-package",
  dataset_id = "demo-1",
  seed_semantics = TRUE,
  overwrite = TRUE
)

pkg_path
} # }
```
