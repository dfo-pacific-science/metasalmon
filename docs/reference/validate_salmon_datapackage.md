# Validate a Salmon Data Package end to end

Reads a package from disk, checks that metadata/data files stay aligned,
verifies coded values against `codes.csv` when present, and then runs
[`validate_dictionary()`](https://dfo-pacific-science.github.io/metasalmon/reference/validate_dictionary.md)
plus
[`validate_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/validate_semantics.md).
This is the quickest pre-flight check before sharing a package-first
submission.

## Usage

``` r
validate_salmon_datapackage(path, require_iris = FALSE)
```

## Arguments

- path:

  Character; directory containing the Salmon Data Package.

- require_iris:

  Logical; if `TRUE`, require non-empty semantic IRIs for measurement
  fields (`term_iri`, `property_iri`, `entity_iri`, and `unit_iri`).

## Value

Invisibly returns a list with components:

- `package`: loaded package list from
  [`read_salmon_datapackage()`](https://dfo-pacific-science.github.io/metasalmon/reference/read_salmon_datapackage.md).

- `semantic_validation`: result from
  [`validate_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/validate_semantics.md).

- `issues`: package-structure issue tibble (empty when validation
  passes).

## Examples

``` r
if (FALSE) { # \dontrun{
pkg_path <- create_sdp(
  mtcars,
  dataset_id = "demo-1",
  table_id = "counts",
  overwrite = TRUE
)
validate_salmon_datapackage(pkg_path, require_iris = FALSE)
} # }
```
