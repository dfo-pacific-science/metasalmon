# Rebuild HNAP-aware EDH XML from a reviewed Salmon Data Package

Reads `metadata/dataset.csv` from an existing Salmon Data Package and
writes a fresh `metadata-edh-hnap.xml` file using the canonical
[`edh_build_hnap_xml()`](https://dfo-pacific-science.github.io/metasalmon/reference/edh_build_hnap_xml.md)
builder. This is the preferred post-review rebuild path after metadata
has been edited manually in Excel or another spreadsheet tool. The
helper refuses to rebuild when obvious review-state markers remain, such
as `REVIEW:`-prefixed IRIs anywhere in the package metadata or
unresolved `MISSING METADATA:` / `MISSING DESCRIPTION:` placeholders in
`metadata/dataset.csv` or `metadata/tables.csv`.

## Usage

``` r
write_edh_xml_from_sdp(
  path,
  output_path = NULL,
  overwrite = TRUE,
  language = "eng",
  file_identifier = NULL,
  date_stamp = Sys.Date()
)
```

## Arguments

- path:

  Character path to the Salmon Data Package directory.

- output_path:

  Optional path for the regenerated XML. Defaults to
  `metadata/metadata-edh-hnap.xml` inside `path`. Parent directories are
  created automatically when needed.

- overwrite:

  Logical; if `FALSE`, error when `output_path` already exists. Default
  is `TRUE`.

- language:

  ISO 639-2/T language code for the primary metadata language (default:
  `"eng"`).

- file_identifier:

  Optional metadata file identifier forwarded to
  [`edh_build_hnap_xml()`](https://dfo-pacific-science.github.io/metasalmon/reference/edh_build_hnap_xml.md).

- date_stamp:

  Metadata date stamp forwarded to
  [`edh_build_hnap_xml()`](https://dfo-pacific-science.github.io/metasalmon/reference/edh_build_hnap_xml.md).

## Value

Invisibly returns the same list as
[`edh_build_hnap_xml()`](https://dfo-pacific-science.github.io/metasalmon/reference/edh_build_hnap_xml.md),
with elements `xml` and `path`.

## Examples

``` r
if (FALSE) { # \dontrun{
pkg_path <- create_sdp(
  mtcars,
  dataset_id = "demo-1",
  table_id = "counts",
  overwrite = TRUE
)

# ...edit metadata/dataset.csv in Excel...
write_edh_xml_from_sdp(pkg_path)
} # }
```
