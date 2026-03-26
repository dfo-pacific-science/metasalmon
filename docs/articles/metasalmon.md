# 5-Minute Quickstart

## Installation

``` r

install.packages("remotes")
remotes::install_github("dfo-pacific-science/metasalmon")
```

## One-shot Workflow

Load the built-in Fraser Coho 2023-2024 example (173 rows) and create a
review-ready Salmon Data Package in one call.

``` r

library(metasalmon)

data_path <- system.file("extdata", "nuseds-fraser-coho-2023-2024.csv", package = "metasalmon")
fraser_coho <- readr::read_csv(data_path, show_col_types = FALSE)

pkg_path <- create_sdp(
  fraser_coho,
  dataset_id = "fraser-coho-2023-2024",
  table_id = "escapement",
  overwrite = FALSE
)

pkg_path
list.files(pkg_path, recursive = TRUE)
```

If `path` is omitted,
[`create_sdp()`](https://dfo-pacific-science.github.io/metasalmon/reference/create_sdp.md)
writes to your working directory using a default folder name like
`fraser-coho-2023-2024-sdp`. In interactive use it can also mention when
a newer `metasalmon` release is available; set `check_updates = FALSE`
to skip that check.

This quickstart uses the bundled `nuseds-fraser-coho-2023-2024.csv`
example, a 173-row Fraser coho slice derived from the official Open
Government Canada Fraser and BC Interior workbook. See
`example-data-README.md` in the bundled `extdata` folder for provenance
and licensing.

If you also need the DFO Enterprise Data Hub / GeoNetwork XML, use the
one-shot path:

``` r

pkg_path <- create_sdp(
  fraser_coho,
  dataset_id = "fraser-coho-2023-2024",
  table_id = "escapement",
  include_edh_xml = TRUE,
  overwrite = FALSE
)
```

That writes the HNAP-aware EDH XML to `metadata/metadata-edh-hnap.xml`.
If you are working from an existing `dataset.csv` row instead of a
one-shot package build, call
[`edh_build_hnap_xml()`](https://dfo-pacific-science.github.io/metasalmon/reference/edh_build_hnap_xml.md)
directly.

## Review In Excel

Open the output folder and review these files:

- `README-review.txt`
- `semantic_suggestions.csv` (when suggestions were found)
- `metadata/dataset.csv`
- `metadata/tables.csv`
- `metadata/column_dictionary.csv`
- `metadata/codes.csv` (when present)
- `data/*.csv` resource files

[`create_sdp()`](https://dfo-pacific-science.github.io/metasalmon/reference/create_sdp.md)
seeds semantic suggestions by default and auto-fills the top-ranked
**column-level** suggestions into missing dictionary fields (`term_iri`,
`property_iri`, `entity_iri`, `unit_iri`, etc.). It also auto-fills top
table-level observation-unit suggestions into missing
`tables.csv$observation_unit_iri` values (and labels when blank). It
does not overwrite existing non-empty semantic values. Code-level
suggestions default to factor and low-cardinality character source
columns; use `semantic_code_scope = "all"` if you want broader
code-level seeding.

The inferred metadata includes `MISSING DESCRIPTION:` and
`MISSING METADATA:` placeholders for required fields so the package is
immediately reviewable in Excel. Replace those placeholders before
publishing. The `metadata/*.csv` files are the canonical package
metadata; `datapackage.json` is a derived export for interoperability.

## How To Decide If `term_iri` Is Correct

Use plain-language checks for each measurement column:

1.  Does the suggested label describe exactly what the column measures?
2.  Does the definition match your intent (not just a similar word)?
3.  Is the scope right (for example species-level vs population-level)?
4.  Is the unit consistent with your values and `unit_iri`?

Keep the IRI only when all checks pass.

Replace it when the term is close but not exact.

Remove it (leave blank) when no candidate is reliable yet.

When the top auto-applied suggestion is wrong, use
`semantic_suggestions.csv` to pick a better alternative and copy that
IRI into `metadata/column_dictionary.csv`.

## Finalize

After Excel edits, save the metadata back to CSV, share the whole folder
(or a zip of the whole folder) when you hand it to someone else, then
run validation again before publishing:

``` r

pkg <- read_salmon_datapackage(pkg_path)
validate_dictionary(pkg$dictionary)
validate_semantics(pkg$dictionary)
```

For a staged, fully explicit workflow (manual artifact inference and
controlled semantic merges), see the publication guide:

- [Publishing Data
  Packages](https://dfo-pacific-science.github.io/metasalmon/articles/data-dictionary-publication.html)
