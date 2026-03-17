# 5-Minute Quickstart

## Installation

``` r

install.packages("remotes")
remotes::install_github("dfo-pacific-science/metasalmon")
```

## One-shot Workflow

Load the built-in NuSEDS Fraser Coho sample and create a review-ready
Salmon Data Package in one call.

``` r

library(metasalmon)

sample_path <- system.file("extdata", "nuseds-fraser-coho-sample.csv", package = "metasalmon")
fraser_coho <- readr::read_csv(sample_path, show_col_types = FALSE)

pkg_path <- create_sdp(
  fraser_coho,
  dataset_id = "fraser-coho-2024",
  table_id = "escapement",
  overwrite = TRUE
)

pkg_path
list.files(pkg_path)
```

If `path` is omitted,
[`create_sdp()`](https://dfo-pacific-science.github.io/metasalmon/reference/create_sdp.md)
writes to your working directory using a default folder name like
`fraser-coho-2024-sdp`.

## Review In Excel

Open the output folder and review these files:

- `dataset.csv`
- `tables.csv`
- `column_dictionary.csv`
- `data/*.csv` resource files
- `semantic_suggestions.csv` (when suggestions were found)
- `README-review.txt`

[`create_sdp()`](https://dfo-pacific-science.github.io/metasalmon/reference/create_sdp.md)
seeds semantic suggestions by default and auto-fills the top-ranked
**column-level** suggestions into missing dictionary fields (`term_iri`,
`property_iri`, `entity_iri`, `unit_iri`, etc.). It does not overwrite
existing non-empty IRI values.

The inferred metadata includes `REVIEW REQUIRED:` placeholders for
required fields so the package is immediately reviewable in Excel.
Replace those placeholders before publishing.

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
IRI into `column_dictionary.csv`.

## Finalize

After Excel edits, run validation again before publishing:

``` r

pkg <- read_salmon_datapackage(pkg_path)
validate_dictionary(pkg$dictionary)
validate_semantics(pkg$dictionary)
```

For a staged, fully explicit workflow (manual artifact inference and
controlled semantic merges), see the publication guide:

- [Publishing Data
  Packages](https://dfo-pacific-science.github.io/metasalmon/articles/data-dictionary-publication.html)
