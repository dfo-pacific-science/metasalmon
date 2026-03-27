# 5-Minute Quickstart

## Before You Start

Complete the one-time [Setup and
Credentials](https://dfo-pacific-science.github.io/metasalmon/articles/setup.html)
guide first if you want to make sure GitHub installs work cleanly and
any optional LLM provider is configured before you begin.

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
  path = "fraser-coho-2023-2024-sdp",
  dataset_id = "fraser-coho-2023-2024",
  table_id = "escapement",
  check_updates = FALSE,
  overwrite = TRUE
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
Government Canada Fraser and BC Interior workbook.

Open Government Canada record:
<https://open.canada.ca/data/en/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6>

See `example-data-README.md` in the bundled `extdata` folder for
provenance and licensing. The package also ships a matching starter
dictionary at `nuseds-fraser-coho-2023-2024-column_dictionary.csv` if
you want a ready-made context file for the package-native LLM review
path.

If you also need the DFO Enterprise Data Hub / GeoNetwork XML, use the
one-shot path:

``` r

pkg_path <- create_sdp(
  fraser_coho,
  path = "fraser-coho-2023-2024-sdp",
  dataset_id = "fraser-coho-2023-2024",
  table_id = "escapement",
  include_edh_xml = TRUE,
  check_updates = FALSE,
  overwrite = TRUE
)
```

That writes the HNAP-aware EDH XML to `metadata/metadata-edh-hnap.xml`.
If you are working from an existing `dataset.csv` row instead of a
one-shot package build, call
[`edh_build_hnap_xml()`](https://dfo-pacific-science.github.io/metasalmon/reference/edh_build_hnap_xml.md)
directly.

## Optional LLM Review Later

The base quickstart does **not** require an API key. If you want
`llm_assess = TRUE`, finish the one-time [Setup and
Credentials](https://dfo-pacific-science.github.io/metasalmon/articles/setup.html)
guide first, then rerun the
[`create_sdp()`](https://dfo-pacific-science.github.io/metasalmon/reference/create_sdp.md)
call with your chosen provider.

## Review In Excel

Open `README-review.txt`, then review these files in this order:

1.  `metadata/column_dictionary.csv`
2.  `metadata/tables.csv`
3.  `metadata/dataset.csv`
4.  `metadata/codes.csv` (when present)
5.  `semantic_suggestions.csv` (only if you want more context or a
    better match)

That `metadata/column_dictionary.csv` file is also a perfectly
reasonable `llm_context_files` input when you want the LLM review step
to reason from the package itself instead of a separate methods note.

[`create_sdp()`](https://dfo-pacific-science.github.io/metasalmon/reference/create_sdp.md)
seeds semantic suggestions by default and auto-fills top-ranked
compatible drafts directly into blank semantic fields in the metadata
CSVs. That includes column-level IRIs in
`metadata/column_dictionary.csv` and strong table-level observation-unit
drafts in `metadata/tables.csv`, using `observation_unit`/`description`
when available and otherwise falling back to `table_label`/`table_id`.
Any auto-applied semantic IRI draft is written back as `REVIEW: <iri>`
so you still confirm it manually. It does not overwrite existing
non-empty semantic values. Code-level suggestions default to factor and
low-cardinality character source columns; use
`semantic_code_scope = "all"` if you want broader code-level seeding.

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

If no candidate fits, request a new term instead of forcing a bad match:

- shared cross-organization/domain terms -\>
  <https://github.com/salmon-data-mobilization/salmon-domain-ontology/issues/new/choose>
- DFO-specific policy/operations terms -\>
  <https://github.com/dfo-pacific-science/dfo-salmon-ontology/issues/new/choose>

## Finalize

After Excel edits, save the metadata back to CSV and reload the package
in R:

``` r

pkg <- read_salmon_datapackage(pkg_path)
validate_salmon_datapackage(pkg_path, require_iris = FALSE)
```

That review-state validation is the first follow-up check. For the full
post-review workflow — reloading the package, detecting unresolved
semantic gaps, deciding shared salmon-domain vs DFO-specific routing,
drafting term requests, rebuilding EDH XML if needed, and only then
running strict final validation — continue with:

- [After Excel Review: Finalize and Publish Your
  Package](https://dfo-pacific-science.github.io/metasalmon/articles/post-review-package-publication.html)

For a staged, fully explicit workflow where you assemble metadata tables
manually instead of continuing from a reviewed
[`create_sdp()`](https://dfo-pacific-science.github.io/metasalmon/reference/create_sdp.md)
package, use:

- [Publishing Data
  Packages](https://dfo-pacific-science.github.io/metasalmon/articles/data-dictionary-publication.html)
