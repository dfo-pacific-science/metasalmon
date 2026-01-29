# Publishing Data Packages

## Overview

This guide walks through the parts of a Salmon Data Package you need to
polish before sharing it. That includes the dictionary that documents
every column, the dataset and table metadata that describe the package,
and the optional codes lists you include when columns use controlled
values. When all of the pieces are ready, `metasalmon` writes the files
that match the Salmon Data Package specification so you can upload or
hand the folder to someone else with confidence.

### 1) Start with your data

``` r

library(metasalmon)
library(readr)

# Replace with your own data path
df <- read_csv("my-table.csv")
```

Keep working copies of your data frame handy so you can re-run these
steps whenever the source data changes.

### 2) Build a starter column dictionary

``` r

dict <- infer_dictionary(
  df,
  dataset_id = "my-dataset-2026",
  table_id = "main-table"
)
```

The dictionary lists every column and assigns a `column_role`
(identifier, attribute, measurement, temporal, or categorical). Move
through the rows and fill in `column_label`, `column_description`, and
`value_type` so reviewers understand what each field means, and mark
columns as `required` when they must appear in every row.

### 3) Describe the dataset and tables

``` r

dataset_meta <- tibble::tibble(
  dataset_id = "my-dataset-2026",
  title = "My Project Data",
  description = "Sample data describing salmon measurements",
  creator = "Your Team",
  contact_name = "Data Steward",
  contact_email = "data@example.gov",
  license = "Open Government License - Canada"
)

table_meta <- tibble::tibble(
  dataset_id = "my-dataset-2026",
  table_id = "main-table",
  file_name = "main-table.csv",
  table_label = "Main Salmon Table",
  description = "Escapement and effort data by population"
)
```

Include extra columns such as `spatial_extent`, `temporal_start`, or
`table_label` when they help others understand the scope.

### 4) Add codes lists when needed

Only create `codes.csv` when a column uses categorical values (species,
run_type, gear, etc.). Each row ties a `code_value` to a short label
and, ideally, the ontology term that explains what the code means.

``` r

codes <- tibble::tibble(
  dataset_id = "my-dataset-2026",
  table_id = "main-table",
  column_name = "RUN_TYPE",
  code_value = "FALL",
  code_label = "Fall run timing"
)
```

If the column reuses a published controlled vocabulary (like the DFO
Salmon Ontology), include the matching IRI in `term_iri` so automated
tools can link to the definition.

### 5) Create the package

``` r

resources <- list(main = df)

pkg_path <- create_salmon_datapackage(
  resources = resources,
  dataset_meta = dataset_meta,
  table_meta = table_meta,
  dict = dict,
  codes = codes,
  path = "my-data-package",
  format = "csv",
  overwrite = TRUE
)

list.files(pkg_path)
```

This writes the CSV files plus `datapackage.json` that follow the Salmon
Data Package specification. The folder is now ready for publication,
archiving, or sharing with colleagues.

### Optional: include DwC-DP export hints

You can attach optional Darwin Core Data Package (DwC-DP) mappings when
you need an export view for biodiversity tools. The default is OFF to
keep SDP canonical.

``` r

dict <- readr::read_csv("inst/extdata/column_dictionary.csv", show_col_types = FALSE)
sem <- suggest_semantics(dict, include_dwc = TRUE)
attr(sem, "dwc_mappings") |>
  dplyr::filter(dwc_table %in% c("event", "occurrence")) |>
  dplyr::select(column_name, dwc_table, dwc_field, term_iri)
```

Keep the SDP column names intact; use the DwC mappings only when
exporting a DwC-DP view.

#### Using suggest_dwc_mappings() directly

For more control over DwC-DP mapping suggestions, use
[`suggest_dwc_mappings()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_dwc_mappings.md):

``` r

dict <- tibble::tibble(
 column_name = c("event_date", "decimal_latitude", "scientific_name"),
 column_label = c("Event Date", "Decimal Latitude", "Scientific Name"),
 column_description = c("Date the event occurred", "Latitude in decimal degrees", "Species scientific name")
)
dict <- suggest_dwc_mappings(dict)
attr(dict, "dwc_mappings")
# Shows suggested DwC-DP table/field mappings with term IRIs
```

### Semantic suggestions with role-aware sources

When using
[`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md),
the function automatically queries role-appropriate sources:

``` r

# Default: ontology suggestions only (DwC mappings OFF)
sem <- suggest_semantics(dict)

# Include DwC-DP mappings alongside ontology suggestions
sem_with_dwc <- suggest_semantics(dict, include_dwc = TRUE)

# View ontology suggestions
suggestions <- attr(sem, "semantic_suggestions")

# View DwC mappings (only when include_dwc = TRUE)
dwc_maps <- attr(sem_with_dwc, "dwc_mappings")
```

The ontology suggestions use role-aware ranking (Phase 2) that
prefers: - QUDT for units - GBIF/WoRMS for taxa/entities - STATO/OBA for
properties - gcdfo patterns for methods

Terms from Wikidata are flagged with `alignment_only = TRUE` and ranked
lower.

### Validation before publication

- Run `validate_dictionary(dict)` to ensure the dictionary has required
  columns and valid `column_role`/`value_type` combinations.
- If you generated `codes.csv`, double-check that every code used in the
  data has an entry there.
- Re-open the package with `read_salmon_datapackage(pkg_path)` to
  confirm the metadata, dictionary, and data align.

### Next steps

- See the “How It Fits Together” section in the README for a visual map
  of how the components interact.
- Read the [Linking to Standard
  Vocabularies](https://dfo-pacific-science.github.io/metasalmon/articles/reusing-standards-salmon-data-terms.md)
  guide when you want to align your dictionary with published
  vocabularies.
- Try the [Using AI to Document Your
  Data](https://dfo-pacific-science.github.io/metasalmon/articles/gpt-collaboration.md)
  workflow for drafting descriptions and ontology-aligned metadata
  quickly.
