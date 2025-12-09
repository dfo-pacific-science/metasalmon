# Getting started with metasalmon

## Overview

The `metasalmon` package helps salmon scientists wrap their
analysis-ready tables into portable, ontology-aware Salmon Data
Packages. A Salmon Data Package is a Frictionless Data Package extended
with salmon-specific semantic fields (IRIs, concept schemes) that link
your data to the DFO Salmon Ontology.

## Quick Example

``` r

library(metasalmon)

# Load your data
df <- readr::read_csv("your-data.csv")

# Infer a starter dictionary
dict <- infer_dictionary(df, dataset_id = "my-dataset", table_id = "my-table")

# Validate it
validate_dictionary(dict)

# Create a package
resources <- list(main_table = df)
dataset_meta <- tibble::tibble(
  dataset_id = "my-dataset",
  title = "My Dataset",
  description = "Description here",
  # ... other fields
)
table_meta <- tibble::tibble(
  dataset_id = "my-dataset",
  table_id = "main_table",
  file_name = "main_table.csv",
  # ... other fields
)

pkg_path <- create_salmon_datapackage(
  resources, dataset_meta, table_meta, dict,
  path = "output-package"
)
```

## Learn More

- **Functions workflow**: See
  [`vignette("functions-workflow")`](https://dfo-pacific-science.github.io/metasalmon/articles/functions-workflow.md)
  for a detailed walkthrough
- **GPT collaboration**: See
  [`vignette("gpt-collaboration")`](https://dfo-pacific-science.github.io/metasalmon/articles/gpt-collaboration.md)
  for using AI assistance
- **Package reference**: Browse function documentation with
  [`?infer_dictionary`](https://dfo-pacific-science.github.io/metasalmon/reference/infer_dictionary.md)

## Package Layout

- `R/`: Core functions for dictionary inference/validation, package
  creation/reading
- `inst/extdata/`: Example data files for tests and vignettes
- `tests/testthat/`: Automated tests
- `vignettes/`: Long-form documentation
