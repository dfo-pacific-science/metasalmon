# metasalmon

R package for creating portable, ontology-aware Salmon Data Packages. Wrap your analysis-ready tables into Frictionless Data Packages extended with semantic fields (IRIs, concept schemes) that link to the DFO Salmon Ontology.

## Features

- **Dictionary inference**: Automatically propose starter dictionaries from your data frames
- **Dictionary validation**: Validate dictionary schemas and semantic fields
- **Data transformation**: Apply dictionaries to rename columns, coerce types, and apply factor levels
- **Package creation**: Assemble Frictionless Data Packages with salmon-specific semantic extensions
- **Package reading**: Load packages back into R as tidy tibbles with metadata

## Quick Start

```r
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
  description = "Description here"
)
table_meta <- tibble::tibble(
  dataset_id = "my-dataset",
  table_id = "main_table",
  file_name = "main_table.csv"
)

pkg_path <- create_salmon_datapackage(
  resources, dataset_meta, table_meta, dict,
  path = "output-package"
)
```

## Data Package Format

A Salmon Data Package is a Frictionless Data Package extended with semantic fields that link to the DFO Salmon Ontology.

### Conceptual Structure

**Components:**

- **datapackage.json**: Package metadata and resource schemas defining columns with semantic IRIs
- **CSV tables**: Data files (one per resource)
- **codes.csv**: Optional code lists for categorical fields
- **Semantic IRIs**: Links in field schemas connecting columns to DFO Salmon Ontology concepts

## Installation

```r
# Install from GitHub (once published)
devtools::install_github("dfo-pacific-science/metasalmon")
```

## Documentation

- **Getting started**: `vignette("metasalmon")`
- **Functions workflow**: `vignette("functions-workflow")`
- **GPT collaboration**: `vignette("gpt-collaboration")`

## DFO Salmon Ontology

This package links your data to the [DFO Salmon Ontology](https://w3id.org/gcdfos/salmon) using semantic IRIs. Key ontology terms include:

- **Classes**: `ConservationUnit`, `Stock`, `EscapementMeasurement`, `BroodYear`, `CatchYear`
- **Schemes**: `WSPBiologicalStatusZoneScheme` (Green/Amber/Red zones), `SalmonOriginScheme`, `EnumerationMethodScheme`, `EstimateMethodScheme`
- **Metrics**: `RelativeAbundanceMetric`, `LongTermTrendMetric`, `PercentChangeMetric`

See the [full ontology](https://w3id.org/gcdfos/salmon) and [repository](https://github.com/dfo-pacific-science/salmon-ontology) for all available terms.

## Development

1) Install R (>= 4.4.0) and required packages:
```r
install.packages(c("devtools", "roxygen2", "testthat", "knitr", "rmarkdown", 
                   "tibble", "readr", "jsonlite", "cli", "rlang", "dplyr", 
                   "tidyr", "purrr", "withr", "frictionless"))
```

2) Build and check:
```r
devtools::document()
devtools::test()
devtools::check()
devtools::build_vignettes()
pkgdown::build_site()
```

## Package Structure

- `R/`: Core functions for dictionary and package operations
- `inst/extdata/`: Example data files (NuSEDS coho sample)
- `tests/testthat/`: Automated tests
- `vignettes/`: Long-form documentation
- `docs/`: pkgdown site output

## Roadmap

- [x] Core dictionary and package functions
- [x] Tests and vignettes
- [x] Documentation with DFO Salmon Ontology IRIs
- [ ] GPT/LLM integration for semantic suggestion
- [ ] Additional export formats and validators
