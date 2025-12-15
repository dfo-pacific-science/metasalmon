# Salmon Data Package Functions Workflow

## Overview

This vignette demonstrates how to use the `metasalmon` package to create
and work with Salmon Data Packages. A Salmon Data Package is a
Frictionless Data Package extended with salmon-specific semantic fields
(IRIs, concept schemes) that make your data portable, discoverable, and
linked to the DFO Salmon Ontology.

## Quick Start: Wrap Your Data As-Is

The simplest workflow is to wrap your existing analysis-ready tables
into a Salmon Data Package with minimal metadata.

### Step 1: Load Your Data

``` r

library(metasalmon)
library(readr)

# Load your data (example: NuSEDS coho sample)
data_path <- system.file("extdata", "nuseds-fraser-coho-sample.csv", package = "metasalmon")
df <- read_csv(data_path, show_col_types = FALSE)
head(df)
```

### Step 2: Infer a Starter Dictionary

The
[`infer_dictionary()`](https://dfo-pacific-science.github.io/metasalmon/reference/infer_dictionary.md)
function analyzes your data frame and proposes a starter dictionary with
column types and roles:

``` r

dict <- infer_dictionary(
  df,
  dataset_id = "nuseds-fraser-coho-2024",
  table_id = "escapement"
)
dict
```

This creates a dictionary tibble with columns for metadata, types, and
semantic fields (IRIs). The semantic fields are left blank for you to
fill in manually or with GPT assistance (see the GPT collaboration
vignette).

### Step 3: Validate the Dictionary

Before using the dictionary, validate it:

``` r

validate_dictionary(dict)
```

This checks that required columns exist, value types are valid, and
there are no duplicate column names. If validation passes, you’re ready
to proceed.

### Step 4: Apply the Dictionary (Optional)

You can apply the dictionary to transform your data (rename columns,
coerce types, apply factor levels):

``` r

# Apply dictionary to transform data
transformed_df <- apply_salmon_dictionary(df, dict)
head(transformed_df)
```

This step is optional—you can package your data “as-is” without
transformation.

### Step 5: Create Dataset and Table Metadata

Create metadata tibbles describing your dataset and tables:

``` r

# Dataset-level metadata
dataset_meta <- tibble::tibble(
  dataset_id = "nuseds-fraser-coho-2024",
  title = "NuSEDS Fraser River Coho Escapement Data (Sample)",
  description = "Sample of coho salmon escapement data from PFMA 29",
  creator = "DFO",
  contact_name = NA_character_,
  contact_email = NA_character_,
  license = "Open Government License - Canada",
  temporal_start = "2001",
  temporal_end = "2024",
  spatial_extent = "PFMA 29",
  dataset_type = "monitoring",
  dataset_iri = NA_character_,
  source_citation = NA_character_
)

# Table-level metadata
table_meta <- tibble::tibble(
  dataset_id = "nuseds-fraser-coho-2024",
  table_id = "escapement",
  file_name = "escapement.csv",
  table_label = "Escapement Data",
  description = "Coho escapement counts and metadata",
  entity_type = NA_character_,
  entity_iri = NA_character_,
  primary_key = "POP_ID"
)
```

### Step 6: Create the Salmon Data Package

Assemble everything into a package:

``` r

# Prepare resources (named list of data frames)
resources <- list(escapement = df)

# Create package
pkg_path <- create_salmon_datapackage(
  resources = resources,
  dataset_meta = dataset_meta,
  table_meta = table_meta,
  dict = dict,
  path = tempdir(),
  format = "csv",
  overwrite = TRUE
)

# Check what was created
list.files(pkg_path)
```

The package includes: - `datapackage.json`: Frictionless Data Package
descriptor with semantic extensions - Resource files (CSV): Your data
tables

### Step 7: Read the Package Back

Load the package for analysis:

``` r

# Read package
pkg <- read_salmon_datapackage(pkg_path)

# Access components
pkg$dataset      # Dataset metadata
pkg$tables      # Table metadata
pkg$dictionary   # Column dictionary
pkg$resources    # Data tables (named list)

# Use the data
head(pkg$resources$escapement)
```

## Guided Semantic Wrap: Adding IRIs and Codes

For a richer package, add semantic annotations (IRIs linking to the DFO
Salmon Ontology) and code lists (controlled vocabularies).

### Enriching the Dictionary with IRIs

Edit your dictionary to add IRIs from the DFO Salmon Ontology:

``` r

# Example: Add OWL class IRIs for columns
# (An OWL class is a "type" in the ontology, e.g. EscapementMeasurement)
# Using DFO Salmon Ontology IRIs (https://w3id.org/gcdfo/salmon#)
dict$term_iri[dict$column_name == "SPECIES"] <- "https://w3id.org/gcdfo/salmon#Stock"
dict$term_type[dict$column_name == "SPECIES"] <- "owl_class"

# Example: Link an escapement measurement column
dict$term_iri[dict$column_name == "NATURAL_SPAWNERS_TOTAL"] <- "https://w3id.org/gcdfo/salmon#EscapementMeasurement"
dict$term_type[dict$column_name == "NATURAL_SPAWNERS_TOTAL"] <- "owl_class"
dict$unit_label[dict$column_name == "NATURAL_SPAWNERS_TOTAL"] <- "fish (count)"

# Validate again
validate_dictionary(dict, require_iris = FALSE)  # Set TRUE to require IRIs
```

### Adding Code Lists

Create a codes tibble for categorical fields:

``` r

codes <- tibble::tibble(
  dataset_id = "nuseds-fraser-coho-2024",
  table_id = "escapement",
  column_name = "RUN_TYPE",
  code_value = "FALL",
  code_label = "Fall run timing",
  code_description = NA_character_,
  concept_scheme_iri = NA_character_,
  term_iri = NA_character_,
  term_type = NA_character_
)

# Apply dictionary with codes to get factor levels
transformed_df <- apply_salmon_dictionary(df, dict, codes = codes)
```

### Creating the Enriched Package

Create the package with codes:

``` r

pkg_path <- create_salmon_datapackage(
  resources = resources,
  dataset_meta = dataset_meta,
  table_meta = table_meta,
  dict = dict,
  codes = codes,  # Include codes
  path = tempdir(),
  format = "csv",
  overwrite = TRUE
)
```

## Best Practices

1.  **Start simple**: Use
    [`infer_dictionary()`](https://dfo-pacific-science.github.io/metasalmon/reference/infer_dictionary.md)
    to bootstrap, then manually enrich semantic fields.
2.  **Validate early**: Run
    [`validate_dictionary()`](https://dfo-pacific-science.github.io/metasalmon/reference/validate_dictionary.md)
    before creating packages.
3.  **Keep data as-is**: You don’t need to transform data unless you
    want to; packaging preserves original structure.
4.  **Use codes for categoricals**: Code lists ensure consistent values
    and enable factor levels in R.
5.  **Link to ontology**: Add IRIs to connect your data to the DFO
    Salmon Ontology for interoperability. Key IRIs include:
    - `https://w3id.org/gcdfo/salmon#ConservationUnit` - Conservation
      Units
    - `https://w3id.org/gcdfo/salmon#Stock` - Stock entities
    - `https://w3id.org/gcdfo/salmon#EscapementMeasurement` - Escapement
      measurements
    - `https://w3id.org/gcdfo/salmon#BroodYear` - Brood year references
    - `https://w3id.org/gcdfo/salmon#CatchYear` - Catch year references
    - `https://w3id.org/gcdfo/salmon#WSPBiologicalStatusZoneScheme` -
      Status zone scheme (Green/Amber/Red)
    - `https://w3id.org/gcdfo/salmon#SalmonOriginScheme` - Origin scheme
      (natural/hatchery)
    - See the full ontology at <https://w3id.org/gcdfo/salmon> for all
      available terms

## Next Steps

- See the GPT collaboration vignette for using AI assistance to propose
  dictionaries and IRIs.
- Explore the `datapackage.json` file to understand the Frictionless
  Data Package structure.
- Check the DFO Salmon Ontology repository for available terms and IRIs.
